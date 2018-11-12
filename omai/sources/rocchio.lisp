
;;;===================================================
;;;
;;; OMAI - AI tools for OM
;;;
;;;===================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;=====================================================
; Authors: Anders Vinjar, Jean Bresson - 2018
;=====================================================
;
; Implementation of the Rocchio algorithm for Information Retrieval
;
;=====================================================

(in-package :omai)

#|

;;; TEST SEQUENCE

(defparameter text (merge-pathnames "data/text.txt" (om::lib-resources-folder (om::find-library "omai"))))
(defparameter words (merge-pathnames "data/words.txt" (om::lib-resources-folder (om::find-library "omai"))))
(defparameter classes (merge-pathnames "data/classes.txt" (om::lib-resources-folder (om::find-library "omai"))))
(defparameter test-set (merge-pathnames "data/unknown.txt" (om::lib-resources-folder (om::find-library "omai"))))

;;; create a feature vector space with all the words in the corpus
;;; each words registers how many time another word is in the same phrase (normalized / length pf the phrases)

(defparameter vspace (make-vs))
(read-words vspace words)
(read-corpus vspace text)

;;; check the n most important words in the feature vector
(print-features vspace "congo" 20)

;;; check whether words have strong similarity (= strong correlations in their feature vectors)
(word-similarity vspace "africa" "congo")
(word-similarity vspace "africa" "america")
(word-similarity vspace "butter" "america")


;;; CLASSIFICATION
(read-classes vspace classes)
(rocchio-classify vspace "sheriff")

(with-open-file (stream test-set :direction :input)
  (loop for word = (read stream nil nil)
        while word
        collect (rocchio-classify vspace (normalize-token word))))

;; ==>
;;
;; (("fruit" :FOODSTUFF 0.3667868) ("california" :PERSON_NAME 0.29967904)
;;  ("peter" :PERSON_NAME 0.30886117) ("egypt" :PLACE_NAME 0.3074144)
;;  ("department" :INSTITUTION 0.5497112) ("hiroshima" :PLACE_NAME 0.2304558)
;;  ("robert" :PERSON_NAME 0.59566295) ("butter" :FOODSTUFF 0.3845482)
;;  ("pepper" :FOODSTUFF 0.36386278) ("asia" :PLACE_NAME 0.39868993)
;;  ("roosevelt" :TITLE 0.2597388) ("moscow" :PLACE_NAME 0.4841283)
;;  ("senator" :TITLE 0.35630104) ("university" :INSTITUTION 0.5805089)
;;  ("sheriff" :TITLE 0.22804837))

|#


;;;=================
;;; VECTOR SPACE
;;;=================
;;; Feature vectors are hash tables. Fine for very sparse vectors. 
;;; We also use a hash-table for storing feature vectors -> co-occurrence matrix is a hash-table of hash-tables.

(defstruct vs
  (matrix (make-hash-table :test #'equal))
  (similarity-fn #'dot-product)
  classes
  proximity-matrix
  (string-map (make-hash-table :test #'equal))
  id-map)

(defun get-feature-vector (vs thing)
  "Retrieves the feature vector for a given word/string."
  (gethash thing (vs-matrix vs)))


(defun print-features (vs thing n &optional print)
  "Prints a ranked list of n context features for a given thing."

  (let ((vector (get-feature-vector vs (normalize-token thing))))
    
    (if vector
        (let ((sorted
               (loop for feat being the hash-keys of vector
                     using (hash-value val)
                     collect (cons feat val) into values
                     finally (return (sort values #'> :key #'cdr)))))
          ;; print the top n features:
          
            (loop 
             for i from 1 to n 
             for (feat . val) in sorted
             do (when print 
                  (format t "~&~a ~a~%" feat val))
             collect (cons feat val)))
    
          (format t "~&ERROR: ~S in not registered in the vector space~%" thing)
       
      )))


;; Classification data:
;; each class label is used as a key in a hash-table, where for each class we
;; store a property list with two keys, :members and :centroid.

(defun get-class-centroid (name vs)
  (getf (gethash name (vs-classes vs)) :centroid))

(defun get-class-members (name vs)
  (getf (gethash name (vs-classes vs)) :members))
     


;;;=================
;;; INITIALIZATION WITH A LIST OF WORDS
;;;=================

(defun normalize-token (string)
 "Text normalization, to be applied to individual tokens."
 (string-downcase 
  (string-trim " ,.!?(){}[]-+@&\";:'*#" string)))

(defmethod read-words (vs (words list))
  (loop for word in words
        ;; create a feature vector for each word:
        do (setf (gethash (normalize-token word) (vs-matrix vs))
                 (make-hash-table :test #'equal)))
  vs)

(defun wordlist-from-file (file)
  (with-open-file (stream file :direction :input)
    (loop for word = (read stream nil nil)
          while word collect word)))

(defmethod read-words (vs (words pathname))
  (read-words vs (wordlist-from-file words)))


;;;=================
;;; TEXT TO VECTOR SPACE
;;;=================

(defparameter *stop-list*
   '("a" "about" "also" "an" "and" "any" "are" "as" "at" 
     "be" "been" "but" "by" "can" "could" "do" "for" "from" 
     "had" "has" "have" "he" "her" "him" "his" "how" "i" "if" 
     "in" "is" "it" "its" "la" "may" "most" "new" "no" "not" 
     "of" "on" "or" "she" "some" "such" "than" "that" "the" 
     "their" "them" "there" "these" "they" "this" "those" "to" 
     "was" "we" "were" "what" "when" "where" "which" "who" 
     "will" "with" "would" "you"))


(defparameter *stop-words* (make-hash-table :test #'equal))

(defun stop-word-p (w)
  "Predicate that checks whether a word is on the stop-list."
  (gethash w *stop-words*))

(defmethod add-words-to-stop-list ((words string))
  (setf (gethash (string-downcase words) *stop-words*) T))

(defmethod add-words-to-stop-list ((words t)) 
  (error "the stop-list only accepts strings!"))

(defmethod add-words-to-stop-list ((words list)) 
  (loop for word in words do 
        (add-words-to-stop-list word)))

(defun init-stop-list (&optional words)
  (clrhash *stop-words*)
  (add-words-to-stop-list words)
  *stop-words*)

;;; INIT WITH *STOP-LIST*
(init-stop-list *stop-list*)

(defun tokenize (string)
  "Splits a sequence into tokens, filters stop-words and normalizes."
  (loop for start = 0 then (+ space 1)
        for space = (position #\space string :start start)
        for token = (normalize-token (subseq string start space))
        unless (or (string= token "") 
                   (stop-word-p token))
        collect token
        until (not space)))


;;;=================
;;; SIMILARITY
;;;=================

(defun dot-product  (hash1 hash2)
  "Computes the inner product of two feature vectors."
  (loop 
      for id1 being the hash-keys of hash1
      for val2 = (gethash id1 hash2)
      when val2
      sum (* (gethash id1 hash1) val2)))


(defun word-similarity (vs w1 w2)
  "Computes the similarity of two word strings in the space."
  (let ((v1 (get-feature-vector vs (normalize-token w1)))
	(v2 (get-feature-vector vs (normalize-token w2))))
    (when (and v1 v2)
      (funcall (vs-similarity-fn vs) v1 v2))))


;;;=================
;;; VECTOR NORMALIZATION
;;;=================
; (euclidean-length (get-feature-vector vspace "congo"))

(defun euclidean-length (feature-vector)
  "Computes the Euclidean norm of a feature vector."
  (sqrt
   (loop
      for value being each hash-value in feature-vector
      sum (expt value 2))))

(defun length-normalize-vector (hash)
  "Destructively modifies a vector to have unit length."
  (loop 
      with length = (euclidean-length hash)
      for j being the hash-keys of hash
      using (hash-value n)
      do (setf (gethash j hash)
	   (/ n length)))
  hash)

(defun length-normalize-vs (vs)
  "Normalizes all vectors in a vector space to have unit length."
  (loop
     for vec being the hash-values of (vs-matrix vs)
     do (length-normalize-vector vec))
  vs)



;;;=================
;;; LEARN
;;;=================

;;; all words in the same lines of a given word are considered its "features"
;;; this function creates these vectors for a series of words
;; record bag-of-words features from the corpus, 
;; updating the counts in the feature vectors: 
(defun vs-learn-line-of-text (vs line)
  ;; nested loop to (1) find target words, and (2) for each
  ;; target extract features and update its feature vector:
  (loop with tokens = (tokenize line)
        for token in tokens
        for i from 0
        for feat-vect = (get-feature-vector vs token)
        when feat-vect ;; only the initialized words are considered  
        do (loop for feature in tokens
                 for j from 0
                 ;;; we don't count a token occurrence as a feature of itself:
                 unless (= i j)
                 do (incf (gethash feature feat-vect 0)))
        ))

; Learns from corpus  
(defmethod read-corpus (vs (corpus pathname))
  (with-open-file (stream corpus :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          do (vs-learn-line-of-text vs line))
    )
  (length-normalize-vs vs))

(defmethod read-corpus (vs (corpus-lines list))
  (loop for line in corpus-lines
        do (vs-learn-line-of-text vs line))
  (length-normalize-vs vs))


;;;=================
;;; CLASSIFICATION
;;;=================

(defun sum-vectors (&rest vectors)
  (let ((sum (make-hash-table :test #'equal)))
    (dolist (vec vectors)
      (maphash #'(lambda (dim val)
		   (incf (gethash dim sum 0) val))
	       vec))
    sum))
          
(defun vector-average (&rest vectors)
  "Computes a centroid for an arbitrary number of vectors."
  (let ((n (length vectors))
        (sum (apply #'sum-vectors vectors)))
    (maphash #'(lambda (dim val)
                 (setf (gethash dim sum) (/ val n)))
             sum)
    sum))


(defmethod load-classes (class-ht (classes list))
  (loop for list in classes
        for class = (first list)
        for members = (mapcar #'normalize-token (second list))
        do (setf (gethash class class-ht) (list :members members)))
  class-ht)

(defmethod load-classes (class-ht (classes pathname))
  (with-open-file (stream classes)
    (loop for list = (read stream nil nil)
          for class = (first list)
          for members = (mapcar #'normalize-token (second list))
          while list
          do (setf (gethash class class-ht) (list :members members)))
    class-ht))


(defun compute-class-centroids (vs)
  "Compute and store the average-vectors for each class."
  (loop 
   with classes = (vs-classes vs)      
   for label being the hash-keys of classes
   using (hash-value class)
   for words = (get-class-members label vs)
   for vectors = (remove nil (mapcar #'(lambda (w) (get-feature-vector vs w)) words))
   for centroid = (length-normalize-vector (apply #'vector-average vectors))
   ;;; for each class label, store the centroid in the 
   ;;; property list we created in `read-classes':
   do (setf (gethash label classes)
            (append (list :centroid centroid) class)))
  vs)


;;; MAIN CALL TO LOAD CLASSES
;;; reinitializes the class vector each time 
(defun read-classes (vs classes)
  "Read class file and store the information in the vs structure."
  (let ((class-ht (make-hash-table)))
    
    (load-classes class-ht classes)
    (setf (vs-classes vs) class-ht)
    
    (compute-class-centroids vs)
    
    vs))



(defun rocchio-classify (vs word)
  "Classify all words labeled :unknown according to centroid distance."
  (let* ((sim-fn (vs-similarity-fn vs))
         (classes (vs-classes vs))
         (vector (get-feature-vector vs word)))
    
    (cond ((null vector)
           (format t "ERROR: ~&~S is not in the vector space!" word))
          ((null classes) 
           (format t "ERROR: no loaded classes!"))
          (t 
           (let ((named-centroids (loop for label being the hash-keys of classes 
                                        collect (cons label (get-class-centroid label vs)))))
                 (loop with max-label with max-sim = 0
                       for (label . center) in named-centroids
                       for sim = (funcall sim-fn vector center)
                       when (> sim max-sim)
                       do (setq max-label label max-sim sim)
                       finally (return (list max-label max-sim)))
                 )))
    ))
   





#|


(defun read-corpus-to-vs (corpus wordlist)
  "Returns a vector space model based on the tokens in the corpus."
  (let ((vs (make-vs)))
    ;; read in the list of words to be modeled:
    (with-open-file (stream wordlist :direction :input)
      (loop with matrix = (vs-matrix vs)
            for word = (read stream nil nil)
            while word 
            ;; create a feature vector for each word:
            do (setf (gethash (normalize-token word) matrix)
                     (make-hash-table :test #'equal))))
    
   ;; record bag-of-words features from the corpus, 
   ;; updating the counts in the feature vectors:
    (with-open-file (stream corpus :direction :input)
      (loop
	 for line = (read-line stream nil nil)
	 while line
	 do
	 ;; nested loop to (1) find target words, and (2) for each
	 ;; target extract features and update its feature vector:
	   (loop
	      with tokens = (tokenize line)
	      for token in tokens
	      for i from 0
	      for feat-vect = (get-feature-vector vs token)
	      when feat-vect ;; only a subset of the words are registered  
	      do 
		(loop 		      
		   for feature in tokens
		   for j from 0
		   ;;; we don't count a token occurrence as a feature of itself:
		   unless (= i j)
		   do (incf (gethash feature feat-vect 0))))))
    vs))


(defun rocchio-classify (vs)
  "Classify all words labeled :unknown according to centroid distance."
  (loop 
     with sim-fn = (vs-similarity-fn vs)
     with classes = (vs-classes vs)
     with unknown-words = (get-class-members :unknown vs)
     with unknown-vectors = (retrieve-vectors vs unknown-words)
     with named-centroids = (loop 
			       for label being the hash-keys of classes 
			       unless (eq label :unknown)
			       collect (cons label (get-class-centroid label vs)))
     for word in unknown-words
     for vec in unknown-vectors
     ;; Find the class label with the closest centroid 
     ;; for each unknown (unlabeled) word. 
     collect 
       (loop with max-label with max-sim = 0
	  for (label . center) in named-centroids
	  for sim = (funcall sim-fn vec center)
	  when (> sim max-sim)
	  do (setq max-label label max-sim sim)
	  finally (return (list word max-label max-sim)))))
|#


;;;==================================
;;; OM OBJECT
;;;==================================

(om::defclass! text-analyzer ()
  ((corpus :initform nil :initarg :corpus :accessor corpus)
   (words :initform nil :initarg :words :accessor words)
   (classes :initform nil :initarg :classes :accessor classes)
   (vs :initform nil :accessor vs)))
      

(defmethod initialize-instance :after ((self text-analyzer) &rest args)
  (setf (vs self) (make-vs))
  (read-words (vs self) (words self))
  (read-corpus (vs self) (corpus self))
  (read-classes (vs self) (classes self))
  self)

(om::defmethod! word-features ((ta text-analyzer) (word string) (n integer))
  :icon :omai
  :initvals '(nil nil 10)
  (print-features (vs ta) word n nil))

(om::defmethod! get-similarty ((ta text-analyzer) (word1 string) (word2 string))
  :icon :omai
  (word-similarity (vs ta) word1 word2))

(om::defmethod! get-word-class ((ta text-analyzer) (word string))
  :icon :omai
  (rocchio-classify (vs ta) word))
