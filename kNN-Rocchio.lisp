(in-package :cl-user)

;; Time-stamp: <2018-06-12 15:12:31 andersvi>

;;;; Rocchio classifier

;;; >>> [TOKENIZATION] <<<
;;;
;;; 1) FIND WORD BOUNDARIES IN LINES (STRINGS):
;;;

;;; characaters to check for end-of-word boundaries
(defparameter *word-separators* '(#\Space #\tab #\Newline
				  #\' #\- #\* #\. #\, #\?
				  #\: #\; #\) #\(  #\" #\!
				  #\& #\/ #\$))

;; used to find word boundaries in input string
(defun find-word-end (string start)
  (position-if  #'(lambda (char) (member char *word-separators*))
		string
		:start start))

;; list of function words:

(defparameter *stop-list*
  '("a" "about" "also" "an" "and" "any" "are" "as" "at" "be" "been"
    "but" "by" "can" "could" "do" "for" "from" "had" "has" "have"
    "he" "her" "him" "his" "how" "i" "if" "in" "is" "it" "its" "la"
    "may" "most" "new" "no" "not" "of" "on" "or" "she" "some" "such"
    "than" "that" "the" "their" "them" "there" "these" "they" "this"
    "those" "to" "was" "we" "were" "what" "when" "where" "which"
    "who" "will" "with" "would" "you"))

;; converting to a hash-table

(defparameter *stop-list-ht*
  (let ((sl (make-hash-table :test #'equal)))
    (loop for word in *stop-list*
       do (setf (gethash word sl) t))
    sl))


;; TOKENIZATION
;;
;; check for cruft in parsed words - empty strings, dangling s's, single-letter
;; abbreviations...

(defun empty-string-p (token) (equal token ""))

;; a list of single-character words in English to allow through.  Some of these are
;; already included in the *stop-list-ht* parameter above, but could well be regarded
;; as a special case here, to allow filtering of single-character noise below.

(defparameter *single-char-words* '("i" "a" "o"))

;; Handle single letter words, which - with the exceptions above - presumably should
;; be disregarded as tokens.  E.g. apostrophe 's, 2

(defun single-letter-abbreviation-p (token)
  (and (= (length token) 1)
       (not (member token *single-char-words*))))

;; NUMERICS = classified as non-tokens here
;;
;; Exclude some potentially significant tokens:

(defparameter *digit-tokens-accepted* '("2d" "3d"))

;; check for written ordinals:
;;
;; checking start to get rid of most, and skip costly search for word endings:

(defparameter *ordinals-beginnings*
  (mapcar #'(lambda (i) (format nil "~R" i) )
	  (loop for i from 0 to 9 collect i)))

(defparameter *ordinals-endings*
  (append (mapcan #'(lambda (i)
		      (mapcar #'(lambda (frmt) (format nil frmt i))
			      '("~R" "~:R")))
		  (append '(0)
			  (loop for i from 1 to 9 collect i)
			  (loop for i from 1 to 9 collect (* i 10))))
	  '("hundred"  "thousand"  "illion" "hundredth" "thousandth" "illionth")))

(defun written-ordinal-p (token)
  (or
   (loop for num in *ordinals-endings*
      for startindex = (max 0 (- (length token) (length num)))
      when (search num token :from-end t :test #'equal :start2 startindex)
      return t)
   (and (loop for beg in *ordinals-beginnings*
	   when (search beg token)
	   do (return t))
	(loop for num in *ordinals-endings*
	   for startindex = (max 0 (- (length token) (length num)))
	   when (search num token :from-end t :test #'equal :start2 startindex)
	   return t))))

(defun any-number-p (token)
  (or (and (digit-char-p (elt token 0))
	   (not (member token *digit-tokens-accepted* :test #'equal)))
      (written-ordinal-p token)))

;; The various checks are combined in a predicate to filter out parsed data we don't
;; want to recognize as tokens.  Trying to order the list by cost, most expensive at
;; bottom:

(defun non-word-p (token)
  (or
   (empty-string-p token)		    ; empty strings
   (single-letter-abbreviation-p token)	    ; single letter cruft, abbreviations
   (gethash token *stop-list-ht*)	    ; function words
   (any-number-p token)			    ; numbers
   ))

(defun normalize-token (token)
  (string-downcase token))

;; #'tokenize - split sentence in separate words, collect those recognized as tokens
;; into list:

(defun tokenize (string-of-words)
  (loop
     for word-start = 0 then (+ word-end 1)
     for word-end = (find-word-end string-of-words word-start)
     for token = (normalize-token (subseq string-of-words word-start word-end))
     until (not word-end)
     unless (non-word-p token)
     collect token))


;;;  >>> [END TOKENIZATION] <<<

;;;
;;; >>> [VECTOR SPACE: LOOK UP, STORAGE, POPULATE] <<<
;;;

;; HELPER FUNCTIONS

;; LOOKUP - mapping dictionary <-> array (strings <-> integer indexes)

(declaim (inline index-to-term term-to-index find-token))

(defun index-to-term (arr index)
  (aref arr index))

(defun term-to-index (term dictionary)
  (gethash term dictionary))

(defun set-term-to-index (term wordset index)
  (setf (gethash term wordset) index))

(defsetf term-to-index set-term-to-index)

(defun find-token (index table)
  (index-to-term table index))

;; - keying by index (integer, from dictionary)

;; UPDATE FEATURE VECTOR WITH MATCH:

(defun add-or-increment-feature (idx feature vs)
  (incf (gethash feature (aref (vs-matrix vs) idx) 0)))

;; For every context (BoW), update vector space with feature-vectors:

(defun update-vs (sentence vs)
  (loop for token in sentence
     for word-index = (gethash token (vs-dictionary vs))
     when word-index
     ;; remaining tokens in the same sentence are features -> add or increment:
     do (let ((context (remove token sentence :test #'equal :count 1)))
	  (dolist (feature context)
	    (add-or-increment-feature word-index feature vs)))))

;; Read terms to model and return count, to be used for allocating storage:

(defun count-terms (file-with-newline-separated-terms)
  (with-open-file (in file-with-newline-separated-terms)
    (loop
       for N from 0
       for word = (read in nil nil)
       while word
       finally (return N))))

;; Set up storage for vector space and mapping dictionary.  Read in terms with #'read
;; - saving some space, and thus time - store lowercase string in mapping dictionary,
;; return storage:

(defun read-terms-from-file (file)
  (let ((n (count-terms file)))
    ;; set up storage
    (let ((table (make-array n :initial-contents (loop repeat n collect (make-hash-table :test #'equal))))
	  (dictionary (make-hash-table :size n :test #'equal)))
      ;; read mappings into dictionary
      (with-open-file (input file)
	(loop
	   for i from 0 below n
	   for word = (read input nil nil)
	   for term = (format nil "~(~A~)" word)
	   do (setf (term-to-index term dictionary) i)))
      ;; return both tables
      (values table dictionary))))

;; read corpus, tokenize, return populated vector space:

(defun read-corpus-to-vs (corpus wordset)
  (multiple-value-bind (table dictionary)
      (read-terms-from-file wordset)
    (let ((vs (make-vs :matrix table :dictionary dictionary)))
      (with-open-file (file-stream corpus)
	(loop
	   for sentence = (read-line file-stream nil nil)
	   while sentence
	   do (update-vs (tokenize sentence) vs)))
      vs)))

;;; >>> [END: VECTOR SPACE: LOOK UP, STORAGE, POPULATE] <<<

(defun get-feature-vector (vs word)
  (let ((idx (term-to-index word (vs-dictionary vs))))
    (when idx
      (svref (vs-matrix vs) idx))))

(defun sort-feature-vector (vec)
  (let ((feature-list '()))
    (maphash #'(lambda (key val) (push (cons key val) feature-list)) vec)
    (sort feature-list #'> :key #'cdr)))

(defun print-features (vs word k)
  (let ((sorted-features (sort-feature-vector (get-feature-vector vs word))))
    (loop for i from 0 below k
       for (feature . value) in sorted-features
       do (format t "~&~A~20T~A" feature value))))

;;;;
;;;; VECTOR OPERATIONS
;;;;

(defun euclidian-length (feature-vector)
  (the double-float					    ;only real values here
       (sqrt
	(loop
	   for value being each hash-value in feature-vector
	   sum (expt value 2)))))

(defun normalize-vector (vector)
  ;; return normalized vector
  (let ((real-length (euclidian-length vector)))
    (maphash #'(lambda (key val)
		 (setf (gethash key vector) (/ val real-length)))
	     vector)
    vector))

(defun length-normalize-vs (space)
  (map nil #'(lambda (vector)
	       (normalize-vector vector))
       (vs-matrix space))
  space)

(defun dot-product (fvec-A fvec-B)
  (when (and fvec-A fvec-B)
    (let* ((sorted-vectors (sort (list fvec-A fvec-B) #'< :key #'hash-table-count))
	   (A (pop sorted-vectors))
	   (B (pop sorted-vectors)))
      (loop
	 for feature-A being each hash-key in A
	 for value-A = (gethash feature-A A)
	 for value-B = (gethash feature-A B)
	 when value-B
	 sum (* value-A value-B)))))

(defun word-similarity (space term-1 term-2)
  (let ((vec-1 (get-feature-vector space term-1))
	(vec-2 (get-feature-vector space term-2)))
    (and vec-1 vec-2
	 (funcall (vs-similarity-fn space) vec-1 vec-2))))


;;; READING IN CORPUS/DATA

(defstruct vs
  matrix
  dictionary
  (similarity-fn #'dot-product)
  classes
  proximity-matrix)

(defun vs-matrix-size (space)
  (car (array-dimensions (vs-matrix space))))

;;;
;;; 1. COMPUTING A PROXIMITY MATRIX AND EXTRACTING KNN RELATIONS
;;;

;;
;; (a) compute proximites
;;
;; need only one value for symmetrical lookup (["egg","hen"] = ["hen",egg"]) - store
;; in 2d diagonal structure

#|

;; TESTING - compare structures for storing proximity-arrays:

;; 2d 'symmetrical' array version
(defun compute-proximities-array (space)
  (let* ((msiz (vs-matrix-size space))
	 (sim-matrix (make-array msiz
				 :initial-contents (loop for j from 1 to msiz
						      collect (make-array j :fill-pointer 0)))))
    (loop
       with matrix = (vs-matrix space)
       with sim-fn = (vs-similarity-fn space)
       for i from 0 below msiz
       for k_i = (aref matrix i)			    ;term 1 ;
       do
	 (loop
	    for j from 0 to i
	    for k_j = (aref matrix j)			    ;term 2 ;
	    when (and k_i k_j)
	    do (setf (aref (aref sim-matrix i) j)
		     (funcall sim-fn k_i k_j))))
    sim-matrix))

;; hash-table-version
(defun compute-proximities-ht (space)
  (let* ((siz (vs-matrix-size space))
	 (sim-matrix (make-hash-table :test 'equal)))	    ;keys are pairs
    (loop
       with matrix = (vs-matrix space)
       with sim-fn = (vs-similarity-fn space)
       ;; for i from 0 below siz
       for i from 0 below siz
       for k_i = (aref matrix i)
       do (loop
	     for j from 0 to i
	     for k_j = (aref matrix j)
	     when (and k_i k_j)
	     do (setf (gethash (cons i j) sim-matrix) (funcall sim-fn k_i k_j))))
    sim-matrix))

;; assoc version - 122 terms
(defun compute-proximities-assoc (space)
  (let* ((siz (vs-matrix-size space))
	 (sim-matrix nil))
    (loop
       with matrix = (vs-matrix space)
       with sim-fn = (vs-similarity-fn space)
       ;; for i from 0 below siz
       for i from 0 below siz
       for k_i = (aref matrix i)
       do (loop
	     for j from 0 to i
	     for k_j = (aref matrix j)
	     when (and k_i k_j)
	     do (setq sim-matrix (acons (cons i j)
					(funcall sim-fn k_i k_j)
					sim-matrix))))
    sim-matrix))

(time (compute-proximities-array *vs*))
  Elapsed time =        0.586
  Allocation   = 8921816 bytes

(time (compute-proximities-assoc *vs*))
  Elapsed time =        0.637
  Allocation   = 9154256 bytes

(time (compute-proximities-ht *vs*))
  Elapsed time =        0.641
  Allocation   = 9181768 bytes

 ==> going for array version

|#

;; compute proximities:
;;

(defun compute-proximities (space)
  (let* ((msiz (vs-matrix-size space))
	 (sim-matrix (make-array msiz
				 :initial-contents (loop for j from 1 to msiz
						      collect (make-array j :fill-pointer 0)))))
    (loop
       with matrix = (vs-matrix space)
       with sim-fn = (vs-similarity-fn space)
       for i from 0 below msiz
       for k_i = (aref matrix i)			    ;term 1
       do (loop
	     for j from 0 to i
	     for k_j = (aref matrix j)			    ;term 2
	     when (and k_i k_j)
	     do (setf (aref (aref sim-matrix i) j)
		      (funcall sim-fn k_i k_j))))
    sim-matrix))

(defun lookup-in-proximity-table (i j proxim-matrix)
  ;; symmetrix matrix: lookup in one side of diagonal
  (aref (aref proxim-matrix (max i j)) (min i j)))

(defun token-from-vs (token space)
  (term-to-index token (vs-dictionary space)))

(defun get-proximity (space word1 word2)
  (let ((pm (vs-proximity-matrix space))
	(i (token-from-vs word1 space))
	(j (token-from-vs word2 space)))
    (and i j
	 (lookup-in-proximity-table i j pm))))

(defun lookup-word-from-index (index dict)
  (with-hash-table-iterator (func dict)
    (loop (multiple-value-bind (entry? key val)
	      (func)
	    (if (and entry? (= val index))
		(return key))))))

(defun find-knn (space word1 &optional (k 5))
  (let ((msiz (vs-matrix-size space))
	(dict (vs-dictionary space)))
    (let ((sorted-neighbors
	   (cdr
	    (sort (loop
		     for i from 0 below msiz
		     for word2 = (lookup-word-from-index i dict)
		     for proximity = (get-proximity space word1 word2)
		     when proximity
		     collect (cons word2 proximity))
		  #'> :key #'cdr))))
      (loop
	 for i from 0 below k
	 for neighbor-w-weight in sorted-neighbors
	 collect neighbor-w-weight))))


#|

;; do some work

;; (defparameter words-file
;;   (make-pathname :directory (pathname-directory *load-pathname*) :name "words.txt"))

(defparameter words-file "words.txt")

(read-corpus-to-vs "brown2.txt" "words.txt")
(defparameter *vs*
  (length-normalize-vs
   (read-corpus-to-vs "brown2.txt" "words.txt")))

(setf (vs-similarity-fn *vs*) #'dot-product)

(euclidian-length (get-feature-vector *vs* "school"))

(time (setf (vs-proximity-matrix *vs*) (compute-proximities *vs*)))

(get-proximity *vs* "kennedy" "europe")
(get-proximity *vs* "president" "kennedy")
(word-similarity *vs* "kennedy" "president")
(get-proximity *vs* "cake" "bread")

(print-features *vs* "congo" 20)

(find-knn *vs* "president")
(find-knn *vs* "congo" 20)
(find-knn *vs* "salt" 15)

(word-similarity *vs* "africa" "congo")
(word-similarity *vs* "africa" "america")
(word-similarity *vs* "butter" "america")


|#


;;;
;;; ROCCHIO CLASSIFIER
;;


;;
;;  READ "classes.txt"
;;

;; setting up a class word-class with relevant slots - easy to subclass, extend and
;; specialize for any later needs.  Perhaps faster to use defstruct most of the
;; time...

(defclass word-class ()
  ((class-name :initarg :name :accessor class-name)
   (class-members :initarg :members :accessor class-members)
   (class-centroid :accessor class-centroid)))

(defmacro make-word-class (name &rest initargs)
  `(make-instance 'word-class :name ,name ,@initargs))

(defmethod print-object ((obj word-class) (stream t))
  (print-unreadable-object (obj stream :type t)
    (format stream
	    "Name: ~S, Members: (~{~S~^ ~}), Centroid: ~A"
	    (class-name obj)
	    (class-members obj)
	    (class-centroid obj))))

;; #'read-classes - storing instances in alist:

(defun lookup-word-class (word-class space)
  (cdr (assoc word-class (vs-classes space))))

(defun read-classes (space input-file)
  (let ((class-table '()))
    (with-open-file (in input-file)
      (loop
	 for class = (read in nil nil)
	 while class
	 for class-name = (car class)
	 do (setf class-table
		  (acons class-name
			 (make-word-class class-name
					  :members (mapcar #'(lambda (symbol)
							       (format nil "~(~A~)" symbol))
							   (cadr class)))
			 class-table))))
    (setf (vs-classes space) class-table)))

#|

;; do some work

(read-classes *vs* "classes.txt")

|#

;;;
;;; 2 (b) COMPUTE-CLASS-CENTROIDS
;;;

(defun compute-one-class-centroid (space class)
  (when class
    (let* ((centroid (make-hash-table :test 'equal))
	   (members (class-members class))
	   (N (length members)))
      (loop
	 for member in members
	 do (maphash #'(lambda (key val)
			 (incf (gethash key centroid 0) (/ val N)))
		     (get-feature-vector space member)))
      centroid)))

(defun store-class-centroid-in-class (class space)
  (setf (class-centroid class)
	(normalize-vector
	 (compute-one-class-centroid space class))))

(defun compute-class-centroids (space)
  (mapc #'(lambda (class)
	    (store-class-centroid-in-class (cdr class) space))
	(vs-classes space)))

#|

;; do some work

(compute-class-centroids *vs*)

|#


;;;
;;; 2 (c) ROCCHIO CLASSIFIER
;;;

(defun class-names-and-centroids (space)
  (loop
     for word-class in (vs-classes space)
     for this-class = (cdr word-class)
     unless (eq (class-name this-class) :unknown)
     collect (cons (class-name this-class) (class-centroid this-class))))

(defun closest-centroid (word-vector classes-and-centroids similarity-func)
  (first
   (sort (loop for (word-class . centroid) in classes-and-centroids
	    for proximity = (funcall similarity-func word-vector centroid)
	    collect (list word-class proximity))
	 #'>
	 :key #'cadr)))

(defun rocchio-classify (space)
  (let ((unlabeled (lookup-word-class :unknown space))
	(classes-and-centroids (class-names-and-centroids space)))
    (loop for word in (class-members unlabeled)
       for word-vector = (get-feature-vector space word)
       collect (cons word
		     (closest-centroid word-vector
				       classes-and-centroids
				       (vs-similarity-fn space))))))

#|

;; do some work

(rocchio-classify *vs*)

|#
