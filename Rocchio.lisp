;;; Feature vectors aare hash tables. Fine for very sparse vectors. We also use
;;; a hash-table for storing feature vectors -> co-occurrence matrix is a
;;; hash-table of hash-tables.

#|
(defparameter vspace 
  (length-normalize-vs
    (read-corpus-to-vs "brown2.txt" "words.txt")))

(euclidean-length (get-feature-vector vspace "congo"))

(print-features vspace "congo" 20)

(word-similarity vspace "africa" "america")
|#

(defstruct vs
  (matrix (make-hash-table :test #'equal))
  (similarity-fn #'dot-product)
  classes
  proximity-matrix
  (string-map (make-hash-table :test #'equal))
  id-map)

;;;
;;; tokenize, parse, split, filter...
;;;

(defun get-feature-vector (vs thing)
  "Retrieves the feature vector for a given word/string."
  (gethash thing (vs-matrix vs)))

(defun print-features (vs thing n)
  "Prints a ranked list of n context features for a given thing."
  (let ((sorted
	 (loop 
	    with vector = (get-feature-vector vs (normalize-token thing))
	    for feat being the hash-keys of vector
	    using (hash-value val)
	    collect (cons val feat) into values
	    finally (return (sort values #'> :key #'first)))))
    ;; print the top n features:
    (loop 
       for i from 1 to n 
       for (val . feat) in sorted
       do (format t "~&~a ~a~%" feat val))))


;; (defun squared-sum (hash)
;;   "Computes the squared sum of a feature vector"
;;   (loop
;;       for i being the hash-values of hash
;;       sum (expt i 2)))

;; (defun euclidean-length (hash)
;;   "Computes the Euclidean norm of a feature vector."
;;   (sqrt (squared-sum hash)))

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

#|

(defparameter vspace 
  (length-normalize-vs
    (read-corpus-to-vs "brown2.txt" "words.txt")))

(euclidean-length (get-feature-vector vspace "congo"))

(print-features vspace "congo" 20)

(word-similarity vspace "africa" "congo")
(word-similarity vspace "africa" "america")
(word-similarity vspace "butter" "america")

|#

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

(defun retrieve-vectors (vs words)
  "Return a set of feature vectors for a given set of words."
  (mapcar #'(lambda (w) (get-feature-vector vs w)) words))

(defun get-class-centroid (name vs)
  (getf (gethash name (vs-classes vs)) :centroid))

(defun get-class-members (name vs)
  (getf (gethash name (vs-classes vs)) :members))
     
(defun compute-class-centroids (vs)
 "Compute and store the average-vectors for each class."
 (loop 
    with classes = (vs-classes vs)      
    for label being the hash-keys of classes
    using (hash-value class)
    for words = (get-class-members label vs)
    for vectors = (retrieve-vectors vs words)
    for centroid = (length-normalize-vector 
		    (apply #'vector-average vectors))
    ;;; For each class label, store the centroid in the 
    ;;; property list we created in `read-classes':
    do (setf (gethash label classes)
	     (append (list :centroid centroid) class)))
 vs)

;; Each class label is used as a key in a hash-table, where for each class we
;; store a property list with two keys, :members and :centroid.

(defun read-classes (vs file)
  "Read class file and store the information in the vs structure."
  (let ((classes (make-hash-table)))
    (with-open-file (stream file)
      (loop 
          for list = (read stream nil nil)
          for class = (first list)
          for members = (mapcar #'normalize-token (second list))
          while list
          do (setf (gethash class classes)
               (list :members members))
          finally (setf (vs-classes vs) classes)))
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

;; example call sequence:

#|

(setf vspace (read-corpus-to-vs "brown2.txt" "words.txt"))
(setf vspace (length-normalize-vs vspace))

(read-classes vspace "classes.txt")
(compute-class-centroids vspace)
(rocchio-classify vspace)

|#

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

