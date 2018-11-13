;; Based on a linear array, `symat' implements a maximally compact
;; representation of symmetrical matrices. The implementation takes advantage of
;; the fact that a given two-dimensional array can be "linearized" as a
;; one-dimensional array ("row-major order")

(defstruct (symat
	     ;; internal constructor used by make-symat:
	     (:constructor internal-make-symat (dimensions table)))
  dimensions
  table)

;; Redefine the default make-* constructor: 
(defun make-symat (dim)
  (internal-make-symat dim (make-array (/ (* dim (1+ dim)) 2))))

(defun symat-ref (sm i j)
  "Accessor function for the symat data type."
  (let ((i (min i j))
	(j (max i j)))  
    ;; Access is computed based on (a) the row-major order index
    ;; minus (b) the redundant values, corresponding to the sum of 
    ;; positive integers up to i:
    (aref (symat-table sm)
	  (- (+ j (* (symat-dimensions sm) i)) ;; (a)
	     (/ (* i (1+ i)) 2)))))            ;; (b)


(defun set-symat-ref (sm i j v)
  "Destructive modifier function to be used by setf."
  (let ((i (min i j))
	(j (max i j)))
    (setf (aref (symat-table sm)
		(- 
		 (+ j (* (symat-dimensions sm) i))
		 (/ (* i (1+ i)) 2)))
	  v)))

;; Make setf work with symat-ref:
(defsetf symat-ref set-symat-ref) 

(defmethod print-object ((sm symat) stream)
  "symat pretty-printing for the Lisp REPL."
  (print-unreadable-object (sm stream :type t :identity t)
    (format stream "[dim: ~d]" (symat-dimensions sm))))



;; Finally, below we add some functionality for mapping between
;; strings (words) and numerical ids. We'll later use the ids for
;; indexing the proximity matrix. We'll use the hash-table in the
;; vs-slot `string-map' for storing the mapping from strings to ids,
;; and the slot `id-map' for storing an array recording the reverse
;; mapping from numerical identifiers back to strings.



(defun map-strings-to-ids (vs)
  ;; We'll use an array for mapping from numerical ids to strings
  ;; (words), stored in the vs-slot `id-map':
  (setf (vs-id-map vs) (make-array (word-count vs)))
  
  ;; We'll use the hash-table in the slot `string-map' for mapping
  ;; strings to ids. Here we assign the mappings:
  (loop 
     with string-map = (vs-string-map vs)
     with id-map = (vs-id-map vs)
     for word being the hash-keys of (vs-matrix vs)
     for i from 0
     do 
       (setf (gethash word string-map) i)
       (setf (aref id-map i) word)))

(defun id2string (id vs)
  (aref (vs-id-map vs) id))

(defun string2id (string vs)
  (gethash string (vs-string-map vs)))

(defun compute-proximities (vs)
  "Computes and stores a proximity matrix for a given vs structure."
  (let* ((n (word-count vs))
	 (prox (make-symat n))
         (sim-fn (vs-similarity-fn vs)))

    ;; Initilize the mapping between words and numerical ids, as used
    ;; for accessing the proximity matrix:
    (map-strings-to-ids vs)

    (loop 
       for i below n
       for vec-i = (get-feature-vector vs (id2string i vs))
       do (loop 
	     for j from i below n 
	     for vec-j = (get-feature-vector vs (id2string j vs))
	     do (setf (symat-ref prox i j)
		      (funcall sim-fn vec-i vec-j))))
			      
    (setf (vs-proximity-matrix vs) prox)))

(defun get-proximity (vs word1 word2)
  "Retrieves the proximity (by look-up) for a given pair of words."
  (symat-ref (vs-proximity-matrix vs) 
             (string2id (normalize-token word1) vs)
             (string2id (normalize-token word2) vs)))

(defun find-knn (vs word &optional (k 5))
  "Return the k nearest neighbors for a given word in the v space."
  (let ((knn))
    (loop 
       with prox = (vs-proximity-matrix vs)
       with i = (string2id (normalize-token word) vs)
       for j below (word-count vs)
       unless (= i j)
       collect (cons j (symat-ref prox i j))
       into neighbors
       finally (setq knn
		     (subseq (sort neighbors #'> :key #'cdr)
			     0 k)))
    (mapcar #'(lambda (n) (list (id2string (car n) vs) (cdr n))) knn)))



#|

(setf vspace (length-normalize-vs
	      (read-corpus-to-vs "brown2.txt" "words.txt")))

(setf (vs-proximity-matrix vspace) (compute-proximities vspace))

;; (compute-proximities vspace)
;;
;; (find-knn vspace "egypt")
;; -> ("congo" "germany" "europe" "italy" "america")
;;
;; (find-knn vspace "butter" 20)
;; -> ("salt" "sauce" "eggs" "pepper" "mustard" "milk" "water" "toast" "meat")

(euclidian-length (get-feature-vector vspace "school"))

(vs-proximity-matrix vspace)

(get-proximity vspace "kennedy" "europe")
(get-proximity vspace "president" "kennedy")
(word-similarity vspace "kennedy" "president")
(get-proximity vspace "food" "bread")

(find-knn vspace "president")
(find-knn vspace "congo" 20)
(find-knn vspace "salt" )

|#
