
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
; Vector space: support for clustering and classification tools
;
;=====================================================

(in-package :omai)

;;;=================
;;; VECTOR SPACE
;;;=================
;;; vectors are stored in a hash-table (fine for sparse vectors). 
;;; We also use a hash-table for storing each feature vector 
;;; -> hash-table of hash-tables.


;;;=================
;;; CONSRUCTION
;;;=================

(defun initialize-vector-space (things)
  
  (declare (type (or list hash-table) things))

  (typecase things
    ;;; already good
    (hash-table things)
    
    ;;; convert the list in hash-table
    (list 
     (let ((vectors (make-hash-table :test 'equal)))
       (loop for thing in things
             ;; create a feature vector for each thing
             do (if (consp thing)
                    ;;; a pair key.vector (ht)
                    (setf (gethash (car thing) vectors)
                          (cdr thing))
                  ;;; an empty vector
                  (setf (gethash thing vectors)
                        (make-hash-table :test #'equal))))
       vectors))
    
    ;;; problem
    (otherwise 
     (format t "~&ERROR: the vector space must be initialized with a list or a hash-table~%")
     nil) 
    ))

;;;=================
;;; ACCESSORS
;;;=================

(defun get-feature-vector (vectors thing)
  "Retrieves the feature vector for a given object."
  (declare (type hash-table vectors))
  (print (list vectors thing))
  (print (gethash (normalize-token thing) vectors)))

(defun vector-count (vectors)
  "The number of elements in the vector space."
  (declare (type hash-table vectors))
  (hash-table-count vectors))


(defun get-features (vectors thing n &optional print)
  "Prints a ranked list of <n> highest-value features for a given thing, unsorted list of all features if N = NIL."
  
  (declare (type hash-table vectors))
  
  (let ((vector (get-feature-vector vectors thing)))
    
    (if vector
        (let ((sorted
               (loop for feat being the hash-keys of vector
                     using (hash-value val)
                     collect (cons feat val) into values
                     finally (return (if n (sort values #'> :key #'cdr) values)))))
          ;; print the top n features:
          
            (loop 
             for i from 1 to (or n (length sorted)) 
             for (feat . val) in sorted
             do (when print 
                  (format t "~&~a ~a~%" feat val))
             collect (cons feat val)))
    
          (format t "~&ERROR: ~S in not registered in the vector space~%" thing)
       
      )))


;;;=================
;;; SIMILARITY
;;;=================

(defmethod vector-similarity ((vectors hash-table) thing1 thing2 test)
  (let ((v1 (get-feature-vector vectors thing1))
        (v2 (get-feature-vector vectors thing2)))
    (when (and v1 v2)
      (funcall test v1 v2))))

(defun dot-product  (hash1 hash2)
  "Computes the inner product of two feature vectors."
  (loop 
      for id1 being the hash-keys of hash1
      for val2 = (gethash id1 hash2)
      when val2
      sum (* (gethash id1 hash1) val2)))

;;;=================
;;; NORMALIZATION
;;;=================

(defun euclidean-length (vector-ht)
  "Computes the Euclidean norm of a feature vector."
  
  (declare (type hash-table vector-ht))

  (sqrt
   (loop for value being each hash-value in vector-ht
         sum (expt value 2))))

(defun length-normalize-vector (vector-ht)
  "Destructively modifies a vector to have unit length."
  
  (declare (type hash-table vector-ht))

  (loop with length = (euclidean-length vector-ht)
      for j being the hash-keys of vector-ht
      using (hash-value n)
      do (setf (gethash j vector-ht)
               (/ n length)))
  vector-ht)


;;;=================
;;; CLASSIFICATION
;;;=================

;; Classification data:
;; each class label is used as a key in a hash-table, where for each class we
;; store a property list with two keys, :members and :centroid.
;; implements Rocchio algorithm for classification

(defstruct vs-class (members) (centroid))


(defun initialize-classes (classes)
  
  (declare (type (or list pathname hash-table) classes))

  (typecase classes
    ;;; already good!
    (hash-table classes)
    ;;; convert the list in hash-table
    (list (read-classes-from-list classes))
    ;; read file and return hash-table
    (pathname (read-classes-from-file classes))
    ;;; problem
    (otherwise 
     (format t "~&ERROR: the classes be initialized with a list or a hash-table~%")
     nil) 

    ))

(defmethod read-classes-from-list ((classes list))
  (let ((class-table (make-hash-table :test 'equal)))
    (loop for list in classes
          for class = (first list)
          for members = (second list)
          do (setf (gethash class class-table) (make-vs-class :members members)))
  class-table))

(defmethod read-classes-from-file ((file pathname))
   (let ((class-table (make-hash-table :test 'equal)))
     (with-open-file (stream file)
       (loop for list = (read stream nil nil)
             for class = (first list)
             for members = (second list)
             while list
             do (setf (gethash class class-table) (make-vs-class :members members))))
     class-table))


(defun get-class-centroid (name classes)
  (let ((class (gethash name classes)))
    (and class (vs-class-centroid class))))

(defun get-class-members (name classes)
  (let ((class (gethash name classes)))
    (and class (vs-class-members class))))
     

;;;---------------------
;;; COMPUTE CENTROIDS...
;;;---------------------

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

(defun compute-class-centroids (classes vectors)
  "Compute and store the average-vectors for each class."
  
  (declare (type hash-table classes vectors))
  
  (loop for label being the hash-keys of classes
        for members = (get-class-members label classes)
        for class-vectors = (remove nil (mapcar #'(lambda (w) (get-feature-vector vectors w)) members))
        for centroid = (length-normalize-vector (apply #'vector-average class-vectors))
        ;;; for each class label, store the centroid in the 
        ;;; property list we created in `read-classes':
        do (print (list label class-vectors))
        do (setf (vs-class-centroid (gethash label classes)) centroid))
  
  classes)


;;;==================================================================
;;; Implementation of the Rocchio algorithm for Information Retrieval
;;;==================================================================

(defun classify (vector classes sim-fn)
  "Classifies vector according to centroid distance to classes."
  
  (let ((named-centroids (print (loop for label being the hash-keys of classes 
                               collect (cons label (get-class-centroid label classes))))))
    (loop with max-label with max-sim = 0
          for (label . center) in named-centroids
          for sim = (funcall sim-fn vector center)
          when (> sim max-sim)
          do (setq max-label label max-sim sim)
          finally (return (list max-label max-sim)))
    ))
   


;;;==================================
;;; OM OBJECT
;;;==================================

(om::defclass! vector-space ()
  ((vectors :initform nil :initarg :vectors :accessor vectors :documentation "hash-table containing vectors")
   (classes :initform nil :initarg :classes :accessor classes :documentation "hash-table containing classes")
   (similarity-fn :initform #'dot-product :accessor similarity-fn))
  (:icon :omai))
      

(defmethod initialize-instance :after ((self vector-space) &rest args)
  
  (setf (vectors self) (initialize-vector-space (vectors self)))
  
  (setf (classes self) (initialize-classes (classes self)))
  
  (when (classes self)
    (compute-class-centroids (classes self) (vectors self)))
  
  self)


(om::defmethod! vector-features ((self vector-space) thing n)
  :icon :omai
  :initvals '(nil nil 10)
  (get-features (vectors self) thing n nil))

(om::defmethod! get-similarity ((self vector-space) thing1 thing2)
  :icon :omai
  (vector-similarity (vectors self) thing1 thing2 (similarity-fn self)))

(om::defmethod! get-class ((self vector-space) thing)
  :icon :omai
  (when (classes self)
    (classify (get-feature-vector (vectors self) thing) 
              (classes self) 
              (similarity-fn self))))
  