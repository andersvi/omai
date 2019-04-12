
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

(defstruct vs-vector (features (make-hash-table :test 'equal) :type hash-table))
(defstruct vs-class (label) (members) (centroid))


;;;=======================
;;; CONSRUCTION OF VECTORS
;;;=======================
    
(defun make-feature-vectors (matrix features &optional names)
  
  (declare (type list matrix features names))
  
  (loop for element in matrix
        for n = 0 then (+ n 1)
        ;; create a feature vector for each thing
        
        collect (let ((vector (make-vs-vector))
                      (element-name (or (nth n names) (format nil "e-~d" n))))
                  (loop for feature in features
                        for value in element do
                        (setf (gethash feature (vs-vector-features vector)) value))
                  (list element-name vector)))
  )
  

;;;=================
;;; UTILS/ACCESSORS
;;;=================

(defun get-feature-vector (vectors thing)
  "Retrieves the feature vector for a given object."
  (declare (type hash-table vectors))
  (let ((vs-vector (gethash thing vectors)))
    (and vs-vector (vs-vector-features vs-vector))))

(defmethod get-feature-value ((vector hash-table) key)
  (gethash key vector))

(defmethod get-feature-value ((vector vs-vector) key)
  (get-feature-value (vs-vector-features vector)))


(defun get-feature-values (vector keys)
  (declare (type hash-table vector)
           (type list keys))
  (mapcar #'(lambda (key) (get-feature-value vector key)) keys))


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


(defmethod get-class-members ((class vs-class))
  (vs-class-members class))



;;;======================
;;; SIMILARITY/COMPARISON
;;;======================

; Destructively modifies a vector to have unit length.
(defun normalize-vector (vector)  
  (declare (type hash-table vector))
  (let ((norm (euclid-norm vector))) 
    (loop for key being the hash-keys of vector
          using (hash-value n)
          do (setf (gethash key vector) (/ n norm)))
    vector))


; returns a new vector with unit-length.
(defun normalized-vector (vector)  
  (declare (type hash-table vector))
  (let ((norm (euclid-norm vector))
        (normalized-vector (make-hash-table :test 'equal)))
    (loop for key being the hash-keys of vector
          using (hash-value n)
          do (setf (gethash key normalized-vector) (/ n norm)))
    normalized-vector))


(defmethod euclid-norm ((l list))
  (sqrt (reduce #'+ (mapcar #'* l l))))

(defmethod euclid-norm ((ht hash-table))
  (sqrt 
   (loop for value being each hash-value in ht
         sum (* value value))))

(defun dot-product  (hash1 hash2)
  "Computes the inner product of two feature vectors."

  (declare (hash-table hash1 hash2))

  (loop for id1 being the hash-keys of hash1
        for val2 = (gethash id1 hash2)
        when val2
        sum (* (gethash id1 hash1) val2)))

(defun vector-diff (hash1 hash2)
  "Computes the difference of two feature vectors."
  
  (declare (hash-table hash1 hash2))
  
  (let ((diff (make-hash-table :test 'equal)))
    (loop for id1 being the hash-keys of hash1
          for val2 = (gethash id1 hash2)
          when val2
          do (setf (gethash id1 diff) (- (gethash id1 hash1) val2)))
    diff))


(defmethod vector-similarity ((vectors hash-table) thing1 thing2 test)
  (let ((v1 (get-feature-vector vectors thing1))
        (v2 (get-feature-vector vectors thing2)))
    (when (and v1 v2)
      (funcall test v1 v2))))



;;;=================
;;; CLASS CENTROIDS
;;;=================

;;; returns a vector with centroid value with all features found in vectors 
(defun compute-centroids (vectors)
  
  (declare (type (or list hash-table) vectors))
  
  (let ((centroids (make-hash-table :test #'equal))
        (size (typecase vectors
                (list (length vectors))
                (hash-table (hash-table-count vectors)))))
  
    ;;; add values for each features
    (typecase vectors 
      (list 
       (loop for vector in vectors do
             (maphash #'(lambda (key val)
                          (incf (gethash key centroids 0) val))
                      vector)))
      (hash-table 
       (loop for vector being the hash-values of vectors do
             (maphash #'(lambda (key val)
                          (incf (gethash key centroids 0) val))
                      (vs-vector-features vector))))
      )
      
    ;;; normalize by number of vectors
    (loop for key being the hash-keys of centroids
          using (hash-value val)
          do (setf (gethash key centroids) (/ val size)))
  
    centroids))


; Compute and store the average-vectors for each class from all vectors currently in this class.

(defun compute-class-centroid (class vectors)
  
  (declare (type hash-table vectors)
           (type vs-class classes))
  
  (let* ((class-vectors (loop for member in (vs-class-members class)
                              for vector = (get-feature-vector vectors member)
                              when vector 
                              collect vector))
         (centroid (compute-centroids class-vectors)))
    (setf (vs-class-centroid class) centroid))
  
  class)

(defun compute-class-centroids (classes vectors)
  
  (declare (type hash-table vectors)
           (type list classes))
  
  (mapcar #'(lambda (c) (compute-class-centroid c vectors)) classes)
  
  )
  


(defun class-likelihood (vector class sim-fn)
  
  (declare (type hash-table vector)
           (type vs-class class))

  (funcall sim-fn 
           (vs-vector-features vector) 
           (normalized-vector (vs-class-centroid class)))
  )



;;;=================
;;; INIT
;;;=================

(defmethod initialize-classes ((classes integer))
  (loop for n from 1 to classes 
        collect (make-vs-class :label (format nil "class-~D" n))))

(defmethod initialize-classes ((classes list))
  
  (loop for class in classes 
        for n from 1 collect
        (cond 
         ((typep class 'vs-class) ;; already formatted
          class)
         
         ((and (consp class) (atom (car class)) (listp (cadr class)))  ; (id (member-IDs))
          (make-vs-class :label (car class) :members (cadr class)))
         
         ((every #'atom class)   ; (members-IDs)
          (make-vs-class :label (format nil "class-~D" (1+ n)) :members class))
         
         ((atom class) ; id 
          (make-vs-class :label class :members nil))
         
         (t nil) ; problem
         )
        ))


(defmethod initialize-vector-space ((input hash-table)) input)
(defmethod initialize-vector-space ((input list))

  (let ((vectors (make-hash-table :test 'equal)))
    
    (loop for thing in input
          ;; create a feature vector for each thing
          do (if (consp thing)
                 
                 ;;; a pair key.vector (ht)
                 (setf (gethash (car thing) vectors) (cadr thing))
               
               ;;; an empty vector
               (setf (gethash thing vectors) (make-vs-vector))))
    
    vectors))

;;;==================================
;;; OM OBJECT
;;;==================================

(om::defclass! vector-space ()
  ((vectors :initform nil :initarg :vectors :accessor vectors :documentation "hash-table containing vs-vectors")
   (features :initform nil :initarg :vectors :accessor features :documentation "a list of features used for display and clustering")
   (classes :initform nil :initarg :classes :accessor classes 
            :documentation "hash-table containing classes number of clustering")
   (similarity-fn :initform #'dot-product :accessor similarity-fn))
  (:icon :omai)
  (:documentation "
A structure to collect and visualize and clustering vectors.

<vectors> should be objects of type (or subtype) of VS-VECTOR, that include a hast-table of features. 

<features> is a list of feature identifiers (typically, strings) used for retrieving features in vs-vectors, and display/clustering them.

<classes> is a list of VS-CLASS elements containing references to vector members.
The classes can be computed on request, and/or given as input to VECTOR-SPACE.
The partition of vector does not need to be total: some vectors can be member of no specific class (some algorithms can be used to retrieve the class of a vector given its similarity with current class members).
Classes can also be empty for initialization of a certain number of clusters to be computed from the data-set.

Allowed input formats for <classes> are:
- List of VS-CLASSES
- List of (class-ID (member-IDs)) 
- List of class-IDs
- Number (integer: ill create a corresponding number of anonymous, empty classes). 

"))
      

(defmethod initialize-instance :after ((self vector-space) &rest args)
  
  (setf (vectors self) (initialize-vector-space (vectors self)))
  
  (setf (classes self) (initialize-classes (classes self)))
  
  (compute-class-centroids (classes self) (vectors self))
  
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
  (let ((vector (gethash thing (vectors self))))
    
    (if vector
        
        (let ((classes (loop for class in (classes self) 
                             when (find thing (vs-class-members class) :test 'equal)
                             collect (vs-class-label class))))
          (car classes))
      
      (om::om-beep-msg "~A not found in vector space" thing))))


(om::defmethod! estimate-class ((self vector-space) thing)
  :icon :omai
  
  (let ((vector (gethash thing (vectors self))))
    
    (if vector
        
        (let ((scores (sort 
                       (loop for class in (classes self) 
                             do (unless (vs-class-centroid class)
                                  (compute-class-centroid class (vectors self)))
                             collect (list (vs-class-label class)
                                           (class-likelihood vector class (similarity-fn self))))
                       '> :key 'cadr)))
          (car scores))
      
      (om::om-beep-msg "~A not found in vector space" thing))))
  


(defmethod classify ((self vector-space))
  
  (loop for vector-id being the hash-keys of (vectors self)
        unless (get-class self vector-id)
        do 
        (let ((estimated-class (car (estimate-class self vector-id))))
          (when estimated-class
            (push vector-id 
                  (vs-class-members (find estimated-class (classes self) :key 'vs-class-label :test 'equal)))))
        ))
                     

