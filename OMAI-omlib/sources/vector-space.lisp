
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

(om::defclass! vector-space ()
  ((vectors :initform nil :initarg :vectors :accessor vectors :documentation "hash-table containing vs-vectors")
   (features :initform nil :initarg :features :accessor features :documentation "a list of features used for display and clustering")
   (classes :initform nil :initarg :classes :accessor classes 
            :documentation "a list containing vs-classes")
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
  (setf (features self) (initialize-features (features self) (vectors self)))
  (setf (classes self) (initialize-classes (classes self)))
  
  (compute-class-centroids self)
  
  self)



;;;=======================
;;; CONSRUCTION OF VECTORS
;;;=======================
    

(defun convert-to-vs-vector (input n) 
  (cond
   ((and (listp input) (listp (cadr input))) 
    ;;; (id (v1 v2 ...))
    (values (cadr input) (car input)))
    
   ((listp input)  
    ;;; (v1 v2 ...) => make a default id
    (values input (format nil "e-~d" n)))
    
    (t 
     ;;; simple values => considered IDs of empty vectors
     (values nil input))
    ))

; if the input is a hash-table we supposed it is already ok 
; this happens for instance when copying from another vector-space's vector slot, 
; or when reloading the object
(defmethod initialize-vector-space ((input hash-table)) input)

(defmethod initialize-vector-space ((input list))
   
  (let ((vectors (make-hash-table :test 'equal)))
    
    (loop for thing in input
          for n = 0 then (+ n 1)
          do (multiple-value-bind (vector id)
                 (convert-to-vs-vector thing n)
               (setf (gethash id vectors) vector)))
    
    vectors))

(defmethod initialize-features ((features list) (vector-ht hash-table))
  
  (let ((max-vector-size (loop for vect being the hash-values in vector-ht
                               maximize (length vect))))
    
    (if (< (length features) max-vector-size)
        (append features 
                (loop for i from (1+ (length features))
                      to max-vector-size
                      collect (format nil "feat_~D" i)))
      features)
    ))

;;;=================
;;; UTILS/ACCESSORS
;;;=================

(defmethod vector-count ((self hash-table))
  "The number of elements in the vector space"
  (hash-table-count self))

(defmethod vector-count ((self vector-space))
  (vector-count (vectors self)))



(defmethod get-feature-vector ((self hash-table) thing)
  "Retrieves the feature vector for a given object."
  (gethash thing self))

(defmethod get-feature-vector ((self vector-space) thing)
  (get-feature-vector (vectors self) thing))


;;; vector = a vector (list)
(defmethod get-feature-value ((self vector-space) (vector list) feature)
  (let ((pos (position feature (features self) :test 'equal)))
    (when pos (nth pos vector))))

;;; vector = a vector-ht key
(defmethod get-feature-value ((self vector-space) (vector t) feature)
  (let ((v (get-feature-vector self vector)))
    (get-feature-value self v feature)))



;;; vector = a vector (list) or a vector-ht key
(om::defmethod! get-feature-values ((self vector-space) vector &optional features)
  :icon :omai
  :numouts 2 
  :indoc '("a vector-space" "a vector identifier in the vector-space" "a list of feature (or NIL)")
  :outdoc '("list of values" "list of features")
  :doc "Returns the feature values of <vector> (a vector ID).


If <features is not provided the values of all features are returned.

The second output returns the list of features."
  

  (let ((feats (or features (features self)))) 
    (values (mapcar #'(lambda (key) (get-feature-value self vector key)) feats)
            feats)
    ))



;;;=============================
;;; VECTOR SIMILARITY/COMPARISON
;;;=============================

(defun euclid-norm (vector)
  (declare (type list vector))
  (sqrt (reduce #'+ (mapcar #'* vector vector))))


; Destructively modifies a vector to have unit length.
(defun normalize-vector (vector)  
  (declare (type list vector))
  
  (let ((norm (euclid-norm vector)))
    (unless (zerop norm)
      (dotimes (i (length vector))
        (setf (nth i vector) (/ (nth i vector) norm)))
      )))

; returns a new vector with unit-length.
(defun normalized-vector (vector)  
  (declare (type list vector))
  
  (let ((norm (euclid-norm vector))) 
    (unless (zerop norm)
      (loop for val in vector 
            collect (/ val norm))
      )))

(defun dot-product (v1 v2)
  "Computes the inner product of two feature vectors."  
  (declare (list v1 v2))

  (loop for val1 in v1 
        for val2 in v2
        sum (* val1 val2)))


(defun vector-diff (v1 v2)
  "Computes the difference of two feature vectors."  
  (declare (list v1 V2))
  
  (loop for val1 in v1 
        for val2 in v2
        collect (- val1 val2)))


(defmethod vector-similarity ((self hash-table) thing1 thing2 function)
  "applies a given function of similarity to two vectors"
  (let ((v1 (get-feature-vector self thing1))
        (v2 (get-feature-vector self thing2)))
    (when (and v1 v2)
      (funcall function v1 v2))))

(defmethod vector-similarity ((self vector-space) thing1 thing2 function)
  (vector-similarity (vectors self) thing1 thing2 function))


(om::defmethod! get-similarity ((self vector-space) thing1 thing2)
  :icon :omai
  :indoc '("a vector-space" "label/id for vector 1" "label/id for vector 2")
  :doc "Retruns a measure of similarity between <thing1> and <thing2> in the vector space (using the vectir space similarity-function)"
  (vector-similarity (vectors self) thing1 thing2 (similarity-fn self)))

;;;=================
;;; CLASSES
;;;=================

(om::defclass! vs-class () 
  ((label :accessor label :initarg :label :initform nil)
   (members :accessor members :initarg :members :initform nil)
   (centroid :accessor centroid :initarg :centroid :initform nil)))



(om::defmethod! get-class ((self vector-space) thing)
  :icon :omai
  :doc "Returns the class of <thing> as currently stored in the vector space.

A warning will indicate if <thing> is found in several classes, and the full list of calss candidates with be returned as second value.  
This function does not make any estimation, so the class of <thing> can be NIL if no complete classification was done on <self>."
  (let ((vector (gethash thing (vectors self))))
    
    (if vector
        
        (let ((classes (loop for class in (classes self) 
                             when (find thing (members class) :test 'equal)
                             collect (label class))))
          (if (> (length classes) 1)
              (progn (print (format nil "Warning: vector ~A was found in several classes." thing))
                (values (car classes) classes))
            (car classes)))
      
      (om::om-beep-msg "~A not found in vector space" thing))))


;;;-----------------
;;; CLASS CENTROID
;;;-----------------

;;; returns a vector with centroid value for all features 
;;; features can be the list of features or just the number of features (considered equal to the size of vectors)
(defmethod compute-centroids ((vectors hash-table) features)
 
  (let* ((vector-size (if (numberp features) features (length features)))
         (vector-num (hash-table-count vectors))
         (centroids (make-list vector-size :initial-element 0)))
       
    ;;; add values for each features
    (loop for vector being the hash-values of vectors do
          (loop for val in vector
                for feat from 0 do
                (setf (nth feat centroids) (+ (nth feat centroids) val))))
    
    ;;; normalize / number of vectors
    (loop for element in centroids collect 
          (/ element vector-num))
    ))


(defmethod compute-centroids ((vectors list) features)
  
  (let* ((vector-size (if (numberp features) features (length features)))
         (vector-num (length vectors))
         (centroids (make-list vector-size :initial-element 0)))
    
    (when (plusp vector-num)
      ;;; add values for each features
      (loop for vector in vectors do
            (loop for val in vector
                  for feat from 0 do
                  (setf (nth feat centroids) (+ (nth feat centroids) val))))
      
      ;;; normalize by number of vectors
      (loop for element in centroids collect 
            (/ element vector-num))
      )))


; Compute and store the average-vectors for each class from all vectors currently in this class.

(defmethod compute-class-centroid ((class vs-class) (vectors hash-table) features)
    
  (let* ((class-vectors (loop for member-id in (members class)
                              collect (get-feature-vector vectors member-id))))
    (setf (centroid class) (compute-centroids class-vectors features))
    
  class))


(defmethod compute-class-centroids ((self vector-space))  
  (mapcar 
   #'(lambda (c) (compute-class-centroid c (vectors self) (features self))) 
   (classes self)))
    

(defmethod class-likelihood ((vector list) (class vs-class) sim-fn)
  (funcall sim-fn vector (normalized-vector (centroid class))))


(om::defmethod! estimate-class ((self vector-space) thing &optional (n-results 1))
  :icon :omai
  :indoc '("a vector-space" "a vector id/label")
  :outdoc '("the label of the estimated class" "the corresponding score")
  :numouts 2
  :doc "Returns a best-guess for the class of <thing> among vector-space classes.

If <thing> is a vector ID (string/symbol) then it is supposed to be a vector already inside <self>.
In this case the function doesn't consider the actual/current classification of <thing> (if any) and tests the vector against all classes in <self>, computing their centroids if necessary.

If <thing> is a list, then it is considered a new vector which can be compared to other vectors in <self>."

  (let ((vector (gethash thing (vectors self))))
    
    
    (if vector
        
        (let ((scores (sort
                       (loop for class in (classes self) 
                          do (unless (centroid class)
                               (compute-class-centroid class (vectors self) (features self)))
                          collect (list (label class)
                                        (class-likelihood vector class (similarity-fn self))))
                       '> :key 'cadr)))
          ;;(print (list thing scores))
	  (loop
	     repeat (min (length scores) n-results)
	     for this in scores
	     collect this))
	
	(om::om-beep-msg "~A not found in vector space" thing))
    ))


(om::defmethod! estimate-class ((self vector-space) (thing list) &optional (n-results 1))

  (let* ((vector thing)
         (scores (sort 
                  (loop for class in (classes self) 
                     do (unless (centroid class)
                          (compute-class-centroid class (vectors self) (features self)))
                     collect (list (label class)
                                   (class-likelihood vector class (similarity-fn self))))
                  '> :key 'cadr)))
    ;; 
    ;;(break)
    ;;(print (list 'scores scores))
    (loop
       repeat (min (length scores) n-results)
       for this in scores
       collect this)))


(defmethod classify ((self vector-space))
  "Estimates class for all unclassified vectrors in the vector-space."
  (loop for vector-id being the hash-keys of (vectors self)
        unless (get-class self vector-id)
        do (let ((estimated-class (estimate-class self vector-id)))
             (when estimated-class
               (push vector-id 
                     (members (find estimated-class (classes self) :key 'label :test 'equal)))))
        ))



;;;-----------------------------
;;; INIT CLASSES IN VECTOR-SPACE
;;;-----------------------------

(defmethod initialize-classes ((classes integer))
  (loop for n from 1 to classes 
        collect (make-instance 'vs-class :label (format nil "class-~D" n))))

(defmethod initialize-classes ((classes list))
  
  (loop for class in classes 
        for n from 1 collect
        
        (cond 
         ((typep class 'vs-class) ;; already formatted
          class)
         
         ((and (consp class) (atom (car class)) (listp (cadr class)))  ; (id (member-IDs))
          (make-instance 'vs-class :label (car class) :members (cadr class)))
         
         ((every #'atom class)   ; (members-IDs)
          (make-instance 'vs-class :label (format nil "class-~D" (1+ n)) :members class))
         
         ((atom class) ; id 
          (make-instance 'vs-class :label class :members nil))
         
         (t nil) ; problem
         )
        ))















  



                     

