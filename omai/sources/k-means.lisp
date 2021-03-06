
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
; K-means clustering
;=====================================================

(in-package :omai)


;;; NOT ORDERED!
(defun vector-values (vector)
  (loop for v being the hash-value of vector collect v))
                   

; Returns the centroid with lower distance to vector 
(defun get-closer-centroid (vector centroids max-distance result)
  (declare (type list vector)
           (type list centroids)
           (type number max-distance))
  
  (cond 
   ((null centroids) result)
   ((null (car centroids)) (get-closer-centroid vector (cdr centroids) max-distance result))
        
   (t (let* ((c (car centroids))
             (new-distance (euclid-norm (vector-diff vector c))))
        (if (< new-distance max-distance)
            (get-closer-centroid vector (cdr centroids) new-distance c)
         (get-closer-centroid vector (cdr centroids) max-distance result))
        ))
   ))


; a clusters-map is a list of indices corresponding to the class of each vector
; => returns for each vector the position of it's centroid in the centroid list 
(defun get-cluster-map (vectors centroids)

   (declare (type hash-table vectors)
            (type list centroids))
           
   (loop for vector being the hash-value of vectors 
         for name being the hash-key of vectors 
         collect (cons 
                  name
                  (position
                   (get-closer-centroid vector centroids most-positive-fixnum nil)
                   centroids
                   ;;; test is eql: ok ?
                   )))
   )


(defun lloyd-km (vectors cluster-map centroids k size)
  
  (declare (type hash-table vectors)
           (type list cluster-map centroids)
           (type integer k))

  (let ((new-cluster-map (get-cluster-map vectors centroids)))
    
    (if (equal cluster-map new-cluster-map)
  
        cluster-map
      
      (lloyd-km vectors new-cluster-map
                (loop for cluster-vectors in (make-clusters new-cluster-map vectors k)
                      collect (compute-centroids cluster-vectors size))
                k size))))



;;================================
; returns a list of cluster: each cluster is a hash-table of vectors
(defun make-clusters (clusters-map vectors k)
  (let ((clusters (make-list k)))
    (loop for elt in clusters-map do
          (unless (nth (cdr elt) clusters)
            (setf (nth (cdr elt) clusters) (make-hash-table :test 'equal)))
          (setf (gethash (car elt) (nth (cdr elt) clusters))
                (gethash (car elt) vectors))
          )
    clusters))


;;================================
;;; get a list of n random vectors 
(defun rec-collect-random-vectors (keys vectors k)
  (declare (type list keys)
           (type hash-table vectors)
           (type integer k))
  
  (if (= k 0) nil
    (let ((rand-key (nth (random (length keys)) keys)))
      (cons (gethash rand-key vectors)
            (rec-collect-random-vectors (remove rand-key keys) vectors (1- k))))))

(defun initialize-centroids (vectors k)
  (declare (type hash-table vectors)
           (type integer k)) 
  (let ((all-keys (loop for k being the hash-keys of vectors collect k)))
    ; (mapcar #'vector-values (rec-collect-random-vectors all-keys vectors k))
    (rec-collect-random-vectors all-keys vectors k)))


;;;========================
;;; main function
;;;========================

;;; returns a list of k groups (classes) 
;;; observations is an HT of feature-vectors of dimension size
(defun compute-k-means (vectors k size)

  (declare (type hash-table vectors)
           (type integer k))

  (declare (type hash-table vectors))

  (if (< (hash-table-count vectors) k) 
 
      (progn (format t "k >= observations !!") nil)
    
      (if (= (hash-table-count vectors) k) 
        
        ;;; just make one class with each observation vector
        (loop for element being the hash-keys of vectors
              for n from 0
              collect (make-instance 'vs-class :label (format nil "class-~D" n) :members (list element)))
        
      ;;; main case here:
      (let* ((init-state (initialize-centroids vectors k))
             (cluster-map (lloyd-km vectors nil init-state k size))
             (clusters (make-clusters cluster-map vectors k)))
        ;;;(map-clusters cluster-map vectors 0 k)
        
        (loop for cluster in clusters 
              for n from 0 
              collect (make-instance 'vs-class
                       :label (format nil "class-~D" n) 
                       :members (loop for key being the hash-keys of cluster collect key))))
      )
    )
  )
    

(om::defmethod! k-means ((self vector-space) (k integer))
  :icon :omai
  :doc "Returns a liust of <k> class computed form observations in <self> by k-means."
  :indoc '("a vector-space" "number of classes wanted")
  :outdoc '("list of vs-class objects")
  :initvals '(nil 2)
  (compute-k-means (vectors self) k (length (features self))))



#|

(defun map-cluster (clusters-map observations cl index)
  (cond ((null clusters-map) nil)
        ((not (= cl (car clusters-map)))
         (map-cluster (cdr clusters-map) observations cl (+ index 1)))
        (t (cons (nth index observations)
                 (map-cluster (cdr clusters-map) observations cl (+ index 1))))))

(defun map-clusters (clusters-map observations cl k)
  (if (= cl k)
      nil
      (cons (map-cluster clusters-map observations cl 0)
	    (map-clusters clusters-map observations (+ cl 1) k))))

|#


