
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


;;; euclidian distance
(defun euclid (fvec)
  (sqrt
   (loop for value in fvec
         sum (expt value 2))))

(defun vsum (vector1 vector2)
  (mapcar #'+ vector1 vector2))

(defun vsub (vector1 vector2)
  (mapcar #'- vector1 vector2))

;; centroid of set
(defun centroid (observations)
  (if (null observations)
      nil
      (mapcar #'(lambda (coord) (/ coord (length observations)))
	      (reduce #'vsum observations))))

;; dot product
(defun innerprod (vector1 vector2)
  (reduce #'+ (mapcar #'* vector1 vector2)))

;; euclidian norm
(defun norm (vector)
  (sqrt (innerprod vector vector)))


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

(defun re-centroids (clusters-map observations k)
  (mapcar #'centroid (map-clusters clusters-map observations 0 k)))

(defun pick-centroid (v cs old-distance result)
  ;; Returns the centroid whose distance between v and itself is the lowest calculated.
  (cond ((null cs) result)
        ((null (car cs)) (pick-centroid v (cdr cs) old-distance result))
        (t (let* ((c (car cs))
		  (new-distance (norm (vsub v c))))
             (cond ((< new-distance old-distance)
                    (pick-centroid v (cdr cs) new-distance c))
                   (t (pick-centroid v (cdr cs) old-distance result)))))))


(defun k-partition (observations cs)
  (loop
     for obs in observations
     collect (position
	      (pick-centroid obs cs most-positive-fixnum cs)
	      cs)))


(defun lloyd-km (observations clusters cs k)
  (let ((new-clusters (k-partition observations cs)))
    (if (equal clusters new-clusters)
	clusters
	(lloyd-km observations
		  new-clusters
		  (re-centroids new-clusters observations k)
		  k))))


(defun initialize (observations k)
  (if (= k 0)
      nil
      (let ((rand (nth (random (length observations)) observations)))
	(cons rand (initialize (remove rand observations) (- k 1))))))


;;;========================
;;; main function
;;;========================

;;; returns a list of groups (classes) 
;;; observations is an HT of feature-vectors
(defun k-means (vectors k)

  (declare (type hash-table vectors)
           (type integer k))

  (declare (type hash-table vectors))

  (if (< (hash-table-count vectors) k) 
 
      (progn (format t "k >= observations !!") nil)
  
    (let ((classes (make-hash-table :test 'equal)))
    
      (if (= (hash-table-count vectors) k) 

          ;;; just make one class with each observation vector
          (loop for element being the hash-keys of vectors
                for n from 0
                do (setf (gethash (format "class-~D" n) classes)
                         (make-vs-class :members (list element))))
        
        ;;; main call here:
        (let ((clusters (lloyd-km vectors nil (initialize observations k) k)))
          (map-clusters clusters vectors 0 k)))
      classes)
    ))
    


