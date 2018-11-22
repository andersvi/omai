(in-package :om)

;; Interface

;; (km observations k) -> clusters
;; (centroid observations) -> centroid
;; (vsum vector1 vector2) -> vector
;; (vsub vector1 vector2) -> vector
;; (innerprod vector1 vector2) -> value
;; (norm vector) -> value

;; (defparameter v3 (list 1 2 3))
;; (sqrt (innerprod V3 V3))

  ;; 3.7416575

;; (norm V3)
;;   3.7416575

;; (vsum V3 (list 10 0 42))
;;   (11 2 45)




(defparameter observations
  '((3.0 7.0) (0.5 1.0) (0.8 0.5)
    (1.0 8.0) (0.9 1.2) (6.0 4.0)
    (7.0 5.5) (4.0 9.0) (9.0 4.0)))

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

(defun initialize (observations k)
  (if (= k 0)
      nil
      (let ((rand (nth (random (length observations)) observations)))
	(cons rand (initialize (remove rand observations) (- k 1))))))

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

;; (defun partition (observations cs)
;;   (if (null observations)
;;       nil
;;       (cons (position (pick-centroid (car observations)
;; 				     cs
;; 				     most-positive-fixnum
;; 				     cs)
;; 		      cs)
;; 	    (partition (cdr observations) cs))))

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

(defun k-means (observations k)
  (cond ((< (length observations) k) (error "k >= observations"))
        ((= (length observations) k) observations)
        ((null observations) nil)
        (t (map-clusters (lloyd-km observations nil (initialize observations k) k)
                         observations 0 k))))



#|

;; (km observations 3)

(setq observations
  (loop repeat 10 collect (list (random 10.0))))

;; (km observations 3)
;; (((1.2346375) (1.0703194))
;;  ((5.77945) (6.0765066) (6.030302) (3.5497833) (3.5672725) (3.602773) (6.5382767))
;;  ((0.84249616)))

(setq observations
  (loop repeat 10 collect (list (random 1.0) (random 1.0) (random 1.0))))

(setq observations
  (loop repeat 10 collect (list (random 10.0) (random 10.0))))

(setq observations
  (loop repeat 10 collect (list (random 10.0))))


(loop for o in observations
     collect (euclid o))

(k-means observations 3)

;; (setq aaa (km (loop repeat 10 collect (list (random 1.0) (random 1.0))) 3))

(defun r20-3d (n)
  (loop repeat n collect (list (random 1.0) (random 1.0)(random 1.0))))

(let ((space (setf aaa (k-means
			(loop repeat 20 collect (list (random 1.0) (random 1.0)(random 1.0)))
			5))))
  (loop for class in space
     collect (loop
		for o in class
		collect o))) 				    ;(list  (euclid o))

(defun test3dc ()
  (3Dc-from-list
   (loop repeat 20 collect (random 1.0))
   (loop repeat 20 collect (random 1.0))
   (loop repeat 20 collect (random 1.0))
   '3dc 3))


|#
