(in-package :om)

;;; UTILS

(defun testcs (&optional (n 100))
  (let ((cs (mki 'chord-seq
		 :lmidic (loop repeat n collect (* 100 (om-random 60 84)))
		 :lonset (loop for i from 0 to 1200 by 100 collect i)
		 :ldur '(100))))
    cs))

(defun testcs2 (&optional (n 100))
  (let ((cs (mki 'chord-seq
		 :lmidic (loop repeat n collect (* 100 (om-random 60 84)))
		 :lvel  (loop repeat n collect (om-random 10 127))
		 :lonset '(0 100)
		 :ldur (loop repeat n collect (om-random 10 500)))))
    cs))

(defun modulo-fit (val modulo lo hi)
  (cond ((<= lo val hi) val)
	((< val hi) (+ lo (mod (- val lo) modulo)))
	(t (modulo-fit (- val modulo) modulo lo hi))))

(defun harmonic-cs2 (&optional (n 100) (lo 6000) (hi 8400))
  (let* ((pat (p-weighting '((0 :weight 1)		    ;uses Patterns lib
			     (1 :weight 30)
			     (2 :weight 1)
			     (4 :weight 3)
			     (4 :weight 3)
			     (7 :weight 8)
			     (8 :weight 0)
			     (9 :weight 2)))))
    (mki 'chord-seq
	 :lmidic (loop
		    with f = 6000
		    repeat n
		    for intervall = (* (nth-random '(-1 1))
				       (p-next pat))
		    for mc = (setf f (modulo-fit (+ f (* 100 intervall))
						 1200 lo hi))
		    collect mc)
	 :lvel  (loop repeat n collect (om-random 10 127))
	 :lonset '(0 100)
	 :ldur (loop repeat n collect (om-random 10 500)))))

;; (flat (lmidic (harmonic-cs2 100 6000 )))

;; (omai::dominant-spread (lmidic (harmonic-cs2)))

(defun cs->fvecs (cs)
  (mat-trans
   (mapcar #'(lambda (l) (om-scale (flat l) 0.0 1.0))
	   (list (lmidic cs)
		 (ldur cs)
		 (lvel cs)))))

;;;===============
 
;; scratchpad to check uses of feature extraction

(defmethod extract-feature-vectors ((self chord-seq) &optional (parameter-list '(lmidic ldur lvel)) (normalize t))
  (mat-trans
   (mapcar #'(lambda (l)
	       (if normalize
		   (om-scale (flat l) -1.0 1.0)
		   (flat l)))
	   (mapcar #'(lambda (p) (funcall p self)) parameter-list))))

;; (defun split-cs-by-features (cs k)
;;   (let ((parameter-list '(lmidic ldur lvel)))
;;     ))

;; (defclass chord-w-class (chord)
;;   ((class :initarg nil :class :accessor class)))

;; (mki 'chord-w-class)

;; (let ((a (mki 'chord-seq)))
;;   (setf (inside a) (loop repeat 10 collect (mki 'chord-w-class :ldur (random 10) :lmidic (* 100 (om-random 48 72)))))
;;   (setf (lonset a) '(0 100))
;;   (lonset a))

(defmethod extract-feature-vectors ((self chord-seq) &optional (parameter-list '(lmidic ldur lvel)) (normalize t))
  (mat-trans
   (mapcar #'(lambda (l)
	       (if normalize
		   (om-scale (flat l) -1.0 1.0)
		   (flat l)))
	   (mapcar #'(lambda (p) (funcall p self)) parameter-list))))

;; (extract-feature-vectors (testcs2) '(lmidic ldur lvel) nil)
;; (extract-feature-vectors mycs '(lmidic lvel ldur) t)

(defun lcs-from-llmc (llmidic)
  (loop
     for lmc in llmidic
     collect (mki 'chord-seq :lmidic lmc :lonset '(50))))

(defun km->3dc-lib (vecs)
  ;; use first three features from fvec for now
  (loop
     for v in vecs
     collect (let ((3dc (3DC-from-list
			 (mapcar 'first v)
			 (mapcar 'second v)
			 (mapcar 'third v)
			 '3dc 4)))
	       (setf (bpfcolor 3dc) (or (pop *16-color-list*) (om-random-color)))
	       3dc)))

;; check patch: ./omai-ws/k-means.omp
;;
;; TODO:
;;
;; - classify cs according to classes coming from k-means on extracted feature vectors:
;;
;;   + backlink to notes (or whatever) from analyzed 'feature'
;;
;; - manouver in classified space: select one class, filter by class, only staccato, low-register, hi-dynamic...
;; 
