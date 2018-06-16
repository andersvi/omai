(in-package :om)


;; scratchpad to check uses of feature extraction

(defmethod extract-feature-vectors ((self chord-seq) &optional (parameter-list '(lmidic ldur lvel)) (normalize t))
  (mat-trans
   (mapcar #'(lambda (l)
	       (if normalize
		   (om-scale (flat l) -1.0 1.0)
		   (flat l)))
	   (mapcar #'(lambda (p) (funcall p self)) parameter-list))))

(defun split-cs-by-features (cs k)
  (let ((parameter-list '(lmidic ldur lvel)))
    ))

(defclass chord-w-class (chord)
  ((class :initarg nil :class :accessor class)))

(mki 'chord-w-class)

(let ((a (mki 'chord-seq)))
  (setf (inside a) (loop repeat 10 collect (mki 'chord-w-class :ldur (random 10) :lmidic (* 100 (om-random 48 72)))))
  (setf (lonset a) '(0 100))
  (lonset a))

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
