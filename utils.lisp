(in-package :om)

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


(defun normalize-values (l)
  (om-scale l 0.0 1.0))

(defun cs->fvecs (cs)
  (mat-trans
   (mapcar #'(lambda (l) (om-scale (flat l) 0.0 1.0))
	   (list (lmidic cs)
		 (ldur cs)
		 (lvel cs)))))

(cs->fvecs (testcs2 20))




;; (r20-3d 4)




