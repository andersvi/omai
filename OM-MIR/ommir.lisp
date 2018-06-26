(in-package :om)

;;; functions for symbolic musical feature extraction.
;;;
;;; Intended for various clustering and classification

(defclass om-mir-mixin ()
  ;; subclass to mix with OM's basic container classes, chord-seq, voice, poly,
  ;; segment, analyse:
  ((mir-features :accessor mir-features)))		    ;hash table of
							    ;extracted features

;;; keep backwards link from feature-vector to input data, ie. select one point
;;; in a cluster (one point representing e.g one input chord-seq), output/play
;;; selected input data.

;;; set up a new (or specialize 3dc?) editor to visualize and navigate sets of
;;; data with extracted features
;;;
;;; ?? - calculate features on demand, on initialization?



;;; UTILITY FUNCTIONS

(defvar *mir-in-log-space* nil "use log space to use more bits when combining many factors")
;; later... use factors 0->1.0 for now, easier debugging.

(defun normalize-histogram (alist &optional (y-max 100))
  (loop
     for val in alist
     maximize (cdr val) into max
     finally (return 
	       (loop
		  for (key . val) in alist
		  collect (cons key (float (* y-max (/ val max))))))))

(defun histogram (vals &optional (normalize? 100))
  (let ((tab '()))
    (loop
       for val in vals
       do (if (assoc val tab)
	      (incf (cdr (assoc val tab)))
	      (setf tab (cons (cons val 1) tab))))
    (if normalize?
	(normalize-histogram tab normalize?)
	tab)))


(defun histogram->bpf (ht &optional (y-range 1.0))
  (let ((f (mki 'bpf :decimals (if (floatp y-range)
				   10			    ;find something sensible here
				   0))))
    (setf (x-points f) (mapcar #'car ht))
    (setf (y-points f) (om* y-range (mapcar #'cdr ht)))
    f))

;;;; PITCH STATISTICS

;;;     P-1 Basic Pitch Histogram

(defmethod pitch-histogram ((self chord-seq) &optional (normalize? 1.0))
  (let ((pitches (flat (lmidic self))))
    (histogram pitches normalize?)))

;;;     P-2 Pitch Class Histogram

(defun pitch->pc (mc &optional (round-to 100) (pcs-in-octave 12))
  ;; return pitch class rounded to 'round-to, modulo 'pcs, ie. (pitch->pc 7105) -> pc: 11
  (mod (round mc round-to) pcs-in-octave))

(defmethod pc-histogram ((self chord-seq) &optional (normalize? 1.0))
  (let ((pitches (flat (lmidic self))))
    (histogram (mapcar #'pitch->pc pitches) normalize?)))

;;;     P-3 Folded Fifths Pitch Class Histogram

(defun reorder-pc-histogram (histogram order)
  "reorder pc-histogram according to order (list of indexes)"
  (mapcar #'(lambda (pc) (nth pc histogram)) order))

(defmethod folded-fifths-pc-histogram ((self chord-seq) &optional (normalize? 1.0))
  (let ((histo (pc-histogram self normalize?))
	(cycle-of-fifths (loop
			    repeat 12
			    for i from 0 by 7
			    collect (mod i 12))))
    (reorder-pc-histogram histo cycle-of-fifths)))


;;;     P-4 Prevalence of Most Common Pitch

(defun count-individuals (data)
  "returns assoc-list of (data . count) for individual items in data"
  (loop
     with tab = '()
     for v in data
     do (if (assoc v tab)
	    (incf (cdr (assoc v tab)))
	    (setf tab (cons (cons v 1) tab)))
     finally (return tab)))

(defun most-common-value-factor (data)
  (let ((N (length data))
	(counts (count-individuals data)))
    (float (/ (cdar (sort counts #'> :key #'cdr)) N))))
  
(defmethod most-common-pitch-fraction ((self chord-seq))
  (let ((pitches (flat (lmidic self))))
    (most-common-value-factor pitches)))

;;;     P-5 Prevalence of Most Common Pitch Class

(defmethod most-common-pc-fraction ((self chord-seq))
  (let ((pcs (mapcar #'pitch->pc (flat (lmidic self)))))
    (most-common-value-factor pcs)))

;;;     P-6 Relative Prevalence of Top Pitches

     ;; P-6 Relative Prevalence of Top Pitches: Relative frequency of the second
     ;; most common pitch in the piece, divided by the relative frequency of the
     ;; most common pitch.

(defun two-most-common-occurences-factor (data)
  (let ((sorted-counts (sort (count-individuals data) #'> :key #'cdr)))
    (float
     (/ (cdr (second sorted-counts))
	(cdr (first sorted-counts))))))

(defmethod relative-prevalence-most-common-pitches ((self chord-seq))
  (let ((pitches (flat (lmidic self))))
    (two-most-common-occurences-factor pitches)))


;;;     P-7 Relative Prevalence of Top Pitch Classes

(defmethod relative-prevalence-most-common-pcs ((self chord-seq))
  (let ((pcs (mapcar #'pitch->pc (flat (lmidic self)))))
    (two-most-common-occurences-factor pcs)))


;;;     P-8 Interval Between Most Prevalent Pitches

(defun interval-between-two-most-common-numbers (data)
  (let ((sorted-counts (sort (count-individuals data) #'> :key #'cdr)))
    (abs (- (car (first sorted-counts))
	    (car (second sorted-counts))))))

(defmethod interval-between-two-most-common-pitches ((self chord-seq))
  (let ((pitches (flat (lmidic self))))
    (interval-between-two-most-common-numbers pitches)))

;;;     P-9 Interval Between Most Prevalent Pitch Classes

(defmethod interval-between-two-most-common-pcs ((self chord-seq))
  "returns integer interval (0-12)"
  (let ((pcs (mapcar #'pitch->pc (flat (lmidic self)))))
    (interval-between-two-most-common-numbers pcs)))

;;;     P-10 Number of Common Pitches

(defun count-individuals-and-total (data)
  "count data, returns two values: (item . count) + total"
  (loop
     with tab = '()
     for total from 1
     for v in data
     do (if (assoc v tab)
	    (incf (cdr (assoc v tab)))
	    (setf tab (cons (cons v 1) tab)))
     finally (return (values tab total))))

(defun number-of-common-pitches (data &optional (threshold 0.09))
  (multiple-value-bind (counts total)
      (count-individuals-and-total data)
    (count-if #'(lambda (x) (>= (/ (cdr x) total) threshold))
	      counts)))

;;;     P-11 Pitch Variety


;;;
;;;     P-12 Pitch Class Variety
;;;

;;;
;;;     P-13 Range
;;;

;;;
;;;     P-14 Most Common Pitch
;;;

;;;
;;;     P-15 Mean Pitch
;;;

;;;
;;;     P-16 Importance of Bass Register
;;;

;;;
;;;     P-17 Importance of Middle Register
;;;

;;;
;;;     P-18 Importance of High Register
;;;

;;;
;;;     P-19 Most Common Pitch Class
;;;

;;;
;;;     P-20 Dominant Spread
;;;

;;;
;;;     P-21 Strong Tonal Centres
;;;

;;;
;;;     P-22 Major or Minor
;;;

;;;
;;;     P-23 Glissando Prevalence
;;;

;;;
;;;     P-24 Average Range of Glissandos
;;;

;;;
;;;     P-25 Vibrato Prevalence
;;;

;;;
;;;     P-26 Microtone Prevalence
;;;


;;;; MELODIC FEATURES


;;;
;;;     M-1 Melodic Interval Histogram
;;;

;;;
;;;     M-2 Most Common Melodic Interval
;;;

;;;
;;;     M-3 Mean Melodic Interval
;;;

;;;
;;;     M-4 Number of Common Melodic Intervals
;;;

;;;
;;;     M-5 Distance Between Most Prevalent Melodic Intervals
;;;

;;;
;;;     M-6 Prevalence of Most Common Melodic Interval
;;;

;;;
;;;     M-7 Relative Prevalence of Most Common Melodic Intervals
;;;

;;;
;;;     M-8 Amount of Arpeggiation
;;;


;;;
;;;     M-9 Repeated Notes
;;;

;;;
;;;     M-10 Chromatic Motion
;;;

;;;
;;;     M-11 Stepwise Motion
;;;

;;;
;;;     M-12 Melodic Thirds
;;;

;;;
;;;     M-13 Melocid Perfect Fourths
;;;

;;;
;;;     M-14 Melodic Tritones
;;;

;;;
;;;     M-15 Melodic Fifths
;;;

;;;
;;;     M-16 Melodic Sixths
;;;

;;;
;;;     M-17 Melodic Sevenths
;;;

;;;
;;;     M-18 Melodic Octaves
;;;

;;;
;;;     M-19 Melodic Large Intervals
;;;

;;;
;;;     M-20 Minor Major Melodic Third Ratio
;;;

;;;
;;;     M-21 Melodic Embellishments
;;;

;;;
;;;     M-22 Direction of Melodic Motion
;;;

;;;
;;;     M-23 Average Length of Melodic Arcs
;;;

;;;
;;;     M-24 Average Interval Spanned by Melodic Arcs
;;;

;;;
;;;     M-25 Melodic Pitch Variety
;;;



;;;
;;;
;;; TODO extend bpf to  handle bar-graphs/histograms
;;; - have bpf miniview reflect mode in editor
;;;



;;; integrate with segmentation/analysis system:
;;;
;;;  - manually/automatically set segments/phrases
;;;  - feature extraction within segment/phrase
;;;  - access data within segments
;;;
;;;


;;;
;;; melodic features
;;; - interval between successive notes
;;; - integrated over more than 2 notes, ie: 1-3, 1-N
;;; - autocorrelation, factor of repeatedness/patterns
