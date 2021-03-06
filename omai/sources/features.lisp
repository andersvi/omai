
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
; Descriptors and misc. MIR-related utilities
;
;=====================================================

(in-package :omai)

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
  ;; if y-max is t, normalize to relative factors where sum of factors = 1.0;
  ;; if y-max is a number, normalize maximum y => y-max
  (loop
     for item in alist
     for val = (cdr item)
     maximize val into max
     sum val into total
     finally (return
	       (if y-max				    ;do some normalization
		   (mapc #'(lambda (item)
			     (rplacd item (if (numberp y-max)
					      ;; largest y -> max-y
					      (float (* y-max (/ (cdr item) max)))
					      ;; factors summing to 1.0
					      (float (/ (cdr item) total)))))
			 alist)
		   alist))))

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
  (let ((f (om::mki 'om::bpf :decimals (if (floatp y-range)
					   10	   ;find something sensible here
					   0))))
    (setf (om::x-points f) (mapcar #'car ht))
    (setf (om::y-points f) (om::om* y-range (mapcar #'cdr ht)))
    f))


;;; Feature extraction: the features below (P-1 - P-26, M-1 - M-25) are the same
;;; as used in the jSymbolic MIR project:
;;; http://jmir.sourceforge.net/jSymbolic.html as suggested in Cory McKays
;;; thesis: "Automatic Music Classification with jMIR"

;;; TODO :: the extraction of melodic features does not currently consider chords


;;;; PITCH STATISTICS

;;;     P-1 Basic Pitch Histogram

(defmethod pitch-histogram ((self om::chord-seq) &optional (normalize? 1.0))
  (let ((pitches (om::flat (om::lmidic self))))
    (histogram pitches normalize?)))

;;;     P-2 Pitch Class Histogram

(defun pitch->pc (mc &optional (round-to 100) (pcs-in-octave 12))
  ;; return pitch class rounded to 'round-to, modulo 'pcs, ie. (pitch->pc 7105) -> pc: 11
  (mod (round mc round-to) pcs-in-octave))

(defmethod pc-histogram ((self om::chord-seq) &optional (normalize? 1.0))
  (let ((pitches (om::flat (om::lmidic self))))
    (histogram (mapcar #'pitch->pc pitches) normalize?)))

;;; P-3 Folded Fifths Pitch Class Histogram: A feature vector consisting of bin
;;; magnitudes of a folded fifths pitch class histogram. Each bin corresponds to
;;; one of the 12 pitch classes, and the bins are ordered such that adjacent
;;; bins are separated by an ascending perfect fifth. Bin 0 corresponds to
;;; C. Enharmonic equivalents are assigned the same pitch class number. The
;;; magnitude of of each bin is proportional to the the number of times notes
;;; occurred at the bin's pitch class in the piece, relative to all other pitch
;;; classes in the piece (the histogram is normalized).

(defun reorder-pc-histogram (histogram order)
  "reorder pc-histogram according to order (list of indexes)"
  (loop
     for index in order
     collect (assoc index histogram)))

(defvar cycle-of-fifths (loop
			   repeat 12
			   for i from 0 by 7
			   collect (mod i 12)))

(defmethod folded-fifths-pc-histogram ((self om::chord-seq) &optional (normalize? 1.0))
  (let ((histo (pc-histogram self normalize?)))
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
  
(defmethod most-common-pitch-fraction ((self om::chord-seq))
  "fraction of most common pitch vs all pitches"
  (let ((pitches (om::flat (om::lmidic self))))
    (most-common-value-factor pitches)))

;;;     P-5 Prevalence of Most Common Pitch Class

(defmethod most-common-pc-fraction ((self om::chord-seq))
  "P-5 Prevalence of Most Common Pitch Class"
  (let ((pcs (mapcar #'pitch->pc (om::flat (om::lmidic self)))))
    (most-common-value-factor pcs)))

;;; P-6 Relative Prevalence of Top Pitches: Relative frequency of the second
;;; most common pitch in the piece, divided by the relative frequency of the
;;; most common pitch.

(defun two-most-common-occurences-factor (data)
  (let ((sorted-counts (sort (count-individuals data) #'> :key #'cdr)))
    (float
     (/ (cdr (second sorted-counts))
	(cdr (first sorted-counts))))))

(defmethod relative-prevalence-most-common-pitches ((self om::chord-seq))
  (let ((pitches (om::flat (om::lmidic self))))
    (two-most-common-occurences-factor pitches)))

;;;     P-7 Relative Prevalence of Top Pitch Classes

(defmethod relative-prevalence-most-common-pcs ((self om::chord-seq))
  (let ((pcs (mapcar #'pitch->pc (om::flat (om::lmidic self)))))
    (two-most-common-occurences-factor pcs)))


;;;     P-8 Interval Between Most Prevalent Pitches

(defun interval-between-two-most-common-numbers (data)
  (let ((sorted-counts (sort (count-individuals data) #'> :key #'cdr)))
    (abs (- (car (first sorted-counts))
	    (car (second sorted-counts))))))

(defmethod interval-between-two-most-common-pitches ((self om::chord-seq))
  (let ((pitches (om::flat (om::lmidic self))))
    (interval-between-two-most-common-numbers pitches)))

;;;     P-9 Interval Between Most Prevalent Pitch Classes

(defmethod interval-between-two-most-common-pcs ((self om::chord-seq))
  "returns integer interval (0-12)"
  (let ((pcs (mapcar #'pitch->pc (om::flat (om::lmidic self)))))
    (interval-between-two-most-common-numbers pcs)))

;;; P-10 Number of Common Pitches: Number of pitches that account individually
;;; for at least 9% of all notes. Enharmonic equivalents are grouped together
;;; for the purpose of this calculation.


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

(defun number-of-common-items (data &optional (threshold 0.09))
  (multiple-value-bind (counts total)
      (count-individuals-and-total data)
    (count-if #'(lambda (x) (>= (/ (cdr x) total) threshold))
	      counts)))

(defmethod number-of-common-pitches ((self om::chord-seq) &optional (threshold 0.09))
  "P-10 Number of Common Pitches: Number of pitches that account individually
for at least 9% of all notes.  Enharmonic equivalents are grouped together for
the purpose of this calculation."
  (let ((pitches (om::flat (om::lmidic self))))
    (number-of-common-items pitches threshold)))

;;; P-11 Pitch Variety: Number of pitches that occur at least once.

(defun number-of-occurring-items (data)
  (length (remove-duplicates data)))

(defmethod pitch-variety ((self om::chord-seq))
  "P-11 Pitch Variety: Number of pitches that occur at least once."
  (number-of-occurring-items (om::flat (om::lmidic self))))

;;; P-12 Pitch Class Variety: Number of pitch classes that occur at least once.

(defmethod pitch-class-variety ((self om::chord-seq))
  "P-11 Pitch Class Variety: Number of pitches that occur at least once."
  (let ((pcs (mapcar #'pitch->pc (om::flat (om::lmidic self)))))
    (number-of-occurring-items pcs)))

;;; P-13 Range: Difference in mc between the highest and lowest pitches.

  ;; watch out for some unqualified 'range methods (staff and
  ;; maquette-params)...

(defmethod range ((self om::chord-seq))
  (let ((pitches (om::flat (om::lmidic self))))
    (- (apply #'max pitches) (apply #'min pitches))))

;;; P-14 Most Common Pitch: MC pitch value of the most frequently
;;; occurring pitch.

(defun most-common-item (data)
  (sort (count-individuals data) #'> :key #'cdr))

(defmethod most-common-pitch ((self om::chord-seq))
  "P-14 Most Common Pitch: MC pitch value of the most frequently occurring pitch."
  (caar (most-common-item (om::flat (om::lmidic self)))))

;;; P-15 Mean Pitch: Mean midicent value

(defmethod mean-pitch ((self om::chord-seq))
  (let ((pitches (om::flat (om::lmidic self))))
    ;; ?? perhaps round to resolution in incoming data ??
    (om::average pitches nil)))

;;; P-16 Importance of Bass Register: Fraction of notes with pitch below 5400.

(defun fraction-of-passed-data (predicate data)
  "returns fraction of data which passes predicate"
  (let ((N (length data))
	(N-filtered (length (remove-if-not predicate data))))
    (float (/ N-filtered N))))

(defmethod importance-of-bass-register ((self om::chord-seq))
  (let ((pitches (om::flat (om::lmidic self))))
    (fraction-of-passed-data #'(lambda (x) (< x 5400)) pitches)))

;;; P-17 Importance of Middle Register: Fraction of notes w. pitches between
;;; 5400 and 7200.

(defmethod importance-of-middle-register ((self om::chord-seq))
  (let ((pitches (om::flat (om::lmidic self))))
    (fraction-of-passed-data #'(lambda (x) (and (>= x 5400) (<= x 7200))) pitches)))

;;; P-18 Importance of High Register: Fraction of notes w. pitches above 7200

(defmethod importance-of-high-register ((self om::chord-seq))
  (let ((pitches (om::flat (om::lmidic self))))
    (fraction-of-passed-data #'(lambda (x) (> x 7200)) pitches)))

;;; P-19 Most Common Pitch Class: The pitch class that occurs most
;;; frequently compared to other pitch classes (0->12).

(defmethod most-common-pitch-class ((self om::chord-seq))
  (pitch->pc (most-common-pitch self)))

;;; P-20 Dominant Spread: Largest number of consecutive pitch classes
;;; separated by perfect 5ths that each individually account for at least 9%
;;; of the total notes in the piece.

;; ... AV is a bit confused about this spec....

(defun item-N-above-threshold-p (item data threshold)
  "predicate to check whether item is occuring more often then a
   threshold (factor) in data"
  (multiple-value-bind (counts total)
      (count-individuals-and-total data)
    (>= (/ (cdr (assoc item counts)) total) threshold)))

(defun count-consecutive-fifths (seq &optional (threshold 0.09))
  "count consecutive fifths, but only if this particular step
makes up more than 'threshold' percent (factor) of data"
  (flet ((dominant-and-more-than-9% (a b)
	   (and (item-N-above-threshold-p a seq threshold)
		(item-N-above-threshold-p b seq threshold)
		;; operate on pitch classes, 0-12:
		(= (abs (- a b)) 7))))
    (loop
       for a in seq for b in (cdr seq)
       count (funcall #'dominant-and-more-than-9% a b))))

(defmethod dominant-spread ((self om::chord-seq))
  "Dominant Spread: Largest number of consecutive pitch classes separated by
perfect 5ths that each individually account for at least 9% of the total notes
in the piece."
  (let ((pcs (mapcar #'pitch->pc (om::flat (om::lmidic self)))))
    (count-consecutive-fifths pcs 0.09)))

;;; P-21 Strong Tonal Centres: Number of isolated peaks in the fifths pitch
;;; histogram that each individually account for at least 9% of all notes in
;;; the piece.

;; AV: what constitutes an isolated peak:
;;	zero in neighbours?
;;	notch = < 0.1 * neighbour?


;;; P-22 Major or Minor: Whether the piece is major or minor, as indicated
;;; by the first encountered major/minor metadata tag in the piece. Set to 0
;;; if the metadata indicates that the piece is major, or set to 1 if the
;;; metadata indicates that it is minor. Defaults to 0 if the key signature
;;; is unknown.


(defmethod major-or-minor ((self om::chord-seq))
  "major-minor extraction is not currently implemented for chord-seqs"
  0.0)

;;; P-23 Glissando Prevalence: factor of notes with Pitch Bend

(defmethod glissando-prevalence ((self om::chord-seq))
  "glissando-prevalence is not currently relevant for chord-seqs"
  0.0)


;;; P-24 Average Range of Glissandos: Average range of Pitch Bends

(defmethod glissando-average-range ((self om::chord-seq))
  "average glissando range is not currently relevant for chord-seqs"
  0.0)

;;; P-25 Vibrato Prevalence: 

(defmethod vibrato-prevalence ((self om::chord-seq))
  "vibrato prevalence is not currently relevant for chord-seqs"
  0.0)


;;; P-26 Microtone Prevalence: fraction of notes with mc-value diverting from
;;; tempered chromatic scale with more than a threshold (default 10 mc)

(defmethod microtone-prevalence ((self om::chord-seq) &optional (threshold 10))
  "P-26 Microtone Prevalence: fraction of notes with mc-value diverting from
tempered chromatic scale with more than a threshold (default 10 mc)"
  (let ((pitches (om::flat (om::lmidic self))))
    (fraction-of-passed-data
     #'(lambda (x) (< threshold (mod x 100) (- 100 threshold)))
     pitches)))

;;;; MELODIC FEATURES


;;; M-1 Melodic Interval Histogram: A feature vector consisting of the
;;; normalized bin magnitudes of occuring melodic intervals.  Rising and falling
;;; intervals are treated as identical.

(defun populate-interval-histogram (intervals &optional (normalize 100))
  ;; expects intervals between 0->127
  (let ((tab (loop for i from 0 to 127 collect (cons i 0))))
    (mapcar #'(lambda (int)
		(if (assoc int tab :test #'=)
		    (incf (cdr (assoc int tab :test #'=)))
		    (error "interval ~A is not 0->127" int)))
	    intervals)
    (normalize-histogram tab normalize)))

(defun mc->semitones (lmidic &optional (round 100))
  ;; transcribe lmidic to semitone step intervals
  (mapcar #'(lambda (x) (round x round))
	  (om::om-abs (om::x->dx (om::flat lmidic)))))

(defmethod melodic-interval-histogram ((self om::chord-seq))
  "M-1 Melodic Interval Histogram: A feature vector consisting of the
normalized bin magnitudes of occuring melodic intervals.  Rising and falling
intervals are treated as identical."
  (let ((intervals (mc->semitones (om::lmidic self))))
    (populate-interval-histogram intervals t)))

(defmethod most-common-melodic-interval ((self om::chord-seq))
  "M-2 Most Common Melodic Interval: Number of semitones corresponding to the
most frequently occurring melodic interval."
  (let ((intervals (om::om-abs (om::x->dx (om::flat (om::lmidic self))))))
    (caar (most-common-item intervals))))

(defmethod mean-melodic-interval ((self om::chord-seq))
  "M-3 Mean Melodic Interval: Mean average (in semitones) of the intervals
involved in each of the melodic intervals in the piece."
  (let ((intervals (om::om-abs (om::x->dx (om::flat (om::lmidic self))))))
    ;; ?? round to resolution in incoming data ??
    (round (om::average intervals nil))))

;;; M-4 Number of Common Melodic Intervals: Number of different melodic intervals
;;; that each account individually for at least 9% of all melodic intervals.

(defun count-items-above-threshold (data &optional (threshold 0.09))
  (let ((total-N (length data))
	(item-counts (most-common-item data)))
    (count-if #'(lambda (item) (>= (/ (cdr item) total-N) threshold))
	      item-counts)))

(defmethod number-of-common-melodic-intervals ((self om::chord-seq))
  "M-4 Number of Common Melodic Intervals: Number of different melodic intervals
that each account individually for at least 9% of all melodic intervals."
  (let ((threshold 0.09))				    ; = 9%
    (count-items-above-threshold
     (om::om-abs (om::x->dx (om::flat (om::lmidic self))))
     threshold)))

;;; M-5 

(defun distance-betweeen-two-most-common-items (data)
  (let* ((intervals-by-occurence (most-common-item data))
	 (a (caar intervals-by-occurence))
	 (b (caadr intervals-by-occurence)))
    (abs (- b a))))

(defmethod distance-betweeen-two-most-common-melodic-intervals ((self om::chord-seq))
  "M-5 Distance Between Most Prevalent Melodic Intervals: Absolute value of the
difference (in semitones) between the most common and second most common melodic
intervals in the piece."
  (distance-betweeen-two-most-common-items
   (om::om-abs (om::x->dx (om::flat (om::lmidic self))))))

;;; M-6 Prevalence of Most Common Melodic Interval: Fraction of all melodic
;;; intervals that corresponds to the most common melodic interval.

(defun fraction-of-most-common-in-data (data)
  (let* ((sorted-items (most-common-item data))
	 (most-common (car sorted-items))
	 (all-the-rest (cdr sorted-items)))
    (/ (cdr most-common) (apply #'+ (mapcar #'cdr all-the-rest)))))

(defmethod prevalence-of-most-common-melodic-interval ((self om::chord-seq))
  "M-6 Prevalence of Most Common Melodic Interval: Fraction of all melodic
intervals that corresponds to the most common melodic interval."
  (let ((intervals (om::om-abs (om::x->dx (om::flat (om::lmidic self))))))
    (fraction-of-most-common-in-data intervals)))

;;; M-7 Relative Prevalence of Most Common Melodic Intervals: Relative frequency of
;;; the second most common melodic interval in the piece, divided by the relative
;;; frequency of the most common melodic interval.

(defun relative-prevalence-of-two-most-common-in-data (data)
  (let* ((sorted-items (most-common-item data))
	 (first (car sorted-items))
	 (second (cadr sorted-items)))
    (/ (cdr second) (cdr first))))

(defmethod relative-prevalence-of-most-common-intervals ((self om::chord-seq))
  "M-7 Relative Prevalence of Most Common Melodic Intervals: Relative frequency of
the second most common melodic interval in the piece, divided by the relative
frequency of the most common melodic interval."
  (let ((intervals (om::om-abs (om::x->dx (om::flat (om::lmidic self))))))
    (relative-prevalence-of-two-most-common-in-data intervals)))


;;; M-8 Amount of Arpeggiation: Fraction of melodic intervals that are repeated
;;; notes, minor thirds, major thirds, perfect fifths, minor sevenths, major
;;; sevenths, octaves, minor tenths or major tenths. This is only a very approximate
;;; measure of the amount of arpeggiation in the music, of course.

(defun fraction-of-items-in-data (items wanted-items &key (test '=))
  (loop for (key . val) in items
     when (find key wanted-items :test test)
     sum val into sum-of-fractions
     finally (return sum-of-fractions)))

(defun arpeggiation-factor (data arpeggiato-intervals)
  (let ((tab (populate-interval-histogram data t)))
    (fraction-of-items-in-data tab arpeggiato-intervals)))

(defun fraction-of-matches-in-data (items &key (test '=))
  ;; to measure matches on a predicate
  (loop for (key . val) in items
     when (funcall test key)
     sum val into sum-of-fractions
     finally (return sum-of-fractions)))

(defmethod amount-of-arpeggiation ((self om::chord-seq))
  "M-8 Amount of Arpeggiation: Fraction of melodic intervals that are repeated
notes, minor thirds, major thirds, perfect fifths, minor sevenths, major
sevenths, octaves, minor tenths or major tenths. This is only a very approximate
measure of the amount of arpeggiation in the music, of course."
  (let ((intervals (mc->semitones (om::lmidic self)))
	(look-for '(0 3 4 7 10 11 12 15 16)))
    (arpeggiation-factor intervals look-for)))


;;; M-9 Repeated Notes: Fraction of melodic intervals that correspond to repeated
;;; notes.

(defmethod repeated-notes ((self om::chord-seq))
  "M-9 Repeated Notes: Fraction of melodic intervals that correspond to repeated notes."
  (let ((intervals (mc->semitones (om::lmidic self)))
	(look-for '(0)))
    (arpeggiation-factor intervals look-for)))

;;; M-10 Chromatic Motion: Fraction of melodic intervals that correspond to a
;;; semitone.

(defmethod chromatic-motion ((self om::chord-seq))
  "M-10 Chromatic Motion: Fraction of melodic intervals that correspond to a semitone."
  (let ((intervals (mc->semitones (om::lmidic self)))
	(look-for '(1)))
    (arpeggiation-factor intervals look-for)))

;;; M-11 Stepwise Motion: Fraction of melodic intervals that correspond to a minor
;;; or major second.

(defmethod stepwise-motion ((self om::chord-seq))
  "M-11 Stepwise Motion: Fraction of melodic intervals that correspond to a minor or major second."
  (let ((intervals (mc->semitones (om::lmidic self)))
	(look-for '(1 2)))
    (arpeggiation-factor intervals look-for)))

;;; M-12 Melodic Thirds: Fraction of melodic intervals that are major or minor
;;; thirds.

(defmethod melodic-thirds ((self om::chord-seq))
  "M-12 Melodic Thirds: Fraction of melodic intervals that are major or minor thirds."
  (let ((intervals (mc->semitones (om::lmidic self)))
	(look-for '(3 4)))
    (arpeggiation-factor intervals look-for)))

;;; M-13 Melocid Perfect Fourths: Fraction of melodic intervals that are perfect
;;; fourths.

(defmethod melodic-fourths ((self om::chord-seq))
  "M-13 Melocid Perfect Fourths: Fraction of melodic intervals that are perfect fourths."
  (let ((intervals (mc->semitones (om::lmidic self)))
	(look-for '(5)))
    (arpeggiation-factor intervals look-for)))

;;; M-14 Melodic Tritones: Fraction of melodic intervals that are tritones.

(defmethod melodic-tritones ((self om::chord-seq))
  "M-14 Melodic Tritones: Fraction of melodic intervals that are tritones."
  (let ((intervals (mc->semitones (om::lmidic self)))
	(look-for '(6)))
    (arpeggiation-factor intervals look-for)))

;;; M-15 Melodic Fifths: Fraction of melodic intervals that are perfect fifths.

(defmethod melodic-fifths ((self om::chord-seq))
  "M-15 Melodic Fifths: Fraction of melodic intervals that are perfect fifths."
  (let ((intervals (mc->semitones (om::lmidic self)))
	(look-for '(7)))
    (arpeggiation-factor intervals look-for)))

;;; M-16 Melodic Sixths: Fraction of melodic intervals that are major or minor
;;; sixths.

(defmethod melodic-sixths ((self om::chord-seq))
  "M-16 Melodic Sixths: Fraction of melodic intervals that are major or minor sixths."
  (let ((intervals (mc->semitones (om::lmidic self)))
	(look-for '(8 9)))
    (arpeggiation-factor intervals look-for)))

;;; M-17 Melodic Sevenths: Fraction of melodic intervals that are major or minor
;;; sevenths.

(defmethod melodic-sevenths ((self om::chord-seq))
  "M-17 Melodic Sevenths: Fraction of melodic intervals that are major or minor sevenths."
  (let ((intervals (mc->semitones (om::lmidic self)))
	(look-for '(10 11)))
    (arpeggiation-factor intervals look-for)))

;;; M-18 Melodic Octaves: Fraction of melodic intervals that are octaves.

(defmethod melodic-octaves ((self om::chord-seq))
  "M-18 Melodic Octaves: Fraction of melodic intervals that are octaves."
  (let ((intervals (mc->semitones (om::lmidic self)))
	(look-for '(12)))
    (arpeggiation-factor intervals look-for)))

;;; M-19 Melodic Large Intervals: Fraction of melodic intervals greater than one
;;; octave.

(defmethod melodic-large-intervals ((self om::chord-seq))
  "M-19 Melodic Large Intervals: Fraction of melodic intervals greater than one octave."
  (let* ((intervals (mc->semitones (om::lmidic self)))
	 (tab (populate-interval-histogram intervals t)))
    (fraction-of-matches-in-data tab :test #'(lambda (x) (> x 12)))))

;;; M-20 Minor Major Melodic Third Ratio: Combined fraction of all melodic intervals
;;; that are minor thirds, divided by the combined fraction of all melodic intervals
;;; that are major thirds. Set to 0 if there are no melodic minor thirds or melodic
;;; major thirds.


(defmethod melodic-minor-major-third-ratio ((self om::chord-seq))
  "M-20 Minor<->Major Melodic Third Ratio: Combined fraction of all melodic intervals
that are minor thirds, divided by the combined fraction of all melodic intervals
that are major thirds."
  (let* ((intervals (mc->semitones (om::lmidic self)))
	 (tab (populate-interval-histogram intervals t))
	 (minor-thirds (cdr (nth 3 tab)))
	 (major-thirds (cdr (nth 4 tab))))
    (or (and (plusp minor-thirds) (plusp major-thirds)
	     ;; AV: should this perhaps ensure a factor 0->1.0?
	     (/ minor-thirds major-thirds))
	0)))

;;; M-21 Melodic Embellishments: Fraction of all notes that are surrounded on
;;; both sides by notes with durations at least three times as long as the
;;; central note. Set to 0 if there are no notes in the piece.

(defmethod melodic-embellishments ((self om::chord-seq))
  "M-21 Melodic Embellishments: Fraction of all notes that are surrounded on
both sides by notes with durations at least three times as long as the
central note. Set to 0 if there are no notes in the piece."
  (let ((durs (om::flat (om::ldur self))))
    (loop
       with N = (length durs)
       with embellishments = 0
       for (a b c) on durs
       while c
       when (and (<=  (* 3 b) a)
		 (<=  (* 3 b) c))
       do (incf embellishments)
       and
       collect (list a b c) into utliste
       finally (return (float (/ embellishments N))))))

;;; M-22 Direction of Melodic Motion: Fraction of melodic intervals that are rising
;;; in pitch. Set to zero if no rising or falling melodic intervals are found.

(defmethod direction-of-melodic-motion ((self om::chord-seq))
  "M-22 Direction of Melodic Motion: Fraction of melodic intervals that are
rising in pitch. Set to zero if no rising or falling melodic
intervals are found."
  (let* ((intervals (mapcar #'(lambda (x) (round x 100))
			    (om::x->dx (om::flat (om::lmidic self)))))
	 (N (length intervals)))
    (loop
       with melodic-motion? = 0
       for (a b) on intervals
       when (and a b)
       ;; melodic motion = not directly repeating pitch
       unless (= a b)		 
       do (incf melodic-motion?)
       finally (return (float (/ melodic-motion? N))))))

;;; M-23 Average Length of Melodic Arcs: Average number of notes that separate
;;; melodic peaks and troughs. Similar assumptions are made in the calculation of
;;; this feature as for the Melodic Interval Histogram. Set to 0 if no melodic arcs
;;; are found.

(defun direction-count (dirs dir-counts previous-dir N-current-dir)
  ;; dirs is a list of steps, ie: melodic directions
  (if (null dirs)
      (nreverse (cons (1+ N-current-dir) dir-counts))
      (let* ((current-dir (car dirs))
	     (peak? (minusp (* current-dir previous-dir)))) ;change in direction
	(direction-count (cdr dirs)
			 (if peak? (cons (1+ N-current-dir) dir-counts) dir-counts)
			 ;; if 0, continue counting until change of dir
			 (if (zerop current-dir) previous-dir current-dir)
			 (if peak? 0 (1+ N-current-dir))))))

;; (let ((aaa '(1 1 1 0 -1 -1 0 0 -1 1 -1 -1 -1)))
;;   (direction-count (cdr aaa) '() (car aaa) 0))

(defun average-arc-length (dirs)
  (let ((counts (direction-count (cdr dirs) '() (car dirs) 0)))
    (float (/ (apply #'+ counts)
	      (length counts)))))

(defmethod average-length-of-melodic-arcs ((self om::chord-seq))
  "M-23 Average Length of Melodic Arcs: Average number of notes that separate
melodic peaks and troughs.  Set to 0 if no melodic arcs are found."
  (let ((intervals (om::x->dx (mc->semitones (om::lmidic self)))))
    (if (zerop (apply #'+ intervals))
	0
	(average-arc-length intervals))))

;;; M-24 Average Interval Spanned by Melodic Arcs: Average melodic interval (in
;;; semitones) separating the top note of melodic peaks and the bottom note of
;;; adjacent melodic troughs.

(defun strip-fringe-zeroes (vals)
  (loop
     for x in vals
     for j from 1
     unless (and (or (= j 1) (= j (length vals)))
		 (zerop x))
     collect x))

(defun local-peak? (a b c)
  ;; TODO: check true local peak, checking context dynamically
  (not (= (signum (- b a)) (signum (- c b)))))

(defun average-intervals-of-arcs (vals)
  (loop
     with prev-val = (car vals)
     for (a b c) on vals
     while (and a b c)
     when (and (local-peak? a b c)
	       (not (zerop (- b prev-val))))
     collect (prog1
		 (- b prev-val)
	       (setf prev-val b))
     into peak-to-peak-intervals
     finally (let* ((last-interval (- (car (last vals)) prev-val))
		    (loi (strip-fringe-zeroes
			  (nconc peak-to-peak-intervals
				 (list last-interval)))))
	       (return
		 (float (/ (apply #'+ loi) (length loi)))))))


(defmethod average-interval-spanned-by-melodic-arcs ((self om::chord-seq))
  "M-24 Average Interval Spanned by Melodic Arcs: Average melodic interval (in
semitones) separating the top note of melodic peaks and the bottom note of
adjacent melodic troughs.  Returns value in mc"
  (let ((pitches (om::flat (om::lmidic self))))
    (average-intervals-of-arcs pitches)))

;;; M-25 Melodic Pitch Variety: Average number of notes that go by before a
;;; note's pitch is repeated (including the repeated note itself).  Notes that
;;; do not recur after 16 notes are not included in this calculation. Set to 0
;;; if there are no qualifying repeated notes in the piece.

(defun how-many-until-repeated (val vals &optional (max 16) &key (test 'eql))
  (or (loop
	 for this in vals
	 for i from 0 below max
	 when (funcall test val this)
	 do (progn 
	      ;; include count for repeated val
	      (return (1+ i))
	      (loop-finish)))
      0))

(defun average-n-before-repetion (seq)
  (loop
     for val in seq
     for tail on (cdr seq)
     for N from 1
     sum (how-many-until-repeated val tail) into total
     ;; return average
     finally (return (float (/ total N)))))

(defmethod melodic-pitch-variety ((self om::chord-seq))
  "M-25 Melodic Pitch Variety: Average number of notes that go by before a
note's pitch is repeated (including the repeated note itself).  Notes that
do not recur after 16 notes are not included in this calculation. Set to 0
if there are no qualifying repeated notes in the piece."
  (let ((pitches (om::flat (om::lmidic self))))
    (average-n-before-repetion pitches)))

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

