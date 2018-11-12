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


;;; P-11 Pitch Variety: Number of pitches that occur at least once in the
;;; piece. Enharmonic equivalents are grouped together for the purpose of this
;;; calculation.


;;; P-12 Pitch Class Variety: Number of pitch classes that occur at
;;; least once in the piece. Enharmonic equivalents are grouped
;;; together for the purpose of this calculation.

;;; P-13 Range: Difference in semitones between the highest and
;;; lowest pitches.

;;; P-14 Most Common Pitch: MIDI pitch value of the most frequently
;;; occurring pitch.

;;; P-15 Mean Pitch: Mean MIDI pitch value, averaged across all pitched
;;; notes in the piece. Set to 0 if there are no pitched notes.

;;; P-16 Importance of Bass Register: Fraction of notes between MIDI pitches
;;; 0 and 54.

;;; P-17 Importance of Middle Register: Fraction of notes between MIDI
;;; pitches 55 and 72.

;;; P-18 Importance of High Register: Fraction of notes between MIDI pitches
;;; 73 and 127.

;;; P-19 Most Common Pitch Class: The pitch class that occurs most
;;; frequently compared to other pitch classes. A value of 0 corresponds to
;;; C, and pitches increase chromatically by semitone in integer units
;;; (e.g. a value of 2 would mean that D is the most common pitch
;;; class). Enharmonic equivalents are treated as a single pitch class.

;;; P-20 Dominant Spread: Largest number of consecutive pitch classes
;;; separated by perfect 5ths that each individually account for at least 9%
;;; of the total notes in the piece.

;;; P-21 Strong Tonal Centres: Number of isolated peaks in the fifths pitch
;;; histogram that each individually account for at least 9% of all notes in
;;; the piece.

;;; P-22 Major or Minor: Whether the piece is major or minor, as indicated
;;; by the first encountered major/minor metadata tag in the piece. Set to 0
;;; if the metadata indicates that the piece is major, or set to 1 if the
;;; metadata indicates that it is minor. Defaults to 0 if the key signature
;;; is unknown.

;;; P-23 Glissando Prevalence: Number of pitched MIDI Note Ons that have at
;;; least one MIDI Pitch Bend associated with them, divided by the total
;;; number of pitched Note Ons in the piece.

;;; P-24 Average Range of Glissandos: Average range of MIDI Pitch Bends,
;;; where "range" is defined as the greatest value of the absolute
;;; difference between 64 and the second data byte of all MIDI Pitch Bend
;;; messages falling between the Note On and Note Off messages of any note
;;; in the piece. Set to 0 if there are no MIDI Pitch Bends in the piece.

;;; P-25 Vibrato Prevalence: Number of pitched notes that have associated
;;; MIDI Pitch Bend messages change direction at least twice in connection
;;; with the note in question, divided by the total number of pitched Note
;;; Ons in the piece.

;;; P-26 Microtone Prevalence: Number of pitched notes that are each
;;; associated with exactly one MIDI Pitch Bend message, divided by the
;;; total number of pitched Note Ons in the piece. Set to 0 if there are no
;;; pitched Note Ons in the piece.


;;;; MELODIC FEATURES


;;; M-1 Melodic Interval Histogram: A feature vector consisting of the bin
;;; magnitudes of the melodic interval histogram described above. Each bin
;;; corresponds to a melodic interval, and the bin index indicates the number of
;;; semitones comprising the interval associated with the bin (there are 128 bins in
;;; all). For example, bin 0 corresponds to repeated pitches, bin 1 to a melodic
;;; interval of one semitone, bin 2 to a melodic interval of 2 semitones, etc. The
;;; magnitude of each bin is proportional to the fraction of melodic intervals in
;;; the piece that are of the kind associated with the bin (this histogram is
;;; normalized). Rising and falling intervals are treated as identical. Melodies are
;;; assumed to be contained within individual MIDI tracks and channels, so melodic
;;; intervals are found separately for each track and channel before being combined
;;; in this histogram. It is also assumed that there is only one melody at a time
;;; per MIDI channel (if multiple notes occur simultaneously on the same MIDI tick
;;; on the same MIDI track and channel, then all notes but the first note on that
;;; tick are ignored). Other than this, all notes on the same track and the same
;;; channel are treated as if they are part of a single melody. It is also assumed
;;; that melodies do not cross MIDI tracks or channels (i.e. that they are each
;;; separately contained in their own track and channel). Only pitched notes are
;;; considered, so all notes on the unpitched MIDI Channel 10 are ignored.

;;; M-2 Most Common Melodic Interval: Number of semitones corresponding to the most
;;; frequently occurring melodic interval.

;;; M-3 Mean Melodic Interval: Mean average (in semitones) of the intervals involved
;;; in each of the melodic intervals in the piece.

;;; M-4 Number of Common Melodic Intervals: Number of different melodic intervals
;;; that each account individually for at least 9% of all melodic intervals.

;;; M-5 Distance Between Most Prevalent Melodic Intervals: Absolute value of the
;;; difference (in semitones) between the most common and second most common melodic
;;; intervals in the piece.

;;; M-6 Prevalence of Most Common Melodic Interval: Fraction of all melodic
;;; intervals that corresponds to the most common melodic interval.

;;; M-7 Relative Prevalence of Most Common Melodic Intervals: Relative frequency of
;;; the second most common melodic interval in the piece, divided by the relative
;;; frequency of the most common melodic interval.

;;; M-8 Amount of Arpeggiation: Fraction of melodic intervals that are repeated
;;; notes, minor thirds, major thirds, perfect fifths, minor sevenths, major
;;; sevenths, octaves, minor tenths or major tenths. This is only a very approximate
;;; measure of the amount of arpeggiation in the music, of course.


;;; M-9 Repeated Notes: Fraction of melodic intervals that correspond to repeated
;;; notes.

;;; M-10 Chromatic Motion: Fraction of melodic intervals that correspond to a
;;; semitone.

;;; M-11 Stepwise Motion: Fraction of melodic intervals that correspond to a minor
;;; or major second.

;;; M-12 Melodic Thirds: Fraction of melodic intervals that are major or minor
;;; thirds.

;;; M-13 Melocid Perfect Fourths: Fraction of melodic intervals that are perfect
;;; fourths.

;;; M-14 Melodic Tritones: Fraction of melodic intervals that are tritones.

;;; M-15 Melodic Fifths: Fraction of melodic intervals that are perfect fifths.

;;; M-16 Melodic Sixths: Fraction of melodic intervals that are major or minor
;;; sixths.

;;; M-17 Melodic Sevenths: Fraction of melodic intervals that are major or minor
;;; sevenths.

;;; M-18 Melodic Octaves: Fraction of melodic intervals that are octaves.

;;; M-19 Melodic Large Intervals: Fraction of melodic intervals greater than one
;;; octave.

;;; M-20 Minor Major Melodic Third Ratio: Combined fraction of all melodic intervals
;;; that are minor thirds, divided by the combined fraction of all melodic intervals
;;; that are major thirds. Set to 0 if there are no melodic minor thirds or melodic
;;; major thirds.

;;; M-21 Melodic Embellishments: Fraction of all notes that are surrounded on both
;;; sides by MIDI Note Ons on the same MIDI channel that have durations at least
;;; three times as long as the central note. Set to 0 if there are no notes in the
;;; piece.

;;; M-22 Direction of Melodic Motion: Fraction of melodic intervals that are rising
;;; in pitch. Set to zero if no rising or falling melodic intervals are found.

;;; M-23 Average Length of Melodic Arcs: Average number of notes that separate
;;; melodic peaks and troughs. Similar assumptions are made in the calculation of
;;; this feature as for the Melodic Interval Histogram. Set to 0 if no melodic arcs
;;; are found.

;;; M-24 Average Interval Spanned by Melodic Arcs: Average melodic interval (in
;;; semitones) separating the top note of melodic peaks and the bottom note of
;;; adjacent melodic troughs. Similar assumptions are made in the calculation of
;;; this feature as for the Melodic Interval Histogram.

;;; M-25 Melodic Pitch Variety: Average number of notes that go by in a MIDI channel
;;; before a note's pitch is repeated (including the repeated note itself). This is
;;; calculated across each channel individually before being combined. Notes that
;;; occur simultaneously on the same MIDI tick are only counted as one note for the
;;; purpose of this calculation. Notes that do not recur after 16 notes in the same
;;; channel are not included in this calculation. Set to 0 if there are no
;;; qualifying repeated notes in the piece.


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
