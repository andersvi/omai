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
;;; 

#|


;;;; PITCH STATISTICS

;;;
;;;     P-1 Basic Pitch Histogram
;;;

;;;
;;;     P-2 Pitch Class Histogram
;;;

;;;
;;;     P-3 Folded Fifths Pitch Class Histogram
;;;

;;;
;;;     P-4 Prevalence of Most Common Pitch
;;;

;;;
;;;     P-5 Prevalence of Most Common Pitch Class
;;;

;;;
;;;     P-6 Relative Prevalence of Top Pitches
;;;

;;;
;;;     P-7 Relative Prevalence of Top Pitch Classes
;;;

;;;
;;;     P-8 Interval Between Most Prevalent Pitches
;;;

;;;
;;;     P-9 Interval Between Most Prevalent Pitch Classes
;;;

;;;
;;;     P-10 Number of Common Pitches
;;;

;;;
;;;     P-11 Pitch Variety
;;;

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


|#

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
