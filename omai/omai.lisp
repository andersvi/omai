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
;============================================================================
; Authors: Anders Vinjar, Jean Bresson - 2018
;============================================================================
; 
; LOADER FILE FOR OM6
;
;============================================================================


(in-package :om)

(om::set-lib-release 1.0 (find-library "omai"))

(compile&load (merge-pathnames "sources/package" *load-pathname*))
(compile&load (merge-pathnames "sources/features" *load-pathname*))
(compile&load (merge-pathnames "sources/vector-space" *load-pathname*))
(compile&load (merge-pathnames "sources/k-means" *load-pathname*))
(compile&load (merge-pathnames "sources/words" *load-pathname*))

(om::fill-library 
 '((nil nil (omai::vector-space) (omai::get-feature-values omai::get-class omai::get-similarity omai::estimate-class omai::make-word-vectors) nil)))

(om::om-print "==============================" )
(om::om-print "OMAI - AI Tools for OM" )
(om::om-print "==============================" )

