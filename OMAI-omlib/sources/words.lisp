
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
; Application of vetor-space for word processing
; In this case features of a word are other words, and values are scores for closeness
;=====================================================

(in-package :omai)


;;;=================
;;; TEXT TO VECTOR SPACE
;;;=================

(defun wordlist-from-file (file)
  (with-open-file (stream file :direction :input)
    (loop for word = (read stream nil nil)
          while word collect word)))

(defparameter *stop-list*
   '("a" "about" "also" "an" "and" "any" "are" "as" "at" 
     "be" "been" "but" "by" "can" "could" "do" "for" "from" 
     "had" "has" "have" "he" "her" "him" "his" "how" "i" "if" 
     "in" "is" "it" "its" "la" "may" "most" "new" "no" "not" 
     "of" "on" "or" "she" "some" "such" "than" "that" "the" 
     "their" "them" "there" "these" "they" "this" "those" "to" 
     "was" "we" "were" "what" "when" "where" "which" "who" 
     "will" "with" "would" "you"))


(defparameter *stop-words* (make-hash-table :test #'equal))

(defun stop-word-p (w)
  "Predicate that checks whether a word is on the stop-list."
  (gethash w *stop-words*))

(defmethod add-words-to-stop-list ((words string))
  (setf (gethash (string-downcase words) *stop-words*) T))

(defmethod add-words-to-stop-list ((words t)) 
  (error "the stop-list only accepts strings!"))

(defmethod add-words-to-stop-list ((words list)) 
  (loop for word in words do 
        (add-words-to-stop-list word)))

(defun init-stop-list (&optional words)
  (clrhash *stop-words*)
  (add-words-to-stop-list words)
  *stop-words*)

;;; INIT WITH *STOP-LIST*
(init-stop-list *stop-list*)


(defun normalize-token (string)
 "Text normalization, to be applied to individual tokens."
 (declare (type string string))
 (string-downcase 
  (string-trim " ,.!?(){}[]-+@&\";:'*#" string)))

(defun tokenize (string)
  "Splits a sequence into tokens, filters stop-words and normalizes."
  (loop for start = 0 then (+ space 1)
        for space = (position #\space string :start start)
        for token = (normalize-token (subseq string start space))
        unless (or (string= token "") 
                   (stop-word-p token))
        collect token
        until (not space)))




;;;=================
;;; LEARN
;;;=================

;;; all words in the same lines of a given word are considered its "features"
;;; this function creates these vectors for a series of words
;; record bag-of-words features from the corpus, 
;; updating the counts in the feature vectors: 

;;; generates a vector-space (hash-table of vectors)
(defun vs-learn-line-of-text (vectors line feature-words)
  ;; nested loop to (1) find target words, and (2) for each
  ;; target extract features and update its feature vector:
  
  (declare (type hash-table vectors)
           (type string line))

  (loop with tokens = (tokenize line)
        for token in tokens
        for i from 0
        for feat-vect = (gethash (normalize-token token) vectors)
        when feat-vect ;; only the initialized words are considered  
        do (loop for feature in tokens
                 for j from 0
                 unless (= i j)  ;;; we don't count a token occurrence as a feature of itself:
                 do (let ((p (position feature feature-words :test 'equal)))
                      (when p (setf (nth p feat-vect) (1+ (nth p feat-vect)))))
                 ))
  )

; Learns from corpus 
(defmethod read-corpus ((vectors-ht hash-table)  (corpus pathname) feature-words)
  (with-open-file (stream corpus :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          do (vs-learn-line-of-text vectors-ht line feature-words))
    ))

(defmethod read-corpus ((vectors-ht hash-table) (corpus-lines list) feature-words)
  (loop for line in corpus-lines
        do (vs-learn-line-of-text vectors-ht line feature-words)))

(defmethod read-corpus ((vectors-ht hash-table) (corpus-lines om:textbuffer) feature-words)
  (loop for line in corpus-lines
        do (vs-learn-line-of-text vectors-ht line feature-words)))



;;;==================
;;; exported function
;;;==================

(defmethod make-word-vectors ((words list) corpus feature-words)
  
  (let ((vectors-ht (make-hash-table :test #'equal)))
    
    (loop for w in words do
          (setf (gethash (normalize-token w) vectors-ht) 
                (make-list (length feature-words) :initial-element 0)))
    
    (read-corpus vectors-ht corpus feature-words)
    
    (loop for vec being the hash-values of vectors-ht
          do (normalize-vector vec))
    
    vectors-ht))

;;;=====================
;;; specialized function
;;;=====================

(defmethod vector-similarity ((self vector-space) (w1 string) (w2 string) test)
  (call-next-method self (normalize-token w1) (normalize-token w2) test))



#|

;;; TEST SEQUENCE

(defparameter text-file (merge-pathnames "data/text.txt" (om::lib-resources-folder (om::find-library "omai"))))
(defparameter words-file (merge-pathnames "data/words.txt" (om::lib-resources-folder (om::find-library "omai"))))
(defparameter classes-file (merge-pathnames "data/classes.txt" (om::lib-resources-folder (om::find-library "omai"))))
(defparameter test-file (merge-pathnames "data/unknown.txt" (om::lib-resources-folder (om::find-library "omai"))))

;;; create a feature vector space with all the words in the corpus
;;; each words registers how many time another word is in the same phrase (normalized / length pf the phrases)

(defparameter *vectors* (make-word-vectors (wordlist-from-file words-file) text-file))
  

;;; check the n most important words in the feature vector
(get-features *vectors* "congo" 20)


;;; check whether words have strong similarity (= strong correlations in their feature vectors)
(vector-similarity *vectors* "africa" "congo" #'dot-product)
(vector-similarity *vectors* "africa" "america" #'dot-product)
(vector-similarity *vectors* "butter" "america" #'dot-product)


;;; CLASSIFICATION

(defparameter *classes* (read-classes-from-file classes-file))
(compute-class-centroids *classes* *vectors*)


(classify (get-feature-vector *vectors* (normalize-token "sheriff")) *classes* #'dot-product)

(with-open-file (stream test-file :direction :input)
  (loop for word = (read stream nil nil)
        while word
        collect (list (normalize-token word) 
                      (classify (get-feature-vector *vectors* (normalize-token word)) *classes* #'dot-product))))

;; ==>
;;
;; (("fruit" :FOODSTUFF 0.3667868) ("california" :PERSON_NAME 0.29967904)
;;  ("peter" :PERSON_NAME 0.30886117) ("egypt" :PLACE_NAME 0.3074144)
;;  ("department" :INSTITUTION 0.5497112) ("hiroshima" :PLACE_NAME 0.2304558)
;;  ("robert" :PERSON_NAME 0.59566295) ("butter" :FOODSTUFF 0.3845482)
;;  ("pepper" :FOODSTUFF 0.36386278) ("asia" :PLACE_NAME 0.39868993)
;;  ("roosevelt" :TITLE 0.2597388) ("moscow" :PLACE_NAME 0.4841283)
;;  ("senator" :TITLE 0.35630104) ("university" :INSTITUTION 0.5805089)
;;  ("sheriff" :TITLE 0.22804837))

|#


