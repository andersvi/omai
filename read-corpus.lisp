;;;
;;; read corpus from file, tokenize, parse, split, filter...
;;;


(defparameter *stop-list*
   '("a" "about" "also" "an" "and" "any" "are" "as" "at" 
     "be" "been" "but" "by" "can" "could" "do" "for" "from" 
     "had" "has" "have" "he" "her" "him" "his" "how" "i" "if" 
     "in" "is" "it" "its" "la" "may" "most" "new" "no" "not" 
     "of" "on" "or" "she" "some" "such" "than" "that" "the" 
     "their" "them" "there" "these" "they" "this" "those" "to" 
     "was" "we" "were" "what" "when" "where" "which" "who" 
     "will" "with" "would" "you"))

(defparameter *stopwords*
    (let ((hash (make-hash-table :test #'equal)))
      (mapcar #'(lambda (w) (setf (gethash w hash) T))
	      *stop-list*)
      hash))

(defun stop-word-p (w)
  "Predicate that checks whether a word is on the stop-list."
  (gethash w *stopwords*))

(defun normalize-token (string)
 "Text normalization, to be applied to individual tokens."
 (string-downcase 
  (string-trim " ,.!?(){}[]-+@&\";:'*#" string)))

(defun tokenize (string)
  "Splits a sequence into tokens, filters stop-words and normalizes."
  (loop 
      for start = 0 then (+ space 1)
      for space = (position #\space string :start start)
      for token = (normalize-token (subseq string start space))
      unless (or (string= token "") 
		 (stop-word-p token))
      collect token
      until (not space)))

(defun read-corpus-to-vs (corpus wordlist)
  "Returns a vector space model based on the tokens in the corpus."
  (let ((vs (make-vs)))
    ;; read in the list of words to be modeled:
    (with-open-file (stream wordlist :direction :input)
      (loop
	 with matrix = (vs-matrix vs)
	 for word = (read stream nil nil)
	 while word 
	 ;; create a feature vector for each word:
	 do (setf (gethash (normalize-token word) matrix)
		  (make-hash-table :test #'equal))))
    
   ;; record bag-of-words features from the corpus, 
   ;; updating the counts in the feature vectors:
    (with-open-file (stream corpus :direction :input)
      (loop
	 for line = (read-line stream nil nil)
	 while line
	 do
	 ;; nested loop to (1) find target words, and (2) for each
	 ;; target extract features and update its feature vector:
	   (loop
	      with tokens = (tokenize line)
	      for token in tokens
	      for i from 0
	      for feat-vect = (get-feature-vector vs token)
	      when feat-vect ;; target word?
	      do 
		(loop 		      
		   for feature in tokens
		   for j from 0
		   ;;; we don't count a token occurrence as a feature of itself:
		   unless (= i j)
		   do (incf (gethash feature feat-vect 0))))))
    vs))


;; (read-corpus-to-vs "brown2.txt" "words.txt")
