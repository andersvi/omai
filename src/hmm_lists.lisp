;; 
;; UNFINISHED
;;

(defun read-corpus-lists (file &optional (n 100))
  ;; read in training data
  (with-open-file (stream file :direction :input)
    (loop
       with n = (+ n 2)
       with hmm = (make-hmm)
       with transitions = (make-array (list n n) :initial-element nil)
       with emissions = (make-array n :initial-element nil)
       initially			; allocate transition and emission matrices
	 (loop
	    for i from 1 to (- n 1)
	    do (setf (aref emissions i) (make-hash-table :test #'equal)))

       ;; read one sexp of input at a time, car = observation, cadr = state (tag),
       ;; map the state (string) to a numeric identifier, and bind a local variable
       ;; to the emission hash table for that very state.  nil correspond to
       ;; sequence breaks, and we use the </seq> marker as the state for these
       ;; (once we have seen all tags, we will arrange for state </seq> to be at
       ;; index n - 1).  count frequencies for all surface forms (on non-nil lines)
       ;; and record state bi-grams, up to the </seq> state on nil input.

       for previous = (state2id hmm "<seq>") then current
       for input = (read stream nil :eof)
       for form = (car input)
       for current = (if input
			 (state2id hmm (cadr input))
			 (- n 1))
       for map = (aref emissions current)
       while input
       when form do (incf (gethash form map 0))
       do
	 (if (aref transitions previous current)
	     (incf (aref transitions previous current))
	     (setf (aref transitions previous current) 1))

       ;; as we advance past the end of a seq (and have incremented the transition
       ;; count into </seq>), reset again to the start of seq state.

       when (= current (- n 1)) do (setf current (state2id hmm "<seq>"))
       finally

       ;; at this point, we have seen n - 1 states, i.e. everything but the special
       ;; end-of-sentence marker; in the loop above, we have hard-coded its numeric
       ;; state index, so we still need to see to it that </seq> actually maps to
       ;; that identifier.

	 (state2id hmm "</seq>")
	 (setf (hmm-transitions hmm) transitions)
	 (setf (hmm-emissions hmm) emissions)
	 (return hmm))))

(setq ic-hmm (read-corpus-lists "eisner.list.cl" 2))
(transition-probability ic-hmm  (state2id ic-hmm "<seq>") (state2id ic-hmm  'A))
(transition-probability ic-hmm  (state2id ic-hmm "<seq>") (state2id ic-hmm  'b))
(transition-probability ic-hmm  (state2id ic-hmm 'a) (state2id ic-hmm  'b))
(transition-probability ic-hmm  (state2id ic-hmm  'b) (state2id ic-hmm  'c))
(transition-probability ic-hmm  (state2id ic-hmm  'c) (state2id ic-hmm  "</seq>"))


(hmm-n ic-hmm)
(hmm-tags ic-hmm)
