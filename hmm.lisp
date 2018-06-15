(in-package :cl-user)

;;; hmm, viterbi, smoothing - adapated to NLP and reading corpus of text

(defstruct hmm
  tags
  (n 0)
  transitions
  emissions)

(defun state2id (hmm tag)
  ;; look up numeric identifier for tag (string), allocate new identifier if
  ;; not yet seen
  (let ((n (position tag (hmm-tags hmm) :test #'string=)))
    (unless n
      (setf (hmm-tags hmm) (nconc (hmm-tags hmm) (list tag)))
      (setf n (hmm-n hmm))
      (incf (hmm-n hmm)))
    n))

;; smoothing: give a tiny amount of probability to unseen transitions, log space
;; here, to avoid underflow when multiplying together

(let ((default (log 1/1000000)))
  (defun transition-probability (hmm previous current)
    (or (aref (hmm-transitions hmm) previous current) default))
  (defun emission-probability (hmm state form)
    ;; use the same default probability in lieu of actual smoothing
    (gethash form (aref (hmm-emissions hmm) state) default)))

;; TODO: find useful representation for corpus: '(observation . state)

(defparameter *observation-state-separator* #\Tab)

(defun read-corpus (file &optional (n 100))
  ;; read in training data
  (with-open-file (stream file :direction :input)
    (loop
       with n = (+ n 2)
       with hmm = (make-hmm)
       with transitions = (make-array (list n n) :initial-element nil)
       with emissions = (make-array n :initial-element nil)
       initially		     ; allocate transition and emission matrices
	 (loop
	    for i from 0 to (- n 1)
	    do (setf (aref emissions i) (make-hash-table :test #'equal)))
       ;;
       ;; read one line of input at a time, break it up into the observation
       ;; (surface form) and state (tag), map the state (string) to a numeric
       ;; identifier, and bind a local variable to the emission hash table for
       ;; that very state.  empty lines correspond to sentence breaks, and we
       ;; use the end-of-sentence marker as the state for these (once we have
       ;; seen all tags, we will arrange for state </seq> to be at index n - 1).
       ;; count frequencies for all surface forms (on non-empty lines) and
       ;; record state bi-grams, up to the </seq> state on empty lines.
       ;;
       for previous = (state2id hmm "<seq>") then current
       for line = (read-line stream nil)
       for sep? = (position *observation-state-separator* line)
       for form = (subseq line 0 sep?)
       for current = (if sep? (state2id hmm (subseq line (+ sep? 1))) (- n 1))
       for map = (aref emissions current)
       while line
       when (and form (not (string= form ""))) do 
	 (incf (gethash form map 0))
       do
	 (if (aref transitions previous current)
	     (incf (aref transitions previous current))
	     (setf (aref transitions previous current) 1))
       ;;          
       ;; as we advance past the end of a sentence (and have incremented the
       ;; transition count into </seq>), reset to the start of sentence state.
       ;;          
       when (= current (- n 1)) do (setf current (state2id hmm "<seq>"))
       finally
       ;;
       ;; at this point, we have seen n - 1 states, i.e. everything but the
       ;; special end-of-sentence marker; in the loop() above, we have hard-
       ;; coded its numeric state index, so we still need to see to it that
       ;; </seq> actually maps to that identifier.
       ;;
	 (state2id hmm "</seq>")
	 (setf (hmm-transitions hmm) transitions)
	 (setf (hmm-emissions hmm) emissions)
	 (return hmm))))

(setf ic-hmm (read-corpus "eisner.tt" 2))
(transition-probability ic-hmm  (state2id ic-hmm "<seq>") (state2id ic-hmm  "H"))
(transition-probability ic-hmm  (state2id ic-hmm "<seq>") (state2id ic-hmm  "C"))
(transition-probability ic-hmm  (state2id ic-hmm "C") (state2id ic-hmm  "H"))
(transition-probability ic-hmm  (state2id ic-hmm  "C") (state2id ic-hmm  "</seq>"))
(emission-probability ic-hmm  (state2id ic-hmm  "C") "3")


(hmm-n ic-hmm)
(hmm-tags ic-hmm)
(hmm-emissions ic-hmm)


(defun train-hmm (hmm)
  (loop
     with transitions = (hmm-transitions hmm)
     with n = (hmm-n hmm)
     for i from 0 to (- n 2)
     ;;
     ;; to normalize raw frequencies to relative frequencies (using maximum
     ;; likelihood estimation), count how often the current (i) state has been
     ;; seen in the training data, i.e. the sum of transitions from this state
     ;; into any of the other states.
     ;;
     for total = (loop
		    for j from 0 to (- n 1)
		    sum (or (aref transitions i j) 0))
     do
     ;; transition probabilities P(s_j|s_i)
       (loop
	  for j from 1 to (- n 1)
	  for count = (aref transitions i j)
	  when count
	  do (setf (aref transitions i j) 
		   (log (/ count total))))
     ;; emission probabilities P(form|s_i)
       (loop
	  with map = (aref (hmm-emissions hmm) i)
	  for form being each hash-key in map
	  using (hash-value count)
	  when count do 
	    (setf (gethash form map) 
		  (log (/ count total)))))
  hmm)

;; (train-hmm ic-hmm)


(defparameter *ices* (train-hmm (read-corpus "eisner.tt" 2)))
(defparameter *wsj* (train-hmm (read-corpus "wsj.tt" 45)))

(defun viterbi (hmm input)
  (let* ((n (hmm-n hmm))
         (l (length input))
         (viterbi (make-array (list n l) :initial-element nil))
         (pointer (make-array (list n l) :initial-element nil)))
    ;;
    ;; the first `row' (at index 0) in our Viterbi matrix corresponds to the
    ;; first observation (whereas the index 0 in our HMM is reserved for the
    ;; start-of-sentence state).  initialize transitions from start state.
    ;;
    (loop
       with form = (first input)
       for state from 1 to (- n 2)
       do
	 (setf (aref viterbi state 0)
	       (+ 					    ;log probs
		(transition-probability hmm 0 state)
		(emission-probability hmm state form)))
	 (setf (aref pointer state 0) 0))

    ;; the main loop of the Viterbi algorithm: fill in the two matrices in one
    ;; left-to-right (in terms of the observation sequence) iteration

    (loop
       for form in (rest input)
       for time from 1 to (- l 1)
       do
	 (loop
	    for current from 1 to (- n 2)
	    do
	      (loop
		 for previous from 1 to (- n 2)
		 for old = (aref viterbi current time)
		 for new = (+				    ;log probs
			    (aref viterbi previous (- time 1))
			    (transition-probability hmm previous current)
			    (emission-probability hmm current form))

		 ;; maximize the Viterbi probability of the current state
		 ;; and time point, also maintaining the back-pointers.

		 when (or (null old) (> new old)) do
		   (setf (aref viterbi current time) new)
		   (setf (aref pointer current time) previous))))

    ;; finally, fill in the matrix with transitions into the final state

    (loop
       for previous from 1 to (- n 2)
       for old = (aref viterbi (- n 1) (- l 1))
       for new = (+					    ;log probs
		  (aref viterbi previous (- l 1))
		  (transition-probability hmm previous (- n 1)))
       when (or (null old) (> new old)) do
	 (setf (aref viterbi (- n 1) (- l 1)) new)
	 (setf (aref pointer (- n 1) (- l 1)) previous))

    ;; read out the most probably state sequence, working right-to-left this
    ;; time, i.e. starting from the final state.

    (loop
       with tags = (hmm-tags hmm)
       with final = (aref pointer (- n 1) (- l 1))
       with result = (list (elt tags final))
       for i from (- l 1) downto 1
       for state = (aref pointer final i) then (aref pointer state i)
       do (push (elt tags state) result)
       finally (return result))))

(viterbi *wsj* '("No" "," "it" "was" "n't" "Black" "Monday" "."))

(let ((input '("When" "the" "judge" "bought" "his" "new" "Sunbird" "from"
	       "James" "E." "Black" "Pontiac-Cadillac"
	       "in" "Ebensburg" "five" "years" "ago" "," "the" "dealership" "had" "``"
	       "certain" "apprehensions" "''" "about" "the" "judge" "'s"
	       "reputation" "," "according" "to" "the" "grand-jury" "report" ".")))
  (mapcar #'(lambda (a b) (list a b)) input (viterbi *wsj* input)))



#|

(defun evaluate-hmm (hmm file)
  (with-open-file (stream file :direction :input)
    (loop
        with total = 0 with correct = 0
        with forms with states
        for line = (read-line stream nil)
        for tab = (position *observation-state-separator* line)
        for form = (subseq line 0 tab)
        for state = (and tab (subseq line (+ tab 1)))
        while line
        when (and form state) do
          (push form forms)
          (push state states)
        else do
          (loop
              for gold in (nreverse states)
              for state in (viterbi hmm (nreverse forms))
              do (incf total)
              when (string= gold state) do (incf correct))
          (setf forms nil) (setf states nil)
        finally (return (float (/ correct total))))))


(evaluate-hmm *wsj* "test.tt")

|#
