;;;; 
;;;; Tagger
;;;;

(defstruct hmm
  states
  (n 0)
  transitions
  ;; setting up a cons here, to keep track of:
  ;;	a) emissions C(state,observation)
  ;;	b) total emitted from one state (no need to recalculate after initial storing)
  (emissions (list :emissions (make-hash-table :test #'equal)
		   :total (make-hash-table :test #'equal))))

;; transition probabilities

(defun transition-probability (hmm state-A state-B)
  (aref (hmm-transitions hmm) state-A state-B))

(defun set-transition-probability (hmm state-A state-B value)
  (setf (aref (hmm-transitions hmm) state-A state-B) value))

(defsetf transition-probability set-transition-probability)

;; emission probabilites

(defun emission-probability (hmm state observation &optional (default nil))
  (gethash (cons state observation) (getf (hmm-emissions hmm) :emissions) default))

(defun set-emission-probability (hmm state observation value)
  (setf (gethash (cons state observation) (getf (hmm-emissions hmm) :emissions) 0) value))

(defsetf emission-probability set-emission-probability)

;; emission counts, keep track of total number of emissions from one state

(defun emission-total-count (hmm state &optional (default 0))
  (gethash state (getf (hmm-emissions hmm) :total) default))

(defun set-emission-total-count (hmm state value)
  (setf (gethash state (getf (hmm-emissions hmm) :total) 0) value))

(defsetf emission-total-count set-emission-total-count)

;; state2id, returns numeric id (zero-based) from (string) state.  Inserts any new
;; ids into hmm-states and updates hmm's state count (n) :

(defun state2id (hmm state-label)
  (1-							    ;0-indexed array
   (or (cdr (assoc state-label (hmm-states hmm) :test #'equal))
       (cdr (assoc state-label (push (cons state-label (incf (hmm-n hmm)))
				     (hmm-states hmm)))))))
;; going backwards

(defun id2state (state hmm)
  (car (find (1+ state) (hmm-states hmm) :key #'cdr)))


;;;
;;; read training data, file with one list per line holding (observation label)
;;;

(defun set-up-hmm (tagset-size)
  ;; 2d array, include space for :seq and :/seq
  (let ((siz (+ 2 tagset-size)))
    (make-hmm :transitions (make-array (list siz siz) :initial-element nil))))

(defun read-corpus-lists (corpus-file state-set-size)
  (let ((hmm (set-up-hmm state-set-size)))
    (with-open-file (in corpus-file)
      (let ((last-tag :seq))
	(labels ((update-transition (label)
		   (let ((i (state2id hmm last-tag))
			 (j (state2id hmm label)))
		     (if (transition-probability hmm i j)
			 (incf (transition-probability hmm i j))
			 (setf (transition-probability hmm i j) 1))
		     (setq last-tag label)))
		 (update-emissions (observation label)
		   (let ((i (state2id hmm label)))
		     (if (emission-probability hmm i observation)
			 (incf (emission-probability hmm i observation))
			 (setf (emission-probability hmm i observation) 1))
		     (if (emission-total-count hmm i)
			 (incf (emission-total-count hmm i))
			 (setf (emission-total-count hmm i) 1))))
		 (update-with-sequence-marks () 
		   (prog1
		       (update-transition :/seq)
		     (setq last-tag :seq))))
	  (loop
	     for input = (read in nil :eof)
	     while (not (eql input :eof))
	     for (observation label) = input
	     do (print (list observation label))
	       (if observation
		   (progn (update-transition label)
			  (update-emissions observation label))
		   (update-with-sequence-marks))
	     finally (state2id hmm :seq)))))
    hmm))


#|
(setf eisner (read-corpus-lists "./eisner.list.cl" 2))

(hmm-states eisner)
(hmm-emissions eisner)
(transition-probability eisner (state2id eisner :seq) (state2id eisner 'A))
(transition-probability eisner (state2id eisner 'B) (state2id eisner 'A))
(transition-probability eisner (state2id eisner 'B) (state2id eisner :/seq))
(emission-probability eisner (state2id eisner 'B) '(0 500 800))
(emission-total-count eisner (state2id eisner 'B))
|#

;;; (b) train-hmm: translate counts to relative frequencies (0->1.0, summing to 1.0
;;; for every state), using log-space to avoid underflow and more effective
;;; combination further down

(defun rowsum (hmm row n)
  (loop
     for col from 0 below n
     sum (or (aref (hmm-transitions hmm) row col) 0)))

;; (defmacro Pprob (n) `(identity ,n))
;; (defmacro Pcombiner (&rest args) `(* ,@args))
;; (defmacro smooth (&body expr) `(or ,@expr 1/1000000))

(defmacro Pprob (n) `(log ,n))
(defmacro Pcombiner (&rest args) `(+ ,@args))
(defmacro smooth (&body expr) `(or ,@expr (log 1/1000000))) ;small default value

(defun update-transition-probs (hmm)
  (let ((n (hmm-n hmm)))
    (loop
       for row from 0 below n
       for total = (rowsum hmm row n)
       do (loop
	     for column from 0 below n
	     when (transition-probability hmm row column)
	     do (setf (transition-probability hmm row column)
		      (Pprob (/ (transition-probability hmm row column) total))))))
  hmm)

(defun update-emission-probs (hmm)
  (let ((emissions (getf (hmm-emissions hmm) :emissions))
	(total-counts (getf (hmm-emissions hmm) :total)))
    (maphash #'(lambda (key val)
		 (let ((this-emission-counts (gethash (car key) total-counts)))
		   (setf (gethash key emissions)
			 (Pprob (/ val this-emission-counts)))))
	     emissions)
    hmm))

(defun train-hmm (hmm)
  (update-transition-probs
   (update-emission-probs hmm)))


#|
(defparameter eisner (train-hmm (read-corpus-lists "./eisner.list.cl" 2)))
(transition-probability eisner (state2id eisner 'B) (state2id eisner 'A))
(emission-probability eisner (state2id eisner 'B) '(0 400 700))
|#


;;;;
;;;; Viterbi
;;;; 

(defun viterbi (hmm sequence)
  (let* ((Q (hmm-n hmm))
	 (L (length sequence))
	 (trellis (make-array (list Q L)))
	 (backpointers (make-array (list Q L)))
	 (Q-1 (1- Q))
	 (L-1 (1- L)))

    ;; initialize with first transition

    (let ((start-state (state2id hmm :seq))
	  (obs (car sequence)))
      (loop
	 for state from 1 below Q-1			    ; :seq and :/seq occluded
	 do
	   (setf (aref trellis state 0)
		 (Pcombiner (smooth (transition-probability hmm start-state state))
			    (smooth (emission-probability hmm state obs))))
	   (setf (aref backpointers state 0) start-state)))

    ;; walk through the rest of the observations
    
    (loop
       for observation in (cdr sequence)
       for t-step from 1
       do (loop
	     for state from 1 below Q-1
	     do (loop
		   for prev-state from 1 below Q-1
		   for current-max = (aref trellis state t-step)
		   for new-max? = (Pcombiner (aref trellis prev-state (- t-step 1))
					     (smooth (transition-probability hmm prev-state state))
					     (smooth (emission-probability hmm state observation)))
		   do (when (or (null current-max) (> new-max? current-max))
			;; update local max and new backpointer
			(setf (aref trellis state t-step) new-max?)
			(setf (aref backpointers state t-step) prev-state)))))

    ;; add transition to end-of-sequence ( :/seq ) :
    (loop
       for prev-state from 1 below Q-1
       for current-max = (aref trellis Q-1 L-1)
       for new-max? = (Pcombiner (aref trellis prev-state L-1)
				 (smooth (transition-probability hmm prev-state (state2id hmm :/seq))))
       do (when (or (null current-max) (> new-max? current-max))
	    (setf (aref trellis Q-1 L-1) new-max?)
	    (setf (aref backpointers Q-1 L-1) prev-state)))

    ;; loop backwards and pick up best path
    (let* ((last-state (aref backpointers Q-1 L-1))
	   (output (list (id2state last-state hmm))))
      (loop
	 for i from L-1 downto 1
	 for state = (aref backpointers last-state i) then (aref backpointers state i)
	 do (push (id2state state hmm) output))
      output)))


#|
(viterbi eisner ’((0 400 700)
		  (0 400 700)
		  (0 500 800)
		  (0 400 700)
		  (0 500 800)
		  (0 200 400)
		  (0 500 800)  
		  (0 500 800)
		  (0 400 700)
		  (0 400 700)
		  (0 400 700)
		  (0 400 700)
		  (0 400 700)
		  (0 400 700)
		  (0 400 700)
		  ))
-> (A A B B B B B B A A A A A A A)
-> (A A B B B B B A A A A A A A)




(read-corpus-lists "./wsj.list.cl" 45)

(setf wsj (train-hmm (read-corpus-lists "./wsj.list.cl" 45)))
(viterbi wsj '("No" "," "it" "was" "n't" "Black" "Monday""."))

 => ("UH" "," "PRP" "VBD" "RB" "NNP" "NNP" ".")



|#
