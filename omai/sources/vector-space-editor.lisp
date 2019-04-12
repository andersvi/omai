

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
; Author: Jean Bresson - 2018
;=====================================================
;
; Vector space visualizer
;
;=====================================================

(in-package :omai) 

(defclass vs-editor (om::OMEditor) 
  ((class-colors :accessor class-colors :initform nil)
   (cached-points :accessor cached-points :initform nil)
   (cached-ranges :accessor cached-ranges :initform nil)))
   
(defstruct cached-point (id) (coordinates) (class))

(defmethod om::object-has-editor ((self vector-space)) t)
(defmethod om::get-editor-class ((self vector-space)) 'vs-editor)

(defmethod om::object-default-edition-params ((self vector-space))
  '((:view-mode :2d)
    (:dimension1 nil)
    (:dimension2 nil)
    (:dimension3 nil)
    (:show-ids nil)))


(defun k-means-button (editor)
  (let ((vs (om::object-value editor)))
    (setf (classes vs) (k-means (vectors vs) (max (length (classes vs)) 1)))
    (cache-vs-points editor)
    (oa:om-invalidate-view (om::main-view editor))))

(defun estimate-button (editor)
  (let ((vs (om::object-value editor)))
    (classify vs)
    (cache-vs-points editor)
    (oa:om-invalidate-view (om::main-view editor))))

(defclass vs-2D-view (om::omeditorview) ())
(defclass vs-3D-view (om::om-opengl-view) ())


(defmethod om::make-editor-window-contents ((editor vs-editor))
  
  (let* ((vs (om::object-value editor))
         (main-view 
          (case (om::editor-get-edit-param editor :view-mode)
           (:2d (oa::om-make-view 'vs-2D-view :editor editor :bg-color (oa:om-def-color :white)))
           (:3d (oa::om-make-view 'vs-3D-view 
                                  :bg-color (oa:om-def-color :white)
                                  ))))
         
         (x-menu (oa::om-make-di 'oa::om-popup-list :items (features vs) 
                                 :value (om::editor-get-edit-param editor :dimension1)
                                 :size (oa::omp 80 24)
                                 :font (oa::om-def-font :font1)
                                 :di-action #'(lambda (b) 
                                                (om::editor-set-edit-param editor :dimension1 (om::om-get-selected-item b))
                                                (cache-vs-points editor) 
                                                (oa:om-invalidate-view (om::main-view editor))
                                                )))
        (y-menu (oa::om-make-di 'oa::om-popup-list :items (features vs) 
                                 :value (om::editor-get-edit-param editor :dimension2)
                                 :size (oa::omp 80 24)
                                 :font (oa::om-def-font :font1)
                                 :di-action #'(lambda (b) 
                                                (om::editor-set-edit-param editor :dimension2 (om::om-get-selected-item b))
                                                (cache-vs-points editor)
                                                (oa:om-invalidate-view (om::main-view editor))
                                                )))
        (z-menu (oa::om-make-di 'oa::om-popup-list :items (features vs) 
                                 :value (om::editor-get-edit-param editor :dimension3)
                                 :size (oa::omp 80 24)
                                 :font (oa::om-def-font :font1)
                                 :enable (equal (om::editor-get-edit-param editor :view-mode) :3d)
                                 :di-action #'(lambda (b) 
                                                (om::editor-set-edit-param editor :dimension3 (om::om-get-selected-item b))
                                                (cache-vs-points editor) 
                                                (oa:om-invalidate-view (om::main-view editor))
                                                )))
        (controller-view (oa::om-make-layout 
                           'oa::om-column-layout 
                           :ratios '(nil nil 1 nil nil 1 nil nil nil nil) :delta 0
                           :subviews (list 
                                      ;; top of the pane
                                      (oa::om-make-di 'oa::om-popup-list :items '(:2d :3d) 
                                                      :value (om::editor-get-edit-param editor :view-mode)
                                                      :size (oa::omp 80 24)
                                                      :font (oa::om-def-font :font1)
                                                      :di-action #'(lambda (b) 
                                                                     (om::editor-set-edit-param editor :view-mode (om::om-get-selected-item b))
                                                                     (om::build-editor-window editor)
                                                                     ;;; (om::init-editor-window editor) ; no need to recache everything (?)
                                                                     (when (equal (om::editor-get-edit-param editor :view-mode) :3D)
                                                                       (om::om-init-3D-view (om::main-view editor))
                                                                       (om::om-set-gl-objects (om::main-view editor) (create-GL-objects editor)))
                                                                     ))
                                      :separator
                                      nil
                                      (oa::om-make-layout 
                                       'oa::om-row-layout 
                                       :subviews (list 
                                                  (oa::om-make-di 'oa::om-button :text "K-means" :font (oa::om-def-font :font1) 
                                                                  :size (oa::omp 76 24)
                                                                  :di-action #'(lambda (b) (declare (ignore b))
                                                                                 (init-colors editor)
                                                                                 (k-means-button editor)))
                                                  
                                                  (oa::om-make-di 'oa:om-simple-text :text "N:" :font (oa::om-def-font :font1) 
                                                                  :size (oa::omp 12 24))
                                                  (om::set-g-component 
                                                   editor :k-means-n 
                                                   (oa::om-make-di 'om::numbox :font (oa::om-def-font :font1) :size (oa::omp 20 20)
                                                                   :bg-color (oa:om-def-color :white)
                                                                   :value (max (length (classes vs)) 1) :min-val 1
                                                                   :after-fun #'(lambda (b)
                                                                                  (setf (classes (om::object-value editor)) (initialize-classes (om::value b)))
                                                                                  (oa:om-invalidate-view (om::main-view editor))))
                                                   
                                                   )))
                                      
                                      (oa::om-make-di 'oa::om-button :text "Estimate class" :font (oa::om-def-font :font1) :size (oa::omp 100 24)
                                                      :di-action #'(lambda (b) (declare (ignore b))
                                                                     (estimate-button editor)))
                                      nil
                                      :separator
                                      (oa::om-make-di 'oa:om-simple-text :text "x:" :font (oa::om-def-font :font1) :size (oa::omp 80 18))
                                      x-menu
                                      (oa::om-make-di 'oa:om-simple-text :text "y:" :font (oa::om-def-font :font1) :size (oa::omp 80 18))
                                      y-menu
                                      (oa::om-make-di 'oa:om-simple-text :text "z:" :font (oa::om-def-font :font1) :size (oa::omp 80 18)
                                                      :fg-color (if (equal (om::editor-get-edit-param editor :view-mode) :3d) 
                                                                    (oa::om-def-color :black) (oa::om-def-color :gray)))
                                      z-menu
                                      (oa::om-make-di 'oa:om-check-box :text "show IDs" :font (oa::om-def-font :font1) :size (oa::omp 80 18)
                                                      :checked-p (om::editor-get-edit-param editor :show-ids)
                                                      :di-action #'(lambda (b) 
                                                                     (om::editor-set-edit-param editor :show-ids (om-api:om-checked-p b))
                                                                     (oa:om-invalidate-view (om::main-view editor))
                                                                     ))
                                      
                                      ))
                          ))
    
    (om::set-g-component editor :x-menu x-menu)
    (om::set-g-component editor :y-menu y-menu)
    (om::set-g-component editor :z-menu z-menu)
    
    (values (oa::om-make-layout 'oa::om-row-layout 
                                :ratios '(1 nil)
                                :subviews (list main-view controller-view))
            main-view)
  
    ))


(defmethod init-colors ((editor vs-editor))
  (setf (class-colors editor)
        (loop repeat (length (classes (om::object-value editor))) 
              collect (om::om-random-color))))

(defmethod om::init-editor-window ((editor vs-editor))
  
  (call-next-method)
  
  (let ((vs (om::object-value editor)))
    
    (oa:om-set-item-list (om::get-g-component editor :x-menu) (features vs))
    (oa:om-set-item-list (om::get-g-component editor :y-menu) (features vs))
    (oa:om-set-item-list (om::get-g-component editor :z-menu) (features vs))
                         
    (om::set-value (om::get-g-component editor :k-means-n) (max (length (classes vs)) 1))
                   
    ;;; Pick an adequate number of random colors for classes
    (init-colors editor)
    
    ;;; Set 3 dimensions for display if not set and/or if set values not among available features 
    (unless (and (om::editor-get-edit-param editor :dimension1)
                 (find (om::editor-get-edit-param editor :dimension1) (features vs)))
      (om::editor-set-edit-param 
       editor :dimension1
       (or (find-if-not #'(lambda (desc)
                            (or (equal desc (om::editor-get-edit-param editor :dimension2))
                                (equal desc (om::editor-get-edit-param editor :dimension3))))
                        (features vs))
           (nth 0 (features vs)))))
    
    (unless (and (om::editor-get-edit-param editor :dimension2)
                 (find (om::editor-get-edit-param editor :dimension2) (features vs)))
      (om::editor-set-edit-param 
       editor :dimension2
       (or (find-if-not #'(lambda (desc)
                            (or (equal desc (om::editor-get-edit-param editor :dimension1))
                                (equal desc (om::editor-get-edit-param editor :dimension3))))
                        (features vs))
           (nth 1 (features vs)))))


    (unless (and (om::editor-get-edit-param editor :dimension3)
                 (find (om::editor-get-edit-param editor :dimension3) (features vs)))
      (om::editor-set-edit-param 
       editor :dimension3
       (or (find-if-not #'(lambda (desc)
                            (or (equal desc (om::editor-get-edit-param editor :dimension1))
                                (equal desc (om::editor-get-edit-param editor :dimension2))))
                        (features vs))
           (nth 2 (features vs)))))
      
    (oa:om-set-item-list (om::get-g-component editor :x-menu) (features vs))
    (oa:om-set-selected-item (om::get-g-component editor :x-menu) (om::editor-get-edit-param editor :dimension1))
    (oa:om-set-item-list (om::get-g-component editor :y-menu) (features vs))
    (oa:om-set-selected-item (om::get-g-component editor :y-menu) (om::editor-get-edit-param editor :dimension2))
    (oa:om-set-item-list (om::get-g-component editor :z-menu) (features vs))
    (oa:om-set-selected-item (om::get-g-component editor :z-menu) (om::editor-get-edit-param editor :dimension3))
    
    
    ;;; Initialize and set a list of cached points
    (setf (cached-points editor)
          (make-array (hash-table-count (vectors vs)) :element-type 'cached-point))
    (cache-vs-points editor)

    (when (equal (om::editor-get-edit-param editor :view-mode) :3D)
      (om::om-init-3D-view (om::main-view editor)))
    
    editor))

(defmethod om::update-to-editor ((editor vs-editor) (from t))
  (call-next-method)
  (om::init-editor-window editor)
  (oa:om-invalidate-view (om::main-view editor)))


(defmethod om::editor-key-action ((editor vs-editor) key)
  (case key
    (#\c (init-colors editor)
         (oa:om-invalidate-view (om::main-view editor)))
    (otherwise (call-next-method))))

;;; Fill cached points and record the min-max range for each dimension
;;; call this each time display dimensions are modified
(defun cache-vs-points (editor)
  
  ;(print "CACHING VS POINTS...")
  
  (let ((vs (om::object-value editor))
        (d1 (om::editor-get-edit-param editor :dimension1))
        (d2 (om::editor-get-edit-param editor :dimension2))
        (d3 (om::editor-get-edit-param editor :dimension3)))
    
    ;(print "DIMENSIONS:")
    ;(print d1)
    ;(print d2)
    ;(print d3)
    
    (setf (cached-ranges editor) 
          (list (list nil nil) (list nil nil) (list nil nil)))

    (loop for vector-id being the hash-key of (vectors vs)
          for vector being the hash-value of (vectors vs)
          for i from 0
          do (let* ((v (vs-vector-features vector))
                    ;(v1 (gethash d1 v))
                    ;(v2 (gethash d2 v))
                    ;(v3 (gethash d3 v))
                    (v1 (or (gethash d1 v) 0))
                    (v2 (or (gethash d2 v) 0))
                    (v3 (or (gethash d3 v) 0))
                    )
       
               (setf (aref (cached-points editor) i)
                     (make-cached-point 
                      :id vector-id 
                      :coordinates (make-array 3 :element-type 'float 
                                               :initial-contents (list v1 v2 v3))
                      :class (let ((c (find vector-id (classes vs) 
                                            :test #'(lambda (id class)
                                                      (find id (vs-class-members class) :test 'equal))))) ;;; find class
                               (and c (vs-class-label c)))))
               (when v1 
                 (let ((range (nth 0 (cached-ranges editor))))
                   (when  (or (null (car range)) (< v1 (car range)))
                     (setf (car range) v1))
                   (when (or (null (cadr range)) (> v1 (cadr range)))
                     (setf (cadr range) v1))))
               
               (when v2 
                 (let ((range (nth 1 (cached-ranges editor))))
                   (when  (or (null (car range)) (< v2 (car range)))
                     (setf (car range) v2))
                   (when (or (null (cadr range)) (> v2 (cadr range)))
                     (setf (cadr range) v2))))

               (when v3 
                 (let ((range (nth 2 (cached-ranges editor))))
                   (when  (or (null (car range)) (< v3 (car range)))
                     (setf (car range) v3))
                   (when (or (null (cadr range)) (> v3 (cadr range)))
                     (setf (cadr range) v3))))
               ))
    
    ;;; default values
    (dotimes (i 3)
      (when (or (null (car (nth i (cached-ranges editor))))
                (null (cadr (nth i (cached-ranges editor))))
                (= (car (nth i (cached-ranges editor))) (cadr (nth i (cached-ranges editor)))))
        (setf (nth i (cached-ranges editor)) (list -1 1))))
    
    (when (equal (om::editor-get-edit-param editor :view-mode) :3D)
      (om::om-set-gl-objects (om::main-view editor) (create-GL-objects editor)))

    ))

    

(defmethod oa::om-draw-contents ((self vs-2D-view))
  
  (let* ((r 3)
         (x0 10) (y0 10)
         (w (- (oa::om-width self) 20))
         (h (- (oa::om-height self) 20))
         (ranges (cached-ranges (om::editor self)))
         (minx (car (nth 0 ranges)))
         (maxx (cadr (nth 0 ranges)))
         (miny (car (nth 1 ranges)))
         (maxy (cadr (nth 1 ranges)))
         (rangex (- maxx minx))
         (rangey (- maxy miny)))
    
    (labels ((real-x-pos (x) (+ x0 (* w (/ (- x minx) rangex))))
             (real-y-pos (y) (+ y0 (* h (/ (- y miny) rangey)))))
    
      (loop for p being the array-elements of (cached-points (om::editor self)) 
            for x = (real-x-pos (or (aref (cached-point-coordinates p) 0) -1))
            for y = (real-y-pos (or (aref (cached-point-coordinates p) 1) -1))
            for color = (when (cached-point-class p)
                          (let ((pos (position (cached-point-class p)
                                               (classes (om::object-value (om::editor self)))
                                               :key 'vs-class-label)))
                            (when pos (nth pos (class-colors (om::editor self))))))
            do
            (oa:om-draw-circle x y r :fill t
                               :color color)
            (when (om::editor-get-edit-param (om::editor self) :show-ids)
              (oa:om-draw-string (+ x 4) (+ y 2) (cached-point-id p) :color (or color (oa::om-def-color :gray))))
            )
      )))




;;;=======================================
;;; 3D DRAWING
;;;=======================================

(defmethod oa::om-draw-contents ((self vs-3D-view))
  (call-next-method)
  (draw-axes self))


(defmethod draw-axes ((self vs-3D-view))
  (opengl:gl-push-matrix)
  (let* ((l 1.0))
    ;X axis
    (opengl:gl-color3-f 0.8 0.3 0.3)
    ;axis
    (opengl:gl-begin opengl:*GL-LINES*)
    (opengl:gl-vertex3-f -0.0 0.0 0.0) 
    (opengl:gl-vertex3-f l 0.0 0.0)
    (opengl:gl-end)
    
    ;Y axis
    (opengl:gl-color3-f 0.3 0.6 0.3)
    ;axis
    (opengl:gl-begin opengl:*GL-LINES*)
    (opengl:gl-vertex3-f 0.0 -0.0 0.0) 
    (opengl:gl-vertex3-f 0.0 l 0.0) 
    (opengl:gl-end)
   

    ;Z axis
    (opengl:gl-color3-f 0.3 0.3 0.6)
    ;axis
    (opengl:gl-begin opengl:*GL-LINES*)
    (opengl:gl-vertex3-f 0.0 0.0 -0.0)
    (opengl:gl-vertex3-f 0.0 0.0 l)
    (opengl:gl-end)
    )
  (om::restore-om-gl-colors-and-attributes)
  (opengl:gl-pop-matrix))

;need to call this sometimes ??
;(gl-user::clear-gl-display-list (3Dp self))


(defmethod create-GL-objects ((self vs-editor))
  (let* ((r .01)
         (x0 0) (y0 0) (z0 0)
         ;(w (- (oa::om-width self) 20))
         ;(h (- (oa::om-height self) 20))
         (ranges (cached-ranges self))
         (minx (car (nth 0 ranges)))
         (maxx (cadr (nth 0 ranges)))
         (miny (car (nth 1 ranges)))
         (maxy (cadr (nth 1 ranges)))
         (minz (car (nth 2 ranges)))
         (maxz (cadr (nth 2 ranges)))
         (rangex (- maxx minx))
         (rangey (- maxy miny)) 
         (rangez (- maxz minz)))
    
    (labels ((real-x-pos (x) (+ x0 (/ (- x minx) rangex)))
             (real-y-pos (y) (+ y0 (/ (- y miny) rangey)))
             (real-z-pos (z) (+ z0 (/ (- z minz) rangez))))
      (loop for p being the array-elements of (cached-points self) 
            for x = (real-x-pos (or (aref (cached-point-coordinates p) 0) -1))
            for y = (real-y-pos (or (aref (cached-point-coordinates p) 1) -1))
            for z = (real-z-pos (or (aref (cached-point-coordinates p) 2) -1))
            for color = (when (cached-point-class p)
                          (let ((pos (position (cached-point-class p)
                                               (classes (om::object-value self))
                                               :key 'vs-class-label)))
                            (when pos (nth pos (class-colors self)))))
            collect
            (make-instance 'om::3d-sphere :center (list x y z) :size r 
                           :color color)
            
            )
      )))  

(defmethod om::om-adapt-camera-to-object ((self vs-3D-view))
  (setf (gl-user::center (gl-user::camera self)) 
        (gl-user::make-xyz :x .5d0 :y .5d0 :z .5d0))
  (call-next-method))



#|

(let ((r (/ (min (oa:om-height self) (oa:om-width self)) 3))
        (cx (/ (oa:om-width self) 2))
        (cy (/ (oa:om-height self) 2)))
    
  (oa:om-with-line-size  (/ r 10)
    (oa:om-draw-circle cx cy r)
    (oa:om-draw-circle (- cx (* r .4))
                       (- cy (* r .3))
                       (/ r 10)
                       :fill nil)
    (oa:om-draw-circle (+ cx (* r .4))
                       (- cy (* r .3))
                       (/ r 10)
                       :fill nil)
    
    (oa:om-draw-arc (- cx (* r .5)) (+ cy (* r .2)) r r 0.5 (- pi 1))

    ))

|#





