; gd.lisp 
; William Halliburton 

(in-package :pink)

(defmacro def-gd-img (name (type size) &body body)
  (let ((stream (gensym)))
    `(progn
      (defun ,(symb "GENERATOR-" name) (,stream)
	(cl-gd:with-image*  ,size
	  ,@body
	  (write-image-to-stream ,stream :png)))
      (defaction ,name ((self ,type))
	(let* ((request (ucw:context.request *context*))
	       (response (ucw:context.response *context*)))
	  (setf (ucw::get-header response "Status") "200 OK"
		(ucw::get-header response "Content-Type") "image/png")
	  (,(symb "GENERATOR-" name) (ucw::html-stream response))
	  (ucw::shutdown request)
	  (ucw::send-headers response)
          (ucw::send-body response))))))


;; gd-demo


(defcomponent gd-demo (widget-component)
  ()
  (:default-initargs :css-class "gd-demo"))


(defmacro action-href-lambda (&body body)
  `(action-href (lambda () (with-call/cc ,@body)) :component ucw::*current-component*))

(defmacro <gd-img (self action)
  `(<:img :src (action-href-lambda (,action ,self))))

(defmethod render ((l gd-demo))
  (<:table 
   (<:tr (<:td (<gd-img l test-img-pie)))
   (<:tr (<:td (<gd-img l test-img-circle)))
   (<:tr (<:td (<gd-img l test-img-triangle)))))


(def-gd-img test-img-triangle (gd-demo (100 100))
  (allocate-color 255 255 255) ; white background
  (let ((red (allocate-color 255 0 0))
	(yellow (allocate-color 255 255 0))
	(orange (allocate-color 255 165 0)))
    ;; thin black border
    (draw-rectangle* 0 0 99 99
		     :color (allocate-color 0 0 0))
    ;; lines are five pixels thick
    (with-thickness (5)
      ;; colored triangle
      (draw-polygon (list 10 10 90 50 50 90)
		    ;; styled color
		    :color (list red red red
				 yellow yellow yellow
				 nil nil nil
				 orange orange orange)))))


(def-gd-img test-img-pie (gd-demo (200 200))
  (allocate-color 68 70 85)		; background color
  (let ((beige (allocate-color 222 200 81))
	(brown (allocate-color 206 150 75))
	(green (allocate-color 104 156 84))
	(red (allocate-color 163 83 84))
	(white (allocate-color 255 255 255))
	(two-pi (* 2 pi)))
    ;; move origin to center of image
    (with-transformation (:x1 -100 :x2 100 :y1 -100 :y2 100 :radians t)
      ;; draw some 'pie slices'
      (draw-arc 0 0 130 130 0 (* .6 two-pi)
		      :center-connect t :filled t :color beige)
      (draw-arc 0 0 130 130 (* .6 two-pi) (* .8 two-pi)
		      :center-connect t :filled t :color brown)
      (draw-arc 0 0 130 130 (* .8 two-pi) (* .95 two-pi)
		      :center-connect t :filled t :color green)
      (draw-arc 0 0 130 130 (* .95 two-pi) two-pi
		      :center-connect t :filled t :color red)
      (with-default-color (white)
	(with-default-font (:small)
	  (draw-string -8 -30 "60%")
	  (draw-string -20 40 "20%")
	  (draw-string 20 30 "15%"))
	(draw-freetype-string -90 75 "Global Revenue"
				    ;; this assumes that 'DEFAULT_FONTPATH'
				    ;; is set correctly
				    :font-name "Vera")))))

(def-gd-img test-img-circle (gd-demo (40 40))
  (allocate-color 255 255 255)
  (let ((black (allocate-color 0 0 0)))
    (with-default-color (black)
      ;; move origin to center and stretch
      (with-transformation (:x1 -100 :width 200 :y1 -100 :height 200)
	(draw-filled-circle 0 0 50)))))



  


		     
		 
  
      
      
