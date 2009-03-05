;;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; * UCW Extensions to YACLML

;;;; ** UCW Tags

(defun expand-link-tag (tag-name &key action href other-attributes body)
  (assert (xor action href)
          (action href)
          "Must specify exactly one of :action and :href attributes.")
  (cond
    (action `(,tag-name :href (action-href (lambda ()
                                             (ucw.rerl.info "Executing action ~S" ',action)
                                             (with-call/cc ,action))
                                           :component *current-component*)
                        ,@other-attributes ,@body))
    (href `(,tag-name :href (add-session-id ,href) ,@other-attributes ,@body))))

(deftag-macro <ucw:a (&attribute action href &allow-other-attributes others &body body)
  "Execute ACTION when the link is followed."
  (expand-link-tag '<:a :action action :href href
                   :other-attributes others :body body))

(deftag-macro <ucw:area (&attribute action href &allow-other-attributes others &body body)
  "Execute ACTION when the link is followed."
  (expand-link-tag '<:area :action action :href href
                   :other-attributes others :body body))

;;;; *** Form tags

(deftag-macro <ucw:simple-form (&attribute (id '(js:gen-js-name-string :prefix "ucw-form"))
					    action 
					    &allow-other-attributes others
					    &body body)
  "A Simple form tag, made for use with SIMPLE-SUBMIT. Does not require javascript."
  (with-unique-names (url query action-id)
    (rebinding (id)
      `(let* ((,action-id (make-new-action (context.current-frame *context*)
					   (lambda ()
                                             (ucw.rerl.info "Executing action ~S" ',action)
                                             (with-call/cc ,action))))
              (,url (compute-url *current-component*
                                 :action-id ,action-id)))
         (<:form :action (print-uri-to-string-sans-query ,url)
                 ,@others
		 (dolist (,query (uri.query ,url))
		   (if (string= ,+action-parameter-name+ (car ,query))
		       (<:input :type "hidden" :name ,+action-parameter-name+
				:value (when ,action (cdr ,query))
				:id ,action-id)
		       (<:input :type "hidden" :name (car ,query) :value (cdr ,query))))
		 ,@body)))))

(deftag-macro <ucw:simple-submit (&attribute action &allow-other-attributes others &body body)
  "Presents an button tag which executes ACTION when clicked."  
  `(<:button :type "submit"
	     :name +action-parameter-name+
	     :value (make-new-action (context.current-frame *context*)
				     (lambda ()
                                       (ucw.rerl.info "Executing action ~S" ',action)
                                       (with-call/cc ,action)))
	     ,@others ,@body))


(deftag-macro <ucw:button (&attribute action &allow-other-attributes others &body body)
  "Presents an butten tag which executes ACTION when clicked."
  (unless action
    (error "Must specify :ACTION attribute in button tags (or use a simple <:button tag)"))  
  `(<:button :onclick (js:js-inline*
                       `(progn
                          (this.form.ucw-set-action-parameter
                           ,(make-new-action (context.current-frame *context*)
                                             (lambda ()
                                               (ucw.rerl.info "Executing action ~S" ',action)
                                               (with-call/cc ,action))))
                         (return true)))
	     ,@others ,@body))

(deftag-macro <ucw:form (&attribute (id '(js:gen-js-name-string :prefix "ucw-form"))
                                    action onsubmit
                         &allow-other-attributes others
                         &body body)
  "Generic FORM tag replacement. When the form is submitted the
  form ACTION will be eval'd, unless another action is specified
  in a nested submit tag. If no ACTION is provided the form won't
  be submittable by pressing enter in a text element."
  (when onsubmit
    (error "<ucw:form does not support the :onsubmit attribute."))
  (with-unique-names (url query action-id)
    (rebinding (id)
      `(let* ((,url (compute-url *current-component*
                                 :action-id (make-new-action (context.current-frame *context*)
                                                             (lambda ()
                                                               (ucw.rerl.info "Executing action ~S" ',action)
                                                               (with-call/cc ,action)))))
              (,action-id (js:gen-js-name-string :prefix "ucw-action-id")))
         (<:form :action (print-uri-to-string-sans-query ,url)
                 :id ,id
                 :onsubmit (js:js-inline* `(return (.ucw-has-action (dojo.by-id ,,id))))
                 ,@others
          ;; render a first fake input on which mozilla will call onclick when the user hits enter
          (<:input :type "image" :style "display: none")
          (dolist (,query (uri.query ,url))
            (if (string= ,+action-parameter-name+ (car ,query))
                (<:input :type "hidden" :name ,+action-parameter-name+
                         :value ,(when action `(cdr ,query))
                         :id ,action-id)
                (<:input :type "hidden" :name (car ,query) :value (cdr ,query))))
           (<ucw:script
            `(let ((form (dojo.by-id ,,id)))
                (setf form.ucw-set-action-parameter
                      (lambda (value)
                        (setf (slot-value (dojo.by-id ,,action-id) 'value)
                              value)))
                (setf form.ucw-has-action
                      (lambda ()
                        (return (not (dojo.string.is-blank (slot-value (dojo.by-id ,,action-id) 'value))))))))
           ,@body)))))

(deftag-macro <ucw::%select (&attribute multiple writer accessor (test '#'eql) (key '#'identity) on-change name
                             &allow-other-attributes others
                             &body body)
  "The implementation of <ucw:select and tal tags with a ucw:accessor (or ucw:writer) attribute.

if multiple is non-nil the writer is a (possibly empty) list of
values, otherwise the single value is used."
  (with-unique-names (v val values)
    (when (and accessor writer)
      (error "Can't specify :WRITER along with :ACCESSOR."))
    (let ((writer (or writer `(lambda (,v) (setf ,accessor ,v)))))
      `(let ((%current-select-value ,accessor)
	     (%current-select-test ,test)
	     (%current-select-key ,key)
	     (%select-table nil)
	     (%multiple ,multiple))
         (declare (ignorable %current-select-value %current-select-test %current-select-key
                             %select-table %multiple))
         (<:select :name (make-new-callback
                          (flet ((get-associated-value (v)
                                   (let ((v (assoc v %select-table :test #'string=)))
                                     (if v
                                         (cdr v)
                                         (error "Unknown option value: ~S." v)))))
                            (if %multiple
                                (lambda (,v)
				  (iterate
				    (for ,val in (ensure-list ,v))
				    (collect (get-associated-value ,val) into ,values)
				    (finally (funcall ,writer ,values))))
				(lambda (,v) (funcall ,writer (get-associated-value ,v)))))
                          :name ,name)
		   ,@(when on-change
                           (assert (not (member :onchange others))
                                   (others)
                                   "Can't supply both :on-change and :onChange" )
                           `(:onchange (js:js-inline*
                                        `(progn
                                           (this.form.ucw-set-action-parameter
                                            ,(register-action
                                              (lambda ()
                                                (with-call/cc
                                                  ,on-change))))
                                           (return (this.form.submit))))))
                   ,@others
                   ,@(when multiple `(:multiple %multiple))
                   ,@body)))))

(deftag-macro <ucw:select (&allow-other-attributes others
                           &body body)
  `(<ucw::%select ,@others ,@body))

(deftag-macro <ucw::%option (&attribute value &allow-other-attributes others &body body)
  (with-unique-names (value-id)
    (rebinding (value)
      `(let ((,value-id (random-string 10)))
	(push (cons ,value-id ,value) %select-table)
	(<:option :value ,value-id
	 ;;NB: we are applying key to both the option value being rendered,
	 ;; as well as the selected value(s).
	 ;;That was how the code worked previously, I don't know if it is desirable.
	 ;;I think the alternative would be to apply the key to ",value" that is
	 ;; the option being rendered, and remove the :key argument from find.

	 ;;The logical operation we are trying to accomplish is
	 ;;(mapcar #'add-selected-attribute
	 ;;	  (find-all %current-select-value(s)
	 ;;		    (list-of-collected-<ucw::%option-calls)
	 ;;		    :key %current-select-key))
		  :selected (when (find
				   (funcall %current-select-key ,value) ;key applied to an option
				   (if %multiple
				       %current-select-value
				       (list %current-select-value))
				   :test %current-select-test
				   :key %current-select-key)
			      T)
	 ,@others ,@body)))))

(deftag-macro <ucw:option (&allow-other-attributes others &body body)
  "Replacement for the standard OPTION tag, must be used with
  <UCW:SELECT tag. Unlike \"regular\" OPTION tags the :value
  attribute can be any lisp object (printable or not)."
  `(<ucw::%option ,@others ,@body))

(deftag-macro <ucw:input (&attribute accessor action reader writer name id (default nil)
			  &allow-other-attributes others)
  "Generic INPUT tag replacement.

If the ACCESSOR attribute is specified then it must be a PLACE
and it's value will be used to fill the input, when the form is
submitted it will be set to the new value.

If ACTION is specefied then when the form is submitted via this
input type=\"submit\" tag the form will be eval'd. when the
submit (or image) is clicked. DEFAULT means that the ACTION
provided for this input tag will be the default action of the
form when pressing enter in a form field. If more then one, then
the latest wins."
  (when (and action accessor)
    (error "Must specify only one of :ACTION or :ACCESSOR."))
  (when (and accessor (or reader writer))
    (error "Can not specify both :ACCESSOR and :READER or :WRITER."))
  (when (and (not action) default)
    (error "Only those <ucw:input tags can be default that have an action"))
  (cond
    ((or reader accessor writer)
     (with-unique-names (v)
       (let ((type (getf others :type))
	     (value (gensym "INPUT-"))
	     (reader (or reader accessor))
	     (writer (or writer `(lambda (,v)
				   (setf ,accessor ,v)))))
	 `(let ((,value ,reader))
	    (<:input :name (make-new-callback ,writer :name ,name)
                     :id ,id
		     :value ,value
		     ,@(when (string= "checkbox" type)
                             `(:checked ,value))
		     ,@others)))))
    (action
     (let ((id (when (or default id) ; only render id when explicitly asked for or we need it
                 (or id (js:gen-js-name-string :prefix "_ucw_input")))))
       (with-unique-names (action-id)
         `(let ((,action-id (make-new-action (context.current-frame *context*)
                                             (lambda ()
                                               (ucw.rerl.info "Executing action ~S" ',action)
                                               (with-call/cc ,action)))))
           (<:input ,@others
            :id ,id
            :name ,name
            :onclick (js:js-inline*
                      `(progn
                        (this.form.ucw-set-action-parameter ,,action-id)
                        (return t))))
           (when ,default
             (<ucw:script `(.form.ucw-set-action-parameter (dojo.by-id ,,id) ,,action-id)))))))
    (t
     (error "Must specify either :ACTION or :ACCESSOR (or use a regular <:input tag.)"))))

(deftag-macro <ucw:textarea (&attribute accessor reader writer name
			     &allow-other-attributes others
			     &body body)
  "TEXTAREA input tag which accepts the ACCESSOR attribute. The
  semantics of the ACCESSOR are the same as the accessor
  attribute of the <ucw:input tag."
  (declare (ignore body))
  (let ((reader (or reader accessor))
	(writer (or writer (with-unique-names (v)
			     `(lambda (,v) (setf ,accessor ,v))))))
    (with-unique-names (read-value)
      `(<:textarea :name (make-new-callback ,writer :name ,name)
		   ,@others
	(when-bind ,read-value ,reader
	  (<:as-html ,read-value))))))

;;;; *** Form tag shortcuts

(defmacro defform-input-tag-shortcut (tag-name input-type)
  `(deftag-macro ,tag-name (&allow-other-attributes others)
     `(<ucw:input :type ,,input-type ,@others)))

(defform-input-tag-shortcut <ucw:text "text")

(defform-input-tag-shortcut <ucw:hidden "hidden")

(defform-input-tag-shortcut <ucw:password "password")

(defform-input-tag-shortcut <ucw:submit "submit")

;;;; *** Miscalenous form convience tags

(deftag-macro <ucw:integer-range-select (&attribute min max (step 1) &allow-other-attributes others &body body)
  "Tag for creating select/options pairs of sequential numbers."
  (if (every #'constantp (list min max step))
      `(<ucw:select ,@others
         ,@body
         ,@(iterate
            (for i from min to max by step)
            (collect `(<ucw:option :value ,i ,(princ-to-string i)))))
      `(<ucw:select ,@others
         ,@body
         (iterate
           (for i from ,min to ,max by ,step)
           (<ucw:option :value i (<:as-html i))))))

(deftag-macro <ucw:month-day-select (&attribute accessor &body body)
  `(<ucw:integer-range-select :min 1 :max 31 :accessor ,accessor ,@body))

(deftag-macro <ucw:month-select (&attribute accessor &allow-other-attributes others &body body)
  `(<ucw:select :accessor ,accessor
     ,@others ,@body
     (<ucw:option :value 1 "January")
     (<ucw:option :value 2 "February")
     (<ucw:option :value 3 "March")
     (<ucw:option :value 4 "April")
     (<ucw:option :value 5 "May")
     (<ucw:option :value 6 "June")
     (<ucw:option :value 7 "July")
     (<ucw:option :value 8 "August")
     (<ucw:option :value 9 "September")
     (<ucw:option :value 10 "October")
     (<ucw:option :value 11 "November")
     (<ucw:option :value 12 "December")))

(deftag <ucw:render-component (&attribute component)
  (emit-code `(ucw::render ,component)))

;;;; Parenscript

(deftag-macro <ucw:script (&body body)
  `(<:script :type "text/javascript"
             (<:as-is ~% "// <![CDATA[" ~%
                      (js:js* ,@body)
                      ~% "// ]]>" ~%)))

;; Copyright (c) 2003-2005 Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
