;; -*- lisp -*-

(in-package :it.bese.ucw)

;;;; * UCW Web Form Library

;;;; ** FORM-FIELDS

;;;; Every input (or a set of inputs which correspond to one "value")
;;;; are represented by a form-field instance.

(defclass form-field ()
  ((validators :accessor validators :initform '() :initarg :validators
               :documentation "List of validators which will be
               applied to this field.")
   (initially-validate :accessor initially-validate :initform t
                       :documentation "When non-NIL the
                       validotars will be run as soon as the page
                       is rendered.")))

(defmethod shared-initialize :after ((field form-field)
                                     slot-names
                                     &key (value nil value-p) &allow-other-keys)
  (declare (ignore slot-names))
  (when value-p
    (setf (value field) value)))

(defmethod render :wrap-around ((field form-field))
  (when (inside-a-form-p)
    (register-form-field field))
  (call-next-method))

(defgeneric form-field-p (object)
  (:method ((object form-field)) t)
  (:method ((object t)) nil))

(defprint-object (field form-field)
  (format *standard-output* "~S" (value field)))


(defclass generic-html-input (form-field html-element)
  ((client-value :accessor client-value :initarg :client-value
                 :initform ""
                 :documentation "The string the client submitted along with this field.")
   (name :accessor name :initarg :name :initform nil)
   (accesskey :accessor accesskey :initarg :accesskey :initform nil)
   (dirty-p-checker :accessor dirty-p-checker :initarg :dirty-p-checker :initform nil
                    :documentation "A JS lambda or constant to indicate dirtiness of this field.")
   (tooltip :accessor tooltip :initarg :tooltip :initform nil)
   (tabindex :accessor tabindex :initarg :tabindex :initform nil))
  (:default-initargs :dom-id (js:gen-js-name-string :prefix "_ucw_")))

(defprint-object (field generic-html-input)
  (format *standard-output* "~S" (client-value field)))

(defmethod initialize-instance :after ((self generic-html-input) &key (value nil value-provided-p) &allow-other-keys)
  (when value-provided-p
    (setf (value self) value)))

(defmethod render :after ((field generic-html-input))
  (<ucw:script :toplevelp t
               `(progn
                 (ucw.field.register ,(dom-id field)
                  ,(dirty-p-checker field)
                  ,(when (validators field)
                         `(macrolet ((fail ()
                                      `(return false)))
                           (lambda (value)
                             ,@(iter (for validator in (validators field))
                                     (collect (javascript-check field validator)))
                             (return true))))
                  ,(initially-validate field))
                 ,@(iter (for validator in (validators field))
                         (awhen (javascript-init field validator)
                           ;; seal them just to be sure that they don't capture each other's variables
                           (collect `((lambda ()
                                        ,it))))))))

(defgeneric value (form-field)
  (:documentation "The lispish translated value that represents the form-field."))

(defmethod value ((field form-field))
  (let ((client-value (client-value field)))
    (if (string= client-value "")
        nil
        client-value)))

(defgeneric (setf value) (new-value form-field)
  (:documentation "Set the value of a form-field with translation to client."))

(defmethod (setf value) (new-value (form-field form-field))
  (setf (client-value form-field) new-value))


;;;; ** Validators

(defclass validator ()
  ((message :accessor message :initarg :message :initform nil)))

;;;; There are three main parts to the validating api:

;;;; 1 - a javascript function which checks if the current value is valid or not.

;;;; 2 - two javascript functions which handle the valid/invalid cases

;;;; 3 - a lisp function whcih checks if the current value is valid or not.
(defgeneric validate (field validator)
  (:documentation "Validate a form-field with a validator."))

;;;; *** validp Top-level function to determine the validity of a form field
(defgeneric validp (form-field)
  (:documentation "Is a form-field valid?"))

(defmethod validp ((field form-field))
  (loop
     for validator in (validators field)
     when (null (validate field validator))
       collect validator into failed-validators
     finally (return (values (null failed-validators)
                             failed-validators))))

(defmethod validp ((component standard-component))
  "Loops over COMPONENT's slots, checks whethere all slots which
  contain form-field objects are valid.

As second value returns list of conses (invalid-field . failed-validator)."
  ;;summary are we valid? 
  (flet ((do-validate-field (field)
           "Run validators on FIELD, return list of (FIELD . VALIDATOR) conses."
           (multiple-value-bind (validp failed) (validp field)
             (declare (ignore validp))
             (mapcar #'(lambda (failed) (cons field failed))
                     failed)))

         (form-fields-of (component)
           "Return slot values of COMPONENT which are FORM-FIELDs"
           (remove-if-not #'form-field-p
                          (mapcar (curry #'slot-value component)
                                  (remove-if-not #'(lambda (name) (slot-boundp component name))
                                                 (mapcar #'mopp:slot-definition-name
                                                         (mopp:class-slots (class-of component))))))))
    (let ((failed (apply #'nconc
                         (mapcar #'do-validate-field
                                 (form-fields-of component)))))
      (values (null failed) failed))))

;;;; *** javascript-check

;;;; This pair of generic functions generate the javascript code for
;;;; checking whether the current value of a field is valid or
;;;; not. The top-level function is generate-javascript-check, 

(defgeneric javascript-check (field validator)
  (:documentation "Generate javascript code for checking FIELD against VALIDATOR.

This is the convenience entry point to generate-javascript-check,
methods defined on this generic funcition should return a list of
javascript code (as per parenscript) which tests against the
bound javascript variable VALUE. When something is not valid
(fail) should be called (which is bound by a local macrolet)"))

(defmethod javascript-check ((field form-field) (validator validator))
  (values))

(defgeneric javascript-init (field validator)
  (:documentation "Validators may return some initialization code here.")
  (:method ((field form-field) (validator validator))
           nil))

;;;; ** Standard Form Validators

;;;; *** not empty

(defclass not-empty-validator (validator)
  ())

(defmethod validate ((field form-field) (validator not-empty-validator))
  (and (client-value field)
       (not (string= "" (client-value field)))))

(defmethod javascript-check ((field form-field) (validator not-empty-validator))
  `(unless value ; this is true for both null and "" in js
    (fail)))

;;;; *** Value-validators

;;;; These are validators that should only be run if there is actually
;;;; a value to test.

(defclass value-validator (validator)
  ()
  (:documentation "Validators that should only be applied if there is a value.
That is, they always succeed on nil."))

(defmethod validate :around ((field form-field) (validator value-validator))
  "Value validators should only be checked if they are not null or empty-strings."
  (or (null (client-value field))
      (= 0 (length (string-trim '(#\Space #\Tab) (client-value field))))
      (call-next-method)))

(defmethod javascript-check :around ((field form-field) (validator value-validator))
  "Value validators should only be checked if they are not null or empty-strings,
javascript if's should take care of that."
  `(when value
    ,(call-next-method)))

;;;; *** length

(defclass length-validator (value-validator)
  ((min-length :accessor min-length :initarg :min-length
               :initform nil)
   (max-length :accessor max-length :initarg :max-length
               :initform nil)))

(defmethod javascript-check ((field form-field) (validator length-validator))
  (with-slots (min-length max-length)
      validator
    (cond
      ((and min-length max-length)
       `(unless (and (<= ,min-length value.length)
                     (<= value.length ,max-length))
         (fail)))
      (min-length
       `(unless (<= ,min-length value.length)
         (fail)))
      (max-length
       `(unless (<= value.length ,max-length)
         (fail)))
      (t t))))

(defmethod validate ((field form-field) (validator length-validator))
  (with-slots (min-length max-length)
      validator
    (let ((length (length (client-value field))))
      (cond
        ((and min-length max-length)
         (<= min-length length max-length))
        (min-length
         (<= min-length length))
        (max-length
         (<= length max-length))
        (t t)))))

;;;; *** same (string=) value

(defclass string=-validator (validator)
  ((other-field :accessor other-field :initarg :other-field))
  (:documentation "Ensures that a field is string= to another one."))

(defmethod validate ((field form-field) (validator string=-validator))
  (let ((value (value field))
        (other-value (value (other-field validator))))
    (string= value other-value)))

(defmethod javascript-init ((field form-field) (validator string=-validator))
  `(let ((field ($ ,(dom-id field)))
         (other-field ($ ,(dom-id (other-field validator)))))
      (ucw.event.connect other-field (list "onchange" "onkeyup")
                         (lambda ()
                           (.ucw-validate field)))))

(defmethod javascript-check ((field form-field) (validator string=-validator))
  `(unless (= (slot-value ($ ,(dom-id field)) 'value)
              (slot-value ($ ,(dom-id (other-field validator))) 'value))
    (fail)))

;;;; Regular expression validator.

(defclass regex-validator (value-validator)
  ((regex :accessor regex :initarg :regex :initform nil)))

(defmethod validate ((field form-field) (validator regex-validator))
  (let ((val (string-trim '(#\Space #\Tab) (value field)))
	(regex (regex validator)))
    (or (null regex)
        (cl-ppcre:scan regex val))))

(defmethod javascript-check ((field form-field) (validator regex-validator))
  `(unless (.match value (regex ,(regex validator)))
    (fail)))

;;;; *** simple e-mail address check.

(defclass e-mail-address-validator (regex-validator)
  ()
  (:default-initargs
   :regex "^([a-zA-Z0-9_\\.\\-\\%])+\\@([a-zA-Z0-9][a-zA-Z0-9\\-]*\\.)+([a-zA-Z0-9]{2,4})\\.?$"))

(defclass hostname-validator (regex-validator)
  ()
  (:default-initargs
   :regex "^([a-zA-Z0-9][a-zA-Z0-9\\-]*\\.)+([a-zA-Z0-9]{2,4})\\.?$"))

(defclass phone-number-validator (regex-validator)
  ()
  (:default-initargs
   :regex "^((\\+\\d{1,3}(-| |\\.)?\\(?\\d\\)?(-| |\\.)?\\d{1,3})|(\\(?\\d{2,3}\\)?))(-| |\\.)?(\\d{3,4})(-| |\\.)?(\\d{4})(( x| ext)\\d{1,5}){0,1}$" ))

;;;; ** Form Inputs

;;;; *** Textarea inputs

(defclass textarea-field (generic-html-input)
  ((rows :accessor rows :initarg :rows :initform nil)
   (cols :accessor cols :initarg :cols :initform nil)))

(defmethod render ((field textarea-field))
  (<ucw:textarea :id (dom-id field)
                 :name (name field)
                 :accessor (client-value field)
                 :rows (rows field)
                 :cols (cols field)
                 :title (tooltip field)
                 :tabindex (tabindex field)
                 :class (css-class field)
		 :style (css-style field)))

;;;; *** Strings

(defclass string-field (generic-html-input)
  ((input-size :accessor input-size
               :initarg :input-size
               :initform nil)
   (maxlength :accessor maxlength
              :initarg :maxlength
              :initform nil)))

(defmethod render ((field string-field))
  (<ucw:text :name (name field)
             :id (dom-id field)
	     :accesskey (accesskey field)
             :accessor (client-value field)
             :title (tooltip field)
             :tabindex (tabindex field)
             :size (input-size field)
             :maxlength (maxlength field)
             :class (css-class field)
	     :style (css-style field)))

;;;; **** Strings with in-field labels

(defclass in-field-string-field (string-field)
  ((in-field-label :accessor in-field-label
                   :initarg :in-field-label
                   :initform nil
                   :documentation "This slot, if non-NIL, will be
                   used as an initial field label. An initial
                   field label is a block of text which is placed
                   inside the input element and removed as soon
                   as the user edits the field. Obviously this
                   field is overidden by an initial :client-value
                   argument."))
  (:default-initargs :client-value nil))

(defmethod render ((field in-field-string-field))
  (<ucw:text :name (name field)
	     :id (dom-id field)
	     :accesskey (accesskey field)
             :reader  (or (client-value field)
                          (in-field-label field)
                          "")
             :writer (lambda (v)
                       (setf (client-value field) v))
             :size (input-size field)
             :title (tooltip field)
             :tabindex (tabindex field)
             :onfocus (js:js*
                       `(unless this.has-changed
                          ,(in-field-on-focus field)))
             :class (css-class field)
	     :style (css-style field))
  (<ucw:script
   (in-field-setup field)))

(defmethod in-field-on-focus ((field in-field-string-field))
  `(setf this.value ""
         this.has-changed t))

(defmethod in-field-setup ((field in-field-string-field))
  `(setf (slot-value ($ ,(dom-id field)) 'has-changed) nil))

;;;; **** Hidden Strings (passwords)

(defclass password-field (string-field)
  ())

(defmethod render ((field password-field))
  (<ucw:password :name (name field)
                 :id (dom-id field)
		 :accesskey (accesskey field)
                 :accessor (client-value field)
                 :size (input-size field)
                 :title (tooltip field)
                 :tabindex (tabindex field)
                 :class (css-class field)
		 :style (css-style field)))

;;;; *** Numbers

(defclass number-field (string-field)
  ())

(defmethod (setf value) ((new-value number) (field number-field))
  (let ((*print-radix* nil)
        (*print-base* 10))
    (setf (client-value field) (format nil (if (integerp new-value) "~d" "~f") new-value))))

(defmethod initialize-instance :after ((field number-field)
                                       &rest initargs)
  (declare (ignore initargs))
  (push (make-instance 'is-a-number-validator)
        (validators field)))

(defmethod value ((field number-field) )
  (if (= 0 (length (client-value field)))
      nil
      (read-from-client-string (client-value field))))

(defclass is-a-number-validator (value-validator)
  ())

(defmethod validate ((field number-field) (validator is-a-number-validator))
  (let ((in-len (length (client-value field))))
    (multiple-value-bind (num len)
	(read-from-client-string (client-value field))
      (and (= len in-len)
	   (numberp num)))))

(defmethod javascript-check ((field number-field) (validator is-a-number-validator))
  `(unless (ucw.field.is-number value)
    (fail)))

(defclass value-range-validator (value-validator)
  ((min-value :accessor min-value :initarg :min-value :initform nil)
   (max-value :accessor max-value :initarg :max-value :initform nil)))

(defclass number-range-validator (value-range-validator is-a-number-validator)
  ())

(defmethod validate ((field number-field) (validator number-range-validator))
  (with-slots (min-value max-value)
      validator
    ;; we shouldn't try to see if it is in a numerical range if it
    ;; isn't a number.
    (and (call-next-method) ;; should be is-a-number-validator
	 (<= (or min-value (1- (value field)))
	     (value field)
	     (or max-value (1+ (value field)))))))

(defmethod javascript-check ((field number-field) (validator number-range-validator))
  (with-slots (min-value max-value) validator
    `(progn
      ,(call-next-method)
      (unless ,(cond
                ((and min-value max-value)
                 `(and (<= ,min-value value) (<= value ,max-value)))
                (min-value `(<= ,min-value value))
                (max-value `(<= ,max-value value))
                (t t))
        (fail)))))

;;;; *** Integers

(defclass integer-field (number-field)
  ())

(defmethod initialize-instance :after ((field integer-field)
                                       &rest initargs
				       &key validators &allow-other-keys)
  (declare (ignore initargs))
  (setf (validators field) (cons (make-instance 'is-an-integer-validator) validators)))

(defmethod value ((field integer-field))
  (parse-integer (client-value field) :junk-allowed t))

(defclass is-an-integer-validator (is-a-number-validator)
  ())

(defmethod validate ((field integer-field) (validator is-an-integer-validator))
  (let ((in-len (length (client-value field))))
    (multiple-value-bind (num len)
	(parse-integer (client-value field) :junk-allowed t)
      (and (= len in-len)
	   (integerp num)))))

(defmethod javascript-check ((field integer-field) (validator is-an-integer-validator))
  `(unless (and (not (is-na-n value))
                (>= 1 (slot-value (.split value ".") 'length)))
    (fail)))

;;;; *** Dates

(defclass universal-time-based-date-field ()
  ()
  (:documentation "A date field that communicates with universal time (as in CLHS) through the value and (setf value) methods."))

(defclass local-time-based-date-field (form-field)
  ()
  (:documentation "A date field that accepts/provides local-time objects through the value and (setf value) methods."))

(defmethod (setf value) ((utime integer) (self local-time-based-date-field))
  (setf (value self) (local-time :universal utime)))

(defclass date-field (universal-time-based-date-field form-field widget-component)
  ((year :accessor year :initform (make-instance 'integer-field
                                                 :input-size 4
                                                 :validators (list
                                                              (make-instance 'number-range-validator
                                                                             :min-value 0
                                                                             :max-value nil))))
   (month :accessor month :initform (make-instance 'integer-field
                                                   :input-size 2
                                                   :validators (list (make-instance 'number-range-validator
                                                                                    :min-value 0
                                                                                    :max-value 12))))
   (day :accessor day :initform (make-instance 'integer-field
                                               :input-size 2
                                               :validators (list (make-instance 'number-range-validator
                                                                                :min-value 0
                                                                                :max-value 31))))))

(defmethod date-ymd ((field date-field))
  (values (value (year field))
          (value (month field))
          (value (day field))))

(defmethod value ((field date-field))
  (multiple-value-bind (year month day)
      (date-ymd field)
    (if (and year month day)
        (encode-universal-time 0 0 0 day month year)
        nil)))

(defmethod (setf value) ((utime integer) (field date-field))
  (multiple-value-bind (second minute hour date month year day-of-week daylight-p zone)
      (decode-universal-time utime 0)
    (declare (ignore second minute hour day-of-week daylight-p zone))
    (setf (year field) year)
    (setf (month field) month)
    (setf (day field) date)))

(defmethod render :after ((field date-field))
  (<ucw:script
   `(progn
     ;; this is not essential, because the subfields have their own
     ;; dirty chackers but may come handy as an API for other scripts
     (setf (slot-value ($ ,(dom-id field)) 'ucw-dirty-p)
      (lambda ()
        (log.debug "Checking dirtyness of date-field " ,(dom-id field))
        (dolist (subnode (map dojo.by-id (array ,@(mapcar #'dom-id (list (day field) (month field) (year field))))))
          (when subnode
            (log.debug "Checking subnode " subnode.id)
            (when (.ucw-dirty-p subnode)
              (return true))))
        (return false))))))

(defclass dmy-date-field (date-field)
  ()
  (:documentation "Date fields which orders the inputs day/month/year"))

(defmethod render ((field dmy-date-field))
  (render (day field))
  (<:as-html "/")
  (render (month field))
  (<:as-html "/")
  (render (year field)))

(defclass mdy-date-field (date-field)
  ()
  (:documentation "Date field which orders the inputs month/day/year"))

(defmethod render ((field mdy-date-field))
  (render (month field))
  (<:as-html "/")
  (render (day field))
  (<:as-html "/")
  (render (year field)))

(defclass is-a-date-validator (value-validator)
  ())

(defmethod validate ((field universal-time-based-date-field) (validator is-a-date-validator))
  (integerp (value field)))

(defmethod validate ((field local-time-based-date-field) (validator is-a-date-validator))
  (typep (value field) 'local-time))

(defclass is-a-date-time-validator (is-a-date-validator)
  ())

(defclass time-range-validator (is-a-date-time-validator value-range-validator)
  ())

(defmethod validate ((field universal-time-based-date-field) (validator time-range-validator))
  (let ((min-value (min-value validator))
        (max-value (max-value validator)))
    (and (call-next-method)
         (<= (or min-value (1- (value field)))
             (value field)
             (or max-value (1+ (value field)))))))

(defmethod validate ((field local-time-based-date-field) (validator time-range-validator))
  (let ((min-value (min-value validator))
        (max-value (max-value validator))
        (value (value field)))
    (and (call-next-method)
         (or (not min-value) (local-time<= min-value value))
         (or (not max-value) (local-time<= value max-value)))))

;;;; *** Select input fields

;;;; We generate lots of strings which are used as the values in
;;;; option tags, this variable, and the integer-to-string function,
;;;; prevent generating large amounts of throw away strings.


#+allegro ;; TODO: Fixes a bug in allegro, remove later when Franz has sorted this out
(excl:without-package-locks 
  (defun common-lisp:map-into (result-sequence function &rest sequences)
   (loop for index below (apply #'min 
                                (length result-sequence)
                                (mapcar #'length sequences))
         do (setf (elt result-sequence index)
                  (apply function
                         (mapcar #'(lambda (seq) (elt seq index))
                                 sequences))))
   result-sequence))

(defvar +string-index-cache+
  (map-into (make-array 50
                        :element-type 'string
                        :adjustable t)
            (let ((i -1))
              (lambda ()
                (princ-to-string (incf i))))))

(defun integer-to-string (i)
  (cond
    ((<= (length +string-index-cache+) i)
     (adjust-array +string-index-cache+ (1+ i))
     (setf (aref +string-index-cache+ i) (princ-to-string i)))
    ((null (aref +string-index-cache+ i))
     (setf (aref +string-index-cache+ i) (princ-to-string i)))
    (t
     (aref +string-index-cache+ i))))

(defclass select-field (generic-html-input)
  ((data-set :accessor data-set :initarg :data-set
             :documentation "The values this select chooses
             from.")
   (test-fn :accessor test-fn :initarg :test-fn)
   (on-change-action-factory :accessor on-change-action-factory-of
                             :initform nil :initarg :on-change-action-factory
                             :documentation "May return (values action-entry progress-label) to be installed on on-change.")
   (data-map :accessor data-map :initform '()
             :documentation "An alist mapping strings, used in
   the <:select's options, to field specific values in the data-set.
   This is an internal field which should only be accessed by render
   and value methods."))
  (:default-initargs :test-fn #'string=)
  (:documentation "Form field used for selecting one value from a
  list of available options."))

(defmethod value ((field select-field))
  (cdr (assoc (client-value field) (data-map field) :test #'string=)))

(defmethod (setf value) (new-value (field select-field))
  (unless (data-map field)
    (setf (data-map field) (build-data-map field)))
  (dolist* ((client-key . value) (data-map field))
    (when (test-select-field-value field new-value value)
      (setf (client-value field) client-key)
      (return-from value new-value)))
  new-value)

(defgeneric test-select-field-value (field new-value value)
  (:method ((field select-field) new-value value)
           (funcall (test-fn field) new-value value)))

(defgeneric build-data-map (field)
  (:documentation "Given FIELD's data-set returns an alist
  mapping strings to the values in FIELD's data-set. Each key in
  the returned alist must be unique."))

(defmethod build-data-map ((field select-field))
  (loop
     for value in (data-set field)
     for index upfrom 0
     collect (cons (integer-to-string index) value)))

(defmethod render ((field select-field))
  (let (on-change-progress-label on-change)
    (awhen (on-change-action-factory-of field)
      (multiple-value-setq (on-change on-change-progress-label) (funcall it)))
    (<ucw:simple-select
               :accessor (client-value field)
               :title (tooltip field)
               :id (dom-id field)
               :accesskey (accesskey field)
               :tabindex (tabindex field)
               :class (css-class field)
               :style (css-style field)
               :on-change on-change
               :on-change-progress-label on-change-progress-label
    (setf (data-map field) (build-data-map field))
    (render-options field))))

(defgeneric render-options (select-field)
  (:documentation "Function used to render the list of options of
  a select field. Methods are always called AFTER the data-map
  has been setup."))

(defmethod render-options ((field select-field))
  (dolist* ((key-string . value) (data-map field))
    (<:option :value key-string
              :selected  (string= (client-value field) key-string)
      (render-value field value))))

(defgeneric render-value (select-field value)
  (:documentation "This function will be passed each value in the field's
   data-set and must produce the body of the corresponding
   <ucw:option tag."))

(defmethod render-value ((field select-field) (value null))
  ;; often you need a select-field that has a "nothing selected" element.
  ;; for convenience nil values are rendered as an empty element
  )

(defmethod render-value ((field select-field) (value t))
  "This default method just wraps princ-to-string in a <:as-html."
  (<:as-html value))

;;;; **** selecting from mappings

;;;; It often happens that you want to select a value based on the
;;;; corresponding key.

(defclass mapping-select-field (select-field)
  ()
  (:documentation "Class used when we want to chose the values of
  a certain mapping based on the keys. We render the keys in the
  select and return the corresponding value from the VALUE
  method."))

(defmethod render-options ((field mapping-select-field))
  (dolist* ((key-string . key) (data-map field))
    (<:option :value key-string
              :selected  (string= (client-value field) key-string)
              (render-key field key))))

(defgeneric render-key (field key)
  (:method ((field mapping-select-field) (key t))
    (<:as-html key)))

(defclass hash-table-select-field (mapping-select-field)
  ())

(defmethod build-data-map ((field hash-table-select-field))
  (loop
     for key being the hash-keys of (data-set field)
     for index upfrom 0
     collect (cons (integer-to-string index) key)))

(defmethod value ((field hash-table-select-field))
  (gethash (call-next-method) (data-set field)))

(defclass alist-select-field (mapping-select-field)
  ()
  (:default-initargs :test-fn #'eql)
  (:documentation "Renders an alist of (key . value)'s so that
VALUE's are displayed <:as-html and KEY's are returned by the VALUE accessor."))

(defmethod build-data-map ((field alist-select-field))
  (iter (for entry in (data-set field))
        (for index upfrom 0)
        (collect (cons (integer-to-string index) entry))))

(defmethod test-select-field-value ((field alist-select-field) new-value value)
  (funcall (test-fn field) new-value (car value)))

(defmethod render-key ((field alist-select-field) entry)
  (<:as-html (cdr entry)))

(defmethod value ((field alist-select-field))
  (car (call-next-method)))

(defclass plist-select-field (mapping-select-field)
  ()
  (:default-initargs :test-fn #'eql)
  (:documentation "Renders a plist of (indicator1 value1 ...) so that
VALUE's are displayed <:as-html and INDICATOR's are returned by the value accessor."))

(defmethod build-data-map ((field plist-select-field))
  (loop
     for (key value) on (data-set field) by #'cddr
     for index upfrom 0
     collect (cons (integer-to-string index) (cons key value))))

(defmethod test-select-field-value ((field plist-select-field) new-value value)
  (funcall (test-fn field) new-value (car value)))

(defmethod render-key ((field plist-select-field) entry)
  (<:as-html (cdr entry)))

(defmethod value ((field plist-select-field))
  (car (call-next-method)))

;;;; *** Radio Buttons 

(defclass radio-group (generic-html-input)
  ((value-widgets :accessor value-widgets :initform '()))
  (:default-initargs  
   ;; XXX: this is really really bad. there's no guarntee that we
   ;; don't already have a callback with this name :(
   :name (strcat "_r_" (random-string +callback-id-length+))))

(defmethod value ((group radio-group))
  (let ((index (parse-integer (client-value group) :junk-allowed t)))    
    (when index
      (value (nth index (value-widgets group))))))

(defmethod (setf value) (value (group radio-group))
  (setf (client-value group)
        (format nil "~D" (position value (value-widgets group) :key #'value :test #'eq))))

(defmethod shared-initialize :after ((group radio-group)
                                     slot-names
                                     &key (values nil values-p)
                                          (value nil value-p)
                                     &allow-other-keys)
  (declare (ignore slot-names))
  (when values-p
    (setf (value-widgets group) '())
    (map nil (curry #'add-value group) values)
    (when value-p
      (setf (value group) value))))

(defmethod add-value ((group radio-group) value)
  (push (make-instance 'radio-button
                       :value value
                       :group group)
        (value-widgets group))
  value)

(defmethod value-widget ((group radio-group) value)
  "Returns the widget associated with the value VALUE in GROUP."
  (find value (value-widgets group) :test #'eq :key #'value))

(defclass radio-button (generic-html-input)
  ((value :accessor value :initarg :value)
   (group :accessor group :initarg :group
          :documentation "The RADIO-GROUP this button is a part
          of."))
  (:documentation "A widget representing a single radio
  button. Should be used in conjunction with a RADIO-GROUP."))

(defmethod render ((button radio-button))
  (register-callback (lambda (value)
                       (setf (client-value (group button)) value))
                     :id (name (group button)))
  (<:input :id (dom-id button)
	   :accesskey (accesskey button)
           :title (tooltip button)
           :tabindex (tabindex button)
           :type "radio"
           :name (name (group button))
           :value (format nil "~D" (position button (value-widgets (group button))))
           :checked (eq (value (group button)) (value button))
           :class (css-class button)
	   :style (css-style button)))

(defprint-object (button radio-button)
  (format *standard-output* "VALUE: ~S GROUP: ~S" (value button) (group button)))

;;;; *** Check box

(defclass checkbox-field (generic-html-input)
  ((enabled-image :initform nil :accessor enabled-image-of :initarg :enabled-image)
   (disabled-image :initform nil :accessor disabled-image-of :initarg :disabled-image)
   (enabled-tooltip :initform nil :accessor enabled-tooltip-of :initarg :enabled-tooltip)
   (disabled-tooltip :initform nil :accessor disabled-tooltip-of :initarg :disabled-tooltip))
  (:default-initargs :client-value nil))

(defmethod render ((field checkbox-field))
  (let* ((value (value field))
         (custom (not (null (enabled-image-of field))))
         (dom-id (dom-id field))
         (hidden-id (strcat dom-id "-hidden")))
    (<ucw:input
     :id hidden-id
     :reader (when value
               "on")
     :writer (lambda (v)
               (setf (value field) (and (not (null v))
                                        (not (string= "" v)))))
     :type "hidden")
    (if custom
        (let ((img-id (js:gen-js-name-string :prefix "_ucw_img")))
          (<:a :id dom-id
               :tabindex (tabindex field)
               :class (css-class field)
               (<:img :id img-id))
          (<ucw:script :toplevelp t
                       `(ucw.field.setup-custom-checkbox ,dom-id ,img-id ,hidden-id
                         ,(enabled-image-of field) ,(enabled-tooltip-of field)
                         ,(disabled-image-of field) ,(disabled-tooltip-of field))))
        (progn
          (<:input
           :id dom-id
           :checked (if value t nil)
           :name (name field)
           :accesskey (accesskey field)
           :title (or (tooltip field) (if value
                                          (enabled-tooltip-of field)
                                          (disabled-tooltip-of field)))
           :tabindex (tabindex field)
           :class (css-class field)
           :style (css-style field)
           :type "checkbox")
          (<ucw:script :toplevelp t
                       `(ucw.field.setup-simple-checkbox ,dom-id ,hidden-id ,custom
                         ,(enabled-tooltip-of field) ,(disabled-tooltip-of field)))))))

(defmethod value ((field checkbox-field))
  (if (and (client-value field)
           (not (string= "" (client-value field))))
      t
      nil))

(defmethod (setf value) (new-value (field checkbox-field))
  (setf (client-value field) (if new-value
                                 t
                                 nil)))

;;;; *** Upload

(defclass file-upload-field (generic-html-input)
  ())

(defmethod render ((field file-upload-field))
  (<ucw:input :id (dom-id field)
              :name (name field)
	      :accesskey (accesskey field)
              :type "file"
              :title (tooltip field)
              :tabindex (tabindex field)
              :accessor (client-value field)
              :class (css-class field)
	      :style (css-style field)))

;;;; *** Submit Button

(defclass submit-button (generic-html-input)
  ((label :accessor label :initform nil :initarg :label)))

(defmethod render ((field submit-button))
  (<:submit :id (dom-id field)
	    :accesskey (accesskey field)
            :title (tooltip field)
            :tabindex (tabindex field)
            :value (label field)
            :class (css-class field)
	    :style (css-style field)))


;;;; ** Simple Form

;;;; This isn't technically part of the form library, but it is
;;;; convenient. Note that a SIMPLE-FORM, unlike the widgets in the
;;;; rest of this file, is a component and can be call'd and answer
;;;; like regular components.

(defcomponent simple-form (html-element)
  ((submit-method :accessor submit-method
                  :initform "post"
                  :initarg :submit-method)
   (abandon-handler :accessor abandon-handler
                    :initform :ask-user
                    :initarg :abandon-handler)
   (default-refresh-action-p :initform t :initarg :default-refresh-action-p
                             :accessor default-refresh-action-p)
   (dom-id :accessor dom-id
           :initform (js:gen-js-name-string :prefix "_ucw_simple_form_")
           :initarg :dom-id)))

(defmethod render :wrapping ((form simple-form))
  (<ucw:form :id (dom-id form)
             :method (submit-method form)
             :abandon-handler (abandon-handler form)
             :action (when (default-refresh-action-p form)
                       (register-action (:with-call/cc t)
                         (refresh-component form)))
             :class (css-class form)
             :style (css-style form)
    (call-next-method)))

