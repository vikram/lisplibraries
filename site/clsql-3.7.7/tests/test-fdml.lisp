;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:    test-fdml.lisp
;;;; Author:  Marcus Pearce <m.t.pearce@city.ac.uk>, Kevin Rosenberg
;;;; Created: 30/03/2004
;;;; Updated: $Id: test-fdml.lisp 10922 2006-05-03 14:39:10Z kevin $
;;;;
;;;; Tests for the CLSQL Functional Data Manipulation Language
;;;; (FDML).
;;;;
;;;; This file is part of CLSQL.
;;;;
;;;; CLSQL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; ======================================================================

(in-package #:clsql-tests)

#.(clsql:locally-enable-sql-reader-syntax)

(setq *rt-fdml*
      '(
	
;; inserts a record using all values only and then deletes it 
(deftest :fdml/insert/1
    (let ((now (get-universal-time)))
      (clsql:insert-records :into [employee] 
       :values `(11 1 "Yuri" "Gagarin" "gagarin@soviet.org"
		    1 1 1.85 t ,(clsql:utime->time now) ,now))
      (values 
       (clsql:select [first-name] [last-name] [email]
                    :from [employee] :where [= [emplid] 11])
       (progn (clsql:delete-records :from [employee] :where [= [emplid] 11])
              (clsql:select [*] :from [employee] :where [= [emplid] 11]))))
  (("Yuri" "Gagarin" "gagarin@soviet.org")) nil)

;; inserts a record using attributes and values and then deletes it
(deftest :fdml/insert/2
    (progn
      (clsql:insert-records :into [employee] 
                           :attributes '(emplid groupid first_name last_name
                                         email ecompanyid managerid)
                           :values '(11 1 "Yuri" "Gagarin" "gagarin@soviet.org"
                                     1 1))
      (values 
       (clsql:select [first-name] [last-name] [email] :from [employee]
                    :where [= [emplid] 11])
       (progn (clsql:delete-records :from [employee] :where [= [emplid] 11])
              (clsql:select [*] :from [employee] :where [= [emplid] 11]))))
  (("Yuri" "Gagarin" "gagarin@soviet.org")) nil)

;; inserts a record using av-pairs and then deletes it
(deftest :fdml/insert/3
    (progn
      (clsql:insert-records :into [employee] 
                           :av-pairs'((emplid 11) (groupid 1)
                                      (first_name "Yuri")
                                      (last_name "Gagarin")
                                      (email "gagarin@soviet.org")
                                      (ecompanyid 1) (managerid 1)))
      (values 
       (clsql:select [first-name] [last-name] [email] :from [employee]
                    :where [= [emplid] 11])
       (progn (clsql:delete-records :from [employee] :where [= [emplid] 11])
              (clsql:select [first-name] [last-name] [email] :from [employee]
                           :where [= [emplid] 11]))))
  (("Yuri" "Gagarin" "gagarin@soviet.org")) nil)

;; inserts a records using a query from another table 
(deftest :fdml/insert/4
    (progn
      (clsql:create-table [employee2] '(([forename] string)
				 ([surname] string)
				 ([email] string)))
      (clsql:insert-records :into [employee2] 
		     :query [select [first-name] [last-name] [email] 
				    :from [employee]]
		     :attributes '(forename surname email))
      (prog1
          (equal (clsql:select [*] :from [employee2])
                 (clsql:select [first-name] [last-name] [email]
                              :from [employee]))
        (clsql:drop-table [employee2] :if-does-not-exist :ignore)))
  t)

;; updates a record using attributes and values and then deletes it
(deftest :fdml/update/1
    (progn
      (clsql:update-records [employee] 
                           :attributes '(first_name last_name email)
                           :values '("Yuri" "Gagarin" "gagarin@soviet.org")
                           :where [= [emplid] 1])
      (values 
       (clsql:select [first-name] [last-name] [email] :from [employee]
                    :where [= [emplid] 1])
       (progn
         (clsql:update-records [employee] 
                              :av-pairs'((first_name "Vladimir")
                                         (last_name "Lenin")
                                         (email "lenin@soviet.org"))
                              :where [= [emplid] 1])
         (clsql:select [first-name] [last-name] [email] :from [employee]
                      :where [= [emplid] 1]))))
  (("Yuri" "Gagarin" "gagarin@soviet.org"))
  (("Vladimir" "Lenin" "lenin@soviet.org")))

;; updates a record using av-pairs and then deletes it
(deftest :fdml/update/2
    (progn
      (clsql:update-records [employee] 
                           :av-pairs'((first_name "Yuri")
                                      (last_name "Gagarin")
                                      (email "gagarin@soviet.org"))
                           :where [= [emplid] 1])
      (values 
       (clsql:select [first-name] [last-name] [email] :from [employee]
                    :where [= [emplid] 1])
       (progn
         (clsql:update-records [employee]
                              :av-pairs'((first_name "Vladimir")
                                         (last_name "Lenin")
                                         (email "lenin@soviet.org"))
                              :where [= [emplid] 1])
         (clsql:select [first-name] [last-name] [email]
                      :from [employee] :where [= [emplid] 1]))))
  (("Yuri" "Gagarin" "gagarin@soviet.org"))
  (("Vladimir" "Lenin" "lenin@soviet.org")))


;; Computed values are not always classified as numeric by psqlodbc
(deftest :fdml/query/1
    (let ((count (caar (clsql:query "SELECT COUNT(*) FROM EMPLOYEE WHERE (EMAIL LIKE '%org')" :field-names nil))))
      (if (stringp count)
	  (nth-value 0 (parse-integer count))
	  (nth-value 0 (truncate count))))
  10)

(deftest :fdml/query/2
    (multiple-value-bind (rows field-names)
	(clsql:query
	 "SELECT FIRST_NAME,LAST_NAME FROM EMPLOYEE WHERE (EMPLID <= 5) ORDER BY LAST_NAME")
      (values rows (mapcar 'string-upcase field-names)))
  (("Leonid" "Brezhnev") ("Nikita" "Kruschev") ("Vladimir" "Lenin")
   ("Josef" "Stalin") ("Leon" "Trotsky"))
  ("FIRST_NAME" "LAST_NAME"))

(deftest :fdml/query/3
    (caar (clsql:query "SELECT EMPLID FROM EMPLOYEE WHERE LAST_NAME = 'Andropov'" :field-names nil))
  6)
  
(deftest :fdml/query/4
    (typep (caar (clsql:query "SELECT HEIGHT FROM EMPLOYEE WHERE LAST_NAME = 'Andropov'" :field-names nil))
     'float)
  t)
  
(deftest :fdml/query/5
    (let ((res (clsql:query (clsql:sql [select [first-name] [sum [emplid]] :from [employee]] 
				       [group-by [first-name]] [order-by [sum [emplid]]])
			    :field-names nil :result-types nil)))
      (mapcar (lambda (p) (list (car p) (truncate (read-from-string (second p)))))
	      res))
  (("Josef" 2) ("Leon" 3) ("Nikita" 4) ("Leonid" 5) ("Yuri" 6)
  ("Konstantin" 7) ("Mikhail" 8) ("Boris" 9) ("Vladimir" 11)))

(deftest :fdml/query/6
    (let ((res (clsql:query (clsql:sql [union [select [emplid] :from [employee]] 
				       [select [groupid] :from [company]]])
			    :field-names nil :result-types nil :flatp t)))
      (values (every #'stringp res)
	      (mapcar #'(lambda (f) (truncate (read-from-string f))) res)))
  t (1 2 3 4 5 6 7 8 9 10))

(deftest :fdml/query/7
    (let ((res (car (clsql:query (clsql:sql [intersect [select [emplid] :from [employee]] 
					    [select [groupid] :from [company]]])
				 :field-names nil :result-types nil :flatp t))))
      (values (stringp res)
	      (nth-value 0 (truncate (read-from-string res)))))
  t 1)

(deftest :fdml/query/8
    (let ((res (clsql:query (clsql:sql [except [select [emplid] :from [employee]] 
				       [select [groupid] :from [company]]])
			    :field-names nil :result-types nil :flatp t)))
      (values (every #'stringp res)
	      (mapcar #'(lambda (f) (truncate (read-from-string f))) res)))
  t (2 3 4 5 6 7 8 9 10))


(deftest :fdml/execute-command/1
    (values
     (clsql:table-exists-p [foo] :owner *test-database-user*)
     (progn
       (clsql:execute-command "create table foo (bar integer)")
       (clsql:table-exists-p [foo] :owner *test-database-user*))
     (progn
       (clsql:execute-command "drop table foo")
       (clsql:table-exists-p [foo] :owner *test-database-user*)))
  nil t nil)


;; compare min, max and average hieghts in inches (they're quite short
;; these guys!) 
(deftest :fdml/select/1
    (let ((max (clsql:select [function "floor"
			     [/ [* [max [height]] 100] 2.54]]
			     :from [employee]
			     :result-types nil 
			     :flatp t))
	  (min (clsql:select [function "floor"
			     [/ [* [min [height]] 100] 2.54]]
			     :from [employee]
			     :result-types nil 
			     :flatp t))
	  (avg (clsql:select [function "floor"
			     [avg [/ [* [height] 100] 2.54]]]
			     :from [employee]
			     :result-types nil 
			     :flatp t)))
      (apply #'< (mapcar #'(lambda (s) (parse-integer s :junk-allowed t))
      			 (append min avg max))))
 t)

(deftest :fdml/select/2
 (clsql:select [first-name] :from [employee] :flatp t :distinct t
                            :field-names nil 
                            :result-types nil 
                            :order-by [first-name])
 ("Boris" "Josef" "Konstantin" "Leon" "Leonid" "Mikhail" "Nikita" "Vladimir"
  "Yuri"))

(deftest :fdml/select/3
    (let ((res (clsql:select [first-name] [count [*]] :from [employee]
			     :result-types nil 
			     :group-by [first-name]
			     :order-by [first-name]
			     :field-names nil)))
      (mapcar (lambda (p) (list (car p) (truncate (read-from-string (second p)))))
	      res))
  (("Boris" 1) ("Josef" 1) ("Konstantin" 1) ("Leon" 1) ("Leonid" 1)
   ("Mikhail" 1) ("Nikita" 1) ("Vladimir" 2) ("Yuri" 1)))

(deftest :fdml/select/4
    (clsql:select [last-name] :from [employee] 
			  :where [like [email] "%org"]
			  :order-by [last-name]
			  :field-names nil 
			  :result-types nil 
			  :flatp t)
 ("Andropov" "Brezhnev" "Chernenko" "Gorbachev" "Kruschev" "Lenin" "Putin"
  "Stalin" "Trotsky" "Yeltsin"))

(deftest :fdml/select/5
    (clsql:select [email] :from [employee] :flatp t :result-types nil 
			  :where [in [employee emplid]
			  [select [managerid] :from [employee]]]
			  :field-names nil)
  ("lenin@soviet.org"))

(deftest :fdml/select/6
    (if (clsql-sys:db-type-has-fancy-math? *test-database-underlying-type*)
        (mapcar #'(lambda (s) (parse-integer s :junk-allowed t))
		(clsql:select [function "trunc" [height]] :from [employee]
			      :result-types nil 
			      :field-names nil
			      :flatp t))
	(mapcar #'(lambda (s) (truncate (parse-integer s :junk-allowed t)))
		(clsql:select [height] :from [employee] :flatp t 
			      :field-names nil :result-types nil)))
 (1 1 1 1 1 1 1 1 1 1))

(deftest :fdml/select/7
    (let ((result (car (clsql:select [max [emplid]] :from [employee] :flatp t 
				     :field-names nil :result-types nil))))
      (values 
       (stringp result)
       (nth-value 0 (truncate (read-from-string result)))))
  t 10)

(deftest :fdml/select/8
    (let ((result (car (clsql:select [min [emplid]] :from [employee] :flatp t 
				     :field-names nil :result-types nil))))
      (values
       (stringp result)
       (nth-value 0 (truncate (read-from-string result)))))
  t 1)

(deftest :fdml/select/9
    (subseq 
     (car 
      (clsql:select [avg [emplid]] :from [employee] :flatp t 
		    :field-names nil :result-types nil)) 
     0 3)
  "5.5")

(deftest :fdml/select/10
    (clsql:select [last-name] :from [employee]
		  :where [not [in [emplid]
		  [select [managerid] :from [company]]]]
		  :result-types nil 
		  :field-names nil 
		  :flatp t
		  :order-by [last-name])
 ("Andropov" "Brezhnev" "Chernenko" "Gorbachev" "Kruschev" "Putin" "Stalin"
  "Trotsky" "Yeltsin"))

(deftest :fdml/select/11
    (clsql:select [last-name] :from [employee] :where [married] :flatp t
		  :field-names nil :order-by [emplid] :result-types nil)
  ("Lenin" "Stalin" "Trotsky"))

(deftest :fdml/select/12
    (let ((v 1))
      (clsql:select [last-name] :from [employee] :where [= [emplid] v]
		    :field-names nil :result-types nil))
  (("Lenin")))

(deftest :fdml/select/13
     (multiple-value-bind (results field-names) 
	 (clsql:select [emplid] [last-name] :from [employee] 
		       :where [= [emplid] 1])
       (values results (mapcar #'string-downcase field-names)))
 ((1 "Lenin"))
 ("emplid" "last_name"))

(deftest :fdml/select/14
     (floatp (car (clsql:select [height] :from [employee] :where [= [emplid] 1] 
				:flatp t)))
  t)

(deftest :fdml/select/15
    (multiple-value-bind (rows field-names)
	(clsql:select [addressid] [street-number] [street-name] [city_field] [zip] 
	 :from [addr]
	 :where [= 1 [addressid]])
      (values
       rows
       (mapcar #'string-downcase field-names)))
  ((1 10 "Park Place" "Leningrad" 123))
  ("addressid" "street_number" "street_name" "city_field" "zip"))

(deftest :fdml/select/16
    (clsql:select [emplid] :from [employee] :where [= 1 [emplid]]
     :field-names nil)
  ((1)))

(deftest :fdml/select/17
    (clsql:select [emplid] [last-name] :from [employee] :where [= 1 [emplid]]
     :field-names nil)
  ((1 "Lenin")))

(deftest :fdml/select/18
    (clsql:select [emplid :string] [last-name] :from [employee] :where [= 1 [emplid]]
     :field-names nil)
  (("1" "Lenin")))

(deftest :fdml/select/19
    (clsql:select [emplid] :from [employee] :order-by [emplid] 
                           :where [between [* [emplid] 10] [* 5 10] [* 10 10]]
                           :field-names nil :result-types nil :flatp t)
 ("5" "6" "7" "8" "9" "10"))

(deftest :fdml/select/20
    (clsql:select [emplid] :from [employee] :order-by [emplid] 
                           :where [not [between [* [emplid] 10] [* 5 10] [* 10 10]]]
                           :field-names nil :result-types nil :flatp t)
 ("1" "2" "3" "4"))

(deftest :fdml/select/21 
  (clsql:select [substring [first-name] 1 4] :from [employee] 
                :flatp t :order-by [emplid] :field-names nil)
 ("Vlad" "Jose" "Leon" "Niki" "Leon" "Yuri" "Kons" "Mikh" "Bori" "Vlad"))

(deftest :fdml/select/22 
   (case *test-database-underlying-type*
     (:mssql (clsql:select [+ [first-name] " " [last-name]] :from [employee]
                           :flatp t :order-by [emplid] :field-names nil))
     (t (clsql:select [|| [first-name] " " [last-name]] :from [employee]
                      :flatp t :order-by [emplid] :field-names nil)))
 ("Vladimir Lenin" "Josef Stalin" "Leon Trotsky" "Nikita Kruschev"
 "Leonid Brezhnev" "Yuri Andropov" "Konstantin Chernenko" "Mikhail Gorbachev"
 "Boris Yeltsin" "Vladimir Putin"))

(deftest :fdml/select/23
 (clsql:select [emplid] :from [employee] :where [in [emplid] '(1 2 3 4)]
                        :flatp t :order-by [emplid] :field-names nil
                        :result-types nil)
 ("1" "2" "3" "4"))

(deftest :fdml/select/24
 (clsql:select [distinct [first-name]] :from [employee] :flatp t
               :order-by [first-name] :field-names nil :result-types nil)
 ("Boris" "Josef" "Konstantin" "Leon" "Leonid" "Mikhail" "Nikita" "Vladimir"
  "Yuri"))

(deftest :fdml/select/25
 (clsql:select [first-name] :from (clsql-sys:convert-to-db-default-case "employee" *default-database*)
  :flatp t :distinct t
  :field-names nil 
  :result-types nil 
  :order-by [first-name])
 ("Boris" "Josef" "Konstantin" "Leon" "Leonid" "Mikhail" "Nikita" "Vladimir"
  "Yuri"))

(deftest :fdml/select/26
 (clsql:select ["table" first-name] ["table" last-name] 
  :from '([employee "table"] [employee "join"])
  :where [and [= ["table" first-name] 
                 ["join" first-name]]
              [not [= ["table" emplid] 
                      ["join" emplid]]]]
  :order-by '(["table" last-name])
  :result-types nil :field-names nil)
 (("Vladimir" "Lenin") ("Vladimir" "Putin")))

(deftest :fdml/select/27 
    (mapcar
     (lambda (f) (truncate (read-from-string f)))
     (clsql:select [coalesce [managerid] 10] :from [employee] :order-by [emplid]
		   :field-names nil :result-types nil :flatp t))
  (10 1 1 1 1 1 1 1 1 1))
  
(deftest :fdml/select/28 
    (mapcar
     (lambda (f) (truncate (read-from-string (car f))))
     (loop for column in `([*] [emplid]) collect         
	   (clsql:select [count column] :from [employee] 
			 :flatp t :result-types nil :field-names nil)))
 (10 10))

(deftest :fdml/select/29 
 (clsql:select [first-name] [last-name] :from [employee] 
                       :result-types nil :field-names nil 
                       :order-by '(([first-name] :asc) ([last-name] :desc)))
 (("Boris" "Yeltsin") ("Josef" "Stalin") ("Konstantin" "Chernenko")
  ("Leon" "Trotsky") ("Leonid" "Brezhnev") ("Mikhail" "Gorbachev")
  ("Nikita" "Kruschev") ("Vladimir" "Putin") ("Vladimir" "Lenin")
  ("Yuri" "Andropov")))

(deftest :fdml/select/30 
 (clsql:select [first-name] [last-name] :from [employee] 
                       :result-types nil :field-names nil 
                       :order-by '(([first-name] :asc) ([last-name] :asc)))
 (("Boris" "Yeltsin") ("Josef" "Stalin") ("Konstantin" "Chernenko")
  ("Leon" "Trotsky") ("Leonid" "Brezhnev") ("Mikhail" "Gorbachev")
  ("Nikita" "Kruschev") ("Vladimir" "Lenin") ("Vladimir" "Putin")
  ("Yuri" "Andropov")))

(deftest :fdml/select/31
 (clsql:select [last-name] :from [employee]                   
              :set-operation [union [select [first-name] :from [employee]
                                            :order-by [last-name]]]
              :flatp t
              :result-types nil 
              :field-names nil)
 ("Andropov" "Boris" "Brezhnev" "Chernenko" "Gorbachev" "Josef" "Konstantin"
 "Kruschev" "Lenin" "Leon" "Leonid" "Mikhail" "Nikita" "Putin" "Stalin"
 "Trotsky" "Vladimir" "Yeltsin" "Yuri"))

(deftest :fdml/select/32
    (clsql:select [emplid] :from [employee]
                :where [= [emplid] [any [select [companyid] :from [company]]]]
                :flatp t :result-types nil :field-names nil)
  ("1"))

(deftest :fdml/select/33
 (clsql:select [last-name] :from [employee] 
              :where [> [emplid] [all [select [groupid] :from [employee]]]]
              :order-by [last-name] 
              :flatp t :result-types nil :field-names nil)
("Andropov" "Brezhnev" "Chernenko" "Gorbachev" "Kruschev" "Putin" "Stalin"
 "Trotsky" "Yeltsin"))

(deftest :fdml/select/34
  (loop for x from 1 below 5
   collect
   (car
    (clsql:select [last-name] :from [employee] 
		  :where [= [emplid] x]
		  :flatp t :result-types nil :field-names nil)))
  ("Lenin" "Stalin" "Trotsky" "Kruschev"))

;; test escaping of single quotes 
(deftest :fdml/select/35 
    (clsql:select "What's up doc?" :from [employee] :flatp t :field-names nil)
  ("What's up doc?" "What's up doc?" "What's up doc?" "What's up doc?"
   "What's up doc?" "What's up doc?" "What's up doc?" "What's up doc?"
   "What's up doc?" "What's up doc?"))

;; test proper treatment of backslash (depending on backend) 
(deftest :fdml/select/36
    (clsql:select "foo\\bar\\baz" :from [employee] :flatp t :field-names nil)
 ("foo\\bar\\baz" "foo\\bar\\baz" "foo\\bar\\baz" "foo\\bar\\baz" 
  "foo\\bar\\baz"  "foo\\bar\\baz" "foo\\bar\\baz" "foo\\bar\\baz" 
  "foo\\bar\\baz" "foo\\bar\\baz"))

(deftest :fdml/select/37
    (clsql:select [emplid] :from [employee] 
                  :order-by [emplid]
                  :limit 5 
                  :field-names nil
                  :flatp t)
  (1 2 3 4 5))

(deftest :fdml/select/38
    (clsql:select [emplid] :from [employee] 
                  :order-by [emplid]
                  :limit 5 
                  :offset 3
                  :field-names nil
                  :flatp t)
  (4 5 6 7 8))

(deftest :fdml/do-query/1
    (let ((result '()))
    (clsql:do-query ((name) [select [last-name] :from [employee]
                                   :order-by [last-name]])
      (push name result))
    result)
 ("Yeltsin" "Trotsky" "Stalin" "Putin" "Lenin" "Kruschev" "Gorbachev"
            "Chernenko" "Brezhnev" "Andropov")) 

(deftest :fdml/map-query/1
    (clsql:map-query 'list #'identity
                    [select [last-name] :from [employee] :flatp t
                            :order-by [last-name]])
  ("Andropov" "Brezhnev" "Chernenko" "Gorbachev" "Kruschev" "Lenin" "Putin"
              "Stalin" "Trotsky" "Yeltsin"))

(deftest :fdml/map-query/2
    (clsql:map-query 'vector #'identity
                    [select [last-name] :from [employee] :flatp t
                            :order-by [last-name]])
  #("Andropov" "Brezhnev" "Chernenko" "Gorbachev" "Kruschev" "Lenin" "Putin"
    "Stalin" "Trotsky" "Yeltsin"))

(deftest :fdml/map-query/3 
 (clsql:map-query 'list #'identity
                  [select [last-name] :from [employee] :order-by [last-name]])
 (("Andropov") ("Brezhnev") ("Chernenko") ("Gorbachev") ("Kruschev") ("Lenin")
  ("Putin") ("Stalin") ("Trotsky") ("Yeltsin")))

(deftest :fdml/map-query/4 
 (clsql:map-query 'list #'identity
                  [select [first-name] [last-name] :from [employee] 
                          :order-by [last-name]])
 (("Yuri" "Andropov") ("Leonid" "Brezhnev") ("Konstantin" "Chernenko")
  ("Mikhail" "Gorbachev") ("Nikita" "Kruschev") ("Vladimir" "Lenin")
  ("Vladimir" "Putin") ("Josef" "Stalin") ("Leon" "Trotsky") 
  ("Boris" "Yeltsin")))
  
(deftest :fdml/loop/1
    (loop for (forename surname)
      being each tuple in
      [select [first-name] [last-name] :from [employee] :order-by [last-name]]
      collect (concatenate 'string forename " " surname))
  ("Yuri Andropov" "Leonid Brezhnev" "Konstantin Chernenko" "Mikhail Gorbachev"
                   "Nikita Kruschev" "Vladimir Lenin" "Vladimir Putin"
   "Josef Stalin" "Leon Trotsky" "Boris Yeltsin"))

(deftest :fdml/loop/2
    (loop for (addressid)
      being each tuple in
      [select [addressid] :from [addr] :order-by [addressid]]
     collect addressid)
  (1 2))

(deftest :fdml/loop/3
    (loop for addressid
      being each tuple in
      [select [addressid] :from [addr] :order-by [addressid]]
      collect addressid)
  (1 2))

;; starts a transaction deletes a record and then rolls back the deletion 
(deftest :fdml/transaction/1
    (let ((results '()))
      ;; test if we are in a transaction
      (push (clsql:in-transaction-p) results)
      ;;start a transaction 
      (clsql:start-transaction)
      ;; test if we are in a transaction
      (push (clsql:in-transaction-p) results)
      ;;Putin has got to go
      (clsql:delete-records :from [employee] :where [= [last-name] "Putin"])
      ;;Should be nil 
      (push 
       (clsql:select [*] :from [employee] :where [= [last-name] "Putin"])
       results)
      ;;Oh no, he's still there
      (clsql:rollback)
      ;; test that we are out of the transaction
      (push (clsql:in-transaction-p) results)
      ;; Check that we got him back alright 
      (push (clsql:select [email] :from [employee] :where [= [last-name] "Putin"]
                         :flatp t)
            results)
      (apply #'values (nreverse results)))
  nil t nil nil ("putin@soviet.org"))

;; starts a transaction, updates a record and then rolls back the update
(deftest :fdml/transaction/2
    (let ((results '()))
      ;; test if we are in a transaction
      (push (clsql:in-transaction-p) results)
      ;;start a transaction 
      (clsql:start-transaction)
      ;; test if we are in a transaction
      (push (clsql:in-transaction-p) results)
      ;;Putin has got to go
      (clsql:update-records [employee]
       :av-pairs '((email "putin-nospam@soviet.org"))
       :where [= [last-name] "Putin"])
      ;;Should be new value  
      (push (clsql:select [email] :from [employee]
                         :where [= [last-name] "Putin"]
                         :flatp t)
            results)
      ;;Oh no, he's still there
      (clsql:rollback)
      ;; test that we are out of the transaction
      (push (clsql:in-transaction-p) results)
      ;; Check that we got him back alright 
      (push (clsql:select [email] :from [employee] :where [= [last-name] "Putin"]
                         :flatp t)
            results)
      (apply #'values (nreverse results)))
  nil t ("putin-nospam@soviet.org") nil ("putin@soviet.org")) 

;; runs an update within a transaction and checks it is committed
(deftest :fdml/transaction/3
    (let ((results '()))
      ;; check status 
      (push (clsql:in-transaction-p) results)
      ;; update records 
      (push
       (clsql:with-transaction () 
         (clsql:update-records [employee] 
                              :av-pairs '((email "lenin-nospam@soviet.org"))
                              :where [= [emplid] 1]))
       results)
      ;; check status 
      (push (clsql:in-transaction-p) results)
      ;; check that was committed 
      (push (clsql:select [email] :from [employee] :where [= [emplid] 1]
                         :flatp t)
            results)
      ;; undo the changes 
      (push
       (clsql:with-transaction () 
         (clsql:update-records [employee] 
                              :av-pairs '((email "lenin@soviet.org"))
                              :where [= [emplid] 1]))
       results)
      ;; and check status 
      (push (clsql:in-transaction-p) results)
      ;; check that was committed 
      (push (clsql:select [email] :from [employee] :where [= [emplid] 1]
                         :flatp t)
            results)
      (apply #'values (nreverse results)))
  nil nil nil ("lenin-nospam@soviet.org") nil nil ("lenin@soviet.org"))

;; runs a valid update and an invalid one within a transaction and checks
;; that the valid update is rolled back when the invalid one fails. 
(deftest :fdml/transaction/4
    (let ((results '()))
      ;; check status
      (push (clsql:in-transaction-p) results)
      (handler-case 
	  (clsql:with-transaction () 
	    ;; valid update
	    (clsql:update-records [employee] 
				  :av-pairs '((email "lenin-nospam@soviet.org"))
				  :where [= [emplid] 1])
	    ;; invalid update which generates an error 
            (clsql:update-records [employee] 
				  :av-pairs
				  '((emale "lenin-nospam@soviet.org"))
				  :where [= [emplid] 1]))
        (clsql:sql-database-error ()
          (progn
            ;; check status 
            (push (clsql:in-transaction-p) results)
            ;; and check nothing done 
            (push (clsql:select [email] :from [employee] :where [= [emplid] 1]
                               :flatp t)
                  results)
            (apply #'values (nreverse results))))))
  nil nil ("lenin@soviet.org"))


))

#.(clsql:restore-sql-reader-syntax-state)
