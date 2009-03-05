;;; autoloads generated on 2007-11-12_11:34:34
;;; by SBCL [1.0.10]

(in-package :cllib)
(export '(autoload))

(DEFUN AUTOLOAD (SYMB FILE &OPTIONAL COMMENT)
  "Declare symbol SYMB to call a function defined in FILE."
  (DECLARE (SYMBOL SYMB) (TYPE (OR SIMPLE-STRING NULL) COMMENT))
  (EXPORT (LIST SYMB))
  (UNLESS (FBOUNDP SYMB)
    (LET ((PATH
           (TRANSLATE-LOGICAL-PATHNAME (CONCATENATE 'STRING "cllib:" FILE))))
      (SETF (FDEFINITION SYMB)
              (LAMBDA (&REST ARGS)
                (SETF (DOCUMENTATION SYMB 'FUNCTION) NIL)
                (FMAKUNBOUND SYMB)
                (FORMAT T "; ~s is being autoloaded from `~a'~%" SYMB FILE)
                (REQUIRE FILE PATH)
                (APPLY (FDEFINITION SYMB) ARGS))
            (DOCUMENTATION SYMB 'FUNCTION)
              (FORMAT NIL "Autoloaded (from ~a)~@[:~%~a~]" FILE COMMENT)))))

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/base.lisp", 2,482 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/withtype.lisp", 2,163 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/string.lisp", 7,684 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/simple.lisp", 7,903 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/miscprint.lisp", 7,621 bytes
(export '(PRINT-ALL-ASCII))
(autoload 'PRINT-ALL-ASCII "miscprint" "Print all ASCII characters with their names and codes.")
(export '(PRINT-ALL-PACKAGES))
(autoload 'PRINT-ALL-PACKAGES "miscprint" "Print all packages.")
(export '(MAKE-HT-READTABLE))
(autoload 'MAKE-HT-READTABLE "miscprint" "Make a readtable which will be able to read hash-tables with #h().")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/symb.lisp", 2,757 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/tilsla.lisp", 5,733 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/log.lisp", 5,112 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/fileio.lisp", 18,407 bytes
(export '(FILE-SIZE))
(autoload 'FILE-SIZE "fileio" "Return the size of file named FN.")
(export '(DIR-SIZE))
(autoload 'DIR-SIZE "fileio" "Return the total size of all files in directory DIR.")
(export '(RENAME-FILES))
(autoload 'RENAME-FILES "fileio" "Rename file from wildcard FROM to wildcard TO.
file:/usr/doc/lisp/HyperSpec/Body/fun_translate-pathname.html")
(export '(COUNT-SEXPS))
(autoload 'COUNT-SEXPS "fileio" "Count the SEXPs in the form, quoted forms counting as 1.")
(export '(CODE-COMPLEXITY))
(autoload 'CODE-COMPLEXITY "fileio" "Count the sexps in the file.")
(export '(PR))
(autoload 'PR "fileio" "Print the OBJECT readably to the STREAM (default `*standard-output*').
Set `*print-pretty*' to the third argument NICE (default T).
Uses `with-standard-io-syntax'.")
(export '(WRITE-TO-FILE))
(autoload 'WRITE-TO-FILE "fileio" "Write the object to the file, readably.
The optional third argument is passed to `pr'.
Return the size of the file.")
(export '(READ-FROM-STREAM))
(autoload 'READ-FROM-STREAM "fileio" "Read from stream.
The REPEAT keyword argument tells how many objects to read.
 If NIL, read once and return the object read;
 if a number, read that many times and return a list of objects read,
 if T (default), read until end of file and return a list of objects read.")
(export '(READ-FROM-FILE))
(autoload 'READ-FROM-FILE "fileio" "Read an object or several objects from a file.
The READTABLE keyword argument (default `*readtable*') specifies
the readtable to use.
Passes REPEAT (default NIL) keyword argument to `read-from-stream'.")
(export '(LOAD-COMPILE-MAYBE))
(autoload 'LOAD-COMPILE-MAYBE "fileio" "Compile the file if newer than the compiled and load it.")
(export '(FILE-EQUAL-P))
(autoload 'FILE-EQUAL-P "fileio" "Check whether the two files are identical, like cmp(1).")
(export '(FILE-CMP))
(autoload 'FILE-CMP "fileio" "Find out how different the files are.
Returns the fraction of different 8-bit bytes.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/closio.lisp", 4,267 bytes
(export '(MACROEXPAND-R))
(autoload 'MACROEXPAND-R "closio" "Recursive macroexpand - unreliable because of `macrolet' &c.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/animals.lisp", 12,141 bytes
(export '(PLAY-ANIMALS))
(autoload 'PLAY-ANIMALS "animals" "Play the famous game!")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/autoload.lisp", 3,840 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/base64.lisp", 4,894 bytes
(export '(BASE64-ENCODE))
(autoload 'BASE64-ENCODE "base64" "Encode the vector of bytes as a string in base64.")
(export '(BASE64-DECODE))
(autoload 'BASE64-DECODE "base64" NIL)

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/list.lisp", 6,435 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/math.lisp", 70,592 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/sorted.lisp", 13,784 bytes
(export '(TOP-BOTTOM-UI))
(autoload 'TOP-BOTTOM-UI "sorted" "Call top-bottom and pretty print the results.
Returns them for possible further processing.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/date.lisp", 28,675 bytes
(export '(DATE))
(autoload 'DATE "date" "Convert to or extract a date.
The argument can be:
   - a date - it is returned untouched;
   - a string - it is destructively parsed;
   - a symbol - it is uninterned and its name is destructively parsed;
   - an integer - interpreted as the number of days since the epoch,
   - a real number - interpreted as the number of seconds since the epoch;
   - a stream - read as Month Day Year;
   - a structure - the appropriate slot is used;
   - a cons - called recursively on CAR;
   - NIL - an error is signalled.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/url.lisp", 46,740 bytes
(export '(URL))
(autoload 'URL "url" "Convert the object into URL.
The argument can be:
   - a URL - returned untouched;
   - a string - it is non-destructively parsed;
   - a symbol - it is uninterned and its name is non-destructively parsed;
   - a stream - read from.")
(export '(URL-TIME))
(autoload 'URL-TIME "url" "Get the time out of the date/time url.")
(export '(BROWSE-URL))
(autoload 'BROWSE-URL "url" "Run the browser (a keyword in `*browsers*' or a list) on the URL.")
(export '(DUMP-URL))
(autoload 'DUMP-URL "url" "Dump the URL line by line.
FMT is the printing format. 2 args are given: line number and the line
itself. FMT defaults to \"~3d: ~a~%\".
OUT is the output stream and defaults to `*standard-output*'.
This is mostly a debugging function, to be called interactively.")
(export '(URL-GET))
(autoload 'URL-GET "url" "Get the URL.
This is the function to be called in programs.
Arguments: URL - what to get, LOC - where to place it.
Keywords: `timeout', `max-retry', `out', `err'.")
(export '(WHOIS))
(autoload 'WHOIS "url" "Get the whois information on a host.")
(export '(FINGER))
(autoload 'FINGER "url" "Finger the mail address.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/card.lisp", 24,881 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/check.lisp", 4,445 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/xml.lisp", 42,816 bytes
(export '(XML-READ-FROM-FILE))
(autoload 'XML-READ-FROM-FILE "xml" "Read all XML objects from the file.")
(export '(XML-XHTML-TIDY))
(autoload 'XML-XHTML-TIDY "xml" "Tidy up the XHTML file.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/html.lisp", 11,732 bytes
(export '(DUMP-URL-TOKENS))
(autoload 'DUMP-URL-TOKENS "html" "Dump the URL token by token.
See `dump-url' about the optional parameters.
This is mostly a debugging function, to be called interactively.")
(export '(XML-READ-FROM-URL))
(autoload 'XML-READ-FROM-URL "html" "Read all XML objects from the stream.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/clhs.lisp", 22,981 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/csv.lisp", 5,620 bytes
(export '(CSV-READ-FILE))
(autoload 'CSV-READ-FILE "csv" "Read comma-separated values into a list of vectors.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/cvs.lisp", 17,364 bytes
(export '(CVS-DIFF2PATCH))
(autoload 'CVS-DIFF2PATCH "cvs" "Convert a CVS diff to a patchable diff.")
(export '(CVS-STAT-LOG))
(autoload 'CVS-STAT-LOG "cvs" "Generate and print some statistics of the CVS repository.
Careful - this will return a huge list!")
(export '(CVS-CHANGE-ROOT))
(autoload 'CVS-CHANGE-ROOT "cvs" "Change Root and Repository files in the CVS directories under ROOT.
When `DRY-RUN' is non-NIL, no actual changes are done.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/laser.lisp", 4,714 bytes
(export '(PRINT-FILES))
(autoload 'PRINT-FILES "laser" "Print the files to a printer: nil: print; t: nprint /q:ddslaser.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/datedl.lisp", 35,062 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/doall.lisp", 1,311 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/elisp.lisp", 14,236 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/fin.lisp", 11,543 bytes
(export '(MGG-COMPARE))
(autoload 'MGG-COMPARE "fin" "Compare several APRs by their present values.
If you have to decide between several mortgages of the same PRINCIPAL and TERM,
you should consider the present value at the 'no points/no closing cost' APR
of the points which you will have to pay for the lower rate.
Suppose you are have a choice of 15-year $100,000 mortgage at
   7.25  % with no points
   6.875 % with 1 point
   6.75  % with 3 points.
The following call:
 (mgg-compare 100000 15 0.0725 '((0.06875 . 1) (0.0675 . 3)))
will tell you that you should choose the 6.875%/1point mortgage because the
1 point you will pay for the lower rate will be justified after only 4.645
years, as opposed to 13.75 years for the 6.75%/3points mortgage.")
(export '(MGG-PREPAY))
(autoload 'MGG-PREPAY "fin" "Print the information about prepaying the loan.")
(export '(MGG-PAYOFF))
(autoload 'MGG-PAYOFF "fin" "Print the payoff time for when the payment is given.")
(export '(BLACK-SCHOLES-CALL))
(autoload 'BLACK-SCHOLES-CALL "fin" "Compute the Black-Scholes value of a call option (American or European).
Arguments are: option strike price, current stock price, risk-free
interest rate for the term, time to expiration, stock volatility.")
(export '(BLACK-SCHOLES-EPUT))
(autoload 'BLACK-SCHOLES-EPUT "fin" "Compute the Black-Scholes value of a European put option.
See `black-scholes-call' for details.")
(export '(SOLOW))
(autoload 'SOLOW "fin" "Show the economy evolution from the steady state with saving rate SAV
and Capital/Income ratio K-Y-0 to the Golden Rate steady state.")
(export '(LUHN))
(autoload 'LUHN "fin" "Check whether the card number CN is valid.
For a card with an even number of digits, double every odd numbered
digit and subtract 9 if the product is greater than 9. Add up all the
even digits as well as the doubled-odd digits, and the result must be a
multiple of 10 or it's not a valid card. If the card has an odd number
of digits, perform the same addition doubling the even numbered digits
instead.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/geo.lisp", 17,875 bytes
(export '(CITE-INFO))
(autoload 'CITE-INFO "geo" "Get the cite info from the U.S. Gazetteer.
Print the results to the stream OUT (defaults to T, discard if NIL)
and return a list of geo-data.")
(export '(WEATHER-REPORT))
(autoload 'WEATHER-REPORT "geo" "Print the weather report for CODE to OUT.")
(export '(FIND-COUNTRY))
(autoload 'FIND-COUNTRY "geo" "Get the COUNTRY struct corresponding to the given SLOT VALUE.
Returns the list of all countries satisfying the condition.
Looks in list LS, which defaults to `*country-list*'.  If slot value
is a float, such as the GDP, VALUE is a cons with the range.
  (find-country SLOT VALUE &optional LS TEST)")
(export '(FETCH-COUNTRY-LIST))
(autoload 'FETCH-COUNTRY-LIST "geo" "Initialize `*country-list*' from `*geo-code-url*'.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/getopt.lisp", 2,743 bytes
(export '(GETOPT))
(autoload 'GETOPT "getopt" "Parse the command line arguments.
`arg-alist' and `opt-alist' are alists of (option-name . argument-type),
`argument-type' should be `:char', `:number', or `:boolean',
`arg-list' is a list of options, like (\"-opt1\" \"val1\" ...)
Returns two values -
   a list of non-option arguments,
   and a plist of keyword args;
 or T if there was an error.
When `allow-less' (`allow-more') is non-NIL (default),
allow missing (additional) non-option arguments.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/matrix.lisp", 21,461 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/stat.lisp", 5,376 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/gnuplot.lisp", 27,855 bytes
(export '(PLOT-DATED-LISTS))
(autoload 'PLOT-DATED-LISTS "gnuplot" "Plot the dated lists from BEGD to ENDD.
Most of the keys are the gnuplot options (see `with-plot-stream' for details.)
REL means plot everything relative to the first value.
EMA is the list of parameters for Exponential Moving Averages.")
(export '(PLOT-LISTS))
(autoload 'PLOT-LISTS "gnuplot" "Plot the given lists of numbers.
Most of the keys are the gnuplot options (see `with-plot-stream' for details.)
LSS is a list of lists, car of each list is the title, cdr is the numbers.")
(export '(PLOT-LISTS-ARG))
(autoload 'PLOT-LISTS-ARG "gnuplot" "Plot the given lists of numbers.
Most of the keys are the gnuplot options (see `with-plot-stream' for details.)
LSS is a list of lists, car of each list is the title, cdr is the list
of conses of abscissas and ordinates. KEY is used to extract the cons.")
(export '(PLOT-ERROR-BARS))
(autoload 'PLOT-ERROR-BARS "gnuplot" "Plot the list with errorbars.
Most of the keys are the gnuplot options (see `with-plot-stream' for details.)
The first element is the title, all other are records from which we
get x, y and ydelta with xkey, ykey and ydkey.")
(export '(PLOT-FUNCTIONS))
(autoload 'PLOT-FUNCTIONS "gnuplot" "Plot the functions from XMIN to XMAX with NUMPTS+1 points.
Most of the keys are the gnuplot options (see `with-plot-stream' for details.)
FNL is a list of (name . function).
E.g.:
  (plot-functions (list (cons 'sine #'sin) (cons 'cosine #'cos)) 0 pi 100
                  :legend '(:bot :left :box) :grid t :plot :wait)")
(export '(PLOT-DATED-LISTS-DEPTH))
(autoload 'PLOT-DATED-LISTS-DEPTH "gnuplot" "Plot the dated lists, DEPTH *days* from the beginning.
OPTS is passed to `plot-lists-arg'.")
(export '(PLOT-HISTOGRAM))
(autoload 'PLOT-HISTOGRAM "gnuplot" "Plot the data in the list as a histogram.
When :MEAN is non-NIL (default), show mean and mean+-standard deviation
 with vertical lines.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/gq.lisp", 31,210 bytes
(export '(GET-URL-XML-TOKENS))
(autoload 'GET-URL-XML-TOKENS "gq" "Dump the URL token by token.
This is just a debugging function, to be called interactively.")
(export '(UPDATE-QUOTES))
(autoload 'UPDATE-QUOTES "gq" "Read the history. Update quotes. Plot (optionally),
if PLOT is non-nil, or if it is not given but there was new data.
If PLOT is T, just plot, do not try to update quotes.
See `*get-quote-url-list*' for available SERVERs.
If DEBUG is non-nil, do not bind `*print-log*' and `*gq-error-stream*'.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/h2lisp.lisp", 12,605 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/htmlgen.lisp", 7,808 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/inspect.lisp", 24,304 bytes
(export '(INSPECT-CLLIB))
(autoload 'INSPECT-CLLIB "inspect" "This function implements the ANSI Common Lisp INSPECT function.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/iter.lisp", 9,970 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/munkres.lisp", 5,314 bytes
(export '(ASSIGNMENT))
(autoload 'ASSIGNMENT "munkres" "Solve the assignment problem using Munkres' algorithm
(AKA Hungarian Algorithm, AKA Bipartite Minimum-Weight Matching).
Returns the total cost and two assignment vectors: X->Y and Y->X.")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/prompt.lisp", 2,410 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/rng.lisp", 67,015 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/rpm.lisp", 33,071 bytes
(export '(SHOW-RPMS))
(autoload 'SHOW-RPMS "rpm" "Print some RPMs.")
(export '(RPM-GET-LIST))
(autoload 'RPM-GET-LIST "rpm" "Get the list of RPMS from URL.")
(export '(RPM-GET-NEW-RPMS))
(autoload 'RPM-GET-NEW-RPMS "rpm" "Download the RPMs from `*rpm-locations*'.")
(export '(RPM-LIST-RPM))
(autoload 'RPM-LIST-RPM "rpm" "Look for the RPM on all sites.
If `local' keyword argument is non-nil, use only the data already
available in `*rpm-locations*'.")
(export '(RPM-CLEAN-UP))
(autoload 'RPM-CLEAN-UP "rpm" "Remove old RPM files.
This will remove the RPM files for which there is a newer version,
as well as corrupt RPM files.
If you are using both up2date and apt-get, you should pass the up2date
cache directory before the apt-get one:
  (rpm-clean-up :dirs '(\"/var/spool/up2date/\" \"/var/cache/apt/archives/\"))
Then the up2date files will be kept even when there is a newer apt-get
package, because we can assume that apt-get collects packages from less
trustworthy sites than up2date (which uses only redhat.com).")

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/server.lisp", 6,557 bytes

;;; file #P"/home/vb/.sbcl/site/clocc/src/cllib/tests.lisp", 15,081 bytes

