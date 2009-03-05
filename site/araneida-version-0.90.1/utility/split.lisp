(in-package :araneida)

(defun split (string &optional max (ws '(#\Space #\Tab)))
  "Split `string' along whitespace as defined by the sequence `ws'.
Whitespace which causes a split is elided from the result.  The whole
string will be split, unless `max' is provided, in which case the
string will be split into `max' tokens at most, the last one
containing the whole rest of the given `string', if any."
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
        (when (and max (>= words (1- max)))
          (return (cons (subseq string start) list)))
        (setf end (position-if #'is-ws string :start start))
        (push (subseq string start end) list)
        (incf words)
        (unless end (return list))
        (setf start (1+ end)))))))

;;; this and that may have different behaviour on strings with
;;; repeated whitespace -- e.g. "foo  bar"

(defun split-quoted (str &optional max (ws '(#\Space #\Tab)))
  "Split `string' along whitespace as defined by the sequence `ws',
but ignoring whitespace in quoted strings.  Whitespace which causes a
split is elided from the result.  The whole string will be split,
unless `max' is a non-negative integer, in which case the string will
be split into `max' tokens at most, the last one containing the whole
rest of the given `string', if any."
  (do ((i 0 (1+ i))
       (words '())
       (split-allowed-p t)
       (word '()))
      ((>= i (length str))
       (reverse (cons (coerce (reverse word) 'string) words)))
    (if (eql (elt str i) #\")
        (setf split-allowed-p (not split-allowed-p)))
    (if (eql (elt str i) #\\)
        (setf i (1+ i)))                ;advance past escape chars
    (if (and split-allowed-p
             (or (not max) (< (length words) (1- max)))
             (member (elt str i) ws))
        (progn
          (setf words (cons (coerce (reverse word) 'string) words))
          (setf word '()))
      (setf word (cons (elt str i) word)))))

#|
(defun join (delim args)
  "Join the list ARGS together as a string, with elements separated by DELIMITER"
  (labels ((join1 (so-far delim args)
                  (if args 
                      (join1 (concatenate 'string so-far delim
                                          (princ-to-string (car args)))
                             delim (cdr args))
                    so-far)))
    (join1 (car args) delim (cdr args))))
(join "," '("uyt" "kjlhlk"))
|#