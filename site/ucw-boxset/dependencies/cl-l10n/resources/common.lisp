
(defun with-indefinit-article (str &key capitalize-first-letter)
  (let ((article (indefinit-article-for str)))
    (strcat (if capitalize-first-letter
                (capitalize-first-letter article)
                article)
            #\Space str)))

(export 'with-indefinit-article)

