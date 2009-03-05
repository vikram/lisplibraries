(in-package :araneida)

(defun copy-stream (from to)
  "Copy into TO from FROM until end of file, without translating or otherwise mauling anything"
  (let ((buf (make-array 4096 :element-type (stream-element-type from)
                         :initial-element #\Space)))
    (do ((pos (read-sequence buf from)  (read-sequence buf from)))
        ((= 0 pos) nil)
      (write-sequence buf to :end pos))))
