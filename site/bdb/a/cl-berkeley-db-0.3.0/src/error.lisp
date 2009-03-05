;;;Copyright (c) 2006-2007, Arthur Smyles
;;;All rights reserved.
;;;
;;;Redistribution and use in source and binary forms, with or without
;;;modification, are permitted provided that the following conditions are met:
;;;
;;;1. Redistributions of source code must retain the above copyright notice,
;;;   this list of conditions and the following disclaimer.
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;         documentation and/or other materials provided with the distribution.
;;;
;;;         THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;         AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;         IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;         ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;;         LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;         CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;         SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;         INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;         CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;         ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;         POSSIBILITY OF SUCH DAMAGE.
;;;
(in-package #:cl-berkeley-db.common-lisp.net)

(define-condition db-condition () ())


(define-condition db-error (error db-condition)
  ((code :reader db-error-code :initarg :code))
  (:report (lambda (bdb-error stream)
             (with-slots (code) bdb-error
               (format stream "error-code: ~A~%error-str: ~A"
                       code (db-error-message bdb-error))))))

(defmacro define-db-error (name code &optional doc)
  `(define-condition ,name (db-error)
     ((code :initform ,code))
     ,@(when doc `((:documentation ,doc)))))

(define-db-error buffer-small DB_BUFFER_SMALL         "User memory too small for return.")
(define-db-error do-not-index DB_DONOTINDEX           "\"Null\" return from 2ndary callbk.")
(define-db-error key-empty DB_KEYEMPTY             "Key/data deleted or never created.")
(define-db-error key-exist DB_KEYEXIST             "The key/data pair already exists.")
(define-db-error lock-deadlock DB_LOCK_DEADLOCK        "Deadlock.")
(define-db-error lock-not-granted DB_LOCK_NOTGRANTED      "Lock unavailable.")
(define-db-error log-buffer-full DB_LOG_BUFFER_FULL      "In-memory log buffer full.")
(define-db-error no-server DB_NOSERVER             "Server panic return.")
(define-db-error no-server-home DB_NOSERVER_HOME        "Bad home sent to server.")
(define-db-error no-server-id DB_NOSERVER_ID          "Bad ID sent to server.")
(define-db-error not-found DB_NOTFOUND             "Key/data pair not found (EOF).")
(define-db-error old-version DB_OLD_VERSION          "Out-of-date version.")
(define-db-error page-not-found DB_PAGE_NOTFOUND        "Requested page not found.")
(define-db-error rep-dup-master DB_REP_DUPMASTER        "There are two masters.")
(define-db-error rep-handle-dead DB_REP_HANDLE_DEAD      "Rolled back a commit.")
(define-db-error rep-hold-election DB_REP_HOLDELECTION     "Time to hold an election.")
(define-db-error rep-ignore DB_REP_IGNORE           "This msg should be ignored.")
(define-db-error rep-isperm DB_REP_ISPERM           "Cached not written perm written.")
(define-db-error rep-join-failure DB_REP_JOIN_FAILURE     "Unable to join replication group.")
(define-db-error rep-lockout DB_REP_LOCKOUT          "API/Replication lockout now.")
;TODO this conflicts with the event with same name. Figure out an elegant way to handle it
;(define-db-error rep-new-master DB_REP_NEWMASTER        "We have learned of a new master.")
(define-db-error rep-new-site DB_REP_NEWSITE          "New site entered system.")
(define-db-error rep-not-perm DB_REP_NOTPERM          "Permanent log record not written.")
(define-db-error rep-unavailable DB_REP_UNAVAIL          "Site cannot currently be reached.")
(define-db-error run-recovery DB_RUNRECOVERY          "Panic return.")
(define-db-error secondary-bad DB_SECONDARY_BAD        "Secondary index corrupt.")
(define-db-error verify-bad DB_VERIFY_BAD           "Verify failed; bad format.")
(define-db-error version-mismatch DB_VERSION_MISMATCH "Environment version mismatch.")

(defun db-error-type (code)
  (cond
    ((eql code DB_BUFFER_SMALL) 'buffer-small)
    ((eql code DB_DONOTINDEX) 'do-not-index)
    ((eql code DB_KEYEMPTY) 'key-empty)
    ((eql code DB_KEYEXIST) 'key-exist)
    ((eql code DB_LOCK_DEADLOCK) 'lock-deadlock)
    ((eql code DB_LOCK_NOTGRANTED) 'lock-not-granted)
    ((eql code DB_LOG_BUFFER_FULL) 'log-buffer-full)
    ((eql code DB_NOSERVER) 'no-server)
    ((eql code DB_NOSERVER_HOME) 'no-server-home)
    ((eql code DB_NOSERVER_ID) 'no-server-id)
    ((eql code DB_NOTFOUND) 'not-found)
    ((eql code DB_OLD_VERSION) 'old-version)
    ((eql code DB_PAGE_NOTFOUND) 'page-not-found)
    ((eql code DB_REP_DUPMASTER) 'rep-dup-master)
    ((eql code DB_REP_HANDLE_DEAD) 'rep-handle-dead)
    ((eql code DB_REP_HOLDELECTION) 'rep-hold-election)
    ((eql code DB_REP_IGNORE) 'rep-ignore)
    ((eql code DB_REP_ISPERM) 'rep-isperm)
    ((eql code DB_REP_JOIN_FAILURE) 'rep-join-failure)
    ((eql code DB_REP_LOCKOUT) 'rep-lockout)
    ((eql code DB_REP_NEWMASTER) 'rep-new-master)
    ((eql code DB_REP_NEWSITE) 'rep-new-site)
    ((eql code DB_REP_NOTPERM) 'rep-not-perm)
    ((eql code DB_REP_UNAVAIL) 'rep-unavailable)
    ((eql code DB_RUNRECOVERY) 'run-recovery)
    ((eql code DB_SECONDARY_BAD) 'secondary-bad)
    ((eql code DB_VERIFY_BAD) 'verify-bad)
    ((eql code DB_VERSION_MISMATCH) 'version-mismatch)
    (t 'db-error)))

(defun db-error-message (err)
    (db_strerror (db-error-code err)))

(defmacro handle-db-error (&body body)
  `(let ((result (progn ,@body)))
    (or (zerop result) (error (db-error-type result) :code result))))

