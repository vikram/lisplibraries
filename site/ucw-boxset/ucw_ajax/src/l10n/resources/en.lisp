(in-package :lang)

(defresources en
  (your-session-has-expired "Your session has expired or has been deleted on the server"))

(define-js-resources en
  (confirm-pending-changes
   #.(format nil "You have pending changes.~%Are you sure you want to abandon them?"))

  (unknown-error-while-processing-server-answer
   "Error while processing server answer. Try to reload the page and if the error persists contact the system administrator!")

  (unknown-server-error "Unknown server error")
  (network-error "There was an error while communicating with the server, try again.")
  
  (progress.tooltip "Click to remove")
  (progress-label.default "Loading...")
  (progress-label.closing-tab "Closing tab...")
  (progress-label.loading-tab "Loading tab..."))
