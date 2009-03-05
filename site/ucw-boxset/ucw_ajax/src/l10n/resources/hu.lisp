(in-package :lang)

(define-js-resources hu
  (confirm-pending-changes
   #.(format nil "Az oldalon kimentetlen adatok vannak amelyek elveszhetnek.~%Folytassuk a műveletet?"))

  (unknown-error-while-processing-server-answer
   "Hiba történt a szerver válasz feldolgozása közben. Próbálja meg frissíteni az oldalt, és ha a hiba ezután is fennáll, akkor lépjen kapcsolatba a karbantartókkal!")

  (unknown-server-error "Ismeretlen eredetű szerver hiba")
  (network-error "Hálózati hiba történt, probálkozzon újra, esetleg kicsit később.")
  
  (progress.tooltip "Kattints az eltávolításhoz")
  (progress-label.default "Töltés...")
  (progress-label.closing-tab "Tab bezárása...")
  (progress-label.loading-tab "Tab letöltése..."))
