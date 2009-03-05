;;; -*- encoding: utf-8 -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(provide 'cl-syntax-sugar)

;; to use from your init.el:
;; (add-to-list 'load-path (expand-file-name "~/workspace/syntax-sugar/emacs/"))
;; (require 'cl-syntax-sugar)

;; some interesting unicode characters:
;; ← ↑ → ↓ ║ = ≠ ≡ ≢ < > ≤ ≥ ∧ ∨ ¬ ∅ … ‼ ′ ″ ∀ ∃ ∈ √ ² ³
;;
;; (loop for i upfrom #X03B1 repeat 25 collect (string i))
;; α β γ δ ε ζ η θ ι κ λ μ ν ξ ο π ρ ς σ τ υ φ χ ψ ω
;;
;; (loop for i upfrom #X0391 repeat 25 collect (string i))
;; Α Β Γ Δ Ε Ζ Η Θ Ι Κ Λ Μ Ν Ξ Ο Π Ρ ΢ Σ Τ Υ Φ Χ Ψ Ω

(defgroup cl-syntax-sugar-faces nil
  "Faces installed by cl-syntax-sugar."
  :prefix "cl-syntax-sugar-"
  :group 'applications)

(defface cl-syntax-sugar-lambda-face
   '((((class color) (background light)) (:foreground "Purple3" :weight bold)))
  "Face for the lambda character."
  :group 'cl-syntax-sugar-faces)

(defface cl-syntax-sugar-greek-face
   '((((class color) (background light)) (:foreground "Purple3" :weight normal)))
  "Face for greek characters."
  :group 'cl-syntax-sugar-faces)

(defun cl-syntax-sugar-greek-letter? (char)
  (or (and (<= #X03B1 char)
           (< char (+ #X03B1 25)))
      (and (<= #X0391 char)
           (< char (+ #X0391 25)))))

(defun cl-syntax-sugar-install-substitute-pattern (pattern replacement)
  (setf replacement (cond
                      ((characterp replacement)
                       (string replacement))
                      ((stringp replacement)
                       replacement)
                      (t (error "Don't know how to treat cl-syntax-sugar replacement %s" replacement))))
  (font-lock-add-keywords
   nil `((,pattern (0 (progn
                        ,(when (and (= 1 (length replacement))
                                    (cl-syntax-sugar-greek-letter? (elt replacement 0)))
                           `(add-text-properties (match-beginning 1) (match-end 1)
                                                 `(face (cl-syntax-sugar-greek-face
                                                         ,@',(when (eql (elt replacement 0) ?λ)
                                                               '(cl-syntax-sugar-lambda-face))))))
                        (compose-region (match-beginning 1) (match-end 1)
                                        ,replacement)
			nil))))))

(defun cl-syntax-sugar-install-greek-letter-rules ()
  "Installs some rules that display the written greek letter names using the single character of the greek letter."
  (let ((char-code #X03B1))
    (dolist (name '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta"
                    "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron"
                    "pi" "rho" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega"))
      (when (> (length name) 3)
        (cl-syntax-sugar-install-substitute-pattern
         (concatenate 'string "[ 	\n()]\\(" name "\\)[ 	\n()]")
         (elt (string char-code) 0)))
      (incf char-code))))

(defun cl-syntax-sugar-install-square-bracket-lambda-rule ()
  "Installs some rules that displays [... !1 ...] as λ[... !1 ...] with the brackets colored."
  (font-lock-add-keywords
   nil `(("\\(\\[\\).*?!.*?\\(\\]\\)"
          (0 (progn
               (add-text-properties (match-beginning 1) (match-end 1) '(display "λ["))
               nil))
          (1 'cl-syntax-sugar-lambda-face)
          (2 'cl-syntax-sugar-lambda-face)))))

(add-hook 'lisp-mode-hook 'cl-syntax-sugar-install-greek-letter-rules)
(add-hook 'lisp-mode-hook 'cl-syntax-sugar-install-square-bracket-lambda-rule)
