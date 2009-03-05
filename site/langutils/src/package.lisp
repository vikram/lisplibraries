;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;; Language utilities asd files



(defpackage #:langutils
  (:use #:cl #:meta #:utils #:s-serialization)
  (:export   ;; initialization
           init-langutils
	   clean-langutils
	   reset-langutils
	   ;; vector documents
	   vector-document
	   make-vector-document
	   document-text
	   document-tags
	   document-annotations
	   vector-document-words
	   get-token-id
	   get-tag
	   string-tag
	   length-of
	   string-tag-tokenized
	   print-vector-document
	   write-vector-document
	   read-vector-document
	   read-vector-document-to-string
	   ;; words in documents
	   word
	   word-tag
	   word-name
	   ;; phrases in documents
	   make-phrase
	   make-phrase-from-sentence
	   make-phrase-from-vdoc
	   phrase
	   phrase-start
	   phrase-end
	   phrase-type
	   phrase-document
	   phrase-length
	   phrase-equal
	   phrase-overlap
	   print-phrase
	   print-window
	   phrase-words
	   phrase-distance
	   phrase-lemmas
	   print-phrase-lemmas
	   find-phrase
	   change-word
	   remove-word
	   add-word
	   lemmatize-phrase
	   get-annotation
	   set-annotation
	   unset-annotation
	   ;; altered phrases
	   altered-phrase
	   make-alterable-phrase
           ;; tokens
	   id-for-token
	   token-for-id
	   tokens-for-ids
	   save-tokens
	   suspicious-word?
	   suspicious-string?
	   ;; lexicon
	   get-lexicon-default-pos
	   get-lexicon-entry
	   get-lexicon-case-forms
	   lexicon-entry
	   lexicon-entry-tag
	   lexicon-entry-tags
	   lexicon-entry-id
	   lexicon-entry-roots
	   lexicon-entry-cases
	   add-lexicon-entry
	   add-lemma
	   ;; lemma
	   get-lemma
	   get-lemma-for-id
	   morph-surface-forms
	   morph-case-surface-forms
	   morph-surface-forms-text
	   ;; tokenizer
	   tokenize-stream
	   tokenize-string
	   tokenize-file
	   ;; text tagger
	   tag 
	   tag-tokenized 
	   ;; vector tagger
	   vector-tag 
	   vector-tag-tokenized 
	   initial-tag 
	   ;; chunker
	   chunk
	   chunk-tokenized
	   all-chunks
	   get-event-chunks
	   get-noun-chunks
	   get-verb-chunks
	   get-adverb-chunks
	   all-vx+nx-phrases
	   ;; stopwords
	   stopword?
	   contains-is?
	   string-stopword?
	   string-contains-is?
	   concise-stopword?
	   string-concise-stopword?
	   ))

(in-package #:langutils)


