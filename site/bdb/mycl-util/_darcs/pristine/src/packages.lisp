;; This software is Copyright (c) 2006, Steffen Siering.
;; <COMPANY> grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; * Package definition

(defpackage :mycl-util
  (:use :common-lisp)
  (:export #:fn  ;;fun
	   #:compose
	   #:fif
	   #:fwhen
	   #:funless
	   #:fchain
	   #:fint
	   #:funion
	   #:lrec
	   #:curry
	   #:rcurry
	   #:rlambda
	   
	   ;;io
           #:readlist
	   #:prompt
	   #:break-loop
	   #:interactive-interpreter
	   #:prompt-generator
	   
	   ;;lazy
	   #:delay
	   #:force
	   
	   ;;list
	   #:last1
	   #:single
	   #:append1
	   #:nconc1
	   #:mklist
	   #:longer
	   #:filter
	   #:group
	   #:find2
	   #:before
	   #:after
	   #:duplicate
	   #:split-if
	   #:most
	   #:best
	   #:mostn
	   #:map->
	   #:mapa-b
	   #:map1-n
	   #:map0-n
	   #:mappend
	   #:mapcars
	   #:rmapcar
	   #:cross-lists
	   #:combine-all
	   #:starts-with
	   #:permutations
	   #:partition
	   #:prepend
	   #:reuse-cons
	   #:find-all
	   #:find-all-if
	   #:self-cons

	   ;;tree
	   #:find-anywhere
	   #:find-anywhere-if
	   #:tree-walk
	   #:flatten
	   #:prune
	   #:tree-search
	   #:tree-depth-first-search
	   #:tree-breadth-first-search
	   #:tree-best-first-search
	   #:tree-beam-search
	   #:tree-iter-wide-search

	   ;;memo
	   #:tmp-memoize
	   #:clear-memoize
	   #:memoize

	   ;;misc
	   #:const
	   #:const-values
	   #:mkstr
	   #:symb
	   #:anon-symb
	   #:reread
	   #:one-of
	   #:random-elt
	   #:build-comparator
	   #:comp-=
	   #:comp-<
	   #:comp->
	   #:comp-<=
	   #:comp->=
	   #:with-comparator

	   ;;;;macros
	   ;;anaphorics
	   #:if-bind
	   #:aif
	   #:when-bind
	   #:awhen

	   ;;loop
	   #:while
	   #:till
	   #:forever

	   ;;meta
	   #:with-gensyms
	   #:with-unique-names
	   #:once-only

	   ;;misc
	   #:abbrev
	   #:abbrevs
	   #:in
	   #:inq
	   #:condlet
	   #:def-construct

	   ;;setf
	   #:allf
	   #:nilf
	   #:tf
	   #:togglef
	   #:toggle!
	   #:concf
	   #:_f
	   #:sortf

	   ;;;struct
	   ;;pipe
	   #:make-pipe
	   #:head
	   #:tail
	   #:empty-pipe
	   #:pipe-elt
	   #:pipe-null-p
	   #:pipe->list
	   #:list->pipe
	   #:pipe-map
	   #:pipe-mapc
	   #:pipe-mappend
	   #:pipe-filter
	   #:pipe-reduce
	   #:pipe-append
	   #:pipe-permutations
	   #:integers

	   ;;misc
	   #:make-queue
	   #:queue-contents
	   #:enqueue
	   #:dequeue
	   #:front
	   #:empty-queue-p
	   #:queue-nconc
	   #:make-trie
	   #:put-trie
	   #:get-trie
	   #:delete-trie
	   #:find-trie
	   #:stack-machine
	   #:state
	   #:current-state
	   #:push-state
	   #:pop-state
	   #:upd-current-state
	   #:upd-by-n-states

	   ;;bin-tree
	   #:create-node
	   #:search-tree
	   #:search-node
	   #:insert-tree
	   #:node-insert
	   #:empty-p
	   #:clear-tree
	   #:delete-tree
	   #:delete-from-node
	   #:tree-root
	   #:binary-tree
	   #:binary-tree-node
	   #:avl-tree-node
	   #:left
	   #:right
	   #:make-binary-tree
	   #:make-avl-tree

	   ;;heap
	   #:make-heap
	   #:heap-insert
	   #:heap-empty-p
	   #:heap-front
	   #:heap-dequeue
	   #:is-in-heap

	   ;;pools
	   #:make-pool
	   #:pool-count
	   #:pool-store
	   #:pool-add-strategie
	   #:pool-next-strategie
	   #:empty-pool-p
	   #:pool-add
	   #:pool-next
	   #:pool-make-max-sized
	   #:make-max-resource-pool
	   #:make-priority-pool
	   #:make-max-sized-priority-pool
	   ))
