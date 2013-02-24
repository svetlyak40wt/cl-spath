
(require 'heresy)

(defpackage
  :spath
  (:documentation
    "SPath is a system for running search queries against s-expressions.
    It is inspired by http://www.defmacro.org/ramblings/s-query.html and loosely by XPath;
    not that it has the full expressive power of XPath, but some of
    the stylistic choices and ideas were taken from there.

    In particular, SPath finds all instances of a match in the same
    sort of way as XPath.

    I, Robin Lee Powell, explicitely place SPath (defined as
    everything found at
    http://digitalkingdom.org/~rlpowell/hobbies/software/spath/) in the public domain.
    ")
    (:use :cl :heresy)
    (:export lazy-expand spath)
    )

(in-package :spath)

; Given a list, return all sublists and all sublists of any lists
; inside the list, and any lists inside those, and so on, as lazily
; as possible.
(defun lazy-expand (llist)
  ;(format t "~%lazy-expand llist: ~A~%" llist)
  (filter/
    #'(lambda (x) (non-null/ x))
    (concat/
      (map/
	#'(lambda (x)
	    ;(format t "~%lazy-expand x: ~S~%" x)
	    ;(format t "~%lazy-expand car x: ~S~%" (car/ x))
	    ;(format t "~%lazy-expand atom car x: ~S~%" (atom (car/ x)))
	    ;(format t "~%lazy-expand list car x: ~S~%" (listp (car/ x)))
	    ;(format t ".")
	    (cond
	      ((atom (car/ x))
	       (list/ x))
	      ((listp (car/ x))
	       (lazy-expand (car/ x)))
	      ;(concat/ (list/ x (lazy-expand (list*/ (car/ x))))))
	      (t
		(append/ x (lazy-expand (car/ x))))))
	(tails/ llist)))))

(defmacro sub-search (expand-fun test from-num llists args)
  ; This looks a bit more complex than it needs to be;
  ; that's because it is.  The complexity here is
  ; required to stay lazy: if the user only wants the
  ; first item, we want to do the absolute minimum of
  ; work possible to give it to them.
  ;
  ; Therefore, we expand each of the lists we're given
  ; (that's the outer map/) and filter down to what we're
  ; digging for, then we *immediately* call search-fun on
  ; that one individual result, again using map/.
  `(progn
     ;(format t "In sub-search : ~S, ~S, ~S, ~S, ~S~%" #',expand-fun ,test ,from-num ,args ,llists)
     (concat/
       (map/
	 #'(lambda (sub-list)
	     (map/
	       #'(lambda (x)
		   (cond
		     ((and (non-null/ x) (non-null/ (subseq ,args ,from-num)))
		      ;(format t "running search-fun on : ~A~%" x)
		      ;(format t "running search-fun on : ~A~%" (non-null/ x))
		      ;(format t "running search-fun on : ~A~%" (describe x))

		      ; search-fun expects a lists of lists
		      ; and returns the same, but we're only
		      ; passing one list, and we only want
		      ; one back.
		      (first/ (search-fun (list/ x) (subseq ,args ,from-num))))
		     (t x)))
	       (filter/
		 #'(lambda (x)
		     ;(format t "~%sub-bit ~S, ~S, ~S~%" x (funcall ,test (car/ x)) ,args)
		     (funcall ,test (car/ x)))
		 (,expand-fun sub-list))))
	 ,llists))))

(defun search-fun (llists args)
  ;(format t "lists: ~A~%" llists)
  ;(format t "args: ~S~%" args)
  ; Here we deal with the recursive base cases.
  (cond
    ((null llists)
     ;; (format t "Returning nil.~%")
     nil)
    ((null args)
     ;; (format t "Returning lists ~S.~%" llists)
     llists)
    (t
      ; We don't want to return empty lists; not useful.
      (filter/
	#'(lambda (x) (non-null/ x))
	(let ((command (first args)))
	  ;; (format t "command: ~S~%" command)
	  ;; (describe command)

	  ; Here we deal with the actual commands themselves
	  (cond
	    ; Command is a general function that we use to test over
	    ; the elements
	    ((functionp command)
	     (sub-search tails/ command 1 llists args))
	    ; Command such as "find 'a"; we just rewrite this in
	    ; terms of a function type command
	    ((eq command :find)
	     ;(format t "finding ~S~%" (second args))
	     ;(format t "finding llists ~S~%" llists)
	     ;(format t "finding expand ~S~%" (tails/ llists))
	     ;(format t "finding new args ~S~%" (list* #'(lambda (x) (equal x (second args))) (cddr args)))
	     (search-fun
	       llists
	       (list* #'(lambda (x) (equal x (second args))) (cddr args))))
	    ; Command such as "dig 'a"
	    ((eq command :dig)
	     ;(format t "digging ~S~%" (second args))
	     ;(format t "digging llists ~S~%" llists)
	     ;(format t "digging expand ~S~%" (concat/ (map/ #'lazy-expand llists)))
	     (sub-search lazy-expand #'(lambda (x) (equal x (second args))) 2 llists args))
	    ; Strip off the 2 arguments we used
	    ; The "next" command
	    ((eq command :next)
	     (search-fun
	       ; Just drop the first element of all lists
	       (map/
		 #'cdr/
		 llists)
	       (rest args)))
	    ; The "item" command
	    ((eq command :item)
	     (search-fun
	       ; Just take the first element of all lists
	       (map/
		 ; example: (spath :list '( (e (b c) e f (g h (i j))) ) find 'e next item item)
		 #'(lambda (x)
		     (cond
		       ((listp/ x)
			(car/ x))
		       (nil nil)))
		 llists)
	       (rest args)))
	    ; Command did not match.
	    (t (error "Command ~A not supported.~%" (first args)))))))))

; Turns a list of symbols in a macro into same-named keywords.  I
; have to wonder how many times *this* wheel has been re-invented.
(defun sym-to-key (syms)
  ;(format t "sym-to-key : ~A~%" syms)
  (mapcar
    #'(lambda
	(x)
	;(describe x)
	(cond
	  ((symbolp x)
	   ;(format t "s-n: ~S~%" (symbol-name x))
	   ;(format t "s-n: ~S~%" (find-symbol (symbol-name x)))
	   (intern (symbol-name x) :keyword))
	  (t x)))
    syms))

; Helper function to safely make something not a lazy list.
(defun de-lazy (llist)
  (if (listp/ llist)
    (to-list llist)
    (identity llist)))

(defun spath-fun (ret-type lists args)
  ;(format t "In spath-fun : ~A, ~A, ~A~%" ret-type lists args)
  ;(format t "In spath-fun desc ret : ~A~%" (describe ret-type))
  (cond
    ((functionp ret-type)
     (funcall
       ret-type
       (search-fun
	 lists
	 args)))
    ; The simplest case: no extra processing.
    ((eq ret-type :none)
     (spath-fun #'identity lists args))
    ; Grab only the first item returned, and return it as a
    ; regular list
    ((eq ret-type :first)
     (spath-fun (lambda (x) (de-lazy (first/ x))) lists args))
    ; Return everything as a list of lists
    ((or (eq ret-type :list) (eq ret-type :lists))
     (spath-fun (lambda (x) (to-list (map/ #'de-lazy x))) lists args))))

(defmacro spath (ret-type lists &body args)
  (let ((new-args (sym-to-key args)))
    ;(format t "new-args: ~S~%" new-args)
    ;(format t "ret-type: ~S~%" ret-type)
    `(spath-fun ,ret-type ,lists (list ,@new-args))))
