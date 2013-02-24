(defpackage :spath-tests
  (:use :cl :spath :fiveam :heresy))

(in-package :spath-tests)

(def-suite spath-tests :description "Tests For SPath")

(in-suite spath-tests)

(test find-test-1
      "Very basic find test."
      (is
	(equal
	  '(
	    (b 3)
	    (b a q (c 1))
	    )
	  (spath :lists '(
		   (a (b 1) (c 1))
		   (b 3)
		   (b a q (c 1))
		   )
		 find 'b))))

(test find-test-2
      "Multiple returns find test."
      (is
	(equal
	  '(
	    (b 3 c 1 b 5)
	    (b 5)
	    (b 4)
	    )
	  (spath :lists '(
		   (b 3 c 1 b 5)
		   (b 4)
		   )
		 find 'b))))

(test find-test-missing
      "Finding things that aren't there."
      (is
	(equal
	  nil
	  (spath :lists '(
		   (a (x 1) (c 1))
		   (x 3)
		   (x a q (c 1))
		   )
		 find 'b))))

(test find-next-test
      "Find + next."
      (is
	(equal
	  '(
	    (3)
	    (a q (c 1))
	    )
	  (spath :lists '(
		   (a (x 1) (c 1))
		   (b 3)
		   (b a q (c 1))
		   )
		 find 'b
		 next))))

(test find-next-item-test
      "Find + next + item."
      (is
	(equal
	  '(
	    3
	    a
	    )
	  (spath :lists '(
		   (a (x 1) (c 1))
		   (b 3)
		   (b a q (c 1))
		   )
		 find 'b
		 next
		 item))))

(test need-dig
      "Using find where dig is needed."
      (is
	(equal
	  nil
	  (spath
	    :lists
	    '(
	      ("request-commands" (("seshion" "1")) "foo")
	      ("request-commands" (("session" "1")) "foo")
	      ("request-commands" ("session" "1") "foo")
	      )
	    find "session"))))

(test find-test-3
      "Another basic find test."
      (is
	(equal
	  '(
	    ("request-commands" (("seshion" "1")) "foo")
	    ("request-commands" (("session" "1")) "foo")
	    ("request-commands" ("session" "1") "foo")
	    )
	  (spath
	    :lists
	    '(
	      ("request-commands" (("seshion" "1")) "foo")
	      ("request-commands" (("session" "1")) "foo")
	      ("request-commands" ("session" "1") "foo")
	      )
	    find "request-commands"))))

(test find-next-item-test-2
      "Another basic find item test."
      (is
	(equal
	  '(
	    (("seshion" "1"))
	    (("session" "1"))
	    ("session" "1")
	    )
	  (spath
	    :lists
	    '(
	      ("request-commands" (("seshion" "1")) "foo")
	      ("request-commands" (("session" "1")) "foo")
	      ("request-commands" ("session" "1") "foo")
	      )
	    find "request-commands"
	    next
	    item))))

(test dig-test-1
      (is
	(equal
	  '(
	    (b c 1 (d 1) (b 3) (e 2))
	    (b 3)
	    )
	  (spath
	    :lists
	    '(
	      (a (b c 1 (d 1) (b 3) (e 2)))
	      )
	    dig 'b))))

(test dig-test-2
      (is
	(equal
	  '(
	    (c 1 (d 1) (b 3) (e 2))
	    )
	  (spath
	    :lists
	    '(
	      (a (b c 1 (d 1) (b 3) (e 2)))
	      )
	    dig 'c))))

(test dig-next-test-1
      (is
	(equal
	  '(
	    (1 (d 1) (b 3) (e 2))
	    )
	  (spath
	    :lists
	    '(
	      (a (b c 1 (d 1) (b 3) (e 2)))
	      )
	    dig 'c
	    next))))

(test dig-next-item-test-1
      (is
	(equal
	  '(
	    1
	    )
	  (spath
	    :lists
	    '(
	      (a (b c 1 (d 1) (b 3) (e 2)))
	      )
	    dig 'c
	    next
	    item))))

(test dig-next-item-test-2
      (is
	(equal
	  '(
	    c
	    3
	    )
	  (spath
	    :lists
	    '(
	      (a (b c 1 (d 1) (b 3) (e 2)))
	      )
	    dig 'b
	    next
	    item))))

(test dig-test-3
      (is
	(equal
	  '(
	    ("session" "1")
	    ("session" "1")
	    )
	  (spath
	    :lists
	    '(
	      ("request-commands" (("seshion" "1")) "foo")
	      ("request-commands" (("session" "1")) "foo")
	      ("request-commands" ("session" "1") "foo")
	      )
	    dig "session"))))

(test very-specific-item-path
      "Search to a particular item with much specificity."
      (is
	(equal
	  '(
	    "1"
	    )
	  (spath
	    :lists
	    '(
	      ("request-commands" ("ses-ion" "1") "foo")
	      ("requet-tcommands" ("session" "1") "foo")
	      ("request-commands" ("session" "1") "foo")
	      )
	    find "request-commands"
	    next
	    item
	    find "session"
	    next
	    item))))

(test very-inspecific-item-path
      "Search to a particular item with little specificity."
      (is
	(equal
	  '(
	    "1"
	    "1"
	    )
	  (spath
	    :lists
	    '(
	      ("request-commands" ("ses-ion" "1") "foo")
	      ("requet-tcommands" ("session" "1") "foo")
	      ("request-commands" ("session" "1") "foo")
	      )
	    dig "session"
	    next
	    item))))

(test medium-specificity-item-path
      "Search to a particular item with a medium amount of specificity."
      (is
	(equal
	  '(
	    "1"
	    )
	  (spath
	    :lists
	    '(
	      ("request-commands" ("ses-ion" "1") "foo")
	      ("requet-tcommands" ("session" "1") "foo")
	      ("request-commands" ("session" "1") "foo")
	      )
	    dig "request-commands"
	    dig "session"
	    next
	    item))))


; Test taken from squery test list:
; \(s-query '(a (b c 1 (d 1) (e 2))) '(b d))
; => ((D 1))
(test s-query-test-1
      (is
	(equal
	  '(
	    (D 1)
	    )
	  (spath
	    :lists
	    '(
	      (a (b c 1 (d 1) (e 2)))
	      )
	    dig 'b
	    dig 'd))))

; Test taken from squery test list:
; \(s-query '(a (b c 1 (d 1) (e 2))) '(b :c))
; => (1)
(test s-query-test-2
      (is
	(equal
	  '(
	    1
	    )
	  (spath
	    :lists
	    '(
	      (a (b c 1 (d 1) (e 2)))
	      )
	    dig 'b
	    dig 'c
	    next
	    item))))


; Test taken from squery test list:
; (s-query '(a (b c 1 (d 1) (e 2))) '(b *))
; => ((D 1) (E 2))
(test s-query-test-3
      (is
	(equal
	  '(
	    (D 1)
	    (E 2)
	    )
	  (spath
	    :first
	    '(
	      (a (b c 1 (d 1) (e 2)))
	      )
	    dig 'b
	    #'listp/))))


; Test taken from squery test list:
; (s-query '(a (b c 1 (d 1) (e 2))) '(b (d e)))
; => ((D 1) (E 2))
(test s-query-test-4
      (skip "We don't support disjunction.  It's a lot of effort for not much benefit."))
;      (is
;	(equal
;	  '(
;	    (D 1)
;	    (E 2)
;	    )
;	  (spath
;	  :lists
;	    '(
;	      (a (b c 1 (d 1) (e 2)))
;	      )
;	    dig b
;	    (dig d or dig e)))))


; Test taken from squery test list:
; (s-query '(a (b c 1 (d 1) (e 2))) '(b d ~))
; => (1)"
(test s-query-test-5
      (is
	(equal
	  '(
	    (1)
	    )
	  (spath
	    :lists
	    '(
	      (a (b c 1 (d 1) (e 2)))
	      )
	    dig 'b
	    dig 'd
	    next))))



; Discovered in testing that this tickles a bug.
(test double-item-failure
      (is
	(equal
	  '(
	    b
	    )
	  (spath
	    :lists
	    '(
	      (e (b c) e f (g h (i j)))
	      )
	    find 'e
	    next
	    item
	    item))))

; Checking for a similar failure with next
(test extra-next-failure
      (is
	(equal
	  nil
	  (spath
	    :lists
	    '(
	      (e (b c) e f (g h (i j)))
	      )
	    find 'b
	    next
	    next
	    next
	    next))))

(test first-test-1
      "Very basic find test but with first instead of lists."
      (is
	(equal
	  '(
	    b 3
	    )
	  (spath :first '(
			  (a (b 1) (c 1))
			  (b 3)
			  (b a q (c 1))
			  )
		 find 'b))))

(test func-ret-test-1
      "Very basic find test but with a function as the return type."
      (is
	(equal
	  '(
	    b a q (c 1)
	    )
	  (spath
	    #'(lambda (x) (to-list (nth/ 1 x)))
	    '(
	      (a (b 1) (c 1))
	      (b 3)
	      (b a q (c 1))
	      )
	    find 'b))))

(test none-test-1
      "Very basic find test but with none the return type."
      (is
	(equal
	  ; Heresy doesn't have working equality.
	  '(
	    (
	     b 3
	     )
	    (
	     b a q (c 1)
	     )
	    )
	  (to-list
	    (map/
	      #'to-list
	      (spath
		:none
		'(
		  (a (b 1) (c 1))
		  (b 3)
		  (b a q (c 1))
		  )
		find 'b))))))

(run!)
