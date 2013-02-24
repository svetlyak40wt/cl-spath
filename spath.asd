(defsystem spath
           :name "spath"
           :version "1.1"
           :maintainer "Robin Lee Powell"
           :author "Robin Lee Powell"
           :licence "Public Domain"
           :description "Search/query system for matching parts of complicated s-expression (list) data structures."
           :depends-on ()
           :components ((:file "spath")))

(defsystem spath-tests
           :name "spath-tests"
           :depends-on (:fiveam)
           :components ((:file "tests")))
