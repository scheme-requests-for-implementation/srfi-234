(cond-expand
  (guile
   (import (scheme base)
           (toposort)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (toposort)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (toposort)
           (srfi 64))))


(test-begin "toposort")

(test-equal
    '(a b d c)
  (topological-sort '((a b c)
                      (b d)
                      (c)
                      (d c))))

(test-equal
    '("a" "b" "d" "c")
  (topological-sort '(("a" "b" "c")
                      ("b" "d")
                      ("c")
                      ("d" "c"))
                    string=?))

(call/cc
 (lambda (cont)
   (with-exception-handler
       (lambda (err)
         (test-equal "graph has circular dependency" (circular-graph-message err))
         (cont #t))
     (lambda ()
       (topological-sort '((a b)
                           (b a)))
       (test-assert #f)))))

(test-end)
