(cond-expand
  (guile
   (import (scheme base)
           (srfi 234)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (srfi 234)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (srfi 234)
           (srfi 64))))


(test-begin "srfi-234")

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

(test-equal
    '((a b c) (b e))
  (edgelist->graph '((a b) (a c) (b e))))

(test-equal
    '((a b c) (b e))
  (edgelist/inverted->graph '((b a) (c a) (e b))))

(call/cc
 (lambda (cont)
   (with-exception-handler
       (lambda (err)
         (test-equal #t (circular-graph? err))
         (test-equal "graph has circular dependency" (circular-graph-message err))
         (test-assert
           (or
             (equal? (circular-graph-cycle err) '(a b))
             (equal? (circular-graph-cycle err) '(b a))))
         (cont #t))
     (lambda ()
       (topological-sort '((a b)
                           (b a)))
       (test-assert #f)))))

(test-end)
