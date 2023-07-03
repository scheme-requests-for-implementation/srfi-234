(cond-expand
  (guile
   (import (scheme base)
           (srfi 234)
           (srfi 1)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (srfi 234)
           (srfi 1)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (srfi 234)
           (srfi 1)
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

(define (permutations edgelist)
  (if (null? edgelist) '(())
      (apply append
             (map (lambda (edge)
                    (map (lambda (permutation)
                           (cons edge permutation))
                         (permutations (delete edge edgelist))))
                  edgelist))))

(test-equal #t
  (every (lambda (edgelist)
           (let* ((graph (edgelist->graph edgelist))
                  (order (topological-sort graph equal?)))
             (cond
              ((equal? order '(top left right bottom)) #t)
              ((equal? order '(top right left bottom)) #t)
              (else order))))
         (permutations '((top left) (top right) (left bottom) (right bottom)))))

(test-equal '(libnewsboat regex-rs strprintf)
  (topological-sort (edgelist->graph '((libnewsboat strprintf) (libnewsboat regex-rs) (regex-rs strprintf)))))

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
