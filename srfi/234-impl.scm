;;; Code adapted from gauche https://github.com/shirok/Gauche/blob/master/lib/util/toposort.scm :
;;;
;;; srfi-234.scm - topological sorting
;;;
;;;  Written by Shiro Kawai (shiro@acm.org)  2001
;;;  Public Domain..  I guess lots of Scheme programmers have already
;;;  written similar code.
;;;


;; error object
(define-record-type <circular-graph>
  (make-circular-graph message cycle)
  circular-graph?
  (message circular-graph-message)
  (cycle circular-graph-cycle))

;;  nodes : a list of (<from> <to0> <to1> ...)
(define topological-sort
  (case-lambda
    ((nodes) (topological-sort-impl nodes eqv?))
    ((nodes eq) (topological-sort-impl nodes eq))))

(define (topological-sort-impl nodes eq)
  (define table (map (lambda (n)
                       (cons (car n) 0))
                     nodes))
  (define queue '())
  (define result '())

  ;; set up - compute number of nodes that each node depends on.
  (define (set-up)
    (for-each
     (lambda (node)
       (for-each
        (lambda (to)
          (define p (assoc to table eq))
          (if p
              (set-cdr! p (+ 1 (cdr p)))
              (set! table (cons
                           (cons to 1)
                           table))))
        (cdr node)))
     nodes))

  ;; traverse
  (define (traverse)
    (unless (null? queue)
      (let ((n0 (assoc (car queue) nodes eq)))
        (set! queue (cdr queue))
        (when n0
          (for-each
           (lambda (to)
             (define p (assoc to table eq))
             (when p
               (let ((cnt (- (cdr p) 1)))
                 (when (= cnt 0)
                   (set! result (cons to result))
                   (set! queue (cons to queue)))
                 (set-cdr! p cnt))))
           (cdr n0)))
        (traverse))))

  (set-up)
  (set! queue
    (apply append
           (map
            (lambda (p)
              (if (= (cdr p) 0)
                  (list (car p))
                  '()))
            table)))
  (set! result queue)
  (traverse)
  (let ((rest (filter (lambda (e)
                        (not (zero? (cdr e))))
                      table)))
    (unless (null? rest)
      (raise (make-circular-graph "graph has circular dependency" (map car rest)))))
  (reverse result))

;; convert an edgelist '((a b) (a c) (b e)) to a graph '((a b c) (b e))
(define edgelist->graph
  (case-lambda
    ((edgelist) (edgelist->graph-impl edgelist eqv?))
    ((edgelist eq) (edgelist->graph-impl edgelist eq))))
(define (edgelist->graph-impl edgelist eq)
  (let loop ((graph '()) (edges edgelist))
    (cond
     ((null? edges) (reverse! graph))
     ((assoc (car (car edges)) graph)
      (let* ((edge (car edges))
             (left (car edge))
             (graph-entry (assoc left graph))
             (right (car (cdr edge))))
        (set-cdr! (cdr graph-entry) (list right))
        (loop graph (cdr edges))))
     ;; use apply list to break up immutable pairs
     (else (loop (cons (apply list (car edges)) graph) (cdr edges))))))
