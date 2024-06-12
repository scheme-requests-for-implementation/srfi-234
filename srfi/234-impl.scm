;;; Code adapted from gauche https://github.com/shirok/Gauche/blob/master/lib/util/toposort.scm :
;;;
;;; srfi-234.scm - topological sorting
;;;
;;;  Written by Shiro Kawai (shiro@acm.org)  2001
;;;  Public Domain..  I guess lots of Scheme programmers have already
;;;  written similar code.
;;;


(define topological-sort
  (case-lambda
    ((graph) (topological-sort-impl graph equal? #f))
    ((graph eq) (topological-sort-impl graph eq #f))
    ((graph eq nodes) (topological-sort-impl graph eq nodes))))

(define (topological-sort-impl graph eq nodes)
  (define table (map (lambda (n)
                       (cons (car n) 0))
                     graph))
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
     graph))

  ;; traverse
  (define (traverse)
    (unless (null? queue)
      (let ((n0 (assoc (car queue) graph eq)))
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
    (if (null? rest)
        (values (reverse result) #f #f)
        (values #f "graph has circular dependency" (map car rest)))))

;; Calculate the connected components from a graph of in-neighbors
;; implements Kosaraju's algorithm: https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm
(define (connected-components graph)
  (define nodes-with-inbound-links (map car graph))
  ;; graph of out-neighbors
  (define graph/inverted (edgelist->graph (graph->edgelist/inverted graph)))
  (define nodes-with-outbound-links (map car graph/inverted))
  ;; for simplicity this uses a list of nodes to query for membership. This is expensive.
  (define visited '())
  (define vertex-list '())
  ;; create vertex-list sorted with outbound elements first
  (define (visit! node)
    (cond ((member node visited) '())
          (else
           ;; mark as visited before traversing
           (set! visited (cons node visited))
           ;; this uses the graph: the outbound connections
           (let ((node-in-graph (assoc node graph)))
             (when node-in-graph
               (for-each visit! (cdr node-in-graph))))
           ;; add to list after traversing
           (set! vertex-list (cons node vertex-list)))))
  ;; for simplicity this uses a list of nodes to query for membership. This is expensive.
  (define in-component '())
  (define components '())
  ;; assign nodes to their components
  (define (assign! u root)
    (unless (member u in-component)
      (set! in-component (cons u in-component))
      (set! components (cons (cons u (car components)) (cdr components)))
      ;; this uses the graph/inverted: the inbound connections
      (let ((node-in-graph (assoc u graph/inverted)))
        (when node-in-graph
          (for-each (cut assign! <> root) (cdr node-in-graph))))))
  (define (assign-as-component! u)
    (unless (member u in-component)
      (set! components (cons '() components))
      (assign! u u)))
  (for-each visit! nodes-with-outbound-links)
  (for-each assign-as-component! vertex-list)
  components)

;; convert an edgelist '((a b) (a c) (b e)) to a graph '((a b c) (b e))
(define edgelist->graph
  (case-lambda
    ((edgelist) (edgelist->graph-impl edgelist equal?))
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
        ;; adjust the right-most cdr
        (let lp ((entry graph-entry))
          (if (null? (cdr entry))
              (set-cdr! entry (list right))
              (lp (cdr entry))))
        (loop graph (cdr edges))))
     ;; use apply list to break up immutable pairs
     (else (loop (cons (apply list (car edges)) graph) (cdr edges))))))

;; convert an inverted edgelist '((b a) (c a) (e b)) to a graph '((a b c) (b e))
(define edgelist/inverted->graph
  (case-lambda
    ((edgelist) (edgelist/inverted->graph-impl edgelist equal?))
    ((edgelist eq) (edgelist/inverted->graph-impl edgelist eq))))
(define (edgelist/inverted->graph-impl edgelist eq)
  (let loop ((graph '()) (edges edgelist))
    (cond
     ((null? edges) (reverse! graph))
     ((assoc (car (cdr (car edges))) graph)
      (let* ((edge (car edges))
             (left (car (cdr edge)))
             (graph-entry (assoc left graph))
             (right (car edge)))
        ;; adjust the right-most cdr
        (let lp ((entry graph-entry))
          (if (null? (cdr entry))
              (set-cdr! entry (list right))
              (lp (cdr entry))))
        (loop graph (cdr edges))))
     ;; reverse instead of reverse! to avoid immutable lists
     (else (loop (cons (reverse (car edges)) graph) (cdr edges))))))

(define (graph->edgelist graph)
  (graph->edgelist/base graph (lambda (top) (list (car top) (car (cdr top))))))

(define (graph->edgelist/inverted graph)
  (graph->edgelist/base graph (lambda (top) (list (car (cdr top)) (car top)))))

(define (graph->edgelist/base graph top-to-edge-fun)
  (let loop ((edgelist '()) (graph graph))
    (cond ((null? graph)
           (reverse! edgelist))
          ((null? (car graph))
           (loop edgelist (cdr graph)))
          ((null? (cdr (car graph)))
           (loop edgelist (cdr graph)))
          (else
           (let* ((top (car graph))
                  (edge (top-to-edge-fun top))
                  (rest (cdr (cdr top))))
             (loop (cons edge edgelist)
                   (cons (cons (car top) rest) (cdr graph))))))))
