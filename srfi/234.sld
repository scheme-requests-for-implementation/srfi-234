(define-library (srfi 234)
    (import
        (scheme base)
        (scheme case-lambda)
        (srfi 1)
        (srfi 11) ;; let-values
        (srfi 26)) ;; cut
    (export topological-sort
            topological-sort/details
            edgelist->graph
            edgelist/inverted->graph
            graph->edgelist
            graph->edgelist/inverted
            connected-components)
    (include "234-impl.scm"))
