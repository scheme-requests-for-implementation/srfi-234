(define-library
    (toposort)
    (import
        (scheme base)
        (scheme case-lambda)
        (srfi 1))
    (export topological-sort
            circular-graph?
            circular-graph-message
            circular-graph-cycle)
    (include "toposort-impl.scm"))
