(module search mzscheme
  
  (require "stream.ss")
  (require (prefix heap: (planet "heap.scm" ("soegaard" "galore.plt" 2))))
 
  (provide (all-defined-except expand-node))
  
  (define-struct node (depth label parent state))
   
  ; make-root: state -> node
  (define (make-root s) (make-node 0 #f #f s))
  
  ; root?: node -> boolean
  (define (root? n) (not (node-parent n)))
 
  ; The problem-space is a function of type (node -> (stream-of node)),
  ; where the depth of each of the children of the node expanded, p,
  ; is (+ (node-depth p) 1) 
  (define-struct problem 
    (root     ; node
     space    ; node -> (stream-of node)  
     goal))   ; state -> boolean
  
  ; type strategy = problem -> (stream-of node)
  ; type resource-limited-strategy = resource -> strategy
  
  ; search: problem strategy  -> (stream-of node)
  (define (search p s) (stream-delay (s p)))
  
  ; expand-node: node problem -> (stream-of node)
  (define (expand-node n p) 
    ; (printf "label:~a; depth:~a~n" (node-label n) (node-depth n))  ; for debugging
    ((problem-space p) n))
  
  ; path : node -> (list-of label)
  (define (path n) 
    (if (root? n) 
        () 
        (append (path (node-parent n)) 
                (list (node-label n)))))
  
  (define-struct resource (amount))
  (define (resource-empty? r) (<= (resource-amount r) 0))
  (define (use r) 
    ; (printf ".") ; debug
    (if (> (resource-amount r) 0)
        (set-resource-amount! r (- (resource-amount r) 1))))
    
  ; depth-first: resource-limited-strategy
  ; resource: the maximum number of nodes which may be expanded
  (define (depth-first r)
    (lambda (p)
      (define (loop open-nodes)
        (use r)
        (if (or (stream-null? open-nodes)(resource-empty? r))
            stream-null
            (let ((node (stream-car open-nodes)))
              (cond (((problem-goal p) (node-state node))
                     (stream-cons node (stream-delay (loop (stream-cdr open-nodes)))))
                    (else (loop (stream-append (expand-node node p) 
                                               (stream-delay (stream-cdr open-nodes)))))))))
      (loop (stream (problem-root p)))))
 
  ; breadth-first: resource-limited-strategy
  ; resource: the maximum number of nodes which may be expanded
  (define (breadth-first r)
    (lambda (p) 
      (define (loop open-nodes)
        (use r)
        (if (or (stream-null? open-nodes) (resource-empty? r))
            stream-null
            (let ((node (stream-car open-nodes)))
              (cond (((problem-goal p) (node-state node))
                     (stream-cons node (stream-delay (loop (stream-cdr open-nodes)))))
                    (else (loop (stream-append (stream-cdr open-nodes) 
                                               (stream-delay (expand-node node p)))))))))
      (loop (stream (problem-root p)))))
    
;   iterative-deepening: integer integer -> resource-limited-strategy
;   init: the initial depth to search
;   step: how much to increase the depth on each iteration
;   resource: the maximum number of nodes which may be expanded (not a depth limit)
  (define (iterative-deepening init step)
    (lambda (r)
      (define (depth-first depth-limit)
        (lambda (p)
          (define (loop open-nodes)
            (use r) 
            (if (or (stream-null? open-nodes)
                    (resource-empty? r))
                stream-null
                (let ((n (stream-car open-nodes)))
                  (cond (((problem-goal p) (node-state n))
                         (stream-cons n (stream-delay (loop (stream-cdr open-nodes)))))
                        ((<= (node-depth n) depth-limit)
                          (loop (stream-append (expand-node n p) 
                                               (stream-delay (stream-cdr open-nodes)))))
                        (else                  
                          (loop (stream-cdr open-nodes)))))))
          (loop (stream (problem-root p)))))
      (lambda (p) 
        (if (resource-empty? r)
            stream-null
            (stream-append ((depth-first init) p)
                           (stream-delay (((iterative-deepening (+ init step) step) r) p)))))))
  
; The alternative definition of iterative deepening below seems to be incorrect, but oddly works
; with some queries of the Hungarian VAT model, whereas the above, presumaby correct implementation
; of iterative-deepening does not!  There may be due to bugs in the implementation of the
; SRFI 40 stream library, since the only difference between these two versions are the different
; uses of stream-delay.
  
;  (define (iterative-deepening init step)
;    (lambda (r)
;      (define (depth-first depth-limit)
;        (lambda (p)
;          (define (loop open-nodes)
;            (use r) 
;            (if (or (stream-null? open-nodes)
;                    (resource-empty? r))
;                stream-null
;                (let ((n (stream-car open-nodes)))
;                  (cond (((problem-goal p) (node-state n))
;                         (stream-cons n (stream-delay (loop (stream-cdr open-nodes)))))
;                        ((<= (node-depth n) depth-limit)
;                         (loop (stream-append (expand-node n p) 
;                                              (stream-delay (stream-cdr open-nodes)))))
;                        (else                  
;                         (stream-delay (loop (stream-cdr open-nodes))))))))
;          (loop (stream (problem-root p)))))
;      (lambda (p) 
;        (if (resource-empty? r)
;            stream-null
;            (stream-append ((depth-first init) p)
;                           (((iterative-deepening (+ init step) step) r) p))))))
  
  
  ; best-first: (node node -> {-1,0,1}) -> resource-limited-strategy
  ; resource: the maximum number of nodes which may be expanded
  (define (best-first compare)
    (lambda (r)
      (lambda (p)
        (define (loop open-nodes)
          (use r)
          (if (or (heap:empty? open-nodes) (resource-empty? r))
              stream-null
              (let ((node (heap:find-min open-nodes)))
                (cond (((problem-goal p) (node-state node))
                       (stream-cons node (loop (heap:delete node open-nodes))))
                      (else
                       (loop (heap:union (heap:list->heap compare (stream->list (expand-node node p)))
                                         (heap:delete node open-nodes))))))))
        (loop (heap:singleton compare (problem-root p))))))
  
  ) ; end module search