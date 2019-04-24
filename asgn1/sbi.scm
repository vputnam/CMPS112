#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define *label-table* (make-hash))
(define (lab-get key)
        (hash-ref *label-table* key))
(define (lab-put! key value)
        (hash-set! *label-table* key value))


(define *variable-table* (make-hash))
(define (var-get key)
        (hash-ref *variable-table* key))
(define (var-put! key value)
        (hash-set! *variable-table* key value))
(define (var-array! key dim)
        (hash-set! *variable-table* key (make-vector dim) ))

(for-each
     (lambda (pair)
             (var-put! (car pair) (cadr pair)))
        `(
             (+  ,(lambda(x) (+ 0 x) ))
             (-  ,(lambda(x) (- 0 x) ))
             (log10_2 0.301029995663981195213738894724493026768189881)
             (sqrt_2  1.414213562373095048801688724209698078569671875)
             (e       2.718281828459045235360287471352662497757247093)
             (pi      3.141592653589793238462643383279502884197169399)
             (ceil    ,ceiling)
             (exp     ,exp)
             (floor   ,floor)
             (log     ,log)
             (sqrt    ,sqrt)
             (atan    ,atan)
             (sin     ,sin)
             (cos     ,cos)
             (tan     ,tan)
             (acos    ,acos)
             (asin    ,asin )
             (abs     ,abs)
             (round   ,round)    
          ))

(define *function-table* (make-hash))
(define (func-get key)
        (hash-ref *function-table* key))
(define (func-put! key value)
        (hash-set! *function-table* key value))

(for-each
    (lambda (pair)
            (func-put! (car pair) (cadr pair)))
    `(
        (goto   ,(lambda (x) (map (lambda (line) 
(check-line line )) (lab-get (car x)) )  )) 
        (dim    ,(lambda (x) (var-array! (car x) (cadar x) )))
        (let    ,(lambda (x) (var-put! (car x) (evalexpr (cadr x)) )))
        (print  ,(lambda (x) (cond ((null? (cdr x ))
                                    (cond ((string? (car x) )
 (display (car x)) (printf "~n"))
                                    ((number? (car x) ) 
(display (car x)) (printf "~n"))
                                    (else (display (var-get (car x))) 
(printf "~n")) ))
                                    (else (display (car x))
                                      (display (evalexpr (car(cdr x))))
                                    (printf "~n") )) ))
        (/     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,(lambda (x y) (+ x y) ))
        (-       ,(lambda (x y) (- x y) ))
        (*       ,(lambda (x y) (* x y) ))
        (^       ,expt)

 
     ))

 (define (evalexpr expr)
   (cond ((number? expr) (+ expr 0.0)) 
         ((symbol? expr) (cond ((null?
 (var-get expr))(hash-ref *function-table* expr #f))
                               (else 
(hash-ref *variable-table* expr #f))))
         ((pair? expr)   (cond ((null? (cddr expr)) 
(apply (hash-ref *variable-table* (car expr))
                                (map evalexpr (cdr expr))))
                               (else (apply
(hash-ref *function-table* (car expr))
                                (map evalexpr (cdr expr))))
                         ))
         (else #f))
 )

(define (check-line line)
    (cond [(null? (cdr line)) 'null]
            [(< (length (cdr line)) 2)
                 (cond [(list? (cadr line)) 
                        (cond [(< (length (cadr line)) 2) 'null]
                 (else
                        (apply 
(hash-ref *function-table* (caadr line)) (list(cdadr line)) )))
                     ]
                 (else 
                        (apply 
(hash-ref *function-table* (caadr line)) (list(cdadr line)) )))
           ]
          ((symbol? (cadr line)) 
(apply (hash-ref *function-table* (caaddr line)) (list(cdaddr line))))
          (else
(apply (hash-ref *function-table* (caadr line)) (list(cdadr line)) ))
))

(define (insert-label program)
     (map (lambda (line)
          (when ( = 3 (length line))
                (hash-set! *label-table* (cadr line)
                           (member line program) ) 
          )
     )program)
)

(define (ctrl-trans program )
          (map (lambda (line) (check-line line )) program ) 
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (filter p? list)
        (if (null? list) '()
            (let ((a (car list))
                  (fd (filter p? (cdr list))))
                 (if (p? a) (cons a fd) 
		  fd ))))

(define (length x)
        (define (len x n)
                (if (null? x) n
                    (len (cdr x) (+ n 1))))
        (len x 0))


(define (main arglist)
    (display (filter (lambda (n) (< n 4)) '(1 2 3 4 5 6 7)) )
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
             (program (readlist-from-inputfile sbprogfile)))
             (insert-label program)
             (ctrl-trans program)
) ))

(main (vector->list (current-command-line-arguments)))
