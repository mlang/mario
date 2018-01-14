#lang racket

(provide (all-defined-out))

(struct env (memory heading skip?) #:mutable #:transparent)
(struct mem (byte previous next) #:mutable)
(struct cell (char [left #:mutable] [up #:mutable] [right #:mutable] [down #:mutable]) #:prefab)

(define (new-env)
  (env (mem 0 #f #f) 'right #f))

(define (decrease-pointer env)
  (let ([pos (env-memory env)])
    (when (eq? (mem-previous pos) #f)
      (set-mem-previous! pos (mem 0 #f pos)))
    (set-env-memory! env (mem-previous pos))
    env))

(define (increase-pointer env)
  (let ([pos (env-memory env)])
    (when (eq? (mem-next pos) #f)
      (set-mem-next! pos (mem 0 pos #f)))
    (set-env-memory! env (mem-next pos))
    env))

(define (update-byte! env func)
  (let ([memory (env-memory env)])
    (set-mem-byte! memory (modulo (func (mem-byte memory)) 256))))

(define (pad upto how)
  (lambda (str)
    (string-append str (make-string (- upto (string-length str)) how))))

(define (parse-world world)
  (let* ([lines (string-split world "\n" #:trim? #f)]
         [width (apply max (map string-length lines))]
         [grid (list->vector (map (pad width #\Space) lines))])
    (build-vector (* width (vector-length grid))
     (lambda (n)
       (let ([x (modulo n width)] [y (floor (/ n width))])
         (cell (string-ref (vector-ref grid y) x)
	       (and (> x 0) (sub1 n))
	       (and (>= (- n width) 0) (- n width))
	       (and (< (add1 x) width) (add1 n))
	       (and (< (+ n width) (* width (vector-length grid))) (+ n width))))))))

(define (world->cell world)
  (for ([here (in-vector world)])
    (set-cell-left! here (and (cell-left here) (vector-ref world (cell-left here))))
    (set-cell-up! here (and (cell-up here) (vector-ref world (cell-up here))))
    (set-cell-right! here (and (cell-right here) (vector-ref world (cell-right here))))
    (set-cell-down! here (and (cell-down here) (vector-ref world (cell-down here)))))
  (vector-ref world 0))

(define concrete '(#\= #\| #\#))

(define (no-ground? cell)
  (or (eq? (cell-down cell) #f)
      (not (memv (cell-char (cell-down cell)) concrete))))

(define (exec-instruction cell env)
  (if (env-skip? env)
    (set-env-skip?! env #f)
    (case (cell-char cell)
     [(#\+) (update-byte! env add1)]
     [(#\-) (update-byte! env sub1)]
     [(#\() (decrease-pointer env)]
     [(#\)) (increase-pointer env)]
     [(#\.) (write-byte (mem-byte (env-memory env)) (current-output-port))]
     [(#\:) (write-string (format "~a" (mem-byte (env-memory env))) (current-output-port))]
     [(#\,) (set-mem-byte! (env-memory env) (read-byte (current-input-port)))]
     [(#\;) (set-mem-byte! (env-memory env) (read (current-input-port)))]
     [(#\>) (set-env-heading! env 'right)]
     [(#\<) (set-env-heading! env 'left)]
     [(#\@) (set-env-heading! env (case (env-heading env)
  			           [(right) 'left] [(left) 'right]
			           [else (env-heading env)]))]
     [(#\!) (set-env-heading! env #f)]
     [(#\[) (when (= (mem-byte (env-memory env)) 0)
              (set-env-skip?! env #t))])))

(define (find-elevator-end cell dir)
  (and cell (if (not (eq? (cell-char cell) #\"))
	      (find-elevator-end (dir cell) dir)
              (cell-up cell))))

(define (elevate from dir to env)
  (exec-instruction from env)
  (if (eq? from to)
    (case (env-heading env)
     [(left) (eval-program (cell-left from) env)]
     [(right) (eval-program (cell-right from) env)]
     [else 'stuck])
    (elevate (dir from) dir to env)))

(define (eval-program cell env)
  (when (not (eq? cell #f))
    (exec-instruction cell env)
    (if (no-ground? cell)
      (eval-program (cell-down cell) env)
      (case (env-heading env)
       [(right) (eval-program (cell-right cell) env)]
       [(left) (eval-program (cell-left cell) env)]
       [(#f) (if (eq? (cell-char (cell-down cell)) #\#)
	       (let ([above (find-elevator-end cell cell-up)]
	             [below (find-elevator-end cell cell-down)])
	         (cond [above (elevate (cell-up cell) cell-up above env)]
                       [below (elevate (cell-down cell) cell-down below env)]
                       [else 'stopped]))
	       'stopped)]))))
