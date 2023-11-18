(define-syntax when
  (syntax-rules (else)
    ((when test (body ...) (else alt ...))
     (if test
         (begin body ... )
         (begin alt ... )))
    ((when test body ...)
     (if test
         (begin body ...)))))

(define-syntax while
  (syntax-rules ()
    ((while condition body ...)
     (let loop ()
       (if condition
           (begin
             body ...
             (loop)))))))

(define-syntax timed-if
  (syntax-rules (after during as-long do else for)
    ((timed-if (predicate after cooldown) (do consequent ...) (else alternative ...) for timer delta-time)
     (if predicate
         (if (number? cooldown)
             (begin
               (set! timer (+ timer delta-time))
               (if (>= timer cooldown)
                   (begin
                     (set! timer 0)
                     (set! predicate #f)
                     consequent
                     ...))))
         (begin
           alternative
           ...
           (set! predicate #t))))
    ((timed-if (predicate during cooldown) (do consequent ...) (else alternative ...) for timer delta-time)
     (if predicate
         (begin
           (set! timer (+ timer delta-time))
           (if (>= timer cooldown)
               (begin
                 (set! timer 0)
                 (set! predicate #t)))
           consequent
           ...)
         (begin
           alternative
           ...)))
     ((timed-if (predicate as-long cooldown) (do consequent ...) (else alternative ...) for timer delta-time)
     (if predicate
         (begin
           (set! timer (+ timer delta-time))
           (if (>= timer cooldown)
               (begin
                 (set! timer 0)
                 (set! predicate #f)))
           consequent
           ...)
         (begin
           alternative
           ...)))))


(define-syntax for
  (syntax-rules (in from to do)
    ((for element in lst do body ...)
     (let loop ((rest lst))
       (if (not (null? rest))
           (begin
             (let ((element (car rest)))
               body ...)
             (loop (cdr rest))))))
    ((for count from a to b do body ...)
     (let loop ((count a))
       (if (<= count b)
           (begin
             body ...
             (loop (+ count 1))))))))

;
;(define-syntax define-class
;  (syntax-rules (is has public: protected: private:)
;    ((define-class (name)
;       (public:
;        ((local value)
;         ... )
;        defines       
;        ...))
;     (define (name)
;       (let ((local value)
;             ...)
;         defines
;         ...
;         (define (dispatch m)
;           (cond
;             ((eq? m 'local) local)
;             ...
;             (else (begin (display name) (display "::Not a valid message: ")(display m)))))
;         dispatch)))
;    ((define-class (name par ... )
;       (public:
;        (local value)
;        ... )
;       (protected:
;        (local-pr value-pr)
;        ... )
;       body
;       ...)
;     (define (name par ...)
;       (let ((local value)
;             ...
;             (local-pr value-pr)
;             ...)
;         body
;         ...
;         (define (dispatch m)
;           (cond
;             ((eq? m 'par) par)
;             ...
;             ((eq? m 'local) local)
;             ...
;             ((eq? m 'protected) (local-pr value-pr))
;             ...
;             (else 'message-does-not-exist)))
;         dispatch)))
;    ((define-class (name par ... ) has class
;       (public:
;        (local value)
;        ... )
;       body
;       ...)
;     (define (name par ...)
;       (let ((obj (class)))
;         (let ((local value)
;               ...)
;           body
;           ...
;           (define (dispatch m)
;             (cond
;               ((eq? m 'par) par)
;               ...
;               ((eq? m 'local) local)
;               ...
;               (else (obj m))))
;           dispatch))))
;    ((define-class (name par ...) is base
;       (public:
;        (local value)
;        ... )
;       body
;       ...)
;     (define (name par ...)
;       (define this (base par ...))
;         (let ((local value)
;               ...)
;           body
;           ...
;           (define (dispatch m)
;             (cond
;               ((eq? m 'par) par)
;               ...
;               ((eq? m 'local) local)
;               ...
;               (else (this m))))
;           dispatch))))
;    ((define-class (name par ... )
;       (public:
;        (local value)
;        ... )
;       (protected:
;        (local-pr value-pr)
;        ... )
;       (private:
;        (p-local p-value)
;        ... )
;       body
;       ...)
;     (define (name par ...)
;       (let* ((local value)
;              ...
;              (local-pr value-pr)
;              ...
;              (p-local p-value)
;              ...)
;         body
;         ...
;         (define (dispatch m)
;           (cond
;             ((eq? m 'par) par)
;             ...
;             ((eq? m 'local) local)
;             ...
;             (else 'message-does-not-exist)))
;         dispatch)))))


