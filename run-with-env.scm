#!/home/nivpgir/.local/bin/scsh \
-o tables -o srfi-37 -s
!#

;; a fixed version for substitute-env-vars, because the one from git is broken
(define (substitute-env-vars str)
  (let lp ((ans '()) (s str))
    (let ((len (string-length s)))
      (cond
       ((zero? len) (apply string-append (reverse! ans)))
       ((string-index s #\$) =>
        (lambda (i)
          (let ((ans (cons (substring s 0 i) ans))
                (s (substring s (+ i 1) len))
                (len (- len (+ i 1))))
            (if (zero? len)
                (lp ans "")
                (let ((next-char (string-ref s 0)))
                  (cond ((char=? #\{ next-char)
                         (cond ((string-index s #\}) =>
                                (lambda (i)
                                  (lp (cons (getenv (substring s 1 i)) ans)
                                      (substring s (+ i 1) len))))
                               (else (error "Unbalanced ${ delimiter in string" s))))
                        (else
                         (let ((i (or (string-index s #\ ) len)))
                               (lp (cons (getenv (substring s 0 i)) ans)
                                   (substring s i len))))))))))
        (else (lp (cons s ans) ""))))))


(define +usage+
  "at least 1 argument must be given")

(if (zero? (length command-line-arguments))
    (begin
      (display  +usage+) (newline)
      (exit 1)))

(define (parse-envar-file p)
  (define string->envar-pair (infix-splitter  "="  -1))
  (let parse-line-from-file ((env-list '())
                             (line (read-line p)))
      (if (eof-object? line) env-list
          (let ((key (first (string->envar-pair line)))
                (val (second (string->envar-pair line))))
            (parse-line-from-file (cons (cons key val) env-list)
                                  (read-line p))))))

(define (main)
  (let ((env-alist (call-with-input-file (argv 1) parse-envar-file)))
    (with-env ,env-alist
              (if (cdr command-line-arguments) ;if no command then don't do anything
                  (if (table-ref opt-table "debug") ;if debug then print the command to be executed
                      (begin
                        (display "executing command:") (newline)
                        (display (string-join
                                  (map substitute-env-vars (cdr command-line-arguments))
                                  " ")) (newline)))
                  (run ,(map substitute-env-vars (cdr command-line-arguments))))
              (begin (display "No command given") (newline))))
  )


;; (main)
