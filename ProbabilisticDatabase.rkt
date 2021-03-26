#lang racket


;//---------------Modules 

(require racket/class)
(require "Stockfun.rkt")
(require "StockRetrieval.rkt")
(require "DatabaseDictionary.rkt")
(provide (all-defined-out))


;//---------------Structures

(define-struct classc (identifier alol) #:transparent)
(define-struct linec (word1 word2 word3) #:transparent)


;//---------------Structure Functions

;structure-lines: alos -> alol
(define(structure-lines alos)
  (cond
    [(empty? alos) empty]
    [(> 3 (length alos)) empty]
    [else (cons
           (make-linec (car alos) (cadr alos) (caddr alos))
           (structure-lines (cdddr alos)))]))
;//takes in an abritrary list and returns a list of lines

;structure-data: identifier alos -> class
(define(make-class id alos)
  (make-classc id (structure-lines alos)))
;//Structures abritrary lists of data and structures into dataset format with class name


;//---------------Datasets


(define spamc (make-classc
               'spam
               (list (make-linec "offer" "is" "secret")
                     (make-linec "click" "secret" "link")
                     (make-linec "secret" "sports" "link"))))
;//Dataset for Spam

(define hamc (make-classc
              'ham
              (list (make-linec "play" "sports" "today")
                    (make-linec "what" "play" "sports")
                    (make-linec "secret" "sports" "event")
                    (make-linec "sports" "is" "today")
                    (make-linec "sports" "cost" "money"))))
;//Dataset for Ham


;//---------------AllClasses

(define allclasses (list spamc hamc))


;//---------------Auxiliaries

;amt-lines: alol -> n
(define(amt-lines alol)
  (cond
    [(empty? alol) 0]
    [(struct? (first alol)) (add1 (amt-lines (rest alol)))]))

;amt-words: alol -> n
(define(amt-words alol)
  (* (amt-lines alol) 3))

;amt-string: s alol -> n
(define(amt-string s alol)
  (cond
    [(empty? alol) 0]
    [(struct? (first alol))
     (+ (apply + (map (lambda(x)
                        (if (string=? s x) 1 0))
                      (list (linec-word1 (first alol))
                            (linec-word2 (first alol))
                            (linec-word3 (first alol)))))
        (amt-string s (rest alol)))]))

;strip-class: classes -> alol
(define(strip-class aloc)
  (foldr (lambda(x y) (append x y))
         empty
         (map (lambda(x) (classc-alol x)) aloc)))


;alol->alos: alol -> alos
(define(alol->alos alol)
  (cond
    [(empty? alol) empty]
    [(struct? (first alol)) (append
                             (list (linec-word1 (first alol))
                                   (linec-word2 (first alol))
                                   (linec-word3 (first alol)))
                             (alol->alos (rest alol)))]))

;remove-duplicates: alo -> alo
(define (remove-duplicate alo)
  (foldr
   (lambda (x y)
     (cons x (filter
              (lambda (z) (not (string=? x z))) y))) empty alo))
;//Removes duplicate strings from a list

;unique-words: alol -> n
(define(unique-words alol)
  (length (remove-duplicate (alol->alos alol))))

;//---------------Functions

;prob-class: class -> n
(define(prob-class c)
  (/ (+ 1 1)
     (+ (amt-lines (classc-alol c))
        (amt-lines (strip-class allclasses)))))

;prob-cons1: set -> alo
(define(prob-cons set given)
  (map (lambda(y) (* (prob-class y) (apply * (map (lambda(x) (prob-message x y)) set)))) given)) 



;LaPlace Smoothing:
;// (Count(w) + k / N + Classes
;//This is what wil be used in order to account for 0%

;prob-message: set given -> n
(define(prob-message set given)
          (cond
            [(empty? given)
             (cond
               [(string? set)
                (/ (+ (amt-string set (strip-class allclasses)) 1)
                   (+ (amt-words (strip-class allclasses)) (unique-words(strip-class allclasses))))]
               [(cons? set) (map (lambda(x) (prob-message x empty)) set)])]
            [(struct? given)
             (cond
               [(string? set)
                (/ (+ (amt-string set (strip-class (list given))) 1)
                   (+ (amt-words (strip-class (list given))) (unique-words(strip-class allclasses))))]
               [(cons? set) (map (lambda(x) (prob-message x given)) set)])]
            [(cons? given)
             (cond
               [(string? set)
                (/ (+ (amt-string set (strip-class given)) 1)
                   (+ (amt-words (strip-class given)) (unique-words(strip-class allclasses))))]
               [(cons? set) (apply +(map (lambda(y) (* (prob-class y) (apply * (map (lambda(x) (prob-message x y)) set)))) given))])]
            [else #false]))

(prob-message "secret" empty)
(prob-message "sports" empty)

(prob-message (list "secret" "is" "secret") empty)
(prob-message (list "sports" "is" "secret") empty)

(prob-message "secret" spamc)
(prob-message "secret" hamc)

(prob-message (list "secret" "is" "secret") spamc)
(prob-message (list "secret" "is" "secret") hamc)

(prob-message "secret" (list spamc hamc))
(prob-message (list "secret" "is" "secret") (list spamc hamc))
                       
