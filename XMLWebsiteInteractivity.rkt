#lang racket
(require web-server/http)
(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)
;Most Basic XML code is

;XML: <Machine>  </Machine> 
;Racket Equivalent: '(machine)

;XML: <Machine> <action></action></machine>
;Racket Equivalent: '(machine (action))


;XML: <machine initial="red">
;                  <action state="red"    next="green" />
;                  <action state="green"  next="yellow" />
;                  <action state="yellow" next="red" />
;             </machine>

;code for a  simple stoplight in XML

;Translation to racket 

;Racket Equivalent: '(machine ((initial "red"))
;                                   (action ((state "red") (next "green")))
;                                  (action ((state "green") (next "yellow")))
;                                  (action ((state "yellow") (next "red"))))


;XML: <ul><li><word /><word /></li><li><word /></li></ul>
;Racket Equivalent: '(ul 
;                                     (li (word) (word))
;                                     (li (word)))

;Racket: '(server ((name "example.org")))
;XML Equivalent: <server> <name "example.org" /></server>

;Racket: ' (carcas (board (grass)) (player ((name "sam"))))
;XML Equivalent: <carcas><board><grass /></board><player><name "sam"></player></carcas>

;Racket: '(start)
;XML Equivalent: <start />

;<word><text><string /></text></word>
(define xc       '((action ((state "red") (next "green")))
                        (action ((state "green") (next "yellow")))
                        (action ((state "yellow") (next "red")))))     

(define a0 '((initial "X")))
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))


(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))



;xexpr-attr: Xexpr v2 -> alo-attr
(define(xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) empty]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             empty))])))

(xexpr-attr e0)
(xexpr-attr e1) 
(xexpr-attr e2) 
(xexpr-attr e3) 
(xexpr-attr e4)


;xexpr-name: Xexpr v2 -> tag
(define(xexpr-name xe)
   (cond
     [(or (symbol? xe) (string? xe) (number? xe) (empty? xe)) #false]
     [else (first xe)]))

(xexpr-name '(machine))
(xexpr-name '(machine (action)))
(xexpr-name '(machine (action ((state "red")))))
(xexpr-name `(machine ,a0 (action ((state "red")))))


;xexpr-content: Xexpr v2 -> alo-content
(define (xexpr-content xe)
  (local ((define content? (rest xe)))
    (cond
      [(empty? xe) empty]
      [else
       (local ((define content (rest(rest xe))))
         (if (xexpr-attr xe)
           content
           content?))])))

;(xexpr-content e0) 
(xexpr-content e1) 
(xexpr-content e2) 
(xexpr-content e3) 
(xexpr-content e4)


;find-attr: alo-attr s -> boolean
(define(find-attr s alo-attr)
  (local ((define find (assq s alo-attr)))
  (cond
    [(empty? alo-attr) #false]
    [(list? find) (second find)]
    [else #false])))
  
(find-attr 'initial (xexpr-attr e1)) 
(find-attr 'initial (xexpr-attr e4))
(find-attr 'wow '((initial "x") (dank "meme") (lmao "ayy")))

;A 1Transition is a list of two items
; (cons (FSM-State (cons FSM-State '()))
;FSM-State is a string that specifies a color

;data example
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))


;find aloxy x -> y
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

;(find empty "red")
(find '((red green)) 'red)
(find '(("red" "green") ("green" "red")) "red")


;FSM: FSM-State -> FSM-State
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
      (lambda (current)
        (overlay (text current 24 "white")
                 (square 100 "solid" current)))]
    [on-key
      (lambda (current key-event)
        (find transitions current))]))

;(simulate "red" fsm-traffic)
;(simulate "black" '(("black" "white") ("white" "black")))
;(simulate '(machine ((initial "black"))
                    ; (action ((state "black") (next "white")))
                     ;(action ((state "white") (next "black")))))



(define (xm->transitions xm)
  (local ((define (xaction->action xa)
            (list (find-attr 'state (xexpr-attr xa))
                  (find-attr 'next (xexpr-attr xa)))))
    (map xaction->action (xexpr-content xm))))

(xm->transitions '(machine ((initial "black"))
                     (action ((state "black") (next "white")))
                     (action ((state "white") (next "black")))))

                   
;simulate-xmachine: XMachine -> FSM-State
(define(simulate-xmachine xm)
  (local ((define xm-state0 (find-attr 'initial (xexpr-attr xm))))
    (simulate xm-state0 (xm->transitions xm) )))

;(simulate-xmachine '(machine ((initial "black"))
                    ; (action ((state "black") (next "white")))
                    ; (action ((state "white") (next "black")))))


     

(read-plain-xexpr/web
 (string-append
  "http://www.ccs.neu.edu/"
  "home/matthias/"
  "HtDP2e/Files/machine-configuration.xml"))


;<meta itemprop="price"
;        content="" />
;<meta itemprop="priceChange"
;        content="" />
;<meta itemprop="priceChangePercent"
;        content="" />
;<meta itemprop="isAfterHours"
;        content="">
;<meta itemprop="afterHoursPrice"
;        content="">

;; Source code for Google Finances


(define prefix "https://www.google.com/finance?q=")
(define SUFFIX "&btnG=Search")
(define SIZE 22) ; font size 

(define-struct data [price delta])


;get-xexpr: Xexpr V.3 string -> [Maybe String]
(define(get-xexpr s expr)
  (if (and (symbol=? (xexpr-name expr) 'meta) (string=? (find-attr 'itemprop (xexpr-attr expr)) s))
      (find-attr 'content (xexpr-attr expr))
      #false))
  

(get-xexpr "price" '(meta ((itemprop "price") (content "72.89"))))
(get-xexpr "priceChange" '(meta ((itemprop "price") (content "72.89"))))



(define aloXexpr '((meta ((itemprop "price") (content "72.89")))
                   (meta ((itemprop "priceChange") (content "-0.07")))
                   (meta ((itemprop "priceChangePercent") (content "-1%")))
                   (meta ((itemprop "isAfterHours") (content "Yes")))
                   (meta ((itemprop "afterHoursPrice") (content "72.44")))))

;get-xexpr-from-list: aloXexpr s -> [Maybe String]
(define(get-xexpr-from-list s alox)
  (cond
    [(empty? alox) #false]
    [(boolean? (get-xexpr s (first alox))) (get-xexpr-from-list s (rest alox))]
    [else (get-xexpr s (first alox))]))

(get-xexpr-from-list "price" aloXexpr)
(get-xexpr-from-list "afterHoursPrice" aloXexpr)
(get-xexpr-from-list "priceChange" aloXexpr)
;(get-xexpr-from-list "price" (read-xexpr/web "https://www.google.com/finance?q=AMD"))
     
;(define (stock-price company)
 ; (get-xexpr
 ; (read-xexpr/web
 ;  (string-append prefix company)) "price"))

;(stock-price "google")





