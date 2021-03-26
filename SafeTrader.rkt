#lang racket
;The Safe Trader

;//---------------Required Local Files

(require "Stockfun.rkt")

;//---------------Structures Used (Only used during Holding)

(define-struct stockstruct (ticker price shares))
;//Where ticker is the ticker of the stock
;//Price is the original price bought
;//Shares is the current amount of shares purchased in Holding
;//Ex (make-stock ("AMD" 6.20 50))

(define holdingtest (list
                     (make-stockstruct "AMD" 6.20 50)
                     (make-stockstruct "NVDA" 40 200)
                     (make-stockstruct "GOOGL" 770 5)))
;//In reality it will be held in a separate file 

;//---------------Metric Determinations
;//Takes in normal data as a numeric value
;//This shows value of a stock based on data
;//Ex: Ideal stock is 1
;//Contributing factors can include
;//An excellent expected return which might add a value of .25


;Market-Capital:
;Earnings-Per-Share: 
;P/E Ratio:
;Expected Return
;Beta:
;Wall-Street-Price:
;UB-Opinion:
;Newspaper-Positivity:
;


;//---------------Stock Holding Functions

;price-change:
;held-for:
;value-persistent?
;growth?
;


;//---------------Communications

;Receive-Email
;Processing
;Send-Email


;//---------------Main Functions

;Stock-Recommendation: ticker -> string
(define(stock-recommendation ticker)
  (cond
    [(symbol? ticker) (stock-recommendation (symbol->string ticker))]    
    [(string=? "&" (identifier ticker))
     (cond
       [(false? (available-stock ticker)) "NotAvailable"]
       [(false? #t) #f]
       [else "Don'tBuy"])]))
;//Finds whether a stock should be bought based on metrics


;Stock-Recommendations: boolean -> alo-opinions
(define(stock-recommendations alot)
  (apply-tickers (map (lambda (x) (stock-recommendation x)) (apply-identifiers alot)) alot))
;//Finds whether the trending should be bought

;Check-Holding Portfolio-Stocks -> alot
(define(check-holding alot)
 (local ((define alotickers (map (lambda(x) (stockstruct-ticker x)) alot)))
  (cond
    [(list? alot) (filter
                   (lambda (x) (string? (second x)))
                   (filter
                    (lambda(x) (or (string=? (first x) "Don'tBuy")
                                   (string=? (first x) "Sell")))
                    (stock-recommendations alotickers)))]
    [else empty])))
;//Creates a state for all current stocks in holding

;Sell-Holding: holding-state -> holding
(define(sell-holding alot)
  (map (lambda(y) (second y)) (filter (lambda(x) (string=? "Sell" (first x))) alot)))
;//Takes in a holding-state and filters the list for tickers that need to be sold
  
































