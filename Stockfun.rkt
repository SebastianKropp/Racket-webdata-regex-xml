#lang racket


;//---------------Modules

(require net/uri-codec)
(require net/http-client)
(require net/url)
(require (planet neil/html-parsing:2:0) net/url)
(provide (all-defined-out))

;//---------------Stock Identifiers

;Identifier: Ticker -> Identifier
(define(identifier ticker)
  (string(string-ref ticker 0)))
;//Finds stock identifier ($,&) for given website data

;Apply-Identifiers
(define(apply-identifiers alot)
  (map (lambda (x) (string-append "&" x)) alot))
;//Adds an Identifier to a given stock

;Remove-Identifier: Ticker -> Ticker
(define(remove-identifier ticker)
  (substring ticker 1 (string-length ticker)))
;//Removes Identifier for url

;Apply-Tickers: alo-tickers alo-tickers -> alo-tickers
(define(apply-tickers aloo alot)
  (map (lambda (x y) (list x y)) aloo alot))
;//Combines two lists

;//---------------Website Scraping Auxiliaries

;Simplified->Complicated: Ticker -> String
(define(s->c ticker)
  (cond
    [(symbol? ticker) (s->c (symbol->string ticker))]
    [(string=? "$" (identifier ticker)) (string-append "http://www.stocktwits.com/symbol/"
                                                       (remove-identifier ticker))]
    [(string=? "&" (identifier ticker)) (string-append "https://unicornbay.com/t/"
                                                       (remove-identifier ticker)
                                                       ".US")]))
;//Checks stock Identifier and returns URL of Identifier

;Port-Website-Info String -> String
(define(port-website-info ticker)
  (cond
    [(symbol? ticker) (port-website-info (s->c ticker))]
    [else (port->string
           (get-pure-port (string->url ticker)))]))
;//Takes website data into a string

;//---------------Functions for Web Scraping Stock Information

;Stock-Price: Ticker -> N
(define(stock-price ticker)
  (local ((define positive-class (regexp-match #rx"<span class=positive>([0-9.]*)</span>"(port-website-info (s->c ticker))))
          (define negative-class (regexp-match #rx"<span class=negative>([0-9.]*)</span>"(port-website-info (s->c ticker)))))
  (cond
    [(symbol? ticker) (stock-price (symbol->string ticker))]
    [(string=? "$" (identifier ticker)) (string->number(second(regexp-match #rx"<span class='price'>([0-9.]*)</span>"(port-website-info (s->c ticker)))))]
    [(string=? "&" (identifier ticker))
     (cond
       [(boolean? negative-class) (string->number(second positive-class))]
       [(boolean? positive-class) (string->number(second negative-class))])]
    [else
     #false])))
;//Scrapes Stock-Price of Identifier URL

;Stock-Prices: alot -> alon
(define(stock-prices alot)
  (map (lambda (x) (stock-price x)) alot))
;//Returns a list of Stock-Prices

;Market-Capital: Ticker -> String
(define(cap ticker)
  (cond
    [(symbol? ticker) (cap (symbol->string ticker))]
    [(string=? "$" (identifier ticker)) (error "Not Enough Info Displayed on Stocktwits.com for Market Capital")]
    [(string=? "&" (identifier ticker)) (string-append "$" (second(regexp-match #rx"#36;([0-9.]*)"(port-website-info (s->c ticker)))) "B")]
    [else
     #false]))
;//Scrapes Market-Capital of Identifier URL

;Earnings-Per-Share: Ticker -> N
(define(eps ticker)
  (local ((define negative-eps (regexp-match #rx"',text:'&#36;([-][0-9.]+)',strength"(port-website-info (s->c ticker))))
          (define positive-eps (regexp-match #rx"',text:'&#36;([0-9.]+)',strength"(port-website-info (s->c ticker)))))
    (cond
      [(symbol? ticker) (eps (symbol->string ticker))]
      [(string=? "$" (identifier ticker)) (error "Not Enough Info Displayed on Stocktwits.com for EPS")]
      [(string=? "&" (identifier ticker))
       (cond
         [(boolean? negative-eps) (string->number(second positive-eps))]
         [(boolean? positive-eps) (string->number(second negative-eps))]
         [else (string->number (second negative-eps))])]
      [else
       #false])))
;//Scrapes Earnings-Per-Share of Identifier URL

;P/E Ratio: Ticker -> N
(define(p/e ticker)
  (local ((define price (stock-price ticker))
          (define earnings (eps ticker)))
  (cond
    [(symbol? ticker) (p/e (symbol->string ticker))]
    [(string=? "$" (identifier ticker)) (error "Not enough information displayed in order to estimate Price:Earnings")] 
    [(string=? "&" (identifier ticker)) (string->number(real->decimal-string(/ price earnings)))]
    [else
     #false])))
;//Scrapes P/E Ratio of Identifier URL

;Expected-Return: Ticker -> N (In Percentage)
(define(er ticker)
  (local ((define positive-er (regexp-match #rx"250,text:'([0-9.]*)" (port-website-info (s->c ticker))))
          (define negative-er (regexp-match #rx"250,text:'([-][0-9.]*)" (port-website-info (s->c ticker)))))
  (cond
    [(symbol? ticker) (er (symbol->string ticker))]
    [(string=? "$" (identifier ticker)) (error "Not enough information displayed in order to estimate Estimated Return")]
    [(string=? "&" (identifier ticker))
     (cond
       [(boolean? negative-er) (string->number(second positive-er))]
       [(boolean? positive-er) (string->number(second negative-er))]
       [else (string->number(second negative-er))])]
    [else
     #false])))
;//Scrapes Expected-Return of Identifier URL

;Beta: ticker -> N
(define(beta ticker)
  (cond
    [(symbol? ticker) (beta (symbol->string ticker))]
    [(string=? "$" (identifier ticker)) (error "Not enough information displayed in order to estimate Beta")]
    [(string=? "&" (identifier ticker)) (string->number(second(regexp-match #rx"Beta',period:1,text:'([0-9.]*)" (port-website-info (s->c ticker)))))]
    [else
     #false]))
;//Scrapes Beta of Identifier URL

;Wall-Street-Price: Ticker -> AloN (orig wall-street)
(define(wall-street-price ticker)
  (local ((define price-target (string->number(second(regexp-match #rx"<div class=keystatistics-item-value>&#36;([0-9.]*)" (port-website-info (s->c ticker)))))))
  (cond
    [(symbol? ticker) (wall-street-price (symbol->string ticker))]
    [(string=? "$" (identifier ticker)) (error "Not enough information displayed in order to estimate Beta")]
    [(string=? "&" (identifier ticker)) (list (stock-price ticker) price-target)]
    [else
      #false])))
;//Scrapes Wall-Street-Price of Identifier URL

;UB-Opinion: ticker -> string
(define(ub-opinion ticker)
  (local ((define single (regexp-match #rx"ticker-buy-sell text-uppercase\">\n<h3 class=dark-blue>\n([A-Z][a-z]*)\n</h3>"(port-website-info (s->c ticker))))
          (define double (regexp-match #rx"ticker-buy-sell text-uppercase\">\n<h3 class=dark-blue>\n([A-Za-z]*) ([A-Za-z]*)"(port-website-info (s->c ticker)))))
  (cond
    [(symbol? ticker) (ub-opinion (symbol->string ticker))]
    [else
     (cond
       [(boolean? single) (third double)]
       [else (second single)])])))
;//Scrapes AI opinion of Stock (UB)


;//---------------Stock Resources

;Trending: Boolean -> Alot
(define(trending boolean)
  (cond
    [(false? boolean) empty]
    [(not(false? boolean))
       (regexp-match* #rx"data-symbol='([A-Z]+)'"
                      (port-website-info "Http://www.Stocktwits.com")
                      #:match-select second)]))
;//Finds trending stocks on message volume 

;Available-Stock: ticker -> boolean
(define(available-stock ticker)
  (cond
    [(not(false? (regexp-match #rx"-resetcontent class=sf-reset>\n<h1>([A-Za-z]*)" (port-website-info (s->c ticker))))) #false]
    [else #true]))
;//Finds whether you can buy a stock

;produce-rStock: number -> alo-ticker
(define(produce-rStock n)
  (local ((define stock-name (second(regexp-match #rx"<p>STOCK:  ([A-Za-z]*)</p>" (port->string (get-pure-port(string->url "http://randomstock.net")))))))
  (cond
    [(zero? n) empty]
    [else (cons stock-name (produce-rStock (sub1 n)))])))
;//Produces random stocks based on number given














