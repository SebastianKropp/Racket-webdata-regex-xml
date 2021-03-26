#lang racket


;//---------------Modules (including Stock*.rkt files which are local)

(require data/heap)
(require net/url)
(require "Stockfun.rkt")
(require "StockRetrieval.rkt")
(provide (all-defined-out))

;//---------------Data Heaps for collection of words/filters

(define collection (make-heap string=?))
;//Receives words to be written
(define dictionary (make-heap string=?))
;//Dictionary of all blacklisted words


;//---------------Structures used

(define-struct wordc (word number)
  #:transparent)
;//Simple structure for organization of words in data


;//---------------Semi-Relevant Functions 

(define(heap->list heap)
  (vector->list(heap->vector heap)))
;//Converts the Heap into a list of strings

;count: string alos -> n
(define(count string alos)
  (cond
    [(empty? alos) 0]
    [(string=? string (first alos)) (add1 (count string (rest alos)))]
    [else (count string (rest alos))]))
;//Counts the recurrences of a string in a list

;remove-duplicates: alo -> alo
(define (remove-duplicates alo)
  (foldr
   (lambda (x y)
     (cons x (filter
              (lambda (z) (not (string=? (wordc-word x) (wordc-word z)))) y))) empty alo))
;//Removes duplicate strings from a list

;sum: alo -> alo
(define(sum alo)
 (foldr + 0 (map (lambda(x) (wordc-number x)) alo)))
;//Sums a list of wordc structures

;post-stock-format: string -> proper URL
(define(post-stock-format string)
  (string-append "https://webhose.io/search?token=72abfc3b-2f40-481c-a11f-3042bb44530d&format=xml&q=" string "%20Stock" "%20News"))
;//Formats a stock for WebHose.io api for retrieval of relevant data

;post-stock-formats: alos -> alourl
(define(post-stock-formats alos)
  (map (lambda(x) (post-stock-format x)) alos))
;//Formats a list of stocks for Webhose.io api for retrival of relevant data

;Webpages->Alowords: alourls -> alos
(define (webpages->alowords alourls)
  (append* (map (lambda (x) (webpage->alowords x)) alourls)))
;//Converts a list of urls to relevant words within <p> tags

;Webpage->Alowords: URL -> alos
(define (webpage->alowords url)
 (append-map string-split
              (regexp-match* #px"<p>([A-z 0-9 ` ~ ! @ # $ & % ^  * ( ) - _ = + | ; : ' , < . > ? ]+)</p>" (port-website-info url) #:match-select cadr)))
;//Converts a webpage (arbitrary data) to words in html with <p> tag

;String->alowords: string -> alos
(define(string->alowords string)
  (append-map string-split
              (regexp-match* #px"<text>([A-z 0-9 ` ~ ! @ # $ & % ^ * - _ = + | ; : ' , < . > ? / {} () \\ \n \nPS ]+)</text>" string #:match-select cadr)))
;//Converts a string of arbitrary data to alowords

;count-words: alos -> alos  
(define(count-words alos)
  (remove-duplicates
   (map
    (lambda(x) (make-wordc (filter-specials x) (count (filter-specials x) alos))) alos)))
;//Converts a list of strings to a wordc list with number of occurences 

;percentage: alo -> alo
(define(percentage alo)
  (map (lambda(x)
         (make-wordc (wordc-word x)
                     (* 100 (exact->inexact(/ (wordc-number x) (sum alo)))))) alo))
;//Converts a list of strings to a wordc list with percentage of occurences 

;post-webhose: proper URL -> XML String
(define(post-webhose url)
  (port->string (post-pure-port (string->url (post-stock-format url)) (string->bytes/utf-8 (post-stock-format url)))))
;//Sends a POST to Webhose.io/api in order to retrieve relevant data in XML

;post-webhose: alourls -> Racket String File
(define(post-webhoses alos)
  (map (lambda (x) (post-webhose (post-stock-format x))) alos))
;//Sends POSTS in 1 second or longer intervals and retrieves relevant data into XML 

;filter-special: string -> word
(define(filter-specials s)
 (list->string
  (filter (lambda(x) (not (or (eq? x #\.)
                              (eq? x #\/)
                              (eq? x #\`)
                              (eq? x #\~)
                              (eq? x #\!)
                              (eq? x #\@)
                              (eq? x #\#)
                              (eq? x #\$)
                              (eq? x #\%)
                              (eq? x #\^)
                              (eq? x #\&)
                              (eq? x #\*)
                              (eq? x #\()
                              (eq? x #\))
                              (eq? x #\-)
                              (eq? x #\_)
                              (eq? x #\=)
                              (eq? x #\+)
                              (eq? x #\[)
                              (eq? x #\])
                              (eq? x #\:)
                              (eq? x #\;)
                              (eq? x #\')
                              (eq? x #\")
                              (eq? x #\.)
                              (eq? x #\>)
                              (eq? x #\<)
                              (eq? x #\,)
                              (eq? x #\?)
                              (eq? x #\/)
                              (eq? x #\|)
                              (eq? x #\\))))
          (string->list s))))
;//Used to filter specials out of a list of strings


;//---------------Main Functions

;write-to-file: Filename(string) Number -> XML File
(define(write-to-file string n)
  (with-output-to-file string (lambda()
                                (write (post-webhoses
                                        (produce-rStock n))))))
;//Writes random stocks with relevant articles into XML with given filename

;read-from-file: Filename(string)  Filename(string) -> File
(define(read-from-file string1 string2)
  (with-output-to-file string2 (lambda()
                                (write (string->alowords
                                         (apply string-append
                                                (first (file->list string1))))))))
;//Reads from a file and writes contents to a file filtering irrelevant data

;apply-structure: Filename(string) Filename(string) -> File
(define(apply-structure string1 string2)
  (with-output-to-file string2 (lambda()
                                 (write `( ,(count-words (first(file->list string1)))
                                           ,(percentage (filter-specials(file->list string1))))))))
;//Reads from a file and writes contents to a file adding structure (assumes irrelevant data has been removed)

;//---------------Order Example

;(write-to-file "RawData.txt" 500)
;//Writes articles with relevant information from 500 random stocks to a file

;(read-from-file "RawData.txt" "RefinedData.txt")
;//Writes and filters irrelevant XML structure to a list of strings

;(apply-structure "RefinedData.txt" "StructuredData.txt")
;//Writes and structures a list of strings to wordc structure with word count and percentage density






















