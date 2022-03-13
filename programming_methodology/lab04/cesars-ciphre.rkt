#lang racket

(provide caesar)


; Alfabety przydatne do wlasnych testow:

(define az (list* #\space (string->list "abcdefghijklmnopqrstuvwxyz")))

;pomocnicze funkcje zip i rotate 
(define (rotate-left xs n)
  (define l (length xs))
  (if (= l 0)
      null
      (let ((k (modulo n l)))
           (append (drop xs k) (take xs k)))))

(define (zip xs ys)
  (if (null? xs)
      null
      (cons (cons (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))

;ROZWIAZANIE

(define (caesar alphabet key)
  (define cipher-xs (zip  alphabet (rotate-left alphabet key)))
  (define (encode x)
    (define (encode-iter cipher)
    (if (null? cipher)
        null
        (if (eq? (caar cipher) x)
            (cdar cipher)
            (encode-iter (cdr cipher)))))
    (encode-iter cipher-xs))
  (define (decode x)
    (define (decode-iter cipher)
    (if (null? cipher)
        null
        (if (eq? (cdar cipher) x)
            (caar cipher)
            (decode-iter (cdr cipher)))))
    (decode-iter cipher-xs))
  (cons encode decode))
        
;TESTY
 (define (enc k xs) (map (car  (caesar az k)) (string->list xs)))
 (define (dec k xs) (map (cdr  (caesar az k)) (string->list xs)))
 (list->string (enc 5 "slowacki to byl wspanialy polski poeta"))
;expected : "xqtafhpneytegcqeaxufsnfqceutqxpneutjyf"
 (list->string (dec 5 "xqtafhpneytegcqeaxufsnfqceutqxpneutjyf"))
;expected :  "slowacki to byl wspanialy polski poeta"
 (list->string (enc 100 "lubie metody programowania"))
;expected : "dmuaxsexlgwqshjgzjtegotfat"
(list->string (dec 100 "dmuaxsexlgwqshjgzjtegotfat"))
;expected : "lubie metody programowania"
 (list->string (enc -15 "album"))
;expected : "mxnfy"
 (list->string (dec -15 "mxnfy"))
;expected : "album"