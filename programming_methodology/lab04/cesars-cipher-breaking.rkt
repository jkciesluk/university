#lang racket

(provide crack-caesar)

; pomocnicze funkcje rotate-left i zip
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

; funkcja caesar z zadania 4a
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
      
  ;; pusty słownik
  (define dict-empty null)

  ;; słownik po dodaniu do niego kolejnego wystapienia litery znaku 'k', drugi element pary to ilosc wystapien
  (define (dict-insert k d)
    (if (null? d)
        (list (cons k 1))
        (if (eq? k (caar d))
            (cons (cons k (+ 1 (cdar d)))
                  (cdr d))
            (cons (car d)
                  (dict-insert k (cdr d))))))
  ;;znajduje najczesciej wystepujacy element, zwraca pare (element, ilosc wystapien)
  (define (maximum d)
    (define x (cons (caar d) (cdar d)))
    (define (max-iter d x)
      (if (null? d)
          x
          (if (> (cdar d) (cdr x))
              (max-iter (cdr d) (cons (caar d) (cdar d)))
              (max-iter (cdr d) x))))
    (max-iter d (cons (caar d) (cdar d))))

  ;;tworzy z listy słownik
  (define (create-dict xs)
    (if (null? xs)
        dict-empty
        (dict-insert (car xs) (create-dict (cdr xs)))))

  ;; ===========
  ;; Rozwiązanie
  ;; ===========

  (define (crack-caesar alphabet by xs)
    (define m (car (maximum (create-dict xs))))
    (define (find-max l)
      (if (eq? m (car l))
          0
          (+ 1 (find-max (cdr l)))))
    (define (find-shift l)
      (if (not (eq? (car l) by))
          (find-shift (cdr l))
          (find-max l)))
    (define shift (find-shift (append alphabet alphabet)))  
    (define crack (cdr(caesar alphabet shift)))
    (define (decoded xs)
      (if (null? xs)
          null
          (cons (crack (car xs)) (decoded (cdr xs)))))
    (decoded xs)
    )
    

  ; Alfabety przydatne do wlasnych testow:

  (define az (list* #\space (string->list "abcdefghijklmnopqrstuvwxyz")))
  
  ; ======
  ; TESTY
  ; ======
 (define (enc k xs) (map (car  (caesar az k)) (string->list xs)))
 (list->string (crack-caesar az #\a (enc 3 "ala ma kota")))
 ; expected: "ala ma kota"

 ;testowe wywolanie enc:
 (list->string (enc 12 "juliusz slowacki to byl wielki polski poeta i pisal bardzo piekne wiersze")) 

 (list->string (crack-caesar az #\space (enc 12 "juliusz slowacki to byl wielki polski poeta i pisal bardzo piekne wiersze")))
 ; expected: "juliusz slowacki to byl wielki polski poeta i pisal bardzo piekne wiersze"

 (crack-caesar (list 1 2 3 4 5 6 7 8 9 0) 0 (list 1 2 3 4 4 2 2 2 2 6 8))
 ;shift: 2
 ;expected: 9 0 1 2 2 0 0 0 0 4 6

 (list->string (crack-caesar az #\space (enc 7 "szyfr cezara polega na szyfrowaniu kazdego znaku poprzez cykliczne przesuniecie jego wartosci o k miejsc w alfabecie w naszym wypadku w prawo")))
 ;expected: "szyfr cezara polega na szyfrowaniu kazdego znaku poprzez cykliczne przesuniecie jego wartosci o k miejsc w alfabecie w naszym wypadku w prawo"