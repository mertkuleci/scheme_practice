#lang scheme

;Q-1: Write a Scheme function called reverse-list that takes a list
;as input and returns a new list with the elements in reverse order.
;For example, calling (reverse-list '(1 2 3 4 5)) should return (5 4 3 2 1).

(define (reverse-list-helper lst acc)
  (if (null? lst)
      acc
      (reverse-list-helper (cdr lst) (cons (car lst) acc))))

(define (reverse-list lst)
  (reverse-list-helper lst '()))

;Q-2: Write a Scheme procedure called multiply-elements that takes a
;list of numbers as input and returns the product of all the elements in the list

(define (multiply-elements lst)
           (if (null? lst)
               1
               (* (car lst) (multiply-elements (cdr lst)))))

;Q-3: Write a procedure list-length that takes a list as input and returns
;the length of the list (the number of elements in the list).

(define (list-length lst acc)
   (if (null? lst)
      acc
      (list-length (cdr lst) (+ 1 acc))))

;Q-4: Write a Scheme procedure named count-vowels
;that takes a string as input and returns the
;count of vowels in the string. For this exercise,
;consider only lowercase vowels (a, e, i, o, u).
;You may assume that the input string contains only
;lowercase letters and no punctuation or spaces.
;For example:
;(count-vowels "hello")   ; should return 2
;(count-vowels "world")   ; should return 1
;(count-vowels "openai")  ; should return 3
;(count-vowels "scheme")  ; should return 2

(define (count-vowels str)
  (if (null? str)
      0
      (let ((first-char (string-ref str 0)))
        (+ (if (vowel? first-char) 1 0)
           (count-vowels (substring str 1)))
        )
      )
  )

(define (vowel? char)
  (cond
    ((char=? char #\a) #t)
    ((char=? char #\e) #t)
    ((char=? char #\i) #t)
    ((char=? char #\o) #t)
    ((char=? char #\u) #t)
    (else #f)
  )
)


;Q-5: Write a Scheme procedure called merge-sort that takes a list of integers as input
;and returns a new list with the integers sorted in ascending order using the merge sort algorithm.

;The merge sort algorithm works as follows:

;1)If the list has zero or one element, it is already sorted, so return the list.
;2)Otherwise, divide the list into two halves.
;3) Recursively apply the merge-sort procedure to each half.
;4) Merge the two sorted halves to produce a new sorted list.

;Here's an example of how the merge-sort procedure should work:
;(merge-sort '(4 2 7 1 5))  ; Output: (1 2 4 5 7)
;(merge-sort '(9 3 6 2 8))  ; Output: (2 3 6 8 9)
;(merge-sort '(5 1 2 3 4))  ; Output: (1 2 3 4 5)


(define (merge-sort lst)
  (if (<= (length lst) 1)
      lst
      (let* ((midpoint (quotient (length lst) 2))
             (left (take lst midpoint))
             (right (drop lst midpoint)))
        (merge (merge-sort left) (merge-sort right)))))

(define (merge left right)
  (if (null? left)
      right
      (if (null? right)
          left
          (if (<= (car left) (car right))
              (cons (car left) (merge (cdr left) right))
              (cons (car right) (merge left (cdr right)))))))


;Q-6: Write a Scheme procedure called flatten-list that takes a
;nested list as input and returns a flattened list containing
;all the elements of the input list in a linear, non-nested form.
;The order of elements should be preserved.

;For example, given the input (flatten-list '(1 (2 (3)) 4 (5 (6 7)) 8)),
;the expected output should be (1 2 3 4 5 6 7 8).

;You can assume that the input list will only contain nested lists
;and atomic values (integers, symbols, etc.).

(define (flatten-list lst acc)
  (if (null? lst)
      acc
      (if (pair? (car lst))
          (flatten-list (car lst) (flatten-list (cdr lst) acc))
          (flatten-list (cdr lst) (cons (car lst) acc)))))

(define (flatten lst)
  (reverse (flatten-list lst '())))


;Q-7: Write a Scheme procedure called remove-duplicates that takes a list as
;input and returns a new list with all duplicate elements removed, preserving
;the original order of the elements. For example, given the input list (1 2 3 2 1 4 4 5),
;the procedure should return (1 2 3 4 5).

;You can assume that the input list will only contain integers.

(define (remove-duplicates lst acc)
  (if (null? lst)
      acc
      (if (member (car lst) acc)
          (remove-duplicates (cdr lst) acc)
          (remove-duplicates (cdr lst) (cons (car lst) acc)))))


;Q-8: Write a procedure called filter-odd-squares that takes a list of
;integers as input and returns a new list containing only the odd squares
;of the integers in the input list. Implement this using lambda expressions.

;For example: 
;(filter-odd-squares '(1 2 3 4 5 6 7 8 9 10))
;the expected output should be (1 9 25 49 81).

(define (filter-odd-squares lst)
  (if (null? lst)
      '()
      (let ((current (car lst))
            (rest (cdr lst)))
        (if (odd? current)
            (cons (* current current) (filter-odd-squares rest))
            (filter-odd-squares rest)))))

;Q-9: Write a Scheme procedure called distinct-pairs that takes a list of
;integers as input and returns a list of all distinct pairs of integers
;from the input list. Each pair should be represented as a list containing
;two elements.
;For example, given the input list (1 2 3), the output should be ((1 2) (1 3) (2 3)),
;as it represents all distinct pairs from the input list.

(define (distinct-pairs lst)
  (define (pairs-helper x lst)
    (if (null? lst)
        '()
        (cons (list x (car lst))
              (pairs-helper x (cdr lst)))))
  
  (cond ((null? lst) '())
        ((null? (cdr lst)) '())
        (else (append (pairs-helper (car lst) (cdr lst))
                      (distinct-pairs (cdr lst))))))

;Q-10: Write a Scheme procedure called count-occurrences that takes two inputs: a list lst
;and an element x. The procedure should return the count of occurrences of x in the
;list lst, considering both the top-level elements and the nested sublists.

;For example, given the input list '(1 (2 3) 4 (1 2) 1), and the element 1,
;the output should be 3, as 1 appears three times in the list.

(define (count-occurrences lst x)
  (if (null? lst)
      0
      (+
       (if (equal? (car lst) x) 1 0)
       (count-occurrences (cdr lst) x)
       (if (pair? (car lst))
           (count-occurrences (car lst) x)
           0))))


;Q-11: Below, a procedure named \get-numbers-only" is given, which has a single
;parameter named \lst". Assume that \lst" is a list (no need to check if it is really a list or not). The
;procedure produces the same list as \lst" where all the items which are not numbers are dropped, but
;the elements of \lst" which are numbers are kept in the same order as given in \lst". You can also
;see some example use of this procedure below. Note that if an element of \lst" is sublist, this sublist
;is also dropped. Please ll{in the blanks to implement this procedure.


;Value: get-numbers-only
;1 ]=> (get-numbers-only '(1 2 3 2))
;Value 13: (1 2 3 2)
;1 ]=> (get-numbers-only '(1 a -3.7 b))
;Value 14: (1 -3.7)
;1 ]=> (get-numbers-only '(1 a (2 3) b 4 c (5) d (e 6)))
;Value 14: (1 4)
;1 ]=> (get-numbers-only '())
;Value: ()
;1 ]=> (get-numbers-only '(a b c))
;Value: ()

(define get-numbers-only
  (lambda (lst)
    (if (null? lst)
        '()
        (if (number? (car lst))
            (cons (car lst) (get-numbers-only (cdr lst)))
            (get-numbers-only (cdr lst))))))




;Q-12: Suppose that we have a list of integers called intList. Let positiveSum be the
;sum of the positive integers in the intList, and let negativeSum be the sum of the negative integers
;in the intList. For example, if we have intList as the list (-2 -1 3 0 5 -1), then positiveSum is
;8 (positive numbers are 3 and 5) and negativeSum is -4 (negative numbers are -2, -1 and -1).
;Write a Scheme function named processList which will take a list of integers intList and return
;the result (2  positiveSum) + negativeSum. For example, if intList is (-2 -1 3 0 5 -1), then we
;would get 12 (because (2  8) + 􀀀4 = 12) as the result.
;When intList is empty, processList should return 0.
;You are guaranteed to have intList as a list of integers (you don't need to check).
;Rules: processList must not use an accumulator. Do not use any let or let* statements. Do not introduce any other function/procedure.

(define processList
  (lambda (intList)
    (if (null? intList)
        0
        (+ (* 2 (apply + (filter positive? intList)))
           (apply + (filter negative? intList))))))


;Q-13: Write a Scheme function called calculate-discount that takes a list of prices and applies a discount based on the price range. The function should perform the following:

;If the price is less than $10, apply a discount of 5%.
;If the price is between $10 and $50 (inclusive), apply a discount of 10%.
;If the price is greater than $50, apply a discount of 15%.
;The function should return a new list with the discounted prices.

;For example, given the input list (12 6 40 60), the function should return (11.4 5.7 36.0 51.0) after applying the respective discounts.7

(define (calculate-discount prices)
  (map
   (lambda (price)
     (cond
      ((< price 10) (* price 0.95))  ; Apply 5% discount for prices < $10
      ((<= price 50) (* price 0.9))   ; Apply 10% discount for prices between $10 and $50
      (else (* price 0.85))))        ; Apply 15% discount for prices > $50
   prices))
