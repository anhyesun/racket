;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname graded-09-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
#|
DO NOT PUT ANY PERSONALLY IDENTIFYING INFORMATION IN THIS FILE. 
YOUR COMPUTER SCIENCE IDs WILL BE SUFFICIENT TO IDENTIFY YOU 
AND, IF YOU HAVE ONE, YOUR PARTNER

Computer Science 110
2018W1

Graded Problem Set 9

Computer Science id (Student 1): 
Computer Science id (Student 2): ________
|#
;;
;; PROBLEM SETUP
;;
;; In this problem assume an online shopping website (like Amazon) is
;; doing a promotion where they are giving users gift certificates.
;; The gift certificate can be used to buy items from the online store.
;; Your task is to write some "Smart Shopping" software to aid the
;; people who have won a gift certificate use it as efficiently as possible.
;;
;; For this problem set, we are making the simplifying assumption
;; that all items found in the online store simply have a name,
;; and a price (in US dollars).
;;
;; You may also assume that the winners all have a list of items
;; they have selected that they are interested in buying (a wish-list).
;; The goal of smart shopping software is to search through this list of
;; wish-list items, and see if there is a set of those items that
;; can be moved to a shopping cart and purchased for a price exactly equal
;; to the gift certificate amount. You have been provided with a Session data
;; definition to help you separate wish-list items from those in a shopping
;; cart.
;;
;; An important part of the problem is that your software should
;; generate a list of items if the sum of their values is a
;; PERFECT match with the value of the gift certificate.
;;
;; For example: Someone given a $20 gift certificate would want
;; to place items into the cart that sum to $20. One $20 item
;; is valid, as would five items costing $2, $2, $3, $6, and $7.
;; In both cases, the sum of all item prices equals exactly $20.
;;
;; On the other hand, if the gift certificate was for $50, the
;; item prices would need to sum to exactly $50 in order to
;; spend the full certificate value.
;;
;; If there is no sum of item prices that match the gift certificate
;; value in the wish-list then your function should produce false.
;;
;; Please read through the following data definitions for
;; a StoreItem and StoreSession.
;;

(@HtDD StoreItem)
(define-struct item (name price))
;; StoreItem is (make-item String Natural)
;; interp. an item found on an online store
;;         with a name and price in US dollars
(define I1  (make-item "scrapbook"     2))
(define I2  (make-item "novel"         9))
(define I3  (make-item "music album"   8))
(define I4  (make-item "watch"        13))
(define I5  (make-item "earbuds"       3))
(define I6  (make-item "sunglasses"    5))
(define I7  (make-item "phone charger" 2))

(define NO-ITEMS empty)
(define SOME-ITEMS (list I1 I2 I3))
(define ALL-ITEMS (list I1 I2 I3 I4 I5 I6 I7))

(define (fn-for-item i)
  (... (item-name i)
       (item-price i)))

(@HtDD StoreSession)
(define-struct session (cart wishlist))
;; StoreSession is (make-session (listof StoreItem) (listof StoreItem))
;; interp. a session using a shopping website
;;  cart is the list of items ready for purchase with the gift certificate
;;  wishlist is the list of items the user is interested in
;;
;; An item will only ever be in the wishlist or the cart, not both.
;;
(define S0 (make-session empty empty))
(define S1 (make-session (list I1) (list I2 I3)))

(define (fn-for-session s)
  (... (fn-for-loi (session-cart s))
       (fn-for-loi (session-wishlist s))))


;;
;; PROBLEM 1
;;
;; Given a wish-list of items a user is interested in, and the value of a gift
;; certificate in US dollars, find the list of items that can be purchased with
;; prices that add up to exactly the gift certificate value, if such a
;; collection exists. Produce false if there is no set of items found in
;; the wish-list with prices that sum to exactly the gift certificate value.
;;
;; You must solve this problem as a backtracking search over a generated binary
;; tree. You are expected to use what you have learned up to and including
;; Module 9b only. In particular, do NOT use accumulators. 
;;
(@Problem 1)
(@HtDF smart-shop)
(@signature Natural StoreSession -> (listof StoreItem) or false)
;; produce satisfied list of item matches with sum of items and given n
(check-expect (smart-shop 20 (make-session empty empty)) false)
(check-expect (smart-shop 20 (make-session (list) (list I2 I3))) false)
(check-expect (smart-shop 20 (make-session (list I2 I3) (list))) false)
(check-expect (smart-shop 19 (make-session (list I2 I3) (list I4 I5))) false)
(check-expect (smart-shop 11 (make-session (list I1 I2) (list I3)))
              (list I1 I2))
(check-expect (smart-shop 20 (make-session (list) ALL-ITEMS))
              (list I1 I3 I5 I6 I7))
(check-expect (smart-shop (+ 2 9 8 13 3 5 2) (make-session (list) ALL-ITEMS))
              ALL-ITEMS)

;(define (smart-shop n s) empty) ; stub 
(@template genrec bin-tree backtracking StoreSession add-param)

(define (smart-shop n s)
  (cond [(match-val? n s) (session-cart s)]
        [(no-match? s) false]
        [else
         (local [(define try (smart-shop n (pick-i s)))]
           (if (not (false? try))
               try
               (smart-shop n (skip-i s))))]))


(@HtDF match-val?)
(@signature Natural StoreSession -> Boolean)
;; produce true if sum of session-cart equals with n
(check-expect (match-val? 20 (make-session empty empty)) false)
(check-expect (match-val? 20 (make-session (list I2) (list I3 I4))) false)
(check-expect (match-val? 9  (make-session (list I2) (list I3 I4))) true)
(check-expect (match-val? 20 (make-session (list I1 I4 I5 I7) (list))) true)

;(define (match-val? n s) false)
(@template StoreSession)
(define (match-val? n s)
  (= n (total (session-cart s))))


(@HtDF total)
(@signature (listof StoreItem) -> Natural)
;; produce total price of given list of items
(check-expect (total (list I1 I2)) (+ 2 9))
(check-expect (total (list I1 I2 I3)) (+ 2 9 8))

(@template StoreItem)
(define (total loi)
  (cond [(empty? loi) 0]
        [else
         (+ (item-price (first loi))
            (total (rest loi)))]))

(@HtDF no-match?)
(@signature StoreSession -> Boolean)
;; produce true if session-wishlist is empty
(check-expect (no-match? (make-session (list I1) (list))) true)
(check-expect (no-match? (make-session (list I1) (list I2))) false)
;(define (no-match? s) false) ; stub

(@template StoreSession)
(define (no-match? s)
  (empty? (session-wishlist s)))


(@HtDF pick-i)
(@signature StoreSession -> StoreSession)
;; pick first wishlist item
(check-expect (pick-i (make-session (list) (list I1)))
              (make-session (list I1) (list)))
(check-expect (pick-i (make-session (list I1) (list I2)))
              (make-session (list I1 I2) (list)))
;(define (pick-i s) empty) ; stub

(@template StoreSession)
(define (pick-i s)
  (make-session (append (session-cart s)
                        (list (first (session-wishlist s))))
                (rest (session-wishlist s))))

(@HtDF skip-i)
(@signature StoreSession -> StoreSession)
;; skip first wishlist item
(check-expect (skip-i (make-session (list) (list I1 I2)))
              (make-session (list) (list I2)))
(check-expect (skip-i (make-session (list I1) (list I2)))
              (make-session (list I1) (list)))
;(define (skip-i s) empty) ; stub

(@template StoreSession)
(define (skip-i s)
  (make-session (session-cart s)
                (rest (session-wishlist s))))