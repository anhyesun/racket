;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname graded-10-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
#|
DO NOT PUT ANY PERSONALLY IDENTIFYING INFORMATION IN THIS FILE. 
YOUR COMPUTER SCIENCE IDs WILL BE SUFFICIENT TO IDENTIFY YOU 
AND, IF YOU HAVE ONE, YOUR PARTNER

Computer Science 110
2018W1

Graded Problem Set 10

Computer Science id (Student 1): 
Computer Science id (Student 2): ________
|#

(@Problem 1)
;;
;; Complete the design of the function called longest,
;; which produces the longest string in a list of strings.
;; You can assume the list contains at least one string
;;
;; Your function MUST BE TAIL-RECURSIVE.
;;
;; Please note the following:
;;  - if multiple strings tie for the longest, produce the
;;    string that appeared first in the list.
;;  - Note the longer? helper function that you may wish to use
;;

(@HtDF longest)
(@signature (listof String) -> String)
;; produce the longest string in los
(check-expect (longest (list "")) "")
(check-expect (longest (list "ab")) "ab")
(check-expect (longest (list "abc" "abcd" "ab" "a")) "abcd")

;(define (longest los) "")   ;stub
(@template (listof String) accumulator)
(define (longest los)
  ;; acc is String: the longest string seen so far
  ;; (longest (list "ab" "abc" "abcd") "")
  ;; (longest (list      "abc" "abcd") "ab")
  ;; (longest (list            "abcd") "abc")
  ;; (longest (list                  ) "abcd")
  (local [(define (longest los acc)
            (cond [(empty? los) acc]
                  [else
                   (if (longer? acc (first los))
                       (longest (rest los) acc)
                       (longest (rest los) (first los)))]))]
    (longest los "")))  

(@HtDF longer?)
(@signature String String -> Boolean)
;; produce true if s1 is longer than s2
(check-expect (longer? "abc"  "de") true)
(check-expect (longer? "ab"  "cde") false)
(check-expect (longer? "abc" "def") false)

(@template String add-param)
(define (longer? s1 s2)
  (> (string-length s1) (string-length s2)))


(@Problem 2)
;;
;; Design a function that consumes a list of natural numbers, and
;; produces the number of them that are in a sequence increasing by
;; 1 starting at the beginning.
;;
;; ASSUME: the list contains at least 1 element.
;; Your function MUST BE TAIL-RECURSIVE.
;;
;; Example:
;;  - (list 5) produces 1
;;  - (list 1 2 3 7 8) produces 3
;;  - (list 8 10 11 12) produces 1
;;
(@HtDF countsequence)
(@signature (listof Natural) -> Natural)
;; produce the number of lon are increasing by 1 from beginning.
;; ASSUME: the list contains at least 1 element
(check-expect (countsequence (list 5)) 1)
(check-expect (countsequence (list 1 2 3 7 8)) 3)
(check-expect (countsequence (list 8 10 11 12)) 1)
(check-expect (countsequence (list 3 4 5 6)) 4)

;(define (countsequence lon) 0) ; stub
(@template (listof Natural) accumulator)
(define (countsequence lon)
  ;; acc is Natural: count the number of sequence by 1 seen so far 
  ;; (countsequence (list 1 2 3 7 8) 1)
  ;; (countsequence (list   2 3 7 8) 2) 
  ;; (countsequence (list     3 7 8) 3) 
  ;; (countsequence (list       7 8) 3) 
  ;; (countsequence (list         8) 3)
  ;; (countsequence (list          ) 3)
  (local [(define (countsequence lon acc)
            (cond [(empty? lon) acc]
                  [else
                   (if (and (not (empty? (rest lon)))
                            (increaseby1? (first lon) (first (rest lon))))
                       (countsequence (rest lon) (add1 acc))
                       (countsequence empty acc))]))
          
          (define (increaseby1? n1 n2)
            (= (+ 1 n1) n2))]
    
    (countsequence lon 1)))




;; This next part is for problem 3.

(@HtDD Region)
(define-struct single (label weight color))
(define-struct group (color subs))
;; Region is one of:
;;  - (make-single String Natural Color)
;;  - (make-group Color ListOfRegion)
;; interp.
;;  an arbitrary-arity tree of regions
;;  single regions have label, weight and color
;;  groups just have a color and a list of sub-regions
;;
;;  weight is a unitless number indicating how much weight
;;  given single contributes to whole tree

;; All the Ss and Gs are Regions
(define S1 (make-single "A" 20 "red"))
(define S2 (make-single "B" 40 "blue"))
(define S3 (make-single "C" 60 "orange"))
(define S4 (make-single "D" 30 "red"))
(define S5 (make-single "E" 10 "blue"))
(define S6 (make-single "F" 80 "yellow"))

(define G1 (make-group "red"    (list S1 S2 S3)))
(define G2 (make-group "blue"   (list G1 S4)))
(define G3 (make-group "orange" (list S5 S6)))
(define G4 (make-group "black"  (list G2 G3)))

(define (fn-for-region r)
  (local [(define (fn-for-region r)
            (cond [(single? r)
                   (... (single-label r)
                        (single-weight r)
                        (single-color r))]
                  [(group? r)
                   (... (group-color r)
                        (fn-for-lor (group-subs r)))]))

          (define (fn-for-lor lor)
            (cond [(empty? lor) (...)]
                  [else
                   (... (fn-for-region (first lor))
                        (fn-for-lor (rest lor)))]))]

    (fn-for-region r)))


(@Problem 3)
;;
;; Please read over the data definition for a Region provided above,
;; which you have used multiple times this semester.
;;
;; Design a function that consumes a region and produces the names of any
;; single regions that have the same color as one of their ancestors.
;;
;; Ancestor means a parent, or a parent of a parent, or a parent of a parent
;; of a parent, etc.
;;
;; For example, the tree with root G4 should produce (list "A" "B"):
;;  - S1 has the same color (red) as it's parent G1.
;;  - S2 also has the same color (blue) as it's parent's parent, G2.
;;  - No other single regions have the same color as an ancestor
;;
;; Your function MUST NOT BE TAIL-RECURSIVE.
;; Any tail-recursive solutions WILL NOT RECEIVE ANY MARKS
;;


;(@HtDF ancester)
(@signature Region -> String)
;; produce a list of single regions' name when they have same color of parents
(check-expect (ancester empty) (list))
(check-expect (ancester S4) (list))
(check-expect (ancester G3) (list))
(check-expect (ancester G4) (list "A" "B"))
(check-expect (ancester G1) (list "A"))
(check-expect (ancester G2) (list "A" "B"))

;(define (ancester r) empty) ; stub

(@template Region (listof String) accumulator)
(define (ancester r)
  ;; acc is (list of String): name of regions that have same color of parents
  ;; rsf is (list of Color): parents' color seen so far 
  (local [(define (fn-for-region r acc rsf)
            (cond [(empty? r) empty]
                  [(single? r)
                   (if (member? (single-color r) rsf)  
                       (cons (single-label r) acc) 
                       acc)]
                  [(group? r)
                   (fn-for-lor (group-subs r)
                               acc
                               (append rsf
                                       (list (group-color r))))]))

          (define (fn-for-lor lor acc rsf)
            (cond [(empty? lor) empty]
                  [else
                   (append (fn-for-region (first lor) acc rsf)
                           (fn-for-lor (rest lor) acc rsf))]))]

    (fn-for-region r empty empty)))
