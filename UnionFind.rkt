;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname unionfind-refined) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
#|
Author: Joe Severini
Written in ASL (advanced student language) in Racket.

Implements a union-find data structure (ie. disjoint-set data structure) with path compression and weighting.
Public functions: (UF = union-find) (N = Real Number)
  uf:create      N -> UF;             creates a union-find of size N (an array of size N UnionEntry structs)
  uf:size        UF -> N              returns the size of the union-find
  uf:same-set?   UF N N -> Bool       are the two items connected? (checks if the base of the sets that the items
                                      are in are the same)
  uf:find        UF N -> N            returns the base of the set that N is in; path compression is implemented here
  uf:union!      UF N N -> Void       finds both bases. If not the same, set the parent of the 'less heavy' base
                                      to be the 'heavier' base.

Time complexity:
 uf:create           O(n)                  Must initialize all N UnionEntries
 uf:size             O(1)                  Returns the size variable from the UF
 uf:find        amortized O(α(n))          complicated, see other sources
 uf:same-set?   amortized O(α(n))          uses uf:find twice, which are amortized O(α(n))
 uf:union       amortized O(α(n))          complicated, see other sources

NOTE: While this was assigned as a homework assignemnt for EECS214 at Northwestern University in the Fall of 2015, ALL code (including comments, function descriptions, 
the testing apparatus, and the tests as well as the actual code) are the sole work of the author.
 
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Union-find functions (i.e. public functions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct UnionFind [size data]) 
(define-struct UnionEntry [weight parent])

; uf:create : N -> UF
; creates a union-find structure of size 'size'. All sets are initialized with the parent being
; equal to itself. (i.e. all sets are initially disjoint). Numbered 0 to N-1.
(define (uf:create size)
  (make-UnionFind size (build-vector size makeUnionEntry)) )
(define (makeUnionEntry n)
  (make-UnionEntry 1 n))

; uf:size : UF -> N
; Returns the number of objects in the UF. (NOT the number of sets)
(define (size uf)
  (UnionFind-size uf))

(check-expect (size (uf:create 10)) 10)
(check-expect (size (uf:create 100)) 100)

; uf:same-set? : UF N N -> Bool
; Finds the base of each set and checks if they are equal (i.e. are in the same set)
(define (uf:same-set? uf obj1 obj2)
 (= (uf:find uf obj1) (uf:find uf obj2)) )    

; uf:find : UF N -> N
; Finds finds the base of the set of object 'obj' by recursively going to the parent. If the 
; parent is equal to the obj, then it is the base of the set. Also implements path compression-
; sets the parent of each examined node to the base of the set.
(define (uf:find uf obj)
  (cond
    [(= (uf:get-parent uf obj) obj) obj]        ;; at the root, can return
    [(not (=  (uf:get-parent uf obj) obj))
     (begin
       (set-UnionEntry-parent! (uf:get-entry uf obj) (uf:find uf (uf:get-parent uf obj)) ) ;; path compression
       (uf:get-parent uf obj))]
))

; uf:union! : UF N N -> Void
; Merges the set of 'obj1' with the set of 'obj2'. Does this by finding the base of each set. If the bases
; are the same, do nothing. If they are different, the 'heavier' set becomes the parent of the 'less heavy'
; set.
(define (uf:union! uf obj1 obj2)
  (local
    [(define root1 (uf:find uf obj1))
     (define root2 (uf:find uf obj2))
     (define weight1 (uf:get-weight uf root1))
     (define weight2 (uf:get-weight uf root2))]
  (cond
    [(uf:same-set? uf root1 root2) void]
    [else
     (cond      ;; heavier wieght on top
       [(or (< weight1 weight2) (= weight1 weight2)) 
            (uf:reparent! uf root1 root2)]
       [(> weight1 weight2) 
            (uf:reparent! uf root2 root1)]
       [else (write "this should be impossible")]
      )])
)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions (i.e. private functions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UFE = UnionFindEntry

; uf:reparent! : UFE UFE -> Void
; Sets the parent of `child` to be `parent` and makes corresponding adjustments to the parent's weight
(define (uf:reparent! uf child parent)
  (begin
   (set-UnionEntry-parent! (uf:get-entry uf child) parent)
   (set-UnionEntry-weight! (uf:get-entry uf parent) (+ (uf:get-weight uf parent)  (uf:get-weight uf child)))
  ))

; uf:get-entry : UF N -> UFE
; Gets the UFE in spot 'ix' from 'uf'.
(define (uf:get-entry uf ix)
  (vector-ref (UnionFind-data uf) ix))

; uf:get-parent : UF N -> N
; gets the parent of the UFE in spot 'ix' from 'uf'
(define (uf:get-parent uf ix)
 (UnionEntry-parent (uf:get-entry uf ix)))

; uf:get-weight : UF N -> N
; gets the weight of the UFE in spot 'ix' from 'uf'
(define (uf:get-weight uf ix)
  (UnionEntry-weight (uf:get-entry uf ix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct testingCommand [command obj1 obj2])

(define (testing-script! uf ListOfCommands)
  (begin
    (if (empty? ListOfCommands) '()
        (begin
          (local
            [(define currentCommand (first ListOfCommands))]
            (if (symbol=? (testingCommand-command currentCommand) 'union)
                (begin
                  (uf:union! uf (testingCommand-obj1 currentCommand) (testingCommand-obj2 currentCommand))
                  ;(write uf)
                  (testing-script! uf (rest ListOfCommands))
                  )
                (local
                  [(define currentResult (uf:same-set? uf (testingCommand-obj1 currentCommand) (testingCommand-obj2 currentCommand)))]
                  (cons currentResult (testing-script! uf (rest ListOfCommands)))))    )))))

(check-expect 
 (testing-script!
  (uf:create 10) '()) 
   '() )

(check-expect 
 (testing-script!
  (uf:create 10)
     (cons (make-testingCommand 'same-set? 0 1) '()))
   '( #false) )

(check-expect 
 (testing-script!
  (uf:create 10)
     (cons (make-testingCommand 'same-set? 0 1) 
     (cons  (make-testingCommand 'union 0 1) '())))
   '( #false) )

(check-expect 
 (testing-script!
  (uf:create 10)
     (cons  (make-testingCommand 'union 0 1) 
     (cons (make-testingCommand 'same-set? 0 1) '())) )
   '( #true) )

(check-expect 
 (testing-script!
  (uf:create 15)
     (cons (make-testingCommand 'same-set? 0 1) 
     (cons (make-testingCommand 'same-set? 0 2) 
     (cons (make-testingCommand 'same-set? 0 3) '()))))
   '( #false #false #false) )

(check-expect 
 (testing-script!
  (uf:create 20)
     (cons (make-testingCommand 'same-set? 0 1) 
     (cons (make-testingCommand 'union 0 1) 
     (cons (make-testingCommand 'same-set? 0 1)
     (cons (make-testingCommand 'union 1 6)
     (cons (make-testingCommand 'union 6 11)
     (cons (make-testingCommand 'same-set? 0 11)
     (cons (make-testingCommand 'same-set? 0 10) '()))))))))
   '( #false #true #true #false) )

(check-expect 
 (testing-script!
  (uf:create 30)
     (cons (make-testingCommand 'union 0 29) 
     (cons (make-testingCommand 'union 0 10) 
     (cons (make-testingCommand 'same-set? 0 10)
     (cons (make-testingCommand 'union 10 16)
     (cons (make-testingCommand 'union 16 17)
     (cons (make-testingCommand 'same-set? 0 17)
     (cons (make-testingCommand 'same-set? 0 29)
     (cons (make-testingCommand 'same-set? 0 11)
     (cons (make-testingCommand 'same-set? 1 29) '()))))))))))
   '( #true #true #true #false #false) )

(check-expect 
 (testing-script!
  (uf:create 15)
     (cons (make-testingCommand 'union 0 10) 
     (cons (make-testingCommand 'union 0 11) 
     (cons (make-testingCommand 'union 0 12)
     (cons (make-testingCommand 'same-set? 0 13)
     (cons (make-testingCommand 'same-set? 0 12)
     (cons (make-testingCommand 'union 13 2)
     (cons (make-testingCommand 'union 0 2)
     (cons (make-testingCommand 'same-set? 0 13)
     (cons (make-testingCommand 'union 5 6)
     (cons (make-testingCommand 'same-set? 5 6) '())))))))))))
   '( #false #true #true #true) )