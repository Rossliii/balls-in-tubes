;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tubes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(require "lib-tubes.rkt")

;; A Game is (make-game Nat Nat (listof (listof Sym)))
(define-struct game (tubesize maxcolours tubes))

;;; Constants

(define emptygame
  (make-game 0 5
             (list empty empty empty empty empty)))

(define emptygame2
  (make-game 10 3 empty))

(define emptygame3
  (make-game 10 3 (list empty empty)))

(define smallgame1
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallgame2
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallinvalidgame1
  (make-game 2 1
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))


(define smallinvalidgame2
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'blue)
                   (list))))

(define smallinvalidgame3
  (make-game 2 2
             (list (list 'blue 'red 'blue)
                   (list 'red)
                   (list))))


(define smallgamefinal
  (make-game 2 2
             (list (list)
                   (list 'blue 'blue)
                   (list 'red 'red))))


(define mediumgame
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   (list))))

(define mediumgamestuck
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   )))

(define largergame
  (make-game 3 3
             (list (list 'blue 'red 'red)
                   (list 'yellow 'blue 'yellow)
                   (list 'red 'yellow 'blue)
                   (list))))

(define biggame
  (make-game 5 3
             (list (list 'blue 'blue 'red 'red 'yellow)
                   (list 'red 'red 'yellow 'blue 'red)
                   (list 'yellow 'blue 'blue 'yellow 'yellow)
                   (list)
                   (list))))

(define biggame2
  (make-game 5 3
             (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                   (list 'red 'red 'yellow 'blue 'red)
                   (list)
                   (list 'blue 'blue 'red 'red 'yellow)
                   (list))))

(define biggamesolve
  (make-game 5 3
             (list (list 'blue 'blue 'blue 'blue 'blue)
                   (list 'red 'red 'red 'red 'red)
                   (list 'yellow 'yellow 'yellow 'yellow 'yellow)
                   (list)
                   (list))))

(define hugegame
  (make-game 4 9
             (list (list 'purple 'pink 'yellow 'blue)
                   (list 'blue 'green 'purple 'white)
                   (list 'orange 'yellow 'black 'blue)
                   (list 'white 'orange 'orange 'pink)
                   (list 'pink 'red 'red 'black)
                   (list 'yellow 'green 'orange 'blue)
                   (list 'white 'purple 'red 'yellow)
                   (list 'green 'red 'green 'black)
                   (list 'purple 'black 'white 'pink)
                   (list)
                   (list))
             ))

;; stubs for finished-game? and next-games
;; Students will need to modify these functions to make solve work correctly
;; These stubs will at least allow the entire program to run without error



(define (next-games gm)
  empty)

;;;;;

;; (solve gm draw-option) determines if the game gm is solveable,
;; and will also draw each possible move depending on the draw-option

;; Examples:
;; students should provide some here, or just in tests

;; solve: Game (anyof 'off 'norm 'slow 'fast) -> Bool

(define (solve gm draw-option)
  (local
    [(define setup (puzzle-setup gm draw-option))                
     (define (solve-helper to-visit visited)
       (cond
         [(empty? to-visit) false]
         [else
          (local
            [(define draw (draw-board (first to-visit) draw-option))] 
            (cond
              [(finished-game? (first to-visit)) true]
              [(member? (first to-visit) visited)
               (solve-helper (rest to-visit) visited)]
              [else
               (local [(define nbrs (next-games (first to-visit)))
                       (define new (filter (lambda (x) (not (member? x visited))) nbrs))
                       (define new-to-visit (append new (rest to-visit)))
                       (define new-visited (cons (first to-visit) visited))]
                 (solve-helper new-to-visit new-visited))]))]))]
    (solve-helper (list gm) empty)))

;; Test cases that can be uncommented as the solution is completed

;(check-expect (solve smallgame1 'slow) true)
;(check-expect (solve mediumgamestuck 'slow) false)

;; Below is the format for testing and timing the solution:
;; be sure to remove any other check-expects when measuring your timing

;(check-expect (time (solve mediumgame 'off)) true)
;(check-expect (time (solve largergame 'off)) true)
;(check-expect (time (solve biggame 'off)) true)
;(check-expect (time (solve hugegame 'off)) true)

;; ***************************
;; Assignment 10
;; ***************************

;; a)
;; (check-colour? size num los) consumes two natural numbers, size
;;   and num, and a list of symbols, los, and  produces true if each symbol in the list
;;   appears exactly size times and if there are at most num different symbols
;;   otherwise,check-colour? will produce false

;; Examples:
(check-expect (check-colour? 1 2 (list 'red 'green)) true)
(check-expect (check-colour? 1 1 (list 'red 'green)) false)

;; check-colour?: Nat Nat (listof Sym) -> Bool
(define (check-colour? size num los)
  (local
    [(define sorted-los (sort (map symbol->string los) string<?))
    (define (los-first los1) (filter (lambda (x) (string=? x (first los1))) los1))
    (define (los-rest los1) (filter (lambda (x) (not (string=? x (first los1)))) los1))
    (define (num-acc los1 acc str)
       (cond
         [(empty? los1) acc]
         [(string=? (first los1) str) (num-acc (rest los1) acc str)] 
         [else (num-acc (rest los1) (add1 acc) (first los1))]))
    (define (size-check size los)
       (cond
         [(empty? los) true]
         [else (and (= (length (los-first los)) size) (size-check size (los-rest los)))]))
    (define (num-check num los)
       (<= (num-acc los 0 "") num))]
    (and (size-check size sorted-los) (num-check num sorted-los))))
         
;; Tests:
(check-expect (check-colour? 0 0 empty) true)
(check-expect (check-colour? 2 2 (list 'red 'green 'red 'green)) true)

;; b)
;; (valid-game? gm) consumes a Game, gm, and produces true if gm is a valid game, and false otherwise

;; Examples:
(check-expect (valid-game? emptygame) true)
(check-expect (valid-game? smallgame1) true)

;; valid-game?: Game -> Bool
(define (valid-game? gm)
  (local
    [(define tubesize (game-tubesize gm))
     (define maxcolours (game-maxcolours gm))
     (define tubes (game-tubes gm))
     (define (colour-extract tubes1)
       (cond
         [(empty? tubes1) empty]
         [else (append (first tubes1) (colour-extract (rest tubes1)))]))
     (define (cond1? length-list tubesize1)
       (empty? (filter (lambda (x) (> x tubesize1)) length-list)))]
    (and (check-colour? tubesize maxcolours (colour-extract tubes))
         (cond1? (map (lambda (y) (length y)) tubes) tubesize))))

;; Tests:
(check-expect (valid-game? smallgame2) true)
(check-expect (valid-game? smallinvalidgame2) false)
(check-expect (valid-game? smallinvalidgame3) false)       


;; c)
;; (remove-completed gm) consumes a Game, gm, and 
;;   produces a Game which is similar to gm but has any completed
;;   tubes removed

;; Examples:
(check-expect (remove-completed largergame) largergame) 
(check-expect (remove-completed (make-game 0 0
                                           (list empty
                                                 empty)))
              (make-game 0 0
                         empty))

;; remove-completed: Game -> Game
(define (remove-completed gm)
  (local
    [(define tubesize (game-tubesize gm))
     (define tubes (game-tubes gm))
     (define used-tubes (filter (lambda (z) (not (empty? z)))(game-tubes gm)))
     (define maxcolours
       (- (game-maxcolours gm)
          (length 
           (filter (lambda (x)
                     (and (= (length x) tubesize)
                          (empty? (filter (lambda (y) (not (symbol=? (first x) y))) x))))
                   used-tubes))))]
    (make-game tubesize maxcolours (filter (lambda (x)
                     (not (and (= (length x) tubesize)
                          (empty? (filter (lambda (y) (not (symbol=? (first x) y))) x)))))
                                           tubes))))

;; Tests:
(check-expect (remove-completed emptygame2)
              emptygame2)
(check-expect (remove-completed emptygame)
              (make-game 0 5 empty))
    
     
       
;; d)
;; (finished-game? gm) consumes a Game, gm, and
;;   produces true if the game is finished, and false
;;   otherwise

;; Examples:
(check-expect (finished-game? largergame) false)
(check-expect (finished-game? biggamesolve) true)

;; finished-game?: Game -> Bool
(define (finished-game? gm)
  (local
    [(define removed-gm (remove-completed gm))
     (define tubesize (game-tubesize removed-gm))
     (define maxcolours (game-maxcolours removed-gm))
     (define tubes (game-tubes removed-gm))]
    (empty? (filter (lambda (x) (not (empty? x))) tubes))))

;; Tests:
(check-expect (finished-game? emptygame3) true)


         

;; e)
;; (num-blocks llos) consumes a list of lists of symbols, llos, and
;;   produces the number of “blocks” contained in llos

;; Examples:
(check-expect (num-blocks (list empty)) 0)
(check-expect (num-blocks (list '(a a a) '(a b a))) 4)

;; num-blocks: (listof (listof Sym)) -> Nat
(define (num-blocks llos)
  (local
    [(define (num-block-list los)
       (length (foldr (lambda (x rror)
                (cond
                  [(empty? rror) (cons x rror)]
                  [(symbol=? x (first rror)) rror]
                  [else (cons x rror)]))
              empty los)))]
    (foldr (lambda (y rror) (+ (num-block-list y) rror)) 0 llos)))

;; Tests:
(check-expect (num-blocks (list '(a a a) '(a b c))) 4)
    

;; f)
;; (equiv-game? gm1 gm2) consumes two Games, gm1 and gm2, and
;;   produces true if gm1 and gm2 are equivalent, and false otherwise. 

;; Examples:
(check-expect (equiv-game? largergame largergame) true)
(check-expect (equiv-game? largergame biggamesolve) false)

;; equiv-game?: Game Game -> Bool
(define (equiv-game? gm1 gm2)
  (local
    [(define (helper lst)
       (filter (lambda (x) (not (empty? x))) lst))
     (define (tube-same tu1 tu2)
       (cond
         [(and (empty? tu1) (empty? tu2)) true]
         [(symbol=? (first tu1) (first tu2)) (tube-same (rest tu1) (rest tu2))]
         [else false]))
     (define (equiv-tubes? t1 t2 acc)
       (cond
         [(and (empty? t1) (empty? t2)) true]
         [(tube-same (first t1) (first t2)) (equiv-tubes? (rest t1) (appendlst (rest t2) acc) empty)]
         [else (equiv-tubes? t1 (rest t2) (cons (first t2) acc))]))
     (define (appendlst lst1 lst2)
       (cond
         [(and (empty? lst1) (empty? lst2)) empty]
         [(empty? lst1) (appendlst lst2 lst1)]
         [else (cons (first lst1) (appendlst (rest lst1) lst2))]))]
    (and (= (game-tubesize gm1) (game-tubesize gm2))
         (= (game-maxcolours gm1) (game-maxcolours gm2))
         (= (length (game-tubes gm1)) (length (game-tubes gm2)))
         (= (length (helper (game-tubes gm1))) (length (helper (game-tubes gm2))))
         (equiv-tubes? (helper (game-tubes gm1)) (helper(game-tubes gm2)) empty)))) 



;; Test:
(check-expect (equiv-game? smallgamefinal
                           (make-game 2 2
                                      (list (list 'red 'red)
                                            (list)
                                            (list 'blue 'blue))))
              true)


;; g)
;; (all-equiv? log1 log2) consumes two lists of Games, log1 and log2, and
;;   produces true if every game in log1 has one
;;   equivalent game in log2, and every game in log2 has one equivalent game
;;   in log1, and otherwise produces false by log1 log2

;; Examples:
(check-expect (all-equiv? (list smallgame1 largergame) (list smallgame1)) false)
(check-expect (all-equiv? (list largergame) (list smallgame1)) false)

;; all-equiv? (listof Game) (listof Game) -> (listof Game)
(define (all-equiv? log1 log2)
  (and
   (= (length log2)
      (length
       (filter (lambda (x) (not (empty?
                                 (filter (lambda (y) (equiv-game? y x)) log1)))) log2)))
   (= (length log1)
      (length
       (filter (lambda (x) (not (empty?
                                 (filter (lambda (y) (equiv-game? y x)) log2)))) log1)))))

;; Tests:
(check-expect (all-equiv? (list smallgame1 smallgame2) (list smallgame2 smallgame1)) true)





