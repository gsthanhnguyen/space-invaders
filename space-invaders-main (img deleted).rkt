#|
SPACE INVADER PROGRAM
- Tank: move right and left at the bottom of the screen when you press the arrow keys
- Tank: fire missiles straight up from its current position when you press the space bar.
- Invaders: appear randomly along the top of the screen and move at a 45 degree angle.
   - When they hit a wall they will bounce off and continue at a 45 degree angle in the other direction.
   - When an invader reaches the bottom of the screen, the game is over.
- Missiles & Invaders: when missles hit the invaders. both of them will disappear
|#

#lang htdp/isl+

(require racket/list) ;gets list-ref

(require 2htdp/universe)
(require 2htdp/image)

;; SPACE INVADERS PROGRAM 


;; ------- Constants:

(define WIDTH  300) 
(define HEIGHT 500)


(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 20)


(define HIT-RANGE 10) 
(define INVADE-RATE 100)
(define FRAME (empty-scene WIDTH HEIGHT))
(define BACKGROUND (place-image (rectangle 150 250 "solid" "white") 150 250 FRAME)) ;image credit: https://hipwallpaper.com/view/472L6i


(define GAMEEND (rectangle 30 30 "solid" "red")) ; image credit: https://www.flaticon.com - Good Ware
(define INVADER (rectangle 30 30 "solid" "yellow")) ; image credit: https://www.flaticon.com - Pixel Budha

(define TANK (rectangle 30 30 "solid" "purple"))  ; image credit: https://www.flaticon.com - photo3idea studio

(define TANK-HEIGHT/2 (/ (image-height TANK) 2)) 

(define MISSILE (rectangle 10 3 "solid" "black")) ; image credit: https://www.flaticon.com - Freepik

                 


;; ---------------------------------------------- Data Definitions:

;; ----- main struct

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

;; ---------------------------------------------- TANK
;; -------struct TANK

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

;; template rules used:
;; compound: 2 fields

;; ----------------------------------------------- INVADERS
;; -------- struct INVADER
(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; template ruleds used:
;; compound: 3 fields


;; -------- ListofInvader
;; listofinvader is one of:
;; - empty
;; - (cons invader loi)
;; - interp: list of invaders appear on background

(define loi0 empty)
(define loi1 (cons I1 empty))
(define loi2 (cons I1 (cons I2 empty)))

#;
(define (fn-for-loi c)
  (cond [(empty? c) empty]
        [else (... (fn-for-1invader (first c))
                   (fn-for-loi (rest c)))]))

;; template rules used:
;; one of 2 cases
;; atomic distinct: empty
;; reference: (first c) is invader
;; self-reference: (rest c) is list of invaders
;; listofinvader is (cons invader listofinvaders)

;; ------------------------------------------------ MISSILES

;; --------- struct MISSILE

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1 

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; template rules used:
;; compound: 2 fields

;; -------- list of MISSILE
;; list of missile is one of:
;; - empty
;; - (cons missile empty)
;; interp: list of missiles fired on background

(define lom0 empty)
(define lom1 (cons M1 empty))
(define lom2 (cons M1 (cons M2 empty)))

(define (fn-for-lom c)
  (cond [(empty? c) empty]
        [else (.... (fn-for-1missile (first c))
                    (fn-for-lom (rest c)))]))

;; template rules used:
;; one of 2 cases
;; list of missiles is (cons missile listofmissiles)
;; reference: (first c) is missile
;; self-reference: (rest c) is list of missiles

;; ------
(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; ---------------------------------------------------- BIG BANG
;; game -> image
;; no need test examples for main function
;; start the program with (main G0)



(define (main c)
  (big-bang c      ; game
    (on-tick tick) ;game -> image
    (to-draw render) ;game -> image
    (on-key key) ; key-event -> game
    (stop-when endgame gameover-scene))) ; event -> boolean


;; ----------------------- TICK
;; list of invader, missile and tank -> image
;; makes tank, invaders and missile change position every second

(check-random (tick (make-game empty empty (make-tank 100 1)))
              (make-game (bouncing (moving-i (random-add (collision-i empty empty))))
                         (moving-m (collision-m empty empty))
                         (bouncing-t (moving-t (make-tank 100 1))))) 

;(define (tick c) empty) ;stub


(define (tick c)
  (make-game (bouncing (moving-i (random-add (collision-i (game-invaders c) (game-missiles c)))))  ;; invader
             (moving-m (collision-m (game-missiles c) (game-invaders c)))  ;; missile   
             (bouncing-t (moving-t (game-tank c))))) ;; tank 


;; ------ COLLISION-I
;; list of invaders & list of missiles -> list of invaders
;; interp: produce list of invaders that have not been hit by missiles from the list of invaders
;;         define successful hit is missiles that meet invaders within "HIT RANGE" on x & y coordinator

(check-expect (collision-i empty (cons (make-missile 150 300) (cons (make-missile 100 200) empty))) empty) ; (cons none-invaders listofmissiles)
(check-expect (collision-i (cons (make-invader 150 100 1) (cons (make-invader 150 100 -1) empty)) empty) ; (cons listofinvaders none-missiles)
              (cons (make-invader 150 100 1) (cons (make-invader 150 100 -1) empty)))
(check-expect (collision-i (cons (make-invader 150 100 1) (cons (make-invader 150 100 -1) empty)) ; (cons listofinvaders listofmissiles)
                           (cons (make-missile 150 100) (cons (make-missile 100 200) empty)))
              (collision-i (cons (make-invader 150 100 -1) empty)
                           (cons (make-missile 150 100) (cons (make-missile 100 200) empty))))
(check-expect (collision-i (cons (make-invader 150 100 1) (cons (make-invader 150 100 -1) empty)) ; (cons listofinvaders listofmissiles)
                           (cons (make-missile 150 300) (cons (make-missile 100 200) empty)))
              (cons (make-invader 150 100 1) (collision-i (cons (make-invader 150 100 -1) empty)
                                                          (cons (make-missile 150 300) (cons (make-missile 100 200) empty)))))


;(define (collision-i c1 c2) empty) ;stub


(define (collision-i c1 c2) ;; c1: listofinvader / c2: listofmissile
  (cond [(empty? c1) empty]
        [(empty? c2) c1]
        [else (if (collision-i-helper (first c1) c2)
                  (collision-i (rest c1) c2)
                  (cons (first c1) (collision-i (rest c1) c2)))]))

;; ------ COLLISION-I-HELPER
;; invader & list of missiles -> boolean
;; interp: produce #true if invader hits the list of missiles

(check-expect (collision-i-helper (make-invader 150 100 1) (cons (make-missile 151 101) (cons (make-missile 100 200) empty))) ; invader hits missiles
              #true)
(check-expect (collision-i-helper (make-invader 150 100 1) (cons (make-missile 151 200) (cons (make-missile 100 200) empty))) ; not hit
              (collision-i-helper (make-invader 150 100 1) (cons (make-missile 100 200) empty)))
                                  

;(define (collision-i-helper i1 i2) #true)


(define (collision-i-helper i1 i2)    ;; i1: (make-invader x y dx) / i2: listofmissile
  (cond [(empty? i2) #false]
        [else (if (and (<= (- (missile-y (first i2)) (invader-y i1)) HIT-RANGE)
                       (<= (- (invader-y i1) (missile-y (first i2))) HIT-RANGE)
                       (<= (- (missile-x (first i2)) (invader-x i1)) HIT-RANGE)
                       (<= (- (invader-x i1) (missile-x (first i2))) HIT-RANGE))
                  #true
                  (collision-i-helper i1 (rest i2)))]))


;; ------ RANDOM-ADD
;; list of invader -> image
;; interp: produce a list of invaders with random quantity that have not been hit by list of missiles
;;         list of invaders appear on the top of background in random position along width

;(define (random-add c) empty) ;stub


(define (random-add c) ;; c = list of invaders
  (if (< (random INVADE-RATE) 3)
      (cons (make-invader (random 300) 0 (if (< (random INVADE-RATE) 6)
                                             1
                                             -1)) c)
      c))
                   
;; ------ MOVING-I
;; list of invader -> image
;; interp: produce list of invader moving with a 45 degree angle

(check-expect (moving-i empty) empty)
(check-expect (moving-i (cons (make-invader 150 100 1) empty))
              (cons (moving-i-helper (make-invader 150 100 1)) empty))
(check-expect (moving-i (cons (make-invader 150 100 1) (cons (make-invader 150 100 -1) empty)))
              (cons (moving-i-helper (make-invader 150 100 1))
                    (cons (moving-i-helper (make-invader 150 100 -1)) empty)))

;(define (moving-i c) empty)


(define (moving-i c)
  (cond [(empty? c) empty]
        [else (cons (moving-i-helper (first c))
                    (moving-i (rest c)))]))
        
;; ------ MOVING-I-HELPER
;; invader -> image 
;; interp: make invader moving with a 45 degree angle with INVADER-X-SPEED and INVADER-Y-SPEED

(check-expect (moving-i-helper (make-invader 150 100 1))
              (make-invader (+ 150 (* INVADER-X-SPEED 1))
                            (+ 100 (* INVADER-Y-SPEED 1))
                            1))
(check-expect (moving-i-helper (make-invader 150 100 -1))
              (make-invader (+ 150 (* INVADER-X-SPEED -1))
                            (+ 100 (* INVADER-Y-SPEED 1))
                            -1)) 

;(define (moving-i-helper c) (make-invader 0 0 0))


(define (moving-i-helper c) ;; c = invader
  (make-invader (+ (invader-x c) (* INVADER-X-SPEED (invader-dx c)))
                (+ (invader-y c) (* INVADER-Y-SPEED (abs (invader-dx c))))
                (invader-dx c)))
  

;; ------ BOUNCING
;; list of invaders -> image
;; interp: make invaders turn opposite direction when they hit the edge on the left and right side

(check-expect (bouncing empty) empty)
(check-expect (bouncing (cons (make-invader 150 100 1) empty))
              (cons (bouncing-helper (make-invader 150 100 1)) empty))
(check-expect (bouncing (cons (make-invader 150 100 1) (cons (make-invader 150 100 -1) empty)))
              (cons (bouncing-helper (make-invader 150 100 1))
                    (cons (bouncing-helper (make-invader 150 100 -1)) empty)))

;(define (bouncing c) empty)


(define (bouncing c)  ;; c = listofinvader
  (cond [(empty? c) empty]
        [else (cons (bouncing-helper (first c))
                    (bouncing (rest c)))]))

;; ------ BOUNCING-HELPER
;; invader -> image
;; interp: produce invader will turn opposite direction when meeting two side of edge
;;         moving from left to right when dir = 1
;;         moving from right to left when dir = -1

(check-expect (bouncing-helper (make-invader 150 100 1)) (make-invader 150 100 1))
(check-expect (bouncing-helper (make-invader 0 100 -1)) (make-invader 0 100 1))
(check-expect (bouncing-helper (make-invader WIDTH 100 1)) (make-invader WIDTH 100 -1))

;(define (bouncing-helper c) (make-invader 0 0 0))


(define (bouncing-helper c)
  (if (<= (invader-x c) 0)
      (make-invader 0 (invader-y c) (* (invader-dx c) -1))
      (if (>= (invader-x c) WIDTH)
          (make-invader WIDTH (invader-y c) (* (invader-dx c) -1))
          c))) 


;; ------ COLLISION-M
;; list of missile & list of invader -> list of missile
;; interp: produce list of missiles that have not been hit by invaders from the list of missiles
;;         define successful hit is missiles that meet invaders within "HIT RANGE" on x & y coordinator


(check-expect (collision-m empty (cons (make-invader 150 100 1) empty)) empty)
(check-expect (collision-m (cons (make-missile 150 300) empty) empty) (cons (make-missile 150 300) empty))
(check-expect (collision-m (cons (make-missile 150 300) (cons (make-missile 100 200) empty)) 
                           (cons (make-invader 150 100 1) (cons (make-invader 150 100 -1) empty))) ;; false
              (cons (make-missile 150 300) (collision-m (cons (make-missile 100 200) empty)
                                                        (cons (make-invader 150 100 1) (cons (make-invader 150 100 -1) empty)))))
(check-expect (collision-m (cons (make-missile 150 300) (cons (make-missile 100 200) empty))
                           (cons (make-invader 150 300 1) (cons (make-invader 150 100 -1) empty))) ;; true
              (collision-m (cons (make-missile 100 200) empty)
                           (cons (make-invader 150 300 1) (cons (make-invader 150 100 -1) empty))))


;(define (collision-m c1 c2) empty)


(define (collision-m c1 c2) ; c1: list of missile / c2: list of invader
  (cond [(empty? c1) empty]
        [(empty? c2) c1]
        [else (if (collision-m-helper (first c1) c2)
                  (collision-m (rest c1) c2)
                  (cons (first c1) (collision-m (rest c1) c2)))]))

;; ------- COLLISION-M-HELPER
;; missile, listofinvader -> boolean
;; interp: produce #true when the missile has been hit a list of invaders

(check-expect (collision-m-helper (make-missile 150 300) empty) #false)
(check-expect (collision-m-helper (make-missile 150 300) (cons (make-invader 150 300 1) (cons (make-invader 150 100 -1) empty))) #true) ;true
(check-expect (collision-m-helper (make-missile 150 300) (cons (make-invader 150 100 1) (cons (make-invader 150 100 -1) empty))) ;false
              (collision-m-helper (make-missile 150 300) (cons (make-invader 150 100 -1) empty)))

;(define (collision-m-helper c1 c2) #true)


(define (collision-m-helper c1 c2) ;; c1: first missile / c2: list of invader
  (cond [(empty? c2) #false]
        [else (if (and (<= (- (missile-y c1) (invader-y (first c2))) HIT-RANGE)
                       (<= (- (invader-y (first c2)) (missile-y c1)) HIT-RANGE)
                       (<= (- (missile-x c1) (invader-x (first c2))) HIT-RANGE)
                       (<= (- (invader-x (first c2)) (missile-x c1)) HIT-RANGE))
                  #true
                  (collision-m-helper c1 (rest c2)))]))
                  
;; ------ MOVING-M
;; listofmissile -> image
;; interp: make list of missiles move up-right with MISSILE-SPEED

(check-expect (moving-m empty) empty)
(check-expect (moving-m (cons (make-missile 150 300) empty))
              (cons (moving-m-helper (make-missile 150 300)) empty))
(check-expect (moving-m (cons (make-missile 150 300) (cons (make-missile 100 200) empty)))
              (cons (moving-m-helper (make-missile 150 300)) (moving-m (cons (make-missile 100 200) empty))))

;(define (moving-m c) empty)


(define (moving-m c)
  (cond [(empty? c) empty]
        [else (cons (moving-m-helper (first c))
                    (moving-m (rest c)))]))


;; ------ MOVING-M-HELPER
;; missile -> image
;; interp: make missile moving up-right

(check-expect (moving-m-helper (make-missile 150 300)) (make-missile 150 (- 300 MISSILE-SPEED)))
(check-expect (moving-m-helper (make-missile 150 100)) (make-missile 150 (- 100 MISSILE-SPEED)))

;(define (moving-m-helper c) (make-missile 0 0))


(define (moving-m-helper c)
  (make-missile (missile-x c) (- (missile-y c) MISSILE-SPEED))) 

  
;; ------ MOVING-T
;; tank -> image
;; interp: make tank moving left or right with TANK-SPEED
;;         when tank-dx = 1 -> tank moving right
;;         when tank-dx = -1 -> tank moving left

(check-expect (moving-t (make-tank 100 1)) (make-tank (+ 100 TANK-SPEED) 1))
(check-expect (moving-t (make-tank 100 -1)) (make-tank (- 100 TANK-SPEED) -1))

;(define (moving-t c) empty)


(define (moving-t c)
  (if (>= (tank-dir c) 1)
      (make-tank (+ (tank-x c) TANK-SPEED) 1)
      (make-tank (- (tank-x c) TANK-SPEED) -1)))

;; ------- BOUNCING-TANK
;; tank -> image
;; interp: make the tank turns to the opposite direction when it hit the left & right edge

(check-expect (bouncing-t (make-tank 100 1)) (make-tank 100 1))
(check-expect (bouncing-t (make-tank 300 1)) (make-tank 300 -1))
(check-expect (bouncing-t (make-tank 0 -1)) (make-tank 0 1))

;(define (bouncing-t c) (make-tank 0 0)) stub


(define (bouncing-t c)
  (if (>= (tank-x c) WIDTH)
      (make-tank (tank-x c) -1)
      (if (<= (tank-x c) 0)
          (make-tank (tank-x c) 1)
          (make-tank (tank-x c) (tank-dir c)))))
           
   
;; -------------------------------------------- RENDER
;; game -> image
;; interp: render game on BACKGROUND

(check-expect (render G0)
              (render-invader empty (render-missile empty (render-tank T0))))

(check-expect (render G3)
              (render-invader (list I1 I2) (render-missile (list M1 M2) (render-tank T1))))
              
;(define (render c) empty-image) 


(define (render c)   ; c = (make-game listofinvaders listofmissiles tank)
  (render-invader (game-invaders c)
                  (render-missile (game-missiles c)
                                  (render-tank (game-tank c)))))
                                            
;; ---------- RENDER-INVADER
;; list of invaders -> image
;; interp: render invaders on BACKGROUND

(check-expect (render-invader empty empty) empty)
(check-expect (render-invader (cons (make-invader 150 300 1) empty) BACKGROUND)
              (render-invader-helper (make-invader 150 300 1) (render-invader empty BACKGROUND)))
(check-expect (render-invader (cons (make-invader 150 300 1) (cons (make-invader 100 200 1) empty)) BACKGROUND)
              (render-invader-helper (make-invader 150 300 1) (render-invader (cons (make-invader 100 200 1) empty) BACKGROUND)))

;(define (render-invader c img) empty-image)


(define (render-invader c1 c2) ;; c1 = (cons (...) (cons (...) empty))
(cond [(empty? c1) c2] 
      [else (render-invader-helper (first c1) (render-invader (rest c1) c2))]))

;; ----- RENDER-INVADER-HELPER 
;; invader -> image
;; interp: render invader on BACKGROUND

(check-expect (render-invader-helper (make-invader 150 300 1) BACKGROUND)
              (place-image INVADER 150 300 BACKGROUND))

;(define (render-invader-helper c1 c2) empty-image) ;stub 


(define (render-invader-helper c1 c2)
  (place-image INVADER (invader-x c1) (invader-y c1) c2))  

;; ----------- RENDER-MISSILE
;; list of missiles -> image
;; interp: render missiles on BACKGROUND

(check-expect (render-missile empty BACKGROUND) BACKGROUND)
(check-expect (render-missile (cons (make-missile 150 300) empty) BACKGROUND)
              (render-missile-helper (make-missile 150 300) (render-missile empty BACKGROUND)))

;(define (render-missile c img) empty-image)


(define (render-missile c1 c2)
  (cond [(empty? c1) c2]
        [else (render-missile-helper (first c1) (render-missile (rest c1) c2))]))

;; ------- RENDER-MISSILE-HELPER
;; missile -> image
;; interp: render missile on BACKGROUND

(check-expect (render-missile-helper (make-missile 150 300) BACKGROUND)
              (place-image MISSILE 150 300 BACKGROUND))

;(define (render-missile-helper p1 p2) BACKGROUND)


(define (render-missile-helper p1 p2)
  (place-image MISSILE (missile-x p1) (missile-y p1) p2))

;; ------------- RENDER-TANK
;; tank -> image
;; interp: render tank image on BACKGROUND

(check-expect (render-tank (make-tank 100 1))
  (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;(define (render-tank c img) empty-image)


(define (render-tank c)
  (place-image TANK (tank-x c) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

                            
;; ----------------------- KEY
;; keyevent -> image
;; if key pressed is:
;;        left arrow -> tank moving on the left direction
;;        right arrow -> tank moving on the right direction
;;        press space bar -> new missile is fired right at the tank position

(check-expect (key (make-game empty empty (make-tank 50 1)) "up") (make-game empty empty (make-tank 50 1))) ; up-key -> not changing tank dir
(check-expect (key (make-game empty empty (make-tank 50 -1)) "right") (make-game empty empty (make-tank 50 1))) ; right
(check-expect (key (make-game empty empty (make-tank 50 1)) "left") (make-game empty empty (make-tank 50 -1))) ;left

(check-expect (key (make-game (cons (make-invader 100 100 1) (cons (make-invader 150 150 -1) empty))  ; first missile fired
                              empty
                              (make-tank 150 1)) " ")
              (make-game (cons (make-invader 100 100 1) (cons (make-invader 150 150 -1) empty))
                         (cons (make-missile 150 (- HEIGHT TANK-HEIGHT/2)) empty)
                         (make-tank 150 1)))

(check-expect (key (make-game (cons (make-invader 100 100 1) (cons (make-invader 150 150 -1) empty))  ; sencond missile fired
                              (cons (make-missile 150 150) empty)
                              (make-tank 170 1)) " ")
              (make-game (cons (make-invader 100 100 1) (cons (make-invader 150 150 -1) empty))
                         (cons (make-missile 170 (- HEIGHT TANK-HEIGHT/2)) (cons (make-missile 150 150) empty))
                         (make-tank 170 1)))

(check-expect (key (make-game (cons (make-invader 100 100 1) (cons (make-invader 150 150 -1) empty))  ; none missile fired
                              empty
                              (make-tank 150 1)) "up")
              (make-game (cons (make-invader 100 100 1) (cons (make-invader 150 150 -1) empty))
                         empty
                         (make-tank 150 1)))

; (define (key c akey) (make-game empty empty empty) ;stub


(define (key c akey)
  (cond [(key=? "left" akey) (make-game (game-invaders c) (game-missiles c) (tank-left (game-tank c)))]
        [(key=? "right" akey) (make-game (game-invaders c) (game-missiles c) (tank-right (game-tank c)))]
        [(key=? " " akey) (make-game (game-invaders c) (missile-fire (game-missiles c) (game-tank c)) (game-tank c))]
        [else c])) 

;; ----- TANK-LEFT
;; game -> image
;; interp: press left arrow key and the tank heads in left direction

(check-expect (tank-left (make-tank 150 1)) (make-tank 150 -1))
(check-expect (tank-left (make-tank 150 -1)) (make-tank 150 -1))

;(define (tank-left c) empty)


(define (tank-left c)  ;; c = game-tank
  (make-tank (tank-x c) -1))


;; ------ TANK-RIGHT
;; game -> image
;; interp: press right arrow key and the tank heads in right direction

(check-expect (tank-right (make-tank 150 1)) (make-tank 150 1))
(check-expect (tank-right (make-tank 150 -1)) (make-tank 150 1))

;(define (tank-right c) empty)


(define (tank-right c)  ;; c = game-tank
  (make-tank (tank-x c) 1))

;; MISSILE FIRE
;; game -> image
;; interp: press space bar and missile is fired right at the position of tank

(check-expect (missile-fire empty (make-tank 150 1)) (cons (make-missile 150 (- HEIGHT TANK-HEIGHT/2)) empty))
(check-expect (missile-fire (cons (make-missile 100 100) empty) (make-tank 150 1)) (cons (make-missile 150 (- HEIGHT TANK-HEIGHT/2))
                                                                                         (cons (make-missile 100 100) empty)))

;(define (missile-fire c1 c2) empty)


(define (missile-fire c1 c2) ;; c1: game-missile / c2: game-tank
  (cons (make-missile (tank-x c2) (- HEIGHT TANK-HEIGHT/2)) c1))


;; ------------------------------- ENDGAME
;; key event -> image
;; interp: game end once an invader reachs to the bottom of the BACKGROUND

(check-expect (endgame (make-game empty empty (make-tank 150 1)))
              (endgame-helper empty))
(check-expect (endgame (make-game (cons (make-invader 150 100 1) empty)
                                  (cons (make-missile 150 150) empty)
                                  (make-tank 150 1)))
              (endgame-helper (cons (make-invader 150 100 1) empty)))
(check-expect (endgame (make-game (cons (make-invader 150 HEIGHT 1) empty)
                                  (cons (make-missile 150 150) empty)
                                  (make-tank 150 1)))
              (endgame-helper (cons (make-invader 150 HEIGHT 1) empty)))

; (define (endgame c) (make-game empty empty empty)) ;stub


(define (endgame c) ;; c = make-game
  (endgame-helper (game-invaders c)))

;; ------------ ENDGAME-HELPER
;; list of invaders -> boolean
;; interp: produce #true if invader meet the bottom of the BACKGROUND

(check-expect (endgame-helper empty) #false)
(check-expect (endgame-helper (cons (make-invader 150 100 1) empty)) (endgame-helper empty))
(check-expect (endgame-helper (cons (make-invader 150 HEIGHT 1) empty)) #true)

;(define (endgame c) empty-image)


(define (endgame-helper c) ;; c = list of invaders
  (cond [(empty? c) #false]
        [else (if (>= (invader-y (first c)) HEIGHT)
                  #true
                  (endgame-helper (rest c)))]))

;; GAME OVER SCENE
;; event -> image
;; interp: produce the GAMEOVER screen when the game ends.
;; no need test examples

;(define (gameover-scene c) empty-image)

(define (gameover-scene c)
  (place-image GAMEEND 150 250 BACKGROUND))

; ======== START GAME
(main G0)

