;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 1000)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;loi body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition


#;
(define (fn-for-game s)
  (... (fn-for-loin (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

;; Template rules used:
;; - reference: invaders field is INVADER , missiles field is MISSILE , tank field is TANK


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dx t)))
;; template rules used
;; - compound: 2 fields


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number  Integer[-1, 1])
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 1))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; template rules used:
;; compound: 3 fields
;; atomic non-distinct: Number
;; atomic non-distinct: Number
;; atomic non-distinct: Number


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
;; atomic non-distinct: Number
;; atomic non-distinct: Number


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; ListOfInvader is one of:
;; empty
;; (cons (make-invader Number Number  Integer[-1, 1])  ListOfInvader)
;; interp. a list of invaders

(define LOIN1 empty)
(define LOIN2 (cons I1 empty))
(define LOIN3 (cons I1 (cons I2 empty)))


#;
(define (fn-for-loin loin)
  (cond [(empty? loin) (...)]                   ;BASE CASE
        [else (... (fn-for-invader (first loin)) ;Invader
                   (fn-for-loin (rest loin)))])) ;NATURAL RECURSION



;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Invader ListOfInvader)
;;  - reference : (first lom) is Invader
;;  - self-reference: (rest lom) is ListOfInvader



;; ListOfMissile is one of:
;; - empty
;; (cons missile ListOfMissile)
;;interp. a list of missiles that exist in the game

(define LOM1 empty)
(define LOM2 (cons M1 empty))
(define LOM3 (cons M1 (cons M2 empty)))
(define LOM4 (cons M1 (cons M2 (cons M3 empty))))


#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]                   ;BASE CASE
        [else (... (fn-for-missile (first lom)) ;Invader
                   (fn-for-lom (rest lom)))])) ;NATURAL RECURSION


;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Missile ListOfMissiles)
;;  - reference : (first lom) is Missile
;;  - self-reference: (rest lom) is ListOfMissile



;;============

;; Functions:

;; Game -> Game
;; start the world with (main G0))
;; 

(define (main G)
  (big-bang G
            (on-tick update-state) ;; Game -> Game
            (to-draw render-game)  ;; Game -> Image
            (on-key handle-key)    ;; WS KeyEvent -> WS
            (stop-when game-over?))) ;; WS -> Boolean




;; Game -> Game
;; produce the next game state
(check-random (update-state G0) (make-game (generate-invader INVADE-RATE) empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
(check-random (update-state G1) (make-game (generate-invader INVADE-RATE) empty (make-tank (+ 50 TANK-SPEED) 1)))
(check-random (update-state G2) (make-game (cons (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1)
                                                 (generate-invader INVADE-RATE)) (list (make-missile 150 (- 300 MISSILE-SPEED))) (make-tank (+ 50 TANK-SPEED) 1)))

;(define (update-state s) s) ; stub

;; template rules used from the Game data definition

(define (update-state s)
  (make-game (update-invaders (game-invaders s) (game-missiles s))
       (update-missiles (game-missiles s))
       (update-tank (game-tank s))))



;; ListOfInvader, ListOfMissile -> ListOfInvaders
;; produces an updates list of the Inveders
(check-random (update-invaders LOIN3 LOM1) (move-invaders LOIN3))
(check-random (update-invaders LOIN3 LOM2) (move-invaders LOIN3))


;;(define (update-invaders loin lom) loin) ;stub


(define (update-invaders loin lom)
  (move-invaders (filter-invaders loin lom))) 


;; ListOfInvader, ListOfMissiles -> ListOfInvader
;; Remove any invaders that have been hit by missiles
(check-expect (filter-invaders LOIN3 LOM3) (cons I2 empty))
(check-expect (filter-invaders LOIN3 LOM4) (cons I2 empty))

;(define (filter-invaders loin lom) loin) ;stub


(define (filter-invaders loin lom)
  (cond [(empty? loin) empty]
        [else (cond[(dead-invader? (first loin) lom) (filter-invaders (rest loin) lom)]
                   [else (cons (first loin) (filter-invaders (rest loin) lom))])])) 

 
;; Invader, ListOfMissiles -> Boolean
;; Returns true only if the Invader is within HIT-RANGE of missile
(check-expect (dead-invader? I1 LOM1) false)
(check-expect (dead-invader? I1 LOM3) true)
(check-expect (dead-invader? I1 LOM4) true)

;;(define (dead-invader? invader lom) false) ; stub


(define (dead-invader? invader lom)
  (cond [(empty? lom) false]                  
        [else (cond [(hit? invader (first lom)) true] 
                    [else (dead-invader? invader (rest lom))])]))


;; Invader, Missile -> Boolean
;; checks whether the invader has been hit by the missile
(check-expect (hit? I1 M3) true)
(check-expect (hit? I3 M3) false)

;;(define (hit? invader m) false);; stub

;; template rules used from the invader data definition
(define (hit? invader m)
  (cond [(and ( <= (abs (- (invader-x invader) (missile-x m))) HIT-RANGE)
             ( <= (abs (- (invader-y invader) (missile-y m))) HIT-RANGE)) true]
        [else false]))

;; ListOfInvader -> ListOfInvader
;; adds to list of invaders 1 randomly placed invader along top of screen
(check-random (move-invaders empty)  (generate-invader INVADE-RATE))
(check-random (move-invaders LOIN3)  (cons (make-invader (+ INVADER-X-SPEED 150)(+ 100 INVADER-Y-SPEED) 1)
                                             (cons (make-invader (- 150 INVADER-X-SPEED) (+ HEIGHT INVADER-Y-SPEED) -1)  (generate-invader INVADE-RATE))))



;(define (move-invaders loin) loin) ; stub


(define (move-invaders loin)
  (cond [(empty? loin) (generate-invader INVADE-RATE)] 
        [else (cons (update-invader (first loin))
                   (move-invaders (rest loin)))]))


;; Number -> ListOfInvader
;; Randomly add an invador to an empty list or not, then returns the list
(check-random (generate-invader INVADE-RATE) (cond [ (<= (random INVADE-RATE)(/ INVADE-RATE 50))
                                                     (cons (make-invader (random WIDTH) 0 1) empty)]
                                                   [else empty]))

;;(define (generate-invader INVADE-RATE) empty) ;; stub



(define (generate-invader i_n)
  (cond [(<= (random i_n) (/ INVADE-RATE 50))  (cons (make-invader (random WIDTH) 0 1) empty)]
       [else empty]))



;; Invader -> Invader
;; updates and returns a given invader
(check-expect (update-invader (make-invader 50 50 1)) (make-invader (+ 50 INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) 1))
(check-expect (update-invader (make-invader 50 50 -1)) (make-invader (- 50 INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) -1))
(check-expect (update-invader (make-invader (+ WIDTH 1) 50 1))
              (make-invader (- (+ WIDTH 1) INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) -1))

;;(define (update-invader invader) I1) ; stub


(define (update-invader invader)
  (cond [(or (>= (invader-x invader) WIDTH) (<= (invader-x invader) 0))
         (make-invader (+ (invader-x invader) (* (* INVADER-X-SPEED (invader-dx invader)) -1))
                       (+ (invader-y invader) INVADER-Y-SPEED) (* (invader-dx invader) -1) )]
        [else  (make-invader (+ (invader-x invader) (* INVADER-X-SPEED (invader-dx invader)))
                             (+ (invader-y invader) INVADER-Y-SPEED) (invader-dx invader) )]))



;; ListOfMissile -> ListOfMissile
;; updates the position of the missiles
(check-expect (update-missiles empty) empty)
(check-expect (update-missiles (cons (make-missile 150 300) empty))  (cons (make-missile 150 (- 300  MISSILE-SPEED)) empty))
(check-expect (update-missiles (cons (make-missile 150 300) (cons (make-missile 140 (- 0 1)) empty))) (cons (make-missile 150 (- 300 MISSILE-SPEED)) empty))
              

;; (define (update-missiles lom) empty) ; stub


(define (update-missiles lom)
  (cond [(empty? lom) empty]                 
        [else (cond [( <= (missile-y (first lom)) 0) (update-missiles (rest lom))]
                    [else (cons (update-missile (first lom)) (update-missiles (rest lom)))])]))


;; Missile -> Missile
;; moves a single missile up the screen by MISSILE-SPEED
(check-expect (update-missile (make-missile 150 300)) (make-missile 150 (- 300 MISSILE-SPEED)))

;;(define (update-missile m) m); stub

;; template rules used from the missile data definition
(define (update-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))



;; Tank -> Tank
;; moves tank by tank speed in specified direction (left or right)
(check-expect (update-tank (make-tank 50 -1)) (make-tank (- 50 TANK-SPEED) -1))
(check-expect (update-tank (make-tank WIDTH 1)) (make-tank (- WIDTH TANK-SPEED) -1))
(check-expect (update-tank (make-tank 0 -1)) (make-tank (+ 0 TANK-SPEED) 1))


;(define (update-tank t) t);stub



(define (update-tank t)
  (cond [(or ( >= (tank-x t) WIDTH) (<= (tank-x t) 0))
         (make-tank (+ (tank-x t) (* TANK-SPEED  (* (tank-dir t) -1))) (* (tank-dir t) -1)) ]
        [else  (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))



;; Game -> Image
;; renders the game state 
(check-expect (render-game G0) (place-image TANK (tank-x T0) (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))
(check-expect (render-game G1) (place-image TANK (tank-x T1) (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))
(check-expect (render-game G2) (render-missiles (game-missiles G2) (render-invaders (game-invaders G2) (render-tank (game-tank G2)))))

;(define (render-game g) BACKGROUND) ; stub


(define (render-game s)
  (render-invaders (game-invaders s)
                  (render-missiles (game-missiles s)
                                  (render-tank (game-tank s)))))


;; Tank -> Image
;;take the Tank from game and places Tank in background at appropriate x,y coordinates
(check-expect (render-tank T0) (place-image TANK (tank-x T0) (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))
(check-expect (render-tank T1) (place-image TANK (tank-x T1) (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))

;;(define (render-tank t) BACKGROUND); stub

;; template rules used from the tank data definition
(define (render-tank t)
  (place-image TANK (tank-x t)  (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))


;; ListOfInvader, image -> image
;; Places invaders into x,y coordinate space
(check-expect (render-invaders empty (render-tank T1)) (render-tank T1))
(check-expect (render-invaders (cons I1 (cons I2 empty)) (render-tank T1)) (place-images (list INVADER INVADER)
                                                                                         (list (make-posn (invader-x I1) (invader-y I1))
                                                                                               (make-posn (invader-x I2) (invader-y I2)))
                                                                                         (render-tank T1)))


;(define (render-invaders loin i) i) ; stub



(define (render-invaders loin i)
  (cond [(empty? loin) i]                
        [else (render-invaders (rest loin) (render-invader (first loin) i))])) 



;; Invader, Image -> Image
;;take invaders from game and places invaders in background at appropriate x,y coordinates

(check-expect (render-invader I1 BACKGROUND) (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND))
(check-expect (render-invader I2 (render-invader I1 BACKGROUND)) (place-image INVADER (invader-x I2) (invader-y I2) (render-invader I1 BACKGROUND)))

;(define (render-invader invader i) BACKGROUND);stub


(define (render-invader invader i)
  (place-image INVADER (invader-x invader) (invader-y invader) i))



;; ListOfMissile -> Image
;; renders the list of Missiles onto the screen

(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list (make-missile 50 50) (make-missile 50 30)) BACKGROUND)
              (place-images (list MISSILE MISSILE)
                            (list (make-posn 50 50) (make-posn 50 30)) BACKGROUND))
(check-expect (render-missiles (list (make-missile 50 50) (make-missile 50 30)) (render-invaders (list I1 I2) (render-tank T1)))
              (place-images (list MISSILE MISSILE)
                            (list (make-posn 50 50) (make-posn 50 30)) (render-invaders (list I1 I2) (render-tank T1))))

;;(define (render-missiles lom i) i) ; stub


(define (render-missiles lom i)
  (cond [(empty? lom) i]                   ;BASE CASE
        [else (render-missiles (rest lom) (render-missile (first lom) i))])) ;NATURAL RECURSION


;; Missile, Image -> Image
;; places an image of a missile on the given image

(check-expect (render-missile M1 BACKGROUND) (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND))
(check-expect (render-missile M2 (render-missile M1 BACKGROUND)) (place-image MISSILE (missile-x M2) (missile-y M2) (render-missile M1 BACKGROUND)))

;(define (render-missile m i) BACKGROUND);stub


(define (render-missile m i)
  (place-image MISSILE (missile-x m) (missile-y m) i))





;; handle-key
;; WS KeyEvent -> WS
;; changes the direction of the tank when an arrow is clicked. Fires a missile when space is clicked
(check-expect (handle-key G0 "left") (make-game empty empty (make-tank (tank-x T0) -1)))
(check-expect (handle-key G1 "right") (make-game empty empty (make-tank (tank-x T1) 1)))
(check-expect (handle-key G1 "left") (make-game empty empty (make-tank (tank-x T1) -1)))
(check-expect (handle-key G0 "right") (make-game empty empty (make-tank (tank-x T0) 1)))
(check-expect (handle-key G3 " ") (make-game (list I1 I2) (list M1 M2 (make-missile (tank-x T1) (- HEIGHT (/ (image-height TANK)2)))) T1))


;;(define (handle-key g key) g) ;stub

;; template
#;
(define (handle-key ws key)
  (cond [(key=? ke " ") (... ws)]
        [else 
         (... ws)]))

(define (handle-key g ke)
  (cond [(key=? ke "left") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1))]
        [(key=? ke " ") (make-game (game-invaders g)
                                   (append (game-missiles g) (list (make-missile (tank-x (game-tank g)) (- HEIGHT (/ (image-height TANK)2)))))
                                   (game-tank g))]
        [else g]))


;(define G0 (make-game empty empty T0))
;(define G1 (make-game empty empty T1))
;(define G2 (make-game (list I1) (list M1) T1))
;(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; the end of the game

;; Game -> Boolean
;; stops the game when an invader reaches the bottom of the screen
(check-expect (game-over? G0) false)
(check-expect (game-over? G2) false)
(check-expect (game-over? G3) true)

;(define (game-over? g) false) ;stub




(define (game-over? g)
  (cond [(invaders-reached? (game-invaders g)) true]                   
        [else false])) 



;; ListOfInvaders -> Boolean
;; returns true if one of the invaders has reached the bottom of the screen
(check-expect (invaders-reached? empty) false)
(check-expect (invaders-reached? (list I1)) false)
(check-expect (invaders-reached? (list I2)) true)

;;(define (invaders-reached? loin) false) ;stub

(define (invaders-reached? loin)
  (cond [(empty? loin) false]                   
        [else (cond [(reached? (first loin)) true] 
                    [else(invaders-reached? (rest loin))])])) 



;; Inveder -> Boolean
;; checks whether the invader has reached the bottom of the screen
(check-expect (reached? I1) false)
(check-expect (reached? I2) true)
(check-expect (reached? I3) true)

;(define (reached? invader) false);stub



(define (reached? invader)
  (cond [ (>= (invader-y invader) HEIGHT) true]
        [else false]))
