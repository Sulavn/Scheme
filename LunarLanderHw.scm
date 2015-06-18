# Scheme

;; Sulav Nakarmi (sulavn@brandeis.edu) 2014-02-05
;; SCIP - Ex 1.17
;; this is the code for problem set -- Lunar Lander 
#lang racket
;;; Problem 1
;; Adds a constraint that makes it so the rate at which fuel can be burned cannot be less than than the total number of gallons.
(define (update ship-state fuel-burn-rate)
  (if (< (/ (fuel ship-state) dt) 1)
  (/ (full-burn ship-state) dt)  
  (make-ship-state
   (+ (height ship-state) (* (velocity ship-state) dt)) ; height
   (+ (velocity ship-state)
      (* (- (* engine-strength fuel-burn-rate) gravity)
         dt))                                           ; velocity
   (- (fuel ship-state) (* fuel-burn-rate dt)))))       ; fuel

;;; Problem 2
;; Modifies play so that a strategy can be employed to land the ship.
(define (full-burn ship-state) 1) 

(define (no-burn ship-state) 0)

(define (ask-user ship-state) (get-burn-rate))

(define (lander-loop ship-state strategy)
  (show-ship-state ship-state) 
  (if (landed? ship-state)
   (end-game ship-state)
    (lander-loop (update ship-state (strategy ship-state)) strategy )))

(define (play strategy) (lander-loop (initial-ship-state) strategy ))

;;; Problem 3
;; Adds a strategy which employs either of the two strategies randomly.
(define (random-choice strategy1 strategy2)
  (lambda (ship-state) 
    (if (= (random 2) 0) 
        (strategy1 ship-state)
        (strategy2 ship-state))))

;;; Problem 4
;; Adds a strategy which chooses based on height, a strategy out of two given ones.
(define (height-choice strategy1 strategy2 height2)
  (lambda (ship-state)
  (if (>= (height ship-state) height2)
      (strategy1 ship-state)
      (strategy2 ship-state))))

;;; Problem 5 
;; Adds a strategy similar to the the strategy in Problem 4, with the freedom to choose which predicate to be used instead of height.
(define (random-choice strategy-1 strategy-2)                       ;randomly chooses between 2 strats
  (choice strategy-1
          strategy-2
          (lambda (ship-state) 
            (= (random 2) 0))))

(define (choice strategy1 strategy2 predicate)
    (lambda (ship-state)      
       (if (predicate ship-state)                                   ;returns a procedure based on a predicate
	   (strategy1 ship-state)
	   (strategy2 ship-state))))

(define (height-choice strategy1 strategy2 height2) 
  (choice strategy1                                                 ;depending on height chooses strat 1 or 2
	  strategy2
          (lambda (ship-state) (>= (height ship-state) height2))))

;;; Problem 6
;; Adds another strategy that randomly asks the user which strategy to use or uses strategy fullburn if the height is below 40.
(define (strategy3)
  (height-choice no-burn (random-choice full-burn ask-user) 40))

;;; Problem 7

; This can be shown using the formula vf^2 = vo^2 + 2 * a * ( hf - ho ). 
; Assuming the ground is 0 hf and vf = 0, the equation now becomes 
; 0 = vo^2 + 2 * a * -ho 
; = 2 * a * ho = vo^2
; = a = vo^2 / ( 2 * ho)
;;; Problem 8
;; Adds the strat that if the acceleration for the ship is always vo^2 / ( 2 * ho), then it will land with vf = 0 as shown in Problem 7.
(define (square x)
  (* x x))
  
(define constant-acc
  (lambda (ship-state)
    (+ (/ (square (velocity ship-state)) (* 2 (height ship-state))) gravity))) ;formula for acceleration 

;;; Problem 9

; Height of 20 = 13.079125960250124 fuel remaining (but crashes) while Height of 30 = 10.68134522276128 fuel remaining. 

;;; Problem 10 
;; Modifies update so that the burn rate cannot be greater than 1. 
(define (update ship-state fuel-burn-rate)
   (let ((limited-burn-rate (min fuel-burn-rate 1)))  
  (make-ship-state
   (+ (height ship-state) (* (velocity ship-state) dt)) ; height
   (+ (velocity ship-state)
      (* (- (* engine-strength limited-burn-rate) gravity)
         dt))                                           ; velocity
   (- (fuel ship-state) (* limited-burn-rate dt)))))       ; fuel

;;; Problem 11 
;; Adds a strategy where the ship is left to fall so that it has starting velocity to employ the strategy specified in Problem 8.
(define optimal-constant-acc
  (lambda (ship-state)
    (if (<= (constant-acc ship-state) 0.8)
        (no-burn ship-state)
        (constant-acc ship-state))))

; Had 9.732359804813962 fuel left and successfully landed.
