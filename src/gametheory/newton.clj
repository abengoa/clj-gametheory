(ns gametheory.newton
	(:use clojure.math.numeric-tower))



(defn df 
	"Approximates the derivative of function f at point x, given a dx."
	[f x dx] 
	(let [dfab (fn [a b] (/ (- (f b) (f a)) (- b a)))] (/ (+ (dfab (- x dx) x) (dfab x (+ x dx))) 2.0)))

(defn newton 
	"Calculates a root of the given function using the Newton's method, starting at x and with an acceptable error margin."
	[f x err] 
	(loop [current-guess x]
		(let [current-value (f current-guess)]
		   (if (< (abs current-value) err)
				current-guess
				(recur (- current-guess (/ current-value (df f current-guess 0.0001))))))))