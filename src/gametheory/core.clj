(ns gametheory.core
	(:require [clojure.math.combinatorics :as comb]))

;; element ->    
(defn build-utility-fn 
	"Builds an utility function from the element arguments. Each element is in the form
		[[a1, a2, ... an] [u1, u2, ... un]]
	where the vector [a1, a2, ... an] is the strategies for all the players and the vector [u1, u2, ... un] lists the outcome utilities for all the players in that situation."
	[& elements]
	(let [utility-table (reduce (fn [m [strategies utilities]] (assoc m (vec strategies) utilities)) {} elements)]
		(fn [player strategies] (nth (get utility-table (vec strategies)) player))))

(defn game 
	"Creates a new game, from the number of players, the strategies for each player and the elements that define the utility function.
	The s param is in the form [[s-p1-1, s-p1-2, ... s-p1-i] [s-p2-1, s-p2-2, ... s-p2-j]  ... [s-pn-1, s-pn-2, ... s-pn-k]] that lists all the strategies for each of the n players.
	Each element of the utility function is in the form
		[[a1, a2, ... an] [u1, u2, ... un]]
	where the vector [a1, a2, ... an] is the strategies for all the players and the vector [u1, u2, ... un] lists the outcome utilities for all the players in that situation."
	[p s & elem]
	{:players (range p) :strategies s :utility-fn (apply build-utility-fn elem)})

(defn players 
	"Gets the number of players in the game."
	[game] (:players game))
	
(defn strategies 
	"Gets the player strategies in the game. If a player parameter is provided (index of the player, zero-started) then only the strategies for the specified player will be returned."
	([game] (:strategies game))
	([game player] (nth (strategies game) player)))

(defn utility
	"Gets the utility value of a game for a player and a given startegy vector. The strategies are expected to be a sequence of all the strategies that all players are following."
	[game player strategies] ((:utility-fn game) player strategies))
	
(defn strategy-space 
	"Returns a sequence of all the strategy combinations possible in a given game."
	[game] (apply comb/cartesian-product (strategies game)))



(defn iff 
	"Computes the 'a if and only if b' boolean value."
	[a b] (or (and a b) (and (not a) (not b))))

(defn team? 
	"Checks if a game is a team game."
	[game]
	(reduce #(and %1 %2) (for [i (players game) j (players game) s1 (strategy-space game) s2 (strategy-space game) :when (not (= i j))] 
		(iff (>= (utility game i s1) (utility game i s2)) (>= (utility game j s1) (utility game j s2))))))

(defn k-sum?
	"Checks if a game is a k-sum game for a given k."
	[game k]
	(let [all-strategies (strategy-space game)]  
		(= 	#{k}
			(set (map #(double (reduce + %))
				(map (fn [f] (map f (players game))) 
					(map (fn [s] #(utility game % s))   all-strategies)))))))

(defn zero-sum? 
	"Checks if a game is zero-sum."
	[game] (k-sum? game 0.0))

(defn- only-player-changes? 
	"Checks if the only difference between the startegy vectors is for the specified player index."
	[player s1 s2] (reduce #(and %1 %2) (map (fn [s1 s2 i] (if (= i player) (not (= s1 s2)) (= s1 s2))) s1 s2 (range))))

(defn- strategy-dominance? 
	"Checks if a given strategy is dominant for a player in the provided game. The comparator predicate is used to compare the different utilities to verify the dominance."
	[comparator game strategy player] (reduce #(and %1 %2) (flatten (for [s (filter #(= (nth % player) strategy) (strategy-space game))] 
		(for [s_x (filter (partial only-player-changes? player s) (strategy-space game))] (comparator (utility game player s) (utility game player s_x)) )))))
		
(defn dominant? 
	"Checks if a given strategy is dominant for a player in the provided game."
	[game strategy player] (strategy-dominance? > game strategy player))
	
(defn get-dominant-strategies 
	"Returns all the dominant strategies for a player in the given game."
	[game player] (filter #(dominant? game % player) (strategies game player)))
	
(defn weakly-dominant? 
	"Checks if a given strategy is weakly dominant for a player in the provided game."
	[game strategy player] (strategy-dominance? >= game strategy player))
	
(defn get-weakly-dominant-strategies 
	"Gets all the weakly dominant strategies for a player in the given game."
	[game player] (filter #(weakly-dominant? game % player) (strategies game player)))


(defn best-response? 
	"Checks if a strategy vector is the best response for a given player."
	[game player strategies] (reduce #(and %1 %2) (for [s_x (strategy-space game) :when (only-player-changes? player strategies s_x)] (>= (utility game player strategies) (utility game player s_x)))))
	
(defn nash-equilibrium? 
	"Checks if a strategy vector is a nash equilibrium in the provided game."
	[game strategies] (reduce #(and %1 %2) (map #(best-response? game % strategies) (range (count strategies)))))
	
(defn get-nash-equilibria 
	"Gets all the nash equilibria from a game."
	[game] (filter (partial nash-equilibrium? game) (strategy-space game)))