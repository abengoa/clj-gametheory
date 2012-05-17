(ns naivesolver)

(defn get-payoffs [game player-strategies target-player]
	(let [p-table (:payoffs game)]
		(reduce (fn [p i] (nth p i)) p-table (concat player-strategies [target-player]))))

(defn find-max-index [l] (second (reduce (fn [[index m-index m] v] (if (> v m) [(inc index) index v] [(inc index) m-index m])) [0 -1 java.lang.Integer/MIN_VALUE] l)))
	

;;(defn solve-for-player [game constraints player-index]
;;	(let [strategies (count (:strategies game))
;;		  players (:players game)]
;;		  
;;		  (let [
;;		  other-players (remove (range 2) player-index)
;;		  available-strategies (nth constraints (first other-players))
;;		  available-strategies (if (nil? available-strategies) strategies)
;;		  best-strategies (map find-max-index
;;					(for [s-other available-strategies]
;;						(map (fn [s] 
;;							(let [player-strategies (if (= player-index 0) [s s-other] [s-other s])]
;;								(get-payoffs game player-strategies player-index))) strategies)
;;					))]
;;			(if (= (repeat (first best-strategies) (count best-strategies)) best-strategies)
;;				(first best-strategies)
;;				nil))))
	
	
(defn solve-for-player [game constraints player-index]
	(let [strategies (count (:strategies game))
		  players (:players game)]
		  (let [
		  other-players (remove #(= % player-index) (range 2))
		  available-strategies [(nth constraints (first other-players))]
		  available-strategies (if (= [nil] available-strategies) (range strategies) available-strategies)
		  best-strategies 
		  (set (map find-max-index
									(for [s-other available-strategies]
										(map (fn [s] 
										(let [player-strategies (if (= player-index 0) [s s-other] [s-other s])]
										(get-payoffs game player-strategies player-index))) (range strategies)))))]
			(if (= (count best-strategies) 1)
				(first best-strategies)
				nil))))

	
(defn solve-game-naive [game]
	(loop [strategies (:strategies game)
		   solution (repeat (count (:players game)) nil)]
		(let [new-solution (map (partial solve-for-player game solution) (range (count (:players game))))]
			(cond
				(= new-solution solution) solution
				(= 0 (count (filter nil? new-solution))) (map-indexed (fn [i v] [(nth (:players game) i) (nth strategies v)]) new-solution)
				true (recur strategies new-solution)))))
				
(solve-game-naive {	:players ["Predator-large mammal" "Prey-large mammal"]
					:strategies ["Active" "Passive"]
					:payoffs [[[1.7,-0.8] [3,-1]]
							  [[1.6,-0.7] [0,0]]]})
							  

(solve-game-naive {	:players ["Predator-insect" "Prey-insect"]
					:strategies ["Active" "Passive"]
					:payoffs [[[2,-7] [6,-8]]
							  [[3,-6] [-1,0]]]})
							  

(solve-game-naive {	:players ["Predator-large mammal2" "Prey-large mammal2"]
					:strategies ["Active" "Passive"]
					:payoffs [[[2,-2] [3,-1]]
							  [[1,-1] [0,0]]]})
							  
							  
;;(solve-game-naive {	:players ["Predator" "Prey"]
;;					:strategies ["Active" "Passive"]
;;					:payoffs [[[3,1] [2,0]]
;;							  [[1,3] [0,-1]]]})
							  
;;(get-payoffs {	:players ["Predator" "Prey"]
;;					:strategies ["Active" "Passive"]
;;					:payoffs [[[3,0] [2,1]]
;;							  [[1,3] [0,-1]]]} [0 0] 0)

;;(solve-for-player {	:players ["Predator" "Prey"]
;;					:strategies ["Active" "Passive"]
;;					:payoffs [[[3,0] [2,1]]
;;							  [[1,3] [0,-1]]]} [nil nil] 0)
							  
;;(solve-for-player {	:players ["Predator" "Prey"]
;;					:strategies ["Active" "Passive"]
;;					:payoffs [[[3,0] [2,1]]
;;							  [[1,3] [0,-1]]]} [0 nil] 1)