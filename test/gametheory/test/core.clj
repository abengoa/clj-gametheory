(ns gametheory.test.core
  (:use [gametheory.core])
  (:use [clojure.test]))

  
;(def uf1 (build-utility-fn [[:a,:a] [1.7,-0.8]] [[:a,:p] [3,-1]] [[:p,:a] [1.6,-0.7]] [[:p,:p] [0,0]]))

(def g(game 2 [[:a :p] [:a :p]] 
	[[:a,:a] [1.7,-0.8]] [[:a,:p] [3,-1]] [[:p,:a] [1.6,-0.7]] [[:p,:p] [0,0]]))

(def zsg (game 2 [[:a :p] [:a :p]] 
	[[:a,:a] [1.7,-1.7]] [[:a,:p] [3,-3]] [[:p,:a] [1.6,-1.6]] [[:p,:p] [0,0]]))
	
(def revolt (game 2 [["Revolt" "Not"] ["Revolt" "Not"]] 
	[["Revolt" "Revolt"] [1,1]] [["Revolt" "Not"] [-1,0]] [["Not" "Revolt"] [0,-1]] [["Not" "Not"] [0,0]]))

(def sidewalk-shuffle (game 2 [["L" "R"] ["L" "R"]] 
	[["L" "L"] [1,1]] [["R" "L"] [0,0]] [["L" "R"] [0,0]] [["R" "R"] [1,1]]))
	
(def prisoner-dilemma (game 2 [["confess" "silent"] ["confess" "silent"]]
	[["confess" "confess"] [-4,-4]] [["confess" "silent"] [0,-5]] [["silent" "confess"] [-5,0]] [["silent" "silent"] [-1,-1]]))

(def movie-game (game 2 [["movie" "home"] ["movie" "home"]]
	[["movie" "movie"] [1,1]] [["home" "movie"] [0,0]] [["movie" "home"] [0,0]] [["home" "home"] [0,0]]))

(def sexes-battle (game 2 [["L" "P"] ["L" "P"]]
	[["L" "L"] [3,1]] [["P" "L"] [0,0]] [["L" "P"] [0,0]] [["P" "P"] [1,3]]))
	
(def technology (game 2 [["New" "Old"] ["New" "Old"]]
	[["New" "New"] [2,2]] [["New" "Old"] [0,0]] [["Old" "New"] [0,0]] [["Old" "Old"] [1,1]]))
	
(def allgames [g zsg revolt sidewalk-shuffle prisoner-dilemma movie-game sexes-battle technology])
	
(deftest players-ok ;;
  (is true (reduce #(and %1 %2) (map #(= 2 (players %)) allgames))))

(deftest team-test
	(is true (= '(false false false true false true false true) (map team? allgames))))

(deftest zero-sum-test
	(is true (= '(false true false false false false false false) (map zero-sum? allgames))))
	
(deftest dominant?-g
	(is (= '(:a) (get-dominant-strategies g 0)))
	(is (= '() (get-dominant-strategies g 1))))
	
(deftest weakly-dominant?-g
	(is (= '(:a) (get-weakly-dominant-strategies g 0)))
	(is (= '() (get-weakly-dominant-strategies g 1))))

(deftest dominant?-zsg
	(is (= '(:a) (get-dominant-strategies zsg 0)))
	(is (= '() (get-dominant-strategies zsg 1))))
	
(deftest weakly-dominant?-zsg
	(is (= '(:a) (get-weakly-dominant-strategies zsg 0)))
	(is (= '() (get-weakly-dominant-strategies zsg 1))))

(deftest dominant?-revolt
	(is (= '() (get-dominant-strategies revolt 0)))
	(is (= '() (get-dominant-strategies revolt 1))))
	
(deftest weakly-dominant?-revolt
	(is (= '() (get-weakly-dominant-strategies revolt 0)))
	(is (= '() (get-weakly-dominant-strategies revolt 1))))
	
(deftest dominant?-sidewalk-shuffle
	(is (= '() (get-dominant-strategies sidewalk-shuffle 0)))
	(is (= '() (get-dominant-strategies sidewalk-shuffle 1))))
	
(deftest weakly-dominant?-sidewalk-shuffle
	(is (= '() (get-weakly-dominant-strategies sidewalk-shuffle 0)))
	(is (= '() (get-weakly-dominant-strategies sidewalk-shuffle 1))))
	
(deftest dominant?-prisoner
	(is (= '("confess") (get-dominant-strategies prisoner-dilemma 0)))
	(is (= '("confess") (get-dominant-strategies prisoner-dilemma 1))))
	
(deftest weakly-dominant?-prisoner
	(is (= '("confess") (get-weakly-dominant-strategies prisoner-dilemma 0)))
	(is (= '("confess") (get-weakly-dominant-strategies prisoner-dilemma 1))))

(deftest dominant?-movie
	(is (= '() (get-dominant-strategies movie-game 0)))
	(is (= '() (get-dominant-strategies movie-game 1))))
	
(deftest weakly-dominant?-movie
	(is (= '("movie") (get-weakly-dominant-strategies movie-game 0)))
	(is (= '("movie") (get-weakly-dominant-strategies movie-game 1))))

(deftest dominant?-sexes
	(is (= '() (get-dominant-strategies sexes-battle 0)))
	(is (= '() (get-dominant-strategies sexes-battle 1))))
	
(deftest weakly-dominant?-sexes
	(is (= '() (get-weakly-dominant-strategies sexes-battle 0)))
	(is (= '() (get-weakly-dominant-strategies sexes-battle 1))))

(deftest dominant?-technology
	(is (= '() (get-dominant-strategies technology 0)))
	(is (= '() (get-dominant-strategies technology 1))))
	
(deftest weakly-dominant?-technology
	(is (= '() (get-weakly-dominant-strategies technology 0)))
	(is (= '() (get-weakly-dominant-strategies technology 1))))
	
(deftest nash-eq
	(is (= '((:a :a)) (get-nash-equilibria g)))
	(is (= '((:a :a)) (get-nash-equilibria zsg)))
	(is (= '(("Revolt" "Revolt") ("Not" "Not")) (get-nash-equilibria revolt)))
	(is (= '(("L" "L") ("R" "R")) (get-nash-equilibria sidewalk-shuffle)))
	(is (= '(("confess" "confess")) (get-nash-equilibria prisoner-dilemma)))
	(is (= '(("movie" "movie") ("home" "home")) (get-nash-equilibria movie-game)))
	(is (= '(("L" "L") ("P" "P")) (get-nash-equilibria sexes-battle)))
	(is (= '(("New" "New") ("Old" "Old")) (get-nash-equilibria technology))))
	