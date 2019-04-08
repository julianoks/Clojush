(ns clojush.pushgp.selection.lexicase-geometric
  (:use [clojush random]))

(defn geometric-lexicase-selection
  "Returns an individual that does the best on the fitness cases when considered one at a
  time in random order. The size of the candidate pool in the i-th iteration is bounded by the i-th term in
  a geometric progression with initial value (population size) and common ratio in (0,1]."
  [pop argmap]
  (loop [capacity (count pop)
         survivors (lshuffle pop)
         cases (lshuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))
      (lrand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (Math/ceil (* (:geometric-lexicase-rate argmap) capacity))
               (take capacity
                (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors))
               (rest cases))))))
