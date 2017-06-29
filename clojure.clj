(fn [state time-left]
  (def turn-directions [:right :left :about-face])
  (def smoke-directions [:forward :backward :left :right :drop])
  (defn add-locs
    "Add local :x and :y coordinates to state matrix"
    [state]
    (reduce 
      #(conj %1 
        (reduce 
          (fn [acc node] (conj acc (assoc node :x (count acc) :y (count %1))))
          [] %2))
      [] state))
  (let [command-options [(repeat 10 {:action :move
                                     :metadata {}})
                         (repeat 2 {:action :turn
                                    :metadata {:direction (rand-nth turn-directions)}})
                         (repeat 4 {:action :shoot
                                      :metadata {}})
                         (repeat 1 {:action :smoke
                                    :metadata {:direction (rand-nth smoke-directions)}})]]

    {:command (rand-nth (flatten command-options))
     :state {}}))
