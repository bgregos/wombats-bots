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
  (defn in?
      "Return true if coll contains elem"
      [elem coll]
      (some #(= elem %) coll))
  (defn filter-arena
      "Filter the arena to return only nodes that contain one of the given type"
      [arena & filters]
      (let [node-list (flatten arena)]
        (filter #(in? (get-in % [:contents :type]) filters) node-list)))
  (let [command-options [(repeat 0 {:action :move
                                     :metadata {}})
                         (repeat 2 {:action :turn
                                    :metadata {:direction (rand-nth turn-directions)}})
                         (repeat 0 {:action :shoot
                                      :metadata {}})
                         (repeat 0 {:action :smoke
                                    :metadata {:direction (rand-nth smoke-directions)}})]]

    {:command (rand-nth (flatten command-options))
     :state {:state (filter-arena (add-locs (get-in state [:arena])) "fog")}}))
