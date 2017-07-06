(fn [state time-left]
  (def turn-directions [:right :left :about-face])
  (def smoke-directions [:forward :backward :left :right :drop])
  (def arena-size 20)
  (def arena-half (/ arena-size 2))
  (def shot-range 5)
  
  (defn add-locs
    "Add local :x and :y coordinates to state matrix"
    [arena]
    (reduce 
      #(conj %1 
        (reduce 
          (fn [acc node] (conj acc (assoc node :x (count acc) :y (count %1))))
          [] %2))
      [] arena))
  
  (defn in?
      "Return true if coll contains elem"
      [elem coll]
      (some #(= elem %) coll))
  
  (defn filter-arena
      "Filter the arena to return only nodes that contain one of the given type"
      ([arena] (flatten arena))
      ([arena & filters]
      (let [node-list (flatten arena)]
        (filter #(in? (get-in % [:contents :type]) filters) node-list))))
  
  (defn get-direction
      "Get the current direction of your wombat from the 2d arena array"
      [arena]
      (get-in (nth (nth arena 3) 3) [:contents :orientation]))
  
  (defn facing
      "Returns true if a move forward will bring you closer to desired location
      If no self coordinates are provided, use distance from {:x 3 :y 3}"
      ([dir node self-node]
        (case dir 
              "s" (if (>= arena-half (:y self-node))
                      (and (< (:y self-node) (:y node)) (<= (:y node) (+ (:y self-node) arena-half)))
                      (or (> (:y node) (:y self-node)) (>= (- (:y self-node) arena-half) (:y node))))
              "w" (if (<= arena-half (:x self-node))
                      (and (> (:x self-node) (:x node)) (>= (:x node) (- (:xa self-node) arena-half)))
                      (or (< (:x node) (:x self-node)) (>= (- (:x node) arena-half) (:x self-node))))
              "n" (if (<= arena-half (:y self-node))
                      (and (> (:y self-node) (:y node)) (>= (:y node) (- (:y self-node) arena-half)))
                      (or (< (:y node) (:y self-node)) (>= (- (:y node) arena-half) (:y self-node))))
              "e" (if (>= arena-half (:x self-node))
                      (and (< (:x self-node) (:x node)) (<= (:x node) (+ (:x self-node) arena-half)))
                      (or (> (:x node) (:x self-node)) (>= (- (:x self-node) arena-half) (:x node))))
              nil))
      ([dir node]
        (facing dir node {:x 3 :y 3})))
  
  (defn distance-to-tile
      "Get the number of moves it would take to move from current location.
      If no self coordinates are provided, use distance from {:x 3 :y 3}"
      ([dir node self-node]
        (+ (Math/abs (- (:y node) (:y self-node)))
           (Math/abs (- (:x node) (:x self-node)))
           (if (facing dir node self-node) 0 1)))
      ([dir node]
        (distance-to-tile dir node {:x 3 :y 3})))
    
  (defn clear?
      "Returns true if given tile is open or contains food"
      [tile]
      (in? (get-in tile [:contents :type]) ["open" "food"]))
  
  (defn turn-to-dir
      "Returns one of [:right :left :about-face]"
      [curr-dir next-dir]
      (def ^:private orientations ["n" "e" "s" "w"])
      (let [curr-idx (.indexOf orientations curr-dir)
            next-idx (.indexOf orientations next-dir)]
            (case (mod (- curr-idx next-idx) 4)
                0  nil
                1  :left
                2  :about-face
                3  :right)))
  
  (defn can-shoot?
      "Returns true if there is a barrier, Zakano, or Wombat within shooting range"
      ([dir arena self]
        (println (:x self))
        (def shootable (case dir
            "n" #(and (= (:x self) (:x %)) (>= 5 (mod (- (:y self) (:y %)) 20)))
            "e" #(and (= (:y self) (:y %)) (>= 5 (mod (- (:x %) (:x self)) 20)))
            "s" #(and (= (:x self) (:x %)) (>= 5 (mod (- (:y %) (:y self)) 20)))
            "w" #(and (= (:y self) (:y %)) (>= 5 (mod (- (:x self) (:x %)) 20)))
            #(false)))
        (let [shootable
              (filter shootable (filter-arena arena "wood-barrier" "steel-barrier" "zakano" "wombat"))]
            (filter #(not (and (= (:x %) (:x self)) (= (:y self) (:y %)))) shootable)))
      ([dir arena] (can-shoot? dir arena {:x 3 :y 3})))
  
  (def possible-points 
    (filter-arena (add-locs (get-in state [:arena])) 
                  "food" "wood-barrier" "zakano" "wombat"))
  (let [command-options [(repeat 0 {:action :move
                                     :metadata {}})
                         (repeat 1 {:action :turn
                                    :metadata {:direction (rand-nth turn-directions)}})
                         (repeat 0 {:action :shoot
                                      :metadata {}})
                         (repeat 0 {:action :smoke
                                    :metadata {:direction (rand-nth smoke-directions)}})]]

    {:command (rand-nth (flatten command-options))
     
     
     :state {:direction (get-direction (:arena state))
             :shootable (can-shoot? (get-direction (:arena state)) (add-locs (:arena state)))
             :distance (distance-to-tile (get-direction (:arena state)) {:x 4 :y 3})}}))