(ns pequod-clj.core
  (:use [numeric.expresso.core :as ex]))

; TODO test numeric.expresso

(def old-final-types [])
(def old-input-types [])
(def old-nature-prices [])
(def old-labor-prices [])

(def lorenz-points [])
(def gini-index-reserve [])

; these are the values set by default in interface

; setup:
(def init-final-price 100)
(def init-intermediate-price 100)
(def init-labor-price 150)
(def init-nature-price 150)

(def finals 4)
(def inputs 4)
(def resources 1)
(def labors 1)

(def final-prices (repeat finals init-final-price))
(def input-prices (repeat inputs init-intermediate-price))
(def nature-prices (repeat resources init-nature-price))
(def labor-prices (repeat labors init-labor-price))

(def price-deltas [0.05 0.05 0.05 0.05])
(def pdlist (repeat (+ finals inputs resources labors) 0.05))

(def delta-delay 5)
(def price-delta 0.1)
(def threshold-met? false)

(def final-goods (range 1 (inc finals)))
(def intermediate-inputs (range 1 (inc inputs)))
(def nature-types (range 1 (inc resources)))
(def labor-types (range 1 (inc labors)))

(def final-surpluses [])
(def input-surpluses [])
(def nature-surpluses [])
(def labor-surpluses [])

(def input-exponents [])
(def nature-exponents [])
(def labor-exponents [])
(def input-exponents [])

(def ticks 0)

(defn standardize-prices []
  (def init-final-price 80)
  (def init-intermediate-price 80)
  (def init-nature-price 80)
  (def init-labor-price 80))

(defn randomize-prices []
  (def init-final-price (+ 40 (rand-nth (range 0 40))))
  (def init-intermediate-price (+ 40 (rand-nth (range 0 40))))
  (def init-nature-price (+ 30 (rand-nth (range 0 30))))
  (def init-labor-price (+ 30 (rand-nth (range 0 30)))))


(defn randomize-councils []
  (def experiment-number (rand-nth (range 0 99999))))


(defn create-ccs [consumer-councils workers-per-council finals]
  (let [effort 1
        cz (/ 0.5 finals)]
    (repeat consumer-councils
            {:num-workers workers-per-council
             :effort effort
             :income (* 500 effort workers-per-council)
             :cy (+ 6 (rand 9)) ; scalar for whole product in utility function
             :utility-exponents (take finals (repeatedly #(+ cz (rand cz))))
             :final-demands (repeat 5 0)})))


(defn create-wcs [worker-councils goods industry]
  (->> goods
       (map #(repeat (/ worker-councils 2 (count goods))
                     {:industry industry :product %}))
       flatten))


(def ccs (create-ccs 100 10 4))

(defn continue-setup-wcs [wc]
  "Assumes wc is a map"
  (letfn [(get-random-subset [input-seq]
            (->> input-seq
                 shuffle
                 (take (rand-nth input-seq))
                 sort))]
    (let [production-inputs (vector (get-random-subset intermediate-inputs)
                                    (get-random-subset nature-types)
                                    (get-random-subset labor-types))
          ; Skipping Lines 362 & 363; hoping improvements in code will make this obsolete
          xe 0.05
          c xe
          input-exponents (when (pos? (count (first production-inputs)))
                            (let [xz (/ 0.2 (count (first production-inputs)))]
                              (take (inc (count (first production-inputs)))
                                    (repeatedly #(+ xz (rand xz))))))
          nature-exponents (let [rz (/ 0.2 (count (second production-inputs)))]
                             (take (inc (count (second production-inputs)))
                                   (repeatedly #(+ 0.05 rz (rand rz)))))
          labor-exponents (let [lz (/ 0.2 (count (last production-inputs)))]
                            (take (inc (count (last production-inputs)))
                                  (repeatedly #(+ 0.05 lz (rand lz)))))
          cq 0.25
          ce 1  ; note: set as S in original Netlogo
          du 7 ; note: set as A in original Netlogo
          k du
          capital-s ce
          capital-a cq
          effort 0.5
          output 0
          labor-quantities [0]]
      (merge wc {:production-inputs production-inputs
                 :xe xe
                 :c c
                 :input-exponents input-exponents
                 :nature-exponents nature-exponents
                 :labor-exponents labor-exponents
                 :cq cq
                 :ce ce
                 :du du
                 :S capital-s
                 :A capital-a
                 :effort effort
                 :output output
                 :labor-quantities labor-quantities}))))

(def wcs (map continue-setup-wcs (create-wcs 80 [1 2 3 4] 1)))


(defn calculate-consumer-utility [cc]
  (let [final-demands (:final-demands cc)
        utility-exponents (:utility-exponents cc)
        cy (:cy cc)]
    (->> (interleave final-demands utility-exponents)
         (partition 2)
         (map #(Math/pow (first %) (last %)))
         (reduce *)
         (* cy))))


(defn update-lorenz-and-gini [ccs]
  (let [num-people (count ccs)
        sorted-wealths (mapv calculate-consumer-utility ccs)
        total-wealth (reduce + sorted-wealths)]
    (when (pos? total-wealth)
      (loop [wealth-sum-so-far 0
             index 0
             gini-index-reserve 0
             lorenz-points []
             num-people-counter 0]
        (if (= num-people num-people-counter)
          lorenz-points
          (recur (+ wealth-sum-so-far (get sorted-wealths index))
                 (inc index)
                 (+ gini-index-reserve
                    (/ index num-people)
                    (- (/ wealth-sum-so-far total-wealth)))
                 (cons (* (/ wealth-sum-so-far total-wealth) 100) lorenz-points)
                 (inc num-people-counter)))))))

(update-lorenz-and-gini ccs)


(defn proposal [wc]
  wc)

(defn consume [cc]
  cc)

(defn iterate-plan []
  (map proposal wcs)
  (map consume ccs)
)

(defn input-count [wc]
  ((comp count flatten :production-inputs) wc))

(map input-count wcs)

(defn mean [nums]
  (float (/ (reduce + nums) (count nums))))


(defn price-change [i]
  "Assumes: supply-list, demand-list, surplus-list"
  (let [supply-list-means (map mean supply-list)
        demand-list-means (map mean demand-list)
        averaged-s-and-d (->> (interleave supply-list-means
                                          demand-list-means)
                              (partition 2)
                              (map #(/ (+ %1 %2) 2)))]
    (->> (interleave (map mean surplus-list) averaged-s-and-d)
         (partition 2)
         (map #(/ %1 %2))
         #(get % i)
         Math/abs)))


(defn other-price-change [j]
  "Assumes: supply-list, demand-list, surplus-list"
  (let [averaged-s-and-d (->> (interleave (flatten supply-list)
                                          (flatten demand-list))
                              (partition 2)
                              (map #(/ (+ %1 %2) 2)))]
    (->> (interleave (flatten surplus-list) averaged-s-and-d)
         (partition 2)
         (map #(/ %1 %2))
         #(get % j))))


(defn check-surpluses [surplus-threshold]
  (letfn [(check-producers [surplus producers inputs]
            (some #(> (Math/abs (get surplus %))
                      (* surplus-threshold
                         (reduce + (map output (get producers %)))))
                  inputs))
          (check-supplies [surplus supply inputs]
            (some #(> (Math/abs (get surplus %))
                      (* surplus-threshold supply))
                  inputs))]
    (let [final-goods-check (check-producers final-surplus final-producers final-goods)
          im-goods-check (check-producers input-surplus input-producers intermediate-inputs)
          nature-check (check-supplies nature-surplus nature-resources-supply nature-types)
          labor-check (check-supplies labor-surplus labor-supply labor-types)]
      (every? nil? [final-goods-check im-goods-check nature-check labor-check]))))


(defn raise-delta [d]
  (let [price-delta (->> [0.1 (+ 0.01 d)]
                         (apply min)
                         (format "%.2f")
                         Float/valueOf)]
   {:price-delta price-delta
    :delta-delay 10}))


(defn lower-delta [d]
  (let [price-delta (->> [0.1 (- d 0.01)]
                         (apply max)
                         (format "%.2f")
                         Float/valueOf)]
   {:price-delta price-delta
    :delta-delay 5}))


; https://github.com/msszczep/pequod2/blob/master/pequod2.nlogo
; http://ccl.northwestern.edu/netlogo/docs/
