(ns pequod-clj.core)

(def old-final-types [])
(def old-input-types [])
(def old-nature-prices [])
(def old-labor-prices [])

(def final-surpluses [])
(def input-surpluses [])
(def nature-surpluses [])
(def labor-surpluses [])

(def lorenz-points [])
(def gini-index-reserve [])


(defn initialize-prices []
  (let [init-final-price 100 ; these are the values set by default in interface
        init-intermediate-price 100
        init-nature-price 150
        init-labor-price 150
        finals 4
        inputs 4
        resources 1
        labors 1
        final-prices (repeat finals init-final-price)
        input-prices (repeat inputs init-intermediate price)
        nature-prices (repeat resources init-intermediate price)
        labor-prices (repeat labors init-labor-price)
        price-deltas [0.05 0.05 0.05 0.05]
        pdlist (repeat (+ finals input resources labors) 0.05)]))


(defn create-ccs [consumer-councils workers-per-council finals]
  (let [effort 1]
    (repeat consumer-councils 
            {:num-workers workers-per-council
             :effort effort
             :income (* 500 effort workers-per-council)
             :cy (+ 6 (rand 9)) ; scalar for whole product in utility function
             :final-demands (repeat 5 0)
             :utility-exponents []
             :cz (/ .5 finals)})))


(defn create-wcs [worker-councils goods industry]
   (flatten
     (for [good goods]
        (repeat (/ worker-councils 2 (count goods))
                {:industry industry
                 :product good}))))


(defn setup-wcs [wc]
  (letfn [(get-random-subset [input-seq]
            (->> input-seq
                 shuffle
                 (take (rand-nth input-seq))
                 sort))]
   (let [l1 (get-random-subset intermediate-inputs)
         l2 (get-random-subset nature-types)
         l3 (get-random-subset labor-types)
         production-inputs (vector l1 l2 l3)
         ])))


(defn setup []
  (do 
    (initialize-prices)
    (let [price-delta 0.1
          delta-delay 5
          threshold-met? false
          final-goods (range 1 (inc finals))
          intermediate-inputs (range 1 (inc inputs))
          nature-types (range 1 (inc resources))
          labor-types (range 1 (inc labors))
          ccs (create-ccs 100 10 finals)
          worker-councils 80 ; number of worker councils
          ]
      (create-wcs worker-councils final-goods 0)
      (create-wcs worker-councils intermediate-inputs 1)
    )))


; TODO: confused about input-count in 362 and 363 - help!

; https://github.com/msszczep/pequod2/blob/master/pequod2.nlogo
; http://ccl.northwestern.edu/netlogo/docs/
