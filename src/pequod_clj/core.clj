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


(defn create-ccs [consumer-councils workers-per-council]
  (let [effort 1]
    (repeat consumer-councils 
            {:num-workers workers-per-council
             :effort effort
             :income (* 500 effort workers-per-council)})))


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
          ccs (create-ccs 100 10)
])
  )  
)
