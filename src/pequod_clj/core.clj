(ns pequod-clj.core)

; (in-ns 'pequod-clj.core)

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
                              (take (count (first production-inputs))
                                    (repeatedly #(+ xz (rand xz))))))
          nature-exponents (let [rz (/ 0.2 (count (second production-inputs)))]
                             (take (count (second production-inputs))
                                   (repeatedly #(+ 0.05 rz (rand rz)))))
          labor-exponents (let [lz (/ 0.2 (count (last production-inputs)))]
                            (take (count (last production-inputs))
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


(def wcs
  (->> (create-wcs 80 [1 2 3 4] 1)
       (map continue-setup-wcs)))


;(sort (first wcs))
;(map :production-inputs wcs)


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


(def solution1 "{{z -> E^((-(k*Log[a]) - b1*k*Log[b1] - c*Log[c] + c*Log[k] + b1*k*Log[p1] + c*Log[s] - c*Log[Î»] - b1*k*Log[Î»])/(c - k + b1*k)), x1 -> E^((-(k*Log[a]) + c*Log[b1] - k*Log[b1] - c*Log[c] + c*Log[k] - c*Log[p1] + k*Log[p1] + c*Log[s] - k*Log[Î»])/(c - k + b1*k)), ef -> E^((-Log[a] - b1*Log[b1] - Log[c] + b1*Log[c] + Log[k] - b1*Log[k] + b1*Log[p1] + Log[s] - b1*Log[s] - Log[Î»])/(c - k + b1*k))}}")

; (Math/log 2.7) -> 0.99325...
; (Math/pow 3 5) -> 243.0

(def solution3 "{{z -> E^((-(k*Log[a]) - b1*k*Log[b1] - b2*k*Log[b2] - b3*k*Log[b3] - c*Log[c] + c*Log[k] + b1*k*Log[p1] + b2*k*Log[p2] + b3*k*Log[p3] + c*Log[s] - c*Log[Î»] - b1*k*Log[Î»] - b2*k*Log[Î»] - b3*k*Log[Î»])/(c - k + b1*k + b2*k + b3*k)), x1 -> E^((-(k*Log[a]) + c*Log[b1] - k*Log[b1] + b2*k*Log[b1] + b3*k*Log[b1] - b2*k*Log[b2] - b3*k*Log[b3] - c*Log[c] + c*Log[k] - c*Log[p1] + k*Log[p1] - b2*k*Log[p1] - b3*k*Log[p1] + b2*k*Log[p2] + b3*k*Log[p3] + c*Log[s] - k*Log[Î»])/(c - k + b1*k + b2*k + b3*k)), x2 -> E^((-(k*Log[a]) - b1*k*Log[b1] + c*Log[b2] - k*Log[b2] + b1*k*Log[b2] + b3*k*Log[b2] - b3*k*Log[b3] - c*Log[c] + c*Log[k] + b1*k*Log[p1] - c*Log[p2] + k*Log[p2] - b1*k*Log[p2] - b3*k*Log[p2] + b3*k*Log[p3] + c*Log[s] - k*Log[Î»])/(c - k + b1*k + b2*k + b3*k)), x3 -> E^((-(k*Log[a]) - b1*k*Log[b1] - b2*k*Log[b2] + c*Log[b3] - k*Log[b3] + b1*k*Log[b3] + b2*k*Log[b3] - c*Log[c] + c*Log[k] + b1*k*Log[p1] + b2*k*Log[p2] - c*Log[p3] + k*Log[p3] - b1*k*Log[p3] - b2*k*Log[p3] + c*Log[s] - k*Log[Î»])/(c - k + b1*k + b2*k + b3*k)), ef -> E^((-Log[a] - b1*Log[b1] - b2*Log[b2] - b3*Log[b3] + b1*Log[p1] + b2*Log[p2] + b3*Log[p3] - b1*Log[Î»] - b2*Log[Î»] - b3*Log[Î»] + (-(k*Log[a]) - b1*k*Log[b1] - b2*k*Log[b2] - b3*k*Log[b3] - c*Log[c] + c*Log[k] + b1*k*Log[p1] + b2*k*Log[p2] + b3*k*Log[p3] + c*Log[s] - c*Log[Î»] - b1*k*Log[Î»] - b2*k*Log[Î»] - b3*k*Log[Î»])/(c - k + b1*k + b2*k + b3*k) - (b1*(-(k*Log[a]) - b1*k*Log[b1] - b2*k*Log[b2] - b3*k*Log[b3] - c*Log[c] + c*Log[k] + b1*k*Log[p1] + b2*k*Log[p2] + b3*k*Log[p3] + c*Log[s] - c*Log[Î»] - b1*k*Log[Î»] - b2*k*Log[Î»] - b3*k*Log[Î»]))/(c - k + b1*k + b2*k + b3*k) - (b2*(-(k*Log[a]) - b1*k*Log[b1] - b2*k*Log[b2] - b3*k*Log[b3] - c*Log[c] + c*Log[k] + b1*k*Log[p1] + b2*k*Log[p2] + b3*k*Log[p3] + c*Log[s] - c*Log[Î»] - b1*k*Log[Î»] - b2*k*Log[Î»] - b3*k*Log[Î»]))/(c - k + b1*k + b2*k + b3*k) - (b3*(-(k*Log[a]) - b1*k*Log[b1] - b2*k*Log[b2] - b3*k*Log[b3] - c*Log[c] + c*Log[k] + b1*k*Log[p1] + b2*k*Log[p2] + b3*k*Log[p3] + c*Log[s] - c*Log[Î»] - b1*k*Log[Î»] - b2*k*Log[Î»] - b3*k*Log[Î»]))/(c - k + b1*k + b2*k + b3*k))/c)}}")

(def s3-ef "E^((-Log[a] - b1*Log[b1] - b2*Log[b2] - b3*Log[b3] + b1*Log[p1] + b2*Log[p2] + b3*Log[p3] - b1*Log[Î»] - b2*Log[Î»] - b3*Log[Î»] + (-(k*Log[a]) - b1*k*Log[b1] - b2*k*Log[b2] - b3*k*Log[b3] - c*Log[c] + c*Log[k] + b1*k*Log[p1] + b2*k*Log[p2] + b3*k*Log[p3] + c*Log[s] - c*Log[Î»] - b1*k*Log[Î»] - b2*k*Log[Î»] - b3*k*Log[Î»])/(c - k + b1*k + b2*k + b3*k) - (b1*(-(k*Log[a]) - b1*k*Log[b1] - b2*k*Log[b2] - b3*k*Log[b3] - c*Log[c] + c*Log[k] + b1*k*Log[p1] + b2*k*Log[p2] + b3*k*Log[p3] + c*Log[s] - c*Log[Î»] - b1*k*Log[Î»] - b2*k*Log[Î»] - b3*k*Log[Î»]))/(c - k + b1*k + b2*k + b3*k) - (b2*(-(k*Log[a]) - b1*k*Log[b1] - b2*k*Log[b2] - b3*k*Log[b3] - c*Log[c] + c*Log[k] + b1*k*Log[p1] + b2*k*Log[p2] + b3*k*Log[p3] + c*Log[s] - c*Log[Î»] - b1*k*Log[Î»] - b2*k*Log[Î»] - b3*k*Log[Î»]))/(c - k + b1*k + b2*k + b3*k) - (b3*(-(k*Log[a]) - b1*k*Log[b1] - b2*k*Log[b2] - b3*k*Log[b3] - c*Log[c] + c*Log[k] + b1*k*Log[p1] + b2*k*Log[p2] + b3*k*Log[p3] + c*Log[s] - c*Log[Î»] - b1*k*Log[Î»] - b2*k*Log[Î»] - b3*k*Log[Î»]))/(c - k + b1*k + b2*k + b3*k))/c)")

(def sol2 "{{z -> E^((-(k*Log[a]) - b1*k*Log[b1] - b2*k*Log[b2] - c*Log[c] + c*Log[k] + b1*k*Log[p1] + b2*k*Log[p2] + c*Log[s] - c*Log[Î»] - b1*k*Log[Î»] - b2*k*Log[Î»])/(c - k + b1*k + b2*k)), x1 -> E^((-(k*Log[a]) + c*Log[b1] - k*Log[b1] + b2*k*Log[b1] - b2*k*Log[b2] - c*Log[c] + c*Log[k] - c*Log[p1] + k*Log[p1] - b2*k*Log[p1] + b2*k*Log[p2] + c*Log[s] - k*Log[Î»])/(c - k + b1*k + b2*k)), x2 -> E^((-(k*Log[a]) - b1*k*Log[b1] + c*Log[b2] - k*Log[b2] + b1*k*Log[b2] - c*Log[c] + c*Log[k] + b1*k*Log[p1] - c*Log[p2] + k*Log[p2] - b1*k*Log[p2] + c*Log[s] - k*Log[Î»])/(c - k + b1*k + b2*k)), ef -> E^((-Log[a] - b1*Log[b1] - b2*Log[b2] - Log[c] + b1*Log[c] + b2*Log[c] + Log[k] - b1*Log[k] - b2*Log[k] + b1*Log[p1] + b2*Log[p2] + Log[s] - b1*Log[s] - b2*Log[s] - Log[Î»])/(c - k + b1*k + b2*k))}}")


(defn convert-log-fragment [fragment]
  (-> fragment
      (clojure.string/replace #"^\(" "")
      (clojure.string/replace #"\)$" "")
      (clojure.string/replace #"Log\[([a-z0-9λ]+)\]" "(Math/log $1)")
      (clojure.string/replace #"\*" " ")
      (clojure.string/replace #"^" "(* ")
      (clojure.string/replace #"$" ")")))

 
(defn join-subtrahends [[f s]]
  (if (= f "+")
      s
      (str "(- " s ")")))


(defn convert-numerator [to-convert]
  (-> to-convert
      (clojure.string/replace #"Î»" "λ")
      (clojure.string/replace #"^\(-" "- ")
      (clojure.string/split #" ")
      ((partial partition 2))
      ((partial map (juxt first (comp convert-log-fragment last))))
      ((partial map join-subtrahends))
      ((partial clojure.string/join " "))
      ((partial str "(+ "))
      (str ")")))


(defn convert-denominator [to-convert]
  (->> to-convert
       (re-seq #"b\d+")
       count
       inc
       (range 1)
       (map #(str "(* k b" % ")"))
       (clojure.string/join " ")
       (str "(+ c (- k) ")
       (conj [])
       (cons ")")
       reverse
       (apply str)))


(defn convert-z [s]
  (letfn [(add-surrounding-string [[n d]]
            (str "(Math/pow Math/E (/ " n " " d "))"))]
    (-> s
        (clojure.string/replace #"^E\^\(" "")
        (clojure.string/replace #"\)$" "")
        (clojure.string/split #"/")
        ((juxt (comp convert-numerator first) 
               (comp convert-denominator second)))
        add-surrounding-string)))


(defn make-variables-and-equations-map [s]
  (->> (-> s
           (clojure.string/replace #"[\{\}]" "")
           (clojure.string/split #","))
       (map #(clojure.string/split % #" -> "))
       (map (juxt (comp keyword clojure.string/trim first) 
                  (comp convert-z second)))
       (into {})))


; 10/5/2018: Resume with (make-variables-and-equations-map solution1)

(defn convert-ef-odd-top [equation]
  (-> equation
      (clojure.string/replace #"Î»" "λ")
      (clojure.string/replace #"^\(?(b\d+)?\*?\(\-\(k\*Log\[a\]\)" "(*$1(- (k*Log[a])")
      (clojure.string/split #" ")
      ((partial partition 2))
      ))

; resume: 9/25/2018
; (map convert-subfield (first (map (comp convert-ef-odd-top first) (last (wolfram->clj s3-ef)))))
; next: put this ^^ code into a function

(defn wolfram->clj [equation]
  "Takes the Wolfram API output as input, returns Clojure code as output"
  (letfn [(clean-wolfram-step-one [text]
            (-> text
                (clojure.string/replace #"\{\{" ", ")
                (clojure.string/replace #"\}\}" "")
                (clojure.string/split #", ")
                rest))
          (final-join [[f s]]
            (if (= f "+")
              s
              (str "(- " s ")")))
          (log->clj [fragment]
            (-> fragment
                (clojure.string/replace #"^\(" "")
                (clojure.string/replace #"\)$" "")
                (clojure.string/replace #"Log\[([a-z0-9λ]+)\]" "(Math/log $1)")
                (clojure.string/replace #"\*" " ")
                (clojure.string/replace #"^" "(* ")
                (clojure.string/replace #"$" ")")))
          (convert-numerator-to-clj [to-convert]
            (-> to-convert
                (clojure.string/replace #"Î»" "λ")
                (clojure.string/replace #"^E\^\(\(" "")
                (clojure.string/replace #"\)$" "")
                (clojure.string/replace #"^-" "- ")
                (clojure.string/split #" ")
                ((partial partition 2))
                ((partial map (juxt first (comp log->clj last))))
                ((partial map final-join))
                ((partial clojure.string/join " "))
                ((partial str "(+ "))
                (str ")")))
          (convert-denominator-to-clj [to-convert]
            (->> to-convert
                 (re-seq #"b\d+")
                 count
                 inc
                 (range 1)
                 (map #(str "(* k b" % ")"))
                 (clojure.string/join " ")
                 (str "(+ c (- k) ")
                 (conj [])
                 (cons ")")
                 reverse
                 (apply str)))
          (convert-n-and-d [[n d]]
            (str "(Math/pow Math/E (/"
                 (convert-numerator-to-clj n)
                 " "
                 (convert-denominator-to-clj d)
                 "))"))
          (convert-numerator-to-clj-odd-ef [to-convert]
            (-> to-convert
;                (clojure.string/replace #"Î»" "λ")
;                (clojure.string/replace #"^\(?(b\d+)?\*?\(\-\(k\*Log\[a\]\)" "(* \1 ( - (k*Log[a])")
;                (clojure.string/split #" ")
;                ((partial partition 2))
;                ((partial map (juxt first (comp log->clj last))))
;                ((partial map final-join))
;                ((partial clojure.string/join " "))
;                ((partial str "(+ "))
;                (str ")")
                ))
          (convert-n-and-d-odd-ef [[n d]]
            (str "(/ "
                 (convert-numerator-to-clj-odd-ef n)
                 " "
                 (convert-denominator-to-clj d)
                 ")"))
          (solve-odd-ef [to-convert]
            (-> to-convert
                (clojure.string/replace #"^E\^\(\(" "")
                (clojure.string/replace #"\)\/c\)$" "")
                ((juxt (comp convert-numerator-to-clj
                             #(clojure.string/replace % #"(.*?) [+-] \(.*" "$1"))
                       (comp ; (partial map convert-n-and-d-odd-ef)
                             (partial map #(clojure.string/split % #"/"))
                             (partial map #(clojure.string/replace % #"^[\+\-] " ""))
                             #(re-seq #"[\+\-] \(.*?\)\/\(.*?\)" %))))
            )
          )
         ]
    (solve-odd-ef equation)
    #_(->> equation
         clean-wolfram-step-one
         (map #(clojure.string/split % #" -> "))
         (map (juxt first (comp convert-n-and-d
                                #(clojure.string/split % #"/")
                                last)))
         (into {}))))


