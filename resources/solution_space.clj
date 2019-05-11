(defn output-numerator [n]
  (str "(+"
       " (- (* k (Math/log a)))"
       " (* c (Math/log k))"
       " (* c (Math/log s))"
       " (- (* c (Math/log λ)))"
       (apply str (map #(format " (- (* b%s k (Math/log b%s))) (* b%s k (Math/log p%s)) (- (* b%s k (Math/log λ)))" % % % % % %) 
                        (range 1 (inc n))))
       ")"))


(defn output-denominator [n]
  (str "(+ c (- k)"
       (apply str (map #(format " (* k b%s)" %) 
                       (range 1 (inc n))))
       ")"))

(defn output [n]
  (str "(Math/pow Math/E (/ "
       (output-numerator n)
       " "
       (output-denominator n)
       "))" ))

(defn x-space-numerator [n x]
  (let [all-but-x (remove #(= x %) 
                          (range 1 (inc n)))] 
    (str "(+"
         " (- (* k (Math/log a)))"
         (apply str (map #(format " (* b%s k (Math/log b%s))" % x) 
                        all-but-x))
         (format " (* c (Math/log b%s))" x)
         (format " (- (* k (Math/log b%s)))" x) 
         (apply str (map #(format " (- (* b%s k (Math/log b%s)))" % %) 
                         all-but-x))
         " (- (* c (Math/log c)))"
         " (* c (Math/log k))"
         (apply str (map #(format " (- (* b%s k (Math/log p%s)))" % x) 
                         all-but-x))
         (format " (* k (Math/log p%s))" x)
         (format " (- (* c (Math/log p%s)))" x)
         (apply str (map #(format " (* b%s k (Math/log p%s))" % %) 
                         all-but-x))
         " (* c (Math/log s))"
         " (- (* k (Math/log λ)))"
         ")")))

(defn x-space [n x]
  (str "(Math/pow Math/E (/ "
       (x-space-numerator n x)
       " "
       (output-denominator n)
       "))" ))
