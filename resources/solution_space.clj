s1-effort
(Math/pow Math/E 
  (/ 
    (+ 
      (- (* (Math/log a))) 
      (- (* b1 (Math/log b1))) 
      (- (* (Math/log c))) 
      (* b1 (Math/log c)) 
      (* (Math/log k)) 
      (- (* b1 (Math/log k))) 
      (* b1 (Math/log p1)) 
      (* (Math/log s)) 
      (- (* b1 (Math/log s))) 
      (- (* (Math/log λ)))
    ) 
  (+ c (- k) (* k b1))))

s2-effort
(Math/pow Math/E 
  (/ 
    (+ 
      (- (* (Math/log a))) 
      (- (* b1 (Math/log b1))) 
      (- (* b2 (Math/log b2))) 
      (- (* (Math/log c))) 
      (* b1 (Math/log c)) 
      (* b2 (Math/log c)) 
      (* (Math/log k)) 
      (- (* b1 (Math/log k))) 
      (- (* b2 (Math/log k))) 
      (* b1 (Math/log p1)) 
      (* b2 (Math/log p2)) 
      (* (Math/log s)) 
      (- (* b1 (Math/log s))) 
      (- (* b2 (Math/log s))) 
      (- (* (Math/log λ)))
    ) 
  (+ c (- k) (* k b1) (* k b2))))

s3-effort
(Math/pow Math/E 
  (/ 
    (+ 
      (- (* (Math/log a))) 
      (- (* b1 (Math/log b1)))
      (- (* b2 (Math/log b2))) 
      (- (* b3 (Math/log b3))) 
      (* b1 (Math/log p1)) 
      (* b2 (Math/log p2)) 
      (* b3 (Math/log p3)) 
      (- (* b1 (Math/log λ))) 
      (- (* b2 (Math/log λ))) 
      (- (* b3 (Math/log λ))) 
      (/ 
        (+ 
           (- (* k (Math/log a))) 
           (- (* b1 k (Math/log b1))) 
           (- (* b2 k (Math/log b2))) 
           (- (* b3 k (Math/log b3))) 
           (- (* c (Math/log c))) 
           (* c (Math/log k)) 
           (* b1 k (Math/log p1)) 
           (* b2 k (Math/log p2)) 
           (* b3 k (Math/log p3)) 
           (* c (Math/log s)) 
           (- (* c (Math/log λ))) 
           (- (* b1 k (Math/log λ))) 
           (- (* b2 k (Math/log λ))) 
           (- (* b3 k (Math/log λ)))
        ) 
        (+ c (- k) (* k b1) (* k b2) (* k b3))
      ) 
      (- 
         (/ 
            (* b1 
               (+ 
                 (- (* k (Math/log a))) 
                 (- (* b1 k (Math/log b1))) 
                 (- (* b2 k (Math/log b2))) 
                 (- (* b3 k (Math/log b3))) 
                 (- (* c (Math/log c))) 
                 (* c (Math/log k)) 
                 (* b1 k (Math/log p1)) 
                 (* b2 k (Math/log p2)) 
                 (* b3 k (Math/log p3)) 
                 (* c (Math/log s)) 
                 (- (* c (Math/log λ))) 
                 (- (* b1 k (Math/log λ))) 
                 (- (* b2 k (Math/log λ))) 
                 (- (* b3 k (Math/log λ)))
               )
           )  
           (+ c (- k) (* k b1) (* k b2) (* k b3))
         )
      ) 
      (- 
         (/ (* b2 
               (+ 
                 (- (* k (Math/log a))) 
                 (- (* b1 k (Math/log b1))) 
                 (- (* b2 k (Math/log b2))) 
                 (- (* b3 k (Math/log b3))) 
                 (- (* c (Math/log c))) 
                 (* c (Math/log k)) 
                 (* b1 k (Math/log p1)) 
                 (* b2 k (Math/log p2)) 
                 (* b3 k (Math/log p3)) 
                 (* c (Math/log s)) 
                 (- (* c (Math/log λ))) 
                 (- (* b1 k (Math/log λ))) 
                 (- (* b2 k (Math/log λ))) 
                 (- (* b3 k (Math/log λ)))
               )
            ) 
            (+ c (- k) (* k b1) (* k b2) (* k b3))
         )
      ) 
      (- (/ (* b3 
               (+ 
                  (- (* k (Math/log a))) 
                  (- (* b1 k (Math/log b1))) 
                  (- (* b2 k (Math/log b2))) 
                  (- (* b3 k (Math/log b3))) 
                  (- (* c (Math/log c))) 
                  (* c (Math/log k)) 
                  (* b1 k (Math/log p1)) 
                  (* b2 k (Math/log p2)) 
                  (* b3 k (Math/log p3)) 
                  (* c (Math/log s)) 
                  (- (* c (Math/log λ))) 
                  (- (* b1 k (Math/log λ))) 
                  (- (* b2 k (Math/log λ))) 
                  (- (* b3 k (Math/log λ)))
               )
            ) 
            (+ c (- k) (* k b1) (* k b2) (* k b3))
         )
      )
    ) 
  c)
)

s4-effort
(Math/pow Math/E 
  (/ 
    (+ 
      (- (* (Math/log a))) 
      (- (* b1 (Math/log b1))) 
      (- (* b2 (Math/log b2))) 
      (- (* b3 (Math/log b3))) 
      (- (* b4 (Math/log b4))) 
      (- (* (Math/log c))) 
      (* b1 (Math/log c)) 
      (* b2 (Math/log c)) 
      (* b3 (Math/log c)) 
      (* b4 (Math/log c)) 
      (* (Math/log k)) 
      (- (* b1 (Math/log k))) 
      (- (* b2 (Math/log k))) 
      (- (* b3 (Math/log k))) 
      (- (* b4 (Math/log k))) 
      (* b1 (Math/log p1)) 
      (* b2 (Math/log p2)) 
      (* b3 (Math/log p3)) 
      (* b4 (Math/log p4)) 
      (* (Math/log s)) 
      (- (* b1 (Math/log s))) 
      (- (* b2 (Math/log s))) 
      (- (* b3 (Math/log s))) 
      (- (* b4 (Math/log s))) 
      (- (* (Math/log λ)))
    ) 
    (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4))
  )
)

s5-effort
(Math/pow Math/E 
  (/ 
    (+ 
       (- ( * (Math/log a))) 
       (- (* b1 (Math/log b1))) 
       (- (* b2 (Math/log b2))) 
       (- (* b3 (Math/log b3))) 
       (- (* b4 (Math/log b4))) 
       (- (* b5 (Math/log b5))) 
       (* b1 (Math/log p1)) 
       (* b2 (Math/log p2)) 
       (* b3 (Math/log p3)) 
       (* b4 (Math/log p4)) 
       (* b5 (Math/log p5)) 
       (- (* b1 (Math/log λ))) 
       (- (* b2 (Math/log λ))) 
       (- (* b3 (Math/log λ))) 
       (- (* b4 (Math/log λ))) 
       (- (* b5 (Math/log λ))) 
       (/ 
         (+ 
           (- (* k (Math/log a))) 
           (- (* b1 k (Math/log b1))) 
           (- (* b2 k (Math/log b2))) 
           (- (* b3 k (Math/log b3))) 
           (- (* b4 k (Math/log b4))) 
           (- (* b5 k (Math/log b5))) 
           (- (* c (Math/log c))) 
           (* c (Math/log k)) 
           (* b1 k (Math/log p1)) 
           (* b2 k (Math/log p2)) 
           (* b3 k (Math/log p3)) 
           (* b4 k (Math/log p4)) 
           (* b5 k (Math/log p5)) 
           (* c (Math/log s)) 
           (- (* c (Math/log λ))) 
           (- (* b1 k (Math/log λ))) 
           (- (* b2 k (Math/log λ))) 
           (- (* b3 k (Math/log λ))) 
           (- (* b4 k (Math/log λ))) 
           (- (* b5 k (Math/log λ)))
         ) 
         (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5))
      ) 
      (- 
        (/ 
          (* b1 
            (+ 
               (- (* k (Math/log a))) 
               (- (* b1 k (Math/log b1))) 
               (- (* b2 k (Math/log b2))) 
               (- (* b3 k (Math/log b3))) 
               (- (* b4 k (Math/log b4))) 
               (- (* b5 k (Math/log b5))) 
               (- (* c (Math/log c))) 
               (* c (Math/log k)) 
               (* b1 k (Math/log p1)) 
               (* b2 k (Math/log p2)) 
               (* b3 k (Math/log p3)) 
               (* b4 k (Math/log p4)) 
               (* b5 k (Math/log p5)) 
               (* c (Math/log s)) 
               (- (* c (Math/log λ))) 
               (- (* b1 k (Math/log λ))) 
               (- (* b2 k (Math/log λ))) 
               (- (* b3 k (Math/log λ))) 
               (- (* b4 k (Math/log λ))) 
               (- (* b5 k (Math/log λ)))
            )
          )  
         (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5))
        )
      ) 
      (- 
(/ 
(* b2 
(+ 
(- (* k (Math/log a))) 
(- (* b1 k (Math/log b1))) 
(- (* b2 k (Math/log b2))) 
(- (* b3 k (Math/log b3))) 
(- (* b4 k (Math/log b4))) 
(- (* b5 k (Math/log b5))) 
(- (* c (Math/log c))) 
(* c (Math/log k)) 
(* b1 k (Math/log p1)) 
(* b2 k (Math/log p2)) 
(* b3 k (Math/log p3)) 
(* b4 k (Math/log p4)) 
(* b5 k (Math/log p5)) 
(* c (Math/log s)) 
(- (* c (Math/log λ))) 
(- (* b1 k (Math/log λ))) 
(- (* b2 k (Math/log λ))) 
(- (* b3 k (Math/log λ))) 
(- (* b4 k (Math/log λ))) 
(- (* b5 k (Math/log λ)))
)
) 
(+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5))
)
) 
(- 
(/ 
(* b3 
(+ 
(- (* k (Math/log a))) 
(- (* b1 k (Math/log b1))) 
(- (* b2 k (Math/log b2))) 
(- (* b3 k (Math/log b3))) 
(- (* b4 k (Math/log b4))) 
(- (* b5 k (Math/log b5))) 
(- (* c (Math/log c))) 
(* c (Math/log k)) 
(* b1 k (Math/log p1)) 
(* b2 k (Math/log p2)) 
(* b3 k (Math/log p3)) 
(* b4 k (Math/log p4)) 
(* b5 k (Math/log p5)) 
(* c (Math/log s)) 
(- (* c (Math/log λ))) 
(- (* b1 k (Math/log λ))) 
(- (* b2 k (Math/log λ))) 
(- (* b3 k (Math/log λ))) 
(- (* b4 k (Math/log λ))) 
(- (* b5 k (Math/log λ)))
)
)  
(+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5))
)
) 
(- 
(/ 
(* b4 
(+ 
(- (* k (Math/log a))) 
(- (* b1 k (Math/log b1)))
(- (* b2 k (Math/log b2)))
(- (* b3 k (Math/log b3))) 
(- (* b4 k (Math/log b4))) 
(- (* b5 k (Math/log b5))) 
(- (* c (Math/log c))) 
(* c (Math/log k)) 
(* b1 k (Math/log p1)) 
(* b2 k (Math/log p2)) 
(* b3 k (Math/log p3)) 
(* b4 k (Math/log p4)) 
(* b5 k (Math/log p5)) 
(* c (Math/log s)) 
(- (* c (Math/log λ))) 
(- (* b1 k (Math/log λ))) 
(- (* b2 k (Math/log λ))) 
(- (* b3 k (Math/log λ))) 
(- (* b4 k (Math/log λ))) 
(- (* b5 k (Math/log λ)))
)
) 
(+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5))
)
) 
(- 
(/ 
(* b5 
(+ 
(- (* k (Math/log a)))
(- (* b1 k (Math/log b1))) 
(- (* b2 k (Math/log b2))) 
(- (* b3 k (Math/log b3))) 
(- (* b4 k (Math/log b4))) 
(- (* b5 k (Math/log b5))) 
(- (* c (Math/log c))) 
(* c (Math/log k)) 
(* b1 k (Math/log p1)) 
(* b2 k (Math/log p2)) 
(* b3 k (Math/log p3)) 
(* b4 k (Math/log p4)) 
(* b5 k (Math/log p5)) 
(* c (Math/log s)) 
(- (* c (Math/log λ))) 
(- (* b1 k (Math/log λ))) 
(- (* b2 k (Math/log λ))) 
(- (* b3 k (Math/log λ))) 
(- (* b4 k (Math/log λ))) 
(- (* b5 k (Math/log λ)))
)
) 
(+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5))
)
)
) c))

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


; TO-TEST
(defn effort-even-numerator [n]
  (let [n-range (range 1 (inc n))] 
    (str "(+"
         " (- (* (Math/log a)))"
         (apply str (map #(format "(- (* b%s (Math/log b%s)))" % %) 
                         n-range))
         " (- (* (Math/log c)))" 
         (apply str (map #(format " (* b%s (Math/log c)))" %) 
                         n-range))
         " (* (Math/log k))"
         (apply str (map #(format " (* b%s (Math/log k)))" %) 
                         n-range))
         (apply str (map #(format " (* b%s (Math/log p%s))" % %) 
                         n-range))
         " (* (Math/log s))"
         (apply str (map #(format " (* b%s (Math/log s)))" %) 
                         n-range))
         " (- (* (Math/log λ)))"
         ")")))


(defn effort-odd-preamble [n]
  (let [n-range (range 1 (inc n))] 
    (str " (- (* (Math/log a)))"
         (apply str (map #(format " (- (* b%s (Math/log b%s)))" % %) 
                         n-range))
         (apply str (map #(format " (* b%s (Math/log p%s))" % %) 
                         n-range))         
         (apply str (map #(format " (- (* b%s (Math/log λ)))" %) 
                         n-range))
         " (- (* (Math/log λ)))"
         )))


(defn effort-odd-quotient-numerator [n]
  (let [n-range (range 1 (inc n))]
    (str "(+"
         " (- (* k (Math/log a)))"
         (apply str (map #(format " (* b%s k (Math/log b%s))" % x) 
                        n-range))
         " (- (* c (Math/log c)))"
         " (* c (Math/log k))"
         (apply str (map #(format " (* b%s k (Math/log p%s))" % x) 
                         n-range))
         " (* c (Math/log s))"
         " (- (* c (Math/log λ)))"
         (apply str (map #(format " (- (* b%s k (Math/log λ)))" %) 
                         n-range))
         ")")))


;; TODO: Rename
(defn handle-b-predecessor [n]
  (let [b-pred (if (zero? n) 
                   "" 
                   (format " (* b%s" n))
        b-post (if (zero? n) 
                   "" 
                   ")")]
    (str b-pred
         (effort-odd-quotient-numerator n)
         (output-denominator n)
         b-post)))


(defn effort-space [n]
  "for n >= 2"
  (if (even? n)
    (str "(Math/pow Math/E (/ "
       (effort-even-numerator n)
       " "
       (output-denominator n)
       "))" )
    (str "(Math/pow Math/E (/ "
         "(+"
         (effort-odd-preamble n)
         (map handle-b-predecessor (range (inc n)))
         ")"
         "c))")))
