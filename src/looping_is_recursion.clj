(ns looping-is-recursion)

(defn power [base exp]
  (let [
        helper (fn [acc base exp]
                 (if (zero? exp) acc
                                 (recur (* acc base) base (dec exp))))
       ]
      (helper 1 base exp)
    ))

(defn last-element [a-seq]
  (if (empty? (rest a-seq) )
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not (== (first seq1) (first seq2))) false
    :else (recur (rest seq1) (rest seq2))
    ))

(defn find-first-index [pred a-seq]
  (let [
        helper (fn [acc pred my-seq]
                 (cond
                   (empty? my-seq) nil
                   (pred (first my-seq)) acc
                   :else (recur (inc acc) pred (rest my-seq))
                   ))
        ]
    (helper 0 pred a-seq)
    ))

(defn avg [a-seq]
  (loop [ sum 0
          number 0
          seque a-seq
         ]
    (cond
      (empty? seque) (if (zero? number) nil (/ sum number))
      :else (recur (+ sum (first seque)) (inc number) (rest seque)))
    ))

(defn parity [a-seq]
  (loop [allowed-set (set nil)
         my-seq a-seq]
    (if (empty? my-seq)
      allowed-set
    (let [
          el (first my-seq)
          new-allowed-set (if (contains? allowed-set el) (disj allowed-set el) (conj allowed-set el))
          ]
      (recur new-allowed-set (rest my-seq) )))
    ))

(defn fast-fibo [n]
  (cond
    (zero? n) 0
    (= 1 n) 1
    :else (loop [fib-2 0
                 fib-1 1
                 m (dec (dec n))]
            (if (zero? m) (+ fib-1 fib-2)
                      (recur fib-1 (+ fib-1 fib-2) (dec m)))
            )
    ))

(defn cut-at-repetition [a-seq]
  (loop [
         passed-seq []
         passed-seq-set (set nil)
         my-seq a-seq
         ]
    (if (empty? my-seq) passed-seq
                       (let [
                             el (first my-seq)
                             ]
                         (if (contains? passed-seq-set el) passed-seq
                                                       (recur (conj passed-seq el) (conj passed-seq-set el) (rest my-seq)))
                         )
                       )
    ))

