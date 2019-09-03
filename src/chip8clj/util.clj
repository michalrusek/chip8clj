(ns chip8clj.util)

(defn to-unsigned-byte [b] (bit-and b 0xFF))

(defn assoc-multiple [v new-vals start]
  (reduce #(assoc %1 (+ %2 start) (to-unsigned-byte (get new-vals %2))) v (range (count new-vals))))

(defn xor-multiple [v new-vals start]
  ;(println v new-vals start)
  (reduce #(assoc %1 (+ %2 start) (bit-xor (get %1 (+ %2 start)) (to-unsigned-byte (get new-vals %2)))) v (range (count new-vals))))