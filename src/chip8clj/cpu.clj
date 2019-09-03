(ns chip8clj.cpu
  (:require [chip8clj.util :as util]))

(defn inc-pc-by-2 [state]
  (assoc state :pc (+ 2 (state :pc))))

(defn opcode-00E0 [state]
  (-> state
      (assoc :screen-buffer (vec (repeat 32 (vec (repeat 64 0)))))
      (inc-pc-by-2)))

(defn opcode-00EE [{:keys [sp stack] :as state}]
  (-> state
      (assoc :pc (last stack))
      (assoc :stack (pop stack))
      (assoc :sp (dec sp))))

(defn opcode-1NNN [state NNN]
  (-> state
      (assoc :pc NNN)))

(defn opcode-2NNN [{:keys [stack sp pc] :as state} NNN]
  (-> state
      (assoc :stack (vec (conj stack (+ 2 pc))))
      (assoc :pc NNN)
      (assoc :sp (inc sp))))

(defn opcode-3XNN [{:keys [pc reg] :as state} X NN]
  (-> state
      (assoc :pc (if (= (reg X) NN) (+ 4 pc) (+ 2 pc)))))

(defn opcode-4XNN [{:keys [pc reg] :as state} X NN]
  (-> state
      (assoc :pc (if-not (= (reg X) NN) (+ 4 pc) (+ 2 pc)))))

(defn opcode-5XY0 [{:keys [pc reg] :as state} X Y]
  (-> state
      (assoc :pc (if (= (reg X) (reg Y)) (+ 4 pc) (+ 2 pc)))))

(defn opcode-6XNN [state X NN]
  (-> state
      (assoc-in [:reg X] NN)
      (inc-pc-by-2)))

(defn opcode-7XNN [state X NN]
  (-> state
      (assoc-in [:reg X] (bit-and 0xFF (+ NN (get-in state [:reg X]))))
      (inc-pc-by-2)))

(defn opcode-8XY0 [{:keys [reg] :as state} X Y]
  (-> state
      (assoc-in [:reg X] (reg Y))
      (inc-pc-by-2)))

(defn opcode-8XY1 [{:keys [reg] :as state} X Y]
  (-> state
      (assoc-in [:reg X] (bit-or (reg X) (reg Y)))
      (inc-pc-by-2)))

(defn opcode-8XY2 [{:keys [reg] :as state} X Y]
  (-> state
      (assoc-in [:reg X] (bit-and (reg X) (reg Y)))
      (inc-pc-by-2)))

(defn opcode-8XY3 [{:keys [reg] :as state} X Y]
  (-> state
      (assoc-in [:reg X] (bit-xor (reg X) (reg Y)))
      (inc-pc-by-2)))

(defn opcode-8XY4 [{:keys [reg] :as state} X Y]
  (let [sum (+ (reg X) (reg Y))]
    (-> state
        (assoc-in [:reg X] (bit-and sum 0xFF))
        (assoc-in [:reg 15] (if (> sum 0xFF) 1 0))
        (inc-pc-by-2))))

(defn opcode-8XY5 [{:keys [reg] :as state} X Y]
  (let [ans (- (reg X) (reg Y))]
    (-> state
        (assoc-in [:reg X] (bit-and ans 0xFF))              ; IS BIT-AND ENOUGH HERE?
        (assoc-in [:reg 15] (if (> ans 0) 1 0))
        (inc-pc-by-2))))

(defn opcode-8XY6 [{:keys [reg] :as state} X Y]
  (-> state
      (assoc-in [:reg 15] (bit-and (reg X) 0x1))
      (assoc-in [:reg X] (bit-shift-right (reg X) 1))
      (inc-pc-by-2)))

(defn opcode-8XY7 [{:keys [reg] :as state} X Y]
  (let [ans (- (reg Y) (reg X))]
    (-> state
        (assoc-in [:reg 15] (if (> (reg Y) (reg X)) 1 0))
        (assoc-in [:reg X] (bit-and ans 0xFF))
        (inc-pc-by-2))))

(defn opcode-8XYE [{:keys [reg] :as state} X Y]
  (-> state
      (assoc-in [:reg 15] (bit-shift-right (reg X) 7))
      (assoc-in [:reg X] (bit-and 0xFF (bit-shift-left (reg X) 1)))
      (inc-pc-by-2)))

(defn opcode-9XY0 [{:keys [reg pc] :as state} X Y]
  (-> state
      (assoc :pc (+ pc (if-not (= (reg X) (reg Y)) 4 2)))))

(defn opcode-ANNN [state NNN]
  (-> state
      (assoc :i NNN)
      (inc-pc-by-2)))

(defn opcode-BNNN [{:keys [reg] :as state} NNN]
  (-> state
      (assoc :pc (+ NNN (reg 0)))))

(defn opcode-CXNN [state X NN]
  (-> state
      (assoc-in [:reg X] (bit-and NN (rand-int 256)))
      (inc-pc-by-2)))

; only sets the VF according to spec (if any bits are flipped from 1 to 0 during sprite draw)
(defn drawing-set-vf [state orig new]
  ;(println "orig " orig "new " new)
  (loop [o orig
         n new
         st state]
    (if-not (empty? o)
      (if (= (first o) (first n) 1)
        (assoc-in st [:reg 15] 1)
        (recur (next o) (next n) st))
      st)))

(defn draw-line [state X Y-orig line]
  (println "line: " line " at (" X ", " Y-orig ")")
  (let [Y (if (> Y-orig 31) (mod Y-orig 31) Y-orig)
        l (if (> (- 64 X) 8) line (subvec line 0 (- 64 X)))]
    (-> state
        (drawing-set-vf (subvec (get-in state [:screen-buffer Y]) X) l)
        (assoc-in [:screen-buffer Y] (util/xor-multiple (get-in state [:screen-buffer Y]) l X)))))

(defn draw-sprite [{:keys [memory screen-buffer] :as state-initial} X Y lines]
  (loop [st state-initial
         N (range lines)]
    ; if we have any more lines to draw - draw them
    (if-not (empty? N)
      ; read one line of graphics data from memory
      (let [b (get-in st [:memory (+ (first N) (st :i))])
            line (reverse (reduce #(conj %1 (bit-and 0x1 (bit-shift-right b %2))) [] (range 8)))]
        ; draw one line and loop
        (recur (draw-line st X (+ Y (first N)) (vec line))
               (next N)))
      ; if no more lines are to be drawn - return state
      st)))

(defn opcode-DXYN [state X Y N]
  (-> state
      (assoc-in [:reg 15] 0) ;set VF to 0 first as it's used for collision detection
      (draw-sprite (get-in state [:reg X]) (get-in state [:reg Y]) N)
      (inc-pc-by-2)))

(defn opcode-EX9E [state X]
  (if (= (get-in state [:reg X]) (state :pressed-key))
    (assoc state :pc (+ 4 (state :pc)))
    (inc-pc-by-2 state)))

(defn opcode-EXA1 [state X]
  (if (= (get-in state [:reg X]) (state :pressed-key))
    (inc-pc-by-2 state)
    (assoc state :pc (+ 4 (state :pc)))))

(defn opcode-FX07 [state X]
  (-> state
      (assoc-in [:reg X] (state :delay-timer))
      (inc-pc-by-2)))

(defn opcode-FX0A [state X]
  (if (> (state :pressed-key) 0)
    (-> state
        (assoc-in [:reg X] (state :pressed-key))
        (inc-pc-by-2))
    state))

(defn opcode-FX15 [state X]
  (-> state
      (assoc :delay-timer (get-in state [:reg X]))
      (inc-pc-by-2)))

(defn opcode-FX18 [state X]
  (-> state
      (assoc :sound-timer (get-in state [:reg X]))
      (inc-pc-by-2)))

(defn opcode-FX1E [state X]
  (-> state
      (assoc :i (bit-and 0xFFFF (+ (get-in state [:reg X]) (state :i))))
      (inc-pc-by-2)))

(defn opcode-FX29 [state X]
  (-> state
      (assoc :i (* 5 (get-in state [:reg X])))
      (inc-pc-by-2)))

(defn opcode-FX33 [state X]
  (let [bcd (format "%03d" (get-in state [:reg X]))
        [h t o] bcd]
    (-> state
        (assoc-in [:memory (state :i)] (Integer/parseInt (str h)))
        (assoc-in [:memory (+ 1 (state :i))] (Integer/parseInt (str t)))
        (assoc-in [:memory (+ 2 (state :i))] (Integer/parseInt (str o)))
        (inc-pc-by-2))))

(defn opcode-FX55 [state X]
  (-> state
      ((fn [st]
         (loop [n (range (+ 1 X))
                s state]
           (if-not (empty? n)
             (recur (next n)
                    (assoc-in s
                              [:memory (+ (first n) (s :i))]
                              (get-in s [:reg (first n)])
                              ))
             s))))
      (inc-pc-by-2)))

(defn opcode-FX65 [state X]
  (-> state
      ((fn [st]
         (loop [n (range (+ 1 X))
                s state]
           (if-not (empty? n)
             (recur (next n)
                    (assoc-in s
                              [:reg (first n)]
                              (get-in s [:memory (+ (first n) (s :i))])
                              ))
             s))))
      (inc-pc-by-2)))

