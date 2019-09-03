(ns chip8clj.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.core.match :refer [match]]
            [chip8clj.cpu :as cpu]
            [chip8clj.log :as log]
            [chip8clj.util :as util]))

(def scale 20)
(def fonts
  [0xF0 0x90 0x90 0x90 0xF0                                 ; 0
   0x20 0x60 0x20 0x20 0x70                                 ; 1
   0xF0 0x10 0xF0 0x80 0xF0                                 ; 2
   0xF0 0x10 0xF0 0x10 0xF0                                 ; 3
   0x90 0x90 0xF0 0x10 0x10                                 ; 4
   0xF0 0x80 0xF0 0x10 0xF0                                 ; 5
   0xF0 0x80 0xF0 0x90 0xF0                                 ; 6
   0xF0 0x10 0x20 0x40 0x40                                 ; 7
   0xF0 0x90 0xF0 0x90 0xF0                                 ; 8
   0xF0 0x90 0xF0 0x10 0xF0                                 ; 9
   0xF0 0x90 0xF0 0x90 0x90                                 ; A
   0xE0 0x90 0xE0 0x90 0xE0                                 ; B
   0xF0 0x80 0x80 0x80 0xF0                                 ; C
   0xE0 0x90 0x90 0x90 0xE0                                 ; D
   0xF0 0x80 0xF0 0x80 0xF0                                 ; E
   0xF0 0x80 0xF0 0x80 0x80])                               ; F

(defn load-rom [game-path]
  (let [game-file (java.io.File. game-path)                 ; "java.io.File." corresponds to "new java.io.File()" \
        ; game-path is passed as a param to constructor, i.e. "new java.io.File(game-path)"
        tmp-array (byte-array (.length game-file))          ; "(.length game-file)" corresponds to "game-file.length()"
        game-stream (java.io.FileInputStream. game-file)]   ; again - "new FileInputStream(game-file)"
    (.read game-stream tmp-array)                           ; "game-stream.read(tmp-array)"
    (.close game-stream)                                    ; "game-stream.close()"
    (into [] tmp-array)))                                   ; put tmp-array into a vector and return it

(defn default-state []
  (let [rom (load-rom "./src/chip8clj/games/BRIX")]
    {:memory        (-> (into (vector-of :int) (repeat 0x1000 0))
                        (util/assoc-multiple rom 0x200)
                        (util/assoc-multiple fonts 0x0))
     :frame         (into (vector-of :boolean) (repeat (* 64 32) 0))
     :i             0
     :pc            0x200
     :stack         (vector-of :int)
     :sp            -1
     :reg           (into (vector-of :int) (repeat 16 0))
     :delay-timer   0
     :sound-timer   0
     :pressed-key   0
     ; screen-buffer will hold 32 rows of 64 columns of graphics memory data
     :screen-buffer (vec (repeat 32 (vec (repeat 64 0))))}))

(defn setup []
  (q/frame-rate 60)
  (q/background 255)
  (q/no-stroke)
  (q/text-font (q/create-font "Arial" 18 true))
  (default-state))

(defn update-timers [state]
  (if (> (state :delay-timer) 0) (assoc state :delay-timer (dec (state :delay-timer))) state))

(defn run-opcode [{:keys [memory pc] :as state} &rest]
  (let [opcode (str (format "%02x" (nth memory pc)) (format "%02x" (nth memory (inc pc))))
        [w x y z] opcode
        NNN (Integer/parseInt (str x y z) 16)
        NN (Integer/parseInt (str y z) 16)
        X (Integer/parseInt (str x) 16)
        Y (Integer/parseInt (str y) 16)
        N (Integer/parseInt (str z) 16)]
    ;(log/log-full-state state opcode)
    (-> (match [w x y z]
               [\0 \0 \e \0] (cpu/opcode-00E0 state)
               [\0 \0 \e \e] (cpu/opcode-00EE state)
               [\1 _ _ _] (cpu/opcode-1NNN state NNN)
               [\2 _ _ _] (cpu/opcode-2NNN state NNN)
               [\3 _ _ _] (cpu/opcode-3XNN state X NN)
               [\4 _ _ _] (cpu/opcode-4XNN state X NN)
               [\5 _ _ _] (cpu/opcode-5XY0 state X Y)
               [\6 _ _ _] (cpu/opcode-6XNN state X NN)
               [\7 _ _ _] (cpu/opcode-7XNN state X NN)
               [\8 _ _ \0] (cpu/opcode-8XY0 state X Y)
               [\8 _ _ \1] (cpu/opcode-8XY1 state X Y)
               [\8 _ _ \2] (cpu/opcode-8XY2 state X Y)
               [\8 _ _ \3] (cpu/opcode-8XY3 state X Y)
               [\8 _ _ \4] (cpu/opcode-8XY4 state X Y)
               [\8 _ _ \5] (cpu/opcode-8XY5 state X Y)
               [\8 _ _ \6] (cpu/opcode-8XY6 state X Y)
               [\8 _ _ \7] (cpu/opcode-8XY7 state X Y)
               [\8 _ _ \e] (cpu/opcode-8XYE state X Y)
               [\9 _ _ _] (cpu/opcode-9XY0 state X Y)
               [\a _ _ _] (cpu/opcode-ANNN state NNN)
               [\b _ _ _] (cpu/opcode-BNNN state NNN)
               [\c _ _ _] (cpu/opcode-CXNN state X NN)
               [\d _ _ _] (cpu/opcode-DXYN state X Y N)
               [\e _ \9 \e] (cpu/opcode-EX9E state X)
               [\e _ \a \1] (cpu/opcode-EXA1 state X)
               [\f _ \0 \7] (cpu/opcode-FX07 state X)
               [\f _ \0 \a] (cpu/opcode-FX0A state X)
               [\f _ \1 \5] (cpu/opcode-FX15 state X)
               [\f _ \1 \8] (cpu/opcode-FX18 state X)
               [\f _ \1 \e] (cpu/opcode-FX1E state X)
               [\f _ \2 \9] (cpu/opcode-FX29 state X)
               [\f _ \3 \3] (cpu/opcode-FX33 state X)
               [\f _ \5 \5] (cpu/opcode-FX55 state X)
               [\f _ \6 \5] (cpu/opcode-FX65 state X)
               :else (throw (Error. (format "Unknown opcode: %s" opcode))))
        (update-timers))))

(defn update-state [state]
  ;(println "FPS: " (q/current-frame-rate))
  (reduce run-opcode state (range 4)))                     ; emulate 10 opcodes

(defn draw-state [{:keys [screen-buffer] :as state}]
  (q/background 0)                                          ; clear the canvas
  (q/fill 255)
  (dotimes [y 32]
    (dotimes [x 64]
      (when-not (zero? (get-in screen-buffer [y x]))
        (q/rect (* x scale) (* y scale) scale scale))))
  (q/fill 170 71 99)
  (q/text (str "FPS: " (q/current-frame-rate)) 4 18))

(defn key-press [state {:keys [key]}]
  (let [val (cond
              (= :1 key) 0x1
              (= :2 key) 0x2
              (= :3 key) 0x3
              (= :4 key) 0xC
              (= :q key) 0x4
              (= :w key) 0x5
              (= :e key) 0x6
              (= :r key) 0xD
              (= :a key) 0x7
              (= :s key) 0x8
              (= :d key) 0x9
              (= :f key) 0xE
              (= :z key) 0xA
              (= :c key) 0xB
              (= :v key) 0xF
              :else 0)]
    (assoc state :pressed-key val)))

(defn key-release [state &rest]
  (assoc state :pressed-key 0))

(q/defsketch chip8clj
             :title "CHIP8"
             :size [(* 64 scale) (* 32 scale)]
             ; setup function called only once, should return initial state
             :setup setup
             ; update-state is called on each iteration before draw-state, should modify state
             :update update-state
             :key-pressed key-press
             :key-released key-release
             ; allows to print to repl
             :features [:no-bind-output]
             ; should only draw state
             :draw draw-state
             ; fun-mode enables the use of setup/update holding/modifying state
             :middleware [m/fun-mode])
