(ns chip8clj.log)

; for now we'll just print everything to console
(defn log-full-state [{:keys [i pc sp reg delay-timer stack] :as state} next-opcode]
  (println "I: " (format "0x%x" i)
           "| PC: " (format "0x%x" pc)
           "| SP: " (format "0x%x" sp)
           "| next-opcode: " (format "0x%s" next-opcode)
           "| delay: " (format "0x%x" delay-timer)
           "| reg: " (map #(format "%d: 0x%x" %2 %1) reg (range (count reg)))
           "| stack: " (map #(format "%d: 0x%x" %2 %1) stack (range (count stack))))
  )