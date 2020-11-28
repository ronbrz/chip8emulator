(ns chip8emu.main

  (:require [clojure.java.io :as io]
            [chip8emu.display :refer :all]))

(defn -main [& args]
  (run-game (nth args 0)))
