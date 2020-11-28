(ns chip8emu.display
  (:gen-class)
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [clojure.java.io :as io]
            [chip8emu.core :refer :all]))

(def width 640)
(def height 320)

(defn draw-display
  [machine]
  (let [display (machine :display)]
    (q/background 255)
    (q/fill 0)
    (doseq [x (range 64)
            y (range 32)]
      (let [pixel ((display y) x)
            xCoord (* x 10)
            yCoord (* y 10)]
        (cond
          (= pixel 1) (let [width 10]
                        (q/rect xCoord yCoord width width)))))))

(defn update-machine
  [machine]
  (delay-tick machine (q/key-pressed?) (q/key-as-keyword)))

(defn file->bytes [file]
  (with-open [xin (io/input-stream file)
              xout (java.io.ByteArrayOutputStream.)]
    (io/copy xin xout)
    (.toByteArray xout)))

(defn getByteVector
  [gamePath]
  (vec (map #(Byte/toUnsignedInt %) (file->bytes gamePath))))

(defn copy-into
  [v1 v2]
  (let [intSpace (take 512 v1)
        zeroRest (- 4095 (count v2))]
    (vec (concat intSpace v2 (repeat zeroRest 0)))))

(defn setup-game
  [game-path]
  (fn []
    (q/frame-rate 300)
    (q/background 255)
    (let [gameVec (getByteVector game-path)
          gameMem (copy-into initmemory gameVec)]
      (assoc chip8 :memory gameMem))))

(defn run-game [game-name]
  (q/sketch
   :host "host"
   :size [width height]
   :setup (setup-game game-name)
   :update update-machine
   :draw draw-display
   :middleware [m/fun-mode]))
