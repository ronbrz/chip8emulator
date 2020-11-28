(ns chip8emu.core-test
  (:require [clojure.test :refer :all]
            [chip8emu.core :refer :all]))

(def clearmachine chip8)

;;;;;;;;;;;;;;;; test opcodes ;;;;;;;;;;;;;;;;
(def full-screen
  (vec (repeat 32 (vec (repeat 64 1)))))


(deftest cls-test
  (let [full-machine (assoc clearmachine :display full-screen)]
    (is (= (clearmachine :display) ((cls full-machine) :display)))))







;; (deftest cls-test
;;   (testing "FIXME, I fail."
;;     (is (= 0 1))))

(def zero-screen
  (vec (concat
        [(vec (concat [1 1 1 1] (repeat 60 0)))
         (vec (concat [1 0 0 1] (repeat 60 0)))
         (vec (concat [1 0 0 1] (repeat 60 0)))
         (vec (concat [1 0 0 1] (repeat 60 0)))
         (vec (concat [1 1 1 1] (repeat 60 0)))]
        (repeat 27 (vec (repeat 64 0))))))

(def zero-screen-over
  (vec (concat
        [(vec (concat [0 1 1 1 1] (repeat 59 0)))
         (vec (concat [0 1 0 0 1] (repeat 59 0)))
         (vec (concat [0 1 0 0 1] (repeat 59 0)))
         (vec (concat [0 1 0 0 1] (repeat 59 0)))
         (vec (concat [0 1 1 1 1] (repeat 59 0)))]
        (repeat 27 (vec (repeat 64 0))))))


(def test-machine
  (set-register clearmachine 1 1 ))

(deftest draw-test
  (let [drawn-machine (draw clearmachine 0 0 5)
        drawn-screen  (drawn-machine :display)
        drawn-shifted-x (draw test-machine 1 0 5)]
    (is (= drawn-screen zero-screen))
    (is (= (drawn-shifted-x :display) zero-screen-over))))

;; Fx55
