(ns chip8emu.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

(def memory (vec (repeat 512 0))) ;; first 512 bytes reserved for interpreter
;; (into (vec (repeat 63 0)) (vec (file->bytes game))) ; append game data to vector
(def registers (vec (repeat 16 0)))
(def program-counter 0)
(def I 0) ;; register
(def dt 0); delay timer
(def st 0); sound timer
(def stack (list)) ; list of numbers
                   ;
(def clear-display (vec (repeat 64 (vec (repeat 32 0)))));; 64x32 display initialized to 0;

;; millis
;; (System/currentTimeMillis)
;;

;; entire initial machine state
(def chip8 {:memory (vec (repeat 512 0))
            :registers (vec (repeat 16 0))
            :pc 0
            :i (vec (repeat 16 0)) ;; i is at i[0], but can have up to 16 offsets?
            :dt 0
            :st 0
            :wait false
            :stack (list)
            :display (vec (repeat 64 (vec (repeat 32 0))))})

;; keys used for keyboard
(def keycodes {
               :1 0x1 :2 0x2 :3 0x3 :4 0xC
               :q 0x4 :w 0x5 :e 0x6 :r 0xD
               :a 0x7 :s 0x8 :d 0x9 :f 0xE
               :z 0xA :x 0x0 :c 0xB :v 0xF})

;;; display numbers ; contains 80 bytes
(def zero  [0xF0 0x90 0x90 0x90 0xF0])
(def one   [0x20 0x60 0x20 0x20 0x70])
(def two   [0xF0 0x10 0xF0 0x80 0xF0])
(def three [0xF0 0x10 0xF0 0x10 0xF0])
(def four  [0x90 0x90 0xF0 0x10 0x10])
(def five  [0xF0 0x80 0xF0 0x10 0xF0])
(def six   [0xF0 0x80 0xF0 0x90 0xF0])
(def seven [0xF0 0x10 0x20 0x40 0x40])
(def eight [0xF0 0x90 0xF0 0x90 0xF0])
(def nine  [0xF0 0x90 0xF0 0x10 0xF0])
(def A     [0xF0 0x90 0xF0 0x90 0x90])
(def B     [0xE0 0x90 0x90 0x90 0xE0])
(def C     [0xF0 0x80 0x80 0x80 0xF0])
(def D     [0xE0 0x90 0x90 0x90 0xE0])
(def E     [0xF0 0x80 0xF0 0x80 0xF0])
(def F     [0xF0 0x80 0xF0 0x80 0x80])

;;;;;;;;;;;;;;;; Chip-8 instructions ;;;;;;;;;;;;;;;;

;;; clear the display
(defn cls [machine]
  (assoc machine :display clear-display))

;;; return from a subroutine
(defn ret [{stack :stack, :as machine}]
  (let [pc (first stack)]
    (assoc (update machine :stack rest) :pc pc)))

;;; jump to address
(defn jp [machine addr]
  (assoc machine :pc addr))

;;; call subroutine at addr
(defn call [machine addr]
  (assoc (update machine :stack conj (machine :pc)) :pc addr))

;;; skip next instruction if register value equals passed value
(defn skip-equal-byte [machine register b]
  (let [rVal ((machine :registers) register)]
    (cond
      (= rVal b) (update machine :pc #(+ 2 %))
      :else (update machine :pc inc))))

;; skip next instruction if register value doesn't equal passed value
(defn skip-not-equal-byte [machine register b]
  (let [rval ((machine :registers) register)]
    (cond
      (not= rVal b) (update machine :pc #(+ 2 %))
      :else (update machine :pc inc))))

;; skip next instruction if registers passed are equal
(defn skip-equal-registers [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)]
    (cond
      (= r1Val r2Val) (update machine :pc #(+ 2 %))
      :else (update machine :pc inc))))

;; put value into register
(defn load-register-byte [machine register b]
  (assoc-in machine [:registers register] b))

;; add value to register
(defn add-register-byte [machine register b]
  (update-in machine [:registers register] #(+ b %)))

;; set register 1 to value in register 2
(defn copy-register [machine r1 r2]
  (assoc-in machine [:registers r1] ((machine :registers) r2)))

;; perform bitwise or on r1 and r2 values, store result in r1
(defn or-register [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)
        orVal (bit-or r1Val r2Val)]
    (assoc-in machine [:registers r1] orVal)))

;; perform bitwise and on r1 and r2 values, store result in r1
(defn and-register [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)
        andVal (bit-and r1Val r2Val)]
    (assoc-in machine [:registers r1] andVal)))

;; perform xor on r1 and r2 values, store result in r1
(defn xor-register [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)
        xorVal (bit-xor r1Val r2Val)]
    (assoc-in machine [:registers r1] xorVal)))

;; add r1 and r2 registers, and store in r1. If result is > 8 bits, set VF to 1
;; can only add up to 255
(defn add-registers [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)
        addVal (+ r1Val r2Val)]
    (assoc-in
     (cond
      (> addVal 255) (assoc-in machine [:registers 0xF] 1)
      :else (assoc-in machine [:registers 0xF] 0))
     [:registers r1]
     (bit-and 0xFF addVal))))

;; subtract r2 from r1, and store in r1.
;; if r2 > r1, set VF to 1
(defn sub-registers [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)
        subVal (mod (- r1Val r2Val) 0x100)]
    (assoc-in
     (cond
      (> r2Val r1Val) (assoc-in machine [:registers 0xF] 1)
      :else (assoc-in machine [:registers 0xF] 0)))
    [:registers r1]
    subVal))

;; shift register right
(defn shift-right-register [machine register]
  (let [rVal ((machine :registers) register)]
    (update-in
     (cond
      (odd? rVal) (assoc-in machine [:registers 0xF] 1)
      :else (assoc-in machine [:registers 0xF] 0))
     [:registers register] bit-shift-right 1)))

;; subtract r1 from r2, store in r1
;; if r1 > r2 set vF to 1
(defn sub-registers-rev [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)
        subVal (mod (- r2Val r1Val) 0x100)]
    (assoc-in
     (cond
       (> r2Val r1Val) (assoc-in machine [:registers 0xF] 1)
       :else (assoc-in machine [:registers 0xF] 0)))
    [:registers r1]
    subVal))

;; shift left, if most significant bit is 1, set vf to 1
(defn shift-left-register [machine register]
  (let [rVal ((machine :registers) register)
        msbSet (= (bit-and 0x80 rVal) 0x80)] ;; most-significant bit set
    (update-in
     (cond
      msbSet (assoc-in machine [:registers 0xF] 1)
      :else (assoc-in machine [:registers 0xF] 0))
     [:registers register]
     #(bit-and 0xFF (bit-shift-left % 1)))))


(defn skip-next-instruction-not-equal [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)]
    (cond
      (= r1Val r2Val) (update-in machine :pc #(+ 2 %))
      :else (update machine :pc inc))))

;; set i register
(defn set-i [machine addr]
  (assoc machine :i addr))


;; jump to location + v0
(defn jump-v0 [machine addr]
  (let [v0 ((machine :registers) 0)
        jmploc (+ v0 addr)]
    (assoc machine :pc jmploc)))

;; generate random number, store in register
(defn random-register
  [machine register mask]
  (let [randnum (rand-int 0x100)
        masked (bit-and randnum mask)]
    (assoc-in machine [:registers register] masked)))


(defn skip-if-keypressed
  [machine keypressed? key-code key-symbol]
  (cond
    (and keypressed? (= (keycodes key-symbol) key-code)) (update machine :pc #(+ 2 %))
    :else (update machine :pc inc)))


(defn skip-if-notkeypressed
  [machine keypressed? key-code key-symbol]
  (cond
    (not (and keypressed? (= (keycodes key-symbol) key-code))) (update machine :pc #(+ 2 %))
    :else (update machine :pc inc)))

(defn loaddt
  [{dt :dt, :as machine} register]
  (assoc-in machine [:registers register] dt))

;; Wait for key press, don't increment pc!
(defn key-wait
  [machine register keypressed? key-symbol]
  (cond
    keypressed? (assoc-in machine [:registers register] (keycodes key-symbol))
    :else machine))

;; set DT from register value
(defn setdt
  [machine register]
  (assoc machine :dt ((machine :registers) register)))

(defn setst
  [machine register]
  (assoc machine :st ((machine :registers) register)))

;; add value from register to i, store in i
(defn addi
  [machine register]
  (let [rVal ((machine :registers) register)
        iVal ((machine :i) 0)
        sumVals (bit-and (+ rVal iVal) 0xFF)]
    (assoc-in machine [:i 0] sumVals)))

;; store BCD representation of Vx in i
(defn bcdi
  [machine register]
  (let [rVal ((machine :registers) register)
        hundreds (quot rVal 100)
        tens    (mod (quot rVal 10) 10)
        ones    (mod rVal 10)]
    (assoc-in
     (assoc-in
      (assoc-in machine [:i 0] hundreds)
      [:i 1] tens)
     [:i 2] ones)))

;; store values from V0 to Vx in i0 to ix
(defn load-to-i
  [machine topr]
  (let [registers (machine :registers)
        iaddrs    (machine :i)
        updatedIs (copy-first-n iaddrs registers topr)]
    (assoc machine :i updatedIs)))

(defn read-from-i
  [machine topr]
  (let [registers (machine :registers)
        iaddrs    (machine :i)
        updatedRs (copy-first-n iaddrs registers topr)]
    (assoc machine :registers updatedRs)))





;;;;;;;; helper
(defn copy-first-n
  [vec1 vec2 n]
  (let [numTake (inc n)])
  (vec (concat (take numTake vec2) (drop numTake vec1))))




;;;;;;;;;;;;;;;; TODO ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; draw ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; all arithmatic operations should be masked with 0xFF
;;;;;;;;;;;;;;;; inc pc on all required commands
;;;;;;;;;;;;;;;; Fx29
(defn file->bytes [file]
  (with-open [xin (io/input-stream file)
              xout (java.io.ByteArrayOutputStream.)]
    (io/copy xin xout)
    (.toByteArray xout)))


;; get commands from bytes
;;(map #(Integer/toString (Byte/toUnsignedInt (nth memory %)) 16) (range 100))
;;

;; separate byte array into 4 bit chunks
(defn fourbits [barr]
  (mapcat (fn [a] [(bit-shift-right a 4) (bit-and 2r1111 a)]) barr))


(defn appendbytes
  ([b1 b2] (bit-xor (bit-shift-left b1 4) b2))
  ([b1 b2 b3] (bit-xor (bit-shift-left b1 8) (bit-shift-left b2 4) b3)))

(defn printBytes [barr]
  (map #(println (Integer/toUnsignedString % 2)) barr))

(defn printHex [barr]
  (map #(println (Integer/toUnsignedString % 16)) barr))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (file->bytes "/home/ronbrz/code/chip8/games/MAZE"))
