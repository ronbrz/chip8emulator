(ns chip8emu.core
  (:gen-class))

(def clear-display (vec (repeat 32 (vec (repeat 64 0)))));; 64x32 display initialized to 0;

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

;; memory is defined as a vector full of numbers, each number being a byte
(def initmemory (vec (concat zero one two three four
                             five six seven eight nine
                             A B C D E F (repeat 4016 0))))

;; entire initial machine state
(def chip8 {:memory initmemory
            :registers (vec (repeat 16 0))
            :pc 512
            :i 0
            :dt 0
            :st 0
            :wait false
            :stack (list)
            :display clear-display})

(defn incpc [machine]
  (update machine :pc #(+ 2 %)))

(defn set-register [machine reg val]
  (assoc-in machine [:registers reg] val))

;;;;;;;;;;;;;;;; Chip-8 instructions ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; ignored
;;; 0NNN
(defn ignored [machine]
  (incpc machine))

;;; clear the display
;;; 00E0
(defn cls [machine]
  (incpc (assoc machine :display clear-display)))

;;; return from a subroutine
;;; 00EE
(defn ret [{stack :stack, :as machine}]
  (let [pc (first stack)]
    (incpc (assoc (update machine :stack rest) :pc pc))))

;;; jump to address
;;; 1nnn
(defn jp [machine addr]
  (assoc machine :pc addr))

;;; call subroutine at addr
;;; 2nnn
(defn call [machine addr]
  (assoc (update machine :stack conj (machine :pc)) :pc addr))

;;; skip next instruction if register value equals passed value
;;; 3xkk
(defn skip-equal-byte [machine register b]
  (let [rVal ((machine :registers) register)]
    (cond
      (= rVal b) (incpc (incpc machine))
      :else (incpc machine))))

;; skip next instruction if register value doesn't equal passed value
;; 4xkk
(defn skip-not-equal-byte [machine register b]
  (let [rVal ((machine :registers) register)]
    (cond
      (not= rVal b) (incpc (incpc machine))
      :else (incpc machine))))

;; skip next instruction if registers passed are equal
;; 5xy0
(defn skip-equal-registers [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)]
    (cond
      (= r1Val r2Val) (incpc (incpc machine))
      :else (incpc machine))))

;; put value into register
;; 6xkk
(defn load-register-byte [machine register b]
  (incpc (assoc-in machine [:registers register] b)))

;; add value to register
;; 7xkk
(defn add-register-byte [machine register b]
  (incpc (update-in machine [:registers register] #(bit-and (+ b %) 0xFF))))

;; set register 1 to value in register 2
;; 8xy0
(defn copy-register [machine r1 r2]
  (incpc (assoc-in machine [:registers r1] ((machine :registers) r2))))

;; perform bitwise or on r1 and r2 values, store result in r1
;; 8xy1
(defn or-register [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)
        orVal (bit-or r1Val r2Val)]
    (incpc (assoc-in machine [:registers r1] orVal))))

;; perform bitwise and on r1 and r2 values, store result in r1
;; 8xy2
(defn and-register [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)
        andVal (bit-and r1Val r2Val)]
    (incpc (assoc-in machine [:registers r1] andVal))))

;; perform xor on r1 and r2 values, store result in r1
;; 8xy3
(defn xor-registers [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)
        xorVal (bit-xor r1Val r2Val)]
    (incpc (assoc-in machine [:registers r1] xorVal))))

;; add r1 and r2 registers, and store in r1. If result is > 8 bits, set VF to 1
;; can only add up to 255
;; 8xy4
(defn add-registers [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)
        addVal (+ r1Val r2Val)]
    (incpc (assoc-in
            (cond
              (> addVal 255) (assoc-in machine [:registers 0xF] 1)
              :else (assoc-in machine [:registers 0xF] 0))
            [:registers r1]
            (bit-and 0xFF addVal)))))

;; subtract r2 from r1, and store in r1.
;; if r2 > r1, set VF to 1
;; 8xy5
(defn sub-registers [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)
        subVal (mod (- r1Val r2Val) 0x100)]
    (incpc (assoc-in
            (cond
              (> r1Val r2Val) (assoc-in machine [:registers 0xF] 1)
              :else (assoc-in machine [:registers 0xF] 0))
            [:registers r1]
            subVal))))

;; shift register right
;; 8xy6
(defn shift-right-register [machine register]
  (let [rVal ((machine :registers) register)]
    (incpc (update-in
            (cond
              (odd? rVal) (assoc-in machine [:registers 0xF] 1)
              :else (assoc-in machine [:registers 0xF] 0))
            [:registers register] bit-shift-right 1))))

;; subtract r1 from r2, store in r1
;; if r1 > r2 set vF to 1
;; 8xy7
(defn sub-registers-rev [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)
        subVal (mod (- r2Val r1Val) 0x100)]
    (incpc (assoc-in
            (cond
              (> r2Val r1Val) (assoc-in machine [:registers 0xF] 1)
              :else (assoc-in machine [:registers 0xF] 0))
            [:registers r1]
            subVal))))

;; shift left, if most significant bit is 1, set vf to 1
;; 8xyE
(defn shift-left-register [machine register]
  (let [rVal ((machine :registers) register)
        msbSet (= (bit-and 0x80 rVal) 0x80)] ;; most-significant bit set
    (incpc (update-in
            (cond
              msbSet (assoc-in machine [:registers 0xF] 1)
              :else (assoc-in machine [:registers 0xF] 0))
            [:registers register]
            #(bit-and 0xFF (bit-shift-left % 1))))))

;; 9xy0
(defn skip-next-instruction-not-equal [machine r1 r2]
  (let [r1Val ((machine :registers) r1)
        r2Val ((machine :registers) r2)]
    (cond
      (not= r1Val r2Val) (incpc (incpc machine))
      :else (incpc machine))))

;; set i register
;; Annn
(defn set-i [machine addr]
  (incpc (assoc machine :i addr)))


;; jump to location + v0
;; Bnnn
(defn jump-v0 [machine addr]
  (let [v0 ((machine :registers) 0)
        jmploc (+ v0 addr)]
    (assoc machine :pc jmploc)))

;; generate random number, store in register
;; Cxnn
(defn random-register
  [machine register mask]
  (let [randnum (rand-int 0x100)
        masked (bit-and randnum mask)]
    (incpc (assoc-in machine [:registers register] masked))))

;; Ex9E
(defn skip-if-keypressed
  [machine keypressed? register key-symbol]
  (let [key-code ((machine :registers) register)]
    (cond
      (and keypressed? (= (keycodes key-symbol) key-code)) (incpc (incpc machine))
      :else (incpc machine))))

;; ExA1
(defn skip-if-not-keypressed
  [machine keypressed? register key-symbol]
  (let [key-code ((machine :registers) register)]
    (cond
      (not (and keypressed? (= (keycodes key-symbol) key-code))) (incpc (incpc machine))
      :else (incpc machine))))

;; Fx07
(defn loaddt
  [{dt :dt, :as machine} register]
  (incpc (assoc-in machine [:registers register] dt)))

;; Wait for key press, don't increment pc!
;; Fx0A
(defn key-wait
  [machine register keypressed? key-symbol]
  (cond
    (and keypressed? (contains? keycodes key-symbol)) (incpc (assoc-in machine [:registers register] (keycodes key-symbol)))
    :else machine))

;; set DT from register value
;; Fx15
(defn setdt
  [machine register]
  (incpc (assoc machine :dt ((machine :registers) register))))

;; Fx18
(defn setst
  [machine register]
  (incpc (assoc machine :st ((machine :registers) register))))

;; add value from register to i, store in i
;; Fx1E
(defn addi
  [machine register]
  (let [rVal ((machine :registers) register)
        iAddr (machine :i)
        sumVals (+ rVal iAddr)]
    (incpc (assoc machine :i sumVals))))

;; store BCD representation of Vx in i
;; Fx33
(defn bcdi
  [machine register]
  (let [rVal ((machine :registers) register)
        iAddr (machine :i)
        hundreds (quot rVal 100)
        tens    (mod (quot rVal 10) 10)
        ones    (mod rVal 10)]
    (incpc (assoc-in
            (assoc-in
             (assoc-in machine [:memory iAddr] hundreds)
             [:memory (inc iAddr)] tens)
            [:memory (+ 2 iAddr)] ones))))

;; store values from V0 to Vx in i0 to ix
;; Fx55
(defn load-to-i
  [machine topr]
  (let [registers (machine :registers)
        iAddr    (machine :i)
        copiedRegisters (take (inc topr) registers) ;; extra?
        iAddrs (iterate inc iAddr)
        toAssign (interleave iAddrs copiedRegisters)
        newMemory (apply assoc (machine :memory) toAssign)]
    (incpc (assoc machine :memory newMemory))))

(defn right-most-bit
  [num]
  (if (odd? num) 1 0))

(defn num-to-binary-vec
  [num]
  (vec (map right-most-bit (reverse (take 8 (iterate #(bit-shift-right % 1) num))))))

;; xor two vectors representing binary numbers
(defn xor-positions
  [vec1 vec2 first-index]
  (cond
    (empty? vec2) vec1
    :else (let [index (mod first-index (count vec1))
                v1Val (vec1 index)
                v2Val (first vec2)
                xordVal (bit-xor v1Val v2Val)]
            (assoc (xor-positions vec1 (rest vec2) (inc index)) index xordVal))))

(defn xor-y-positions
  [screen sprite xCoord yCoord]
  (let [index (mod yCoord (count screen))]
    (cond
      (empty? sprite) screen
      :else (update (xor-y-positions screen (rest sprite) xCoord (inc yCoord))
                    index xor-positions (first sprite) xCoord))))

(defn unset-bit-h
  [before-v after-v]
  (not-every? false? (map (fn [b a] (and (= b 1) (= a 0))) before-v after-v)))

;; return true if any bits from old screen changed from 1 to 0
(defn unset-bits
  [before-screen after-screen]
  (not-every? false? (map unset-bit-h before-screen after-screen)))

;; draw sprite to screen
;; Dxyn
(defn draw
  [machine Vx Vy numBytes]
  (let [iAddr (machine :i)
        byteAddrs (take numBytes (iterate inc iAddr))
        spriteBytes (map (machine :memory) byteAddrs)
        sprite-vecs (map num-to-binary-vec spriteBytes)
        xCoord ((machine :registers) Vx)
        yCoord ((machine :registers) Vy)
        newScreen (xor-y-positions (machine :display) sprite-vecs xCoord yCoord)]
    (incpc
     (assoc
      (cond
        (unset-bits (machine :display) newScreen) (assoc-in machine [:registers 0xF] 1)
        :else (assoc-in machine [:registers 0xF] 0))
      :display newScreen))))


;; Fx65
(defn read-from-i
  [machine topr]
  (let [registers (machine :registers)
        iAddr    (machine :i)
        copiedAddrs (take (inc topr) (iterate inc (machine :i)))
        addrValues (map (machine :memory) copiedAddrs)
        updatedRs (apply assoc registers (interleave (range (inc topr)) addrValues))]
    (incpc
     (assoc machine :registers updatedRs))))

;; Fx29
(defn load-digit-sprite
  [machine register]
  (let [digit ((machine :registers) register)]
    (incpc
     (case digit
       0x0 (assoc machine :i 0)
       0x1 (assoc machine :i 5)
       0x2 (assoc machine :i 10)
       0x3 (assoc machine :i 15)
       0x4 (assoc machine :i 20)
       0x5 (assoc machine :i 25)
       0x6 (assoc machine :i 30)
       0x7 (assoc machine :i 35)
       0x8 (assoc machine :i 40)
       0x9 (assoc machine :i 45)
       0xA (assoc machine :i 50)
       0xB (assoc machine :i 55)
       0xC (assoc machine :i 60)
       0xD (assoc machine :i 65)
       0xE (assoc machine :i 70)
       0xF (assoc machine :i 75)))))


(defn two-bytes-four-bits
  [b1 b2]
  [(bit-shift-right b1 4) (bit-and 2r1111 b1)
   (bit-shift-right b2 4) (bit-and 2r1111 b2)])

(defn append-bytes
  ([b1 b2] (bit-xor (bit-shift-left b1 4) b2))
  ([b1 b2 b3] (bit-xor (bit-shift-left b1 8) (bit-shift-left b2 4) b3)))

(defn get-opcode
  [machine]
  (let [byte1 ((machine :memory) (machine :pc))
        byte2 ((machine :memory) (inc (machine :pc)))]
    (two-bytes-four-bits byte1 byte2)))

;;;;;;;;;;;;;;;; Mapping functions to hex codes ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn zero-ops
  [machine [b1 b2 b3 b4]]
  (case b4
    0x0 (cls machine)
    0xE (ret machine)
    machine))

(defn eight-ops
  [machine [b1 b2 b3 b4]]
  (case b4
    0x0 (copy-register machine b2 b3)
    0x1 (or-register machine b2 b3)
    0x2 (and-register machine b2 b3)
    0x3 (xor-registers machine b2 b3)
    0x4 (add-registers machine b2 b3)
    0x5 (sub-registers machine b2 b3)
    0x6 (shift-right-register machine b2)
    0x7 (sub-registers-rev machine b2 b3)
    0xE (shift-left-register machine b2)))

(defn e-ops
  [machine [b1 b2 b3 b4] keypressed? key-symbol]
  (case b3
    0x9 (skip-if-keypressed machine keypressed? b2 key-symbol)
    0xA (skip-if-not-keypressed machine keypressed? b2 key-symbol)))

(defn f-ops
  [machine [b1 b2 b3 b4] keypressed? key-symbol]
  (let [command (append-bytes b3 b4)]
    (case command
      0x07 (loaddt machine b2)
      0x0A (key-wait machine b2 keypressed? key-symbol)
      0x15 (setdt machine b2)
      0x18 (setst machine b2)
      0x1E (addi machine b2)
      0x29 (load-digit-sprite machine b2)
      0x33 (bcdi machine b2)
      0x55 (load-to-i machine b2)
      0x65 (read-from-i machine b2))))

(defn tick
  [machine keypressed? key-symbol]
  (let [[b1 b2 b3 b4 :as command] (get-opcode machine)]
    (case b1
      0x0 (zero-ops machine command)
      0x1 (jp machine (append-bytes b2 b3 b4))
      0x2 (call machine (append-bytes b2 b3 b4))
      0x3 (skip-equal-byte machine b2 (append-bytes b3 b4))
      0x4 (skip-not-equal-byte machine b2 (append-bytes b3 b4))
      0x5 (skip-equal-registers machine b2 b3)
      0x6 (load-register-byte machine b2 (append-bytes b3 b4))
      0x7 (add-register-byte machine b2 (append-bytes b3 b4))
      0x8 (eight-ops machine command)
      0x9 (skip-next-instruction-not-equal machine b2 b3)
      0xA (set-i machine (append-bytes b2 b3 b4))
      0xB (jump-v0 machine (append-bytes b2 b3 b4))
      0xC (random-register machine b2 (append-bytes b3 b4))
      0xD (draw machine b2 b3 b4)
      0xE (e-ops machine command keypressed? key-symbol)
      0xF (f-ops machine command keypressed? key-symbol))))

(defn delay-tick
  [machine keypressed? key-symbol]
  (let [dt (machine :dt)]
    (cond
      (> dt 0) (update (tick machine keypressed? key-symbol) :dt dec)
      :else (tick machine keypressed? key-symbol))))

