(ns chip8emu.main
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [chip8emu.display :refer :all]))


(def cli-options
  [["-p" "--path GAMEPATH" "Path to game"]
  ["-d" "--gamedir DIR" "Point at games directory"
   :default "./games/"
   ]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["This is an emulator for the chip8 system"
        ""
        "Usage: program-name [options] game"
        ""
        "Options:"
        options-summary
        ""
        "The game specified will be looked for in GAMEDIR"
        ""
        "Happy Gaming!"]
       (string/join "\n")))

(defn validate-args [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options)
      {:exit-message (usage summary)}
      (:path options)
      {:path (:path options)}
      (= 1 (count arguments))
      {:path (str (:gamedir options) (first arguments))}
      :else
      {:exit-message (usage summary)})))

(defn exit [msg]
  (println msg)
  (System/exit 0))

(defn -main [& args]
  (try
    (let [{:keys [path exit-message]} (validate-args args)]
      (if exit-message
        (exit exit-message)
        (run-game path)))
    (catch Exception e (str "Error trying to run game: " (.getMessage e)))))
