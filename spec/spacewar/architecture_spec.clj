(ns spacewar.architecture-spec
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [speclj.core :refer [describe it should=]]))

(defn- clj-source-files []
  (->> (file-seq (io/file "src"))
       (filter #(.isFile %))
       (filter #(re-matches #".*\.clj[cs]?" (.getName %)))))

(defn- read-ns-form [file]
  (with-open [reader (io/reader file)]
    (read {:read-cond :allow :features #{:clj} :eof nil}
          (java.io.PushbackReader. reader))))

(defn- lib-name [entry]
  (cond
    (symbol? entry) entry
    (vector? entry) (first entry)
    :else nil))

(defn- required-libs [ns-form]
  (let [parts (rest ns-form)
        require-clauses (loop [xs parts
                               acc []]
                          (cond
                            (empty? xs) acc
                            (#{:require :use} (first xs)) (recur (drop 2 xs) (into acc (second xs)))
                            (keyword? (first xs)) (recur (drop 2 xs) acc)
                            :else (recur (rest xs) acc)))]
    (->> require-clauses
         (map lib-name)
         (remove nil?))))

(defn- high-level-ns? [ns-name]
  (let [s (str ns-name)]
    (or (= s "spacewar.geometry")
        (= s "spacewar.vector")
        (= s "spacewar.util")
        (str/starts-with? s "spacewar.game-logic"))))

(defn- forbidden-lib? [lib]
  (let [s (str lib)]
    (or (str/starts-with? s "quil.")
        (str/starts-with? s "spacewar.ui")
        (= s "clojure.java.io"))))

(defn- violations []
  (for [file (clj-source-files)
        :let [ns-form (read-ns-form file)]
        :when (and (seq? ns-form) (= 'ns (first ns-form)))
        :let [ns-name (second ns-form)]
        :when (high-level-ns? ns-name)
        lib (required-libs ns-form)
        :when (forbidden-lib? lib)]
    {:ns ns-name :file (str file) :lib lib}))

(describe "dependency direction"
  (it "keeps game logic and kernel modules free of UI, quil, and filesystem IO"
    (should= [] (violations))))
