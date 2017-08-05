(ns semperos.beeswax
  (:require [clojure.java.io :as io]
            [clojure.tools.reader.edn :as edn]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [clj-http.client :as http]
            [cheshire.core :as json])
  (:import [java.io PushbackReader]
           [clojure.lang LineNumberingPushbackReader]))

(def debug? (atom false))

(defn -d [& args]
  (when @debug?
    (apply prn (interpose "  <|>  " args))))

;;;;;;;;;;;;;;
;; Builtins ;;
;;;;;;;;;;;;;;

(declare eval-form)

(defn append**
  [stack _]
  (-d "append** stack" stack)
  (let [b (peek stack)
        stack (pop stack)
        a (peek stack)
        stack (pop stack)
        ret (cond
              (string? a)
              (str a b)

              (or (vector? a) (set? a))
              (conj a b)

              (list? a)
              (concat a '(b))

              :else
              (throw (ex-info (str "Can't append to type " (type a))
                              {:base a
                               :item b})))]
    (-d "append** bindings" a b ret stack)
    (conj stack ret)))

(defn dup**
  [stack _]
  (let [x (peek stack)]
    (conj stack x)))

(defn eq**
  [stack _]
  (let [b (peek stack)
        stack (pop stack)
        a (peek stack)
        stack (pop stack)]
    (->> (= a b)
         (conj stack))))

(defn get**
  [stack _]
  (-d "get** stack" stack)
  (let [associative (peek stack)
        stack (pop stack)
        k (peek stack)
        stack (pop stack)
        ret (get associative k)]
    (-d "get** bindings" associative k ret)
    (conj stack ret)))

(defn http-request**
  [stack _]
  (let [request-map (peek stack)
        stack (pop stack)
        ret (http/request request-map)]
    (conj stack ret)))

(defn if**
  [stack env]
  (-d "if** stack" stack)
  (let [f-quot (peek stack)
        _ (-d "f-quot" f-quot)
        stack (pop stack)
        _ (-d "top o stack" (peek stack))
        t-quot (peek stack)
        stack (pop stack)
        condition (peek stack)
        stack (pop stack)
        ret (if condition
              (eval-form t-quot stack env)
              (eval-form f-quot stack env))]
    (-d "if** bindings" condition t-quot f-quot ret)
    (:stack ret)))

(defn parse-json**
  [stack _]
  (-d "parse-json** stack" stack)
  (let [json-str (peek stack)
        stack (pop stack)
        ret (json/parse-string json-str)]
    (-d "parse-json** bindings" json-str ret stack)
    (conj stack ret)))

(defn parse-yaml**
  [stack _]
  (-d "parse-yaml** stack" stack)
  (let [yaml-str (peek stack)
        stack (pop stack)
        ret (yaml/parse-string yaml-str)]
    (-d "parse-yaml** bindings" yaml-str ret stack)
    (conj stack ret)))

(defn print**
  [stack _]
  (let [x (peek stack)
        stack (pop stack)]
    (print x)
    stack))

(defn read-file**
  [stack _]
  (-d "read-file** stack" stack)
  (let [slurpable (peek stack)
        stack (pop stack)
        ret (slurp slurpable)]
    (-d "read-file** bindings" slurpable ret stack)
    (conj stack ret)))

(defn regex-match**
  [stack _]
  (let [s (peek stack)
        stack (pop stack)
        regex-pattern (peek stack)
        stack (pop stack)]
    (->> (re-find (re-pattern regex-pattern s))
         (conj stack))))

(defn rot**
  [stack _]
  (let [a (peek stack)
        stack (pop stack)
        b (peek stack)
        stack (pop stack)
        c (peek stack)
        stack (pop stack)]
    (conj stack b a c)))

(defn set**
  [stack _]
  (let [associative (peek stack)
        stack (pop stack)
        k (peek stack)
        stack (pop stack)
        v (peek stack)
        stack (pop stack)]
    (->> (assoc associative k v)
         (conj stack))))

(defn swap**
  [stack _]
  (let [a (peek stack)
        stack (pop stack)
        b (peek stack)
        stack (pop stack)]
    (conj stack a b)))

(defn type**
  [stack _]
  (let [x (peek stack)
        stack (pop stack)
        ret (type x)]
    (conj stack ret)))

(def builtin-words
  {
   'append append**
   'dup dup**
   'eq eq**
   'get get**
   'http-request http-request**
   'if if**
   'parse-json parse-json**
   'parse-yaml parse-yaml**
   'print print**
   'read-file read-file**
   'regex-match regex-match**
   'rot rot**
   'set set**
   'swap swap**
   'type type**
   ;; 'apply apply**
   })

(def word-set (set (keys builtin-words)))

(defprotocol Invocable
  (definition [this] "Return definition of the invocable.")
  (invoke [this stack env] "Invoke this invocable given a stack and an environment with words."))

(defn invoke*
  [invocable stack env]
  (-d "invoking" invocable (type (first (:definition invocable))))
  (reduce
   (fn [stack x]
     (:stack (eval-form x stack env)))
   stack
   (definition invocable)))

(defrecord Quotation [definition]
  Invocable
  (definition [this] (:definition this))
  (invoke [this stack env]
    (invoke* this stack env)))

(defrecord Word [name definition]
  Invocable
  (definition [this] (:definition this))
  (invoke [this stack env]
    (invoke* this stack env)))

(extend-type clojure.lang.IFn
  Invocable
  (invoke [this stack env]
    (this stack env)))

(defn resolve-word
  [form env]
  (-d "resolve" form env semperos.beeswax.Invocable)
  (or (get builtin-words (word-set form))
      (get env form)
      (and (instance? semperos.beeswax.Invocable form)
           form)))

(defn eval-form [form stack env]
  (if-let [word (resolve-word form env)]
    {:stack (invoke word stack env)
     :env env}
    {:stack (conj stack form)
     :env env}))

(def ^:const def-open '<def)
(def ^:const def-close 'def>)
(def ^:const quot-open '<<)
(def ^:const quot-close '>>)

(defn read-delimited
  ([rdr eof delimiter] (read-delimited rdr eof delimiter []))
  ([rdr eof delimiter definition]
   (let [form (edn/read {:eof eof} rdr)]
     (if-not (= form delimiter)
       (cond
         ;; Support quotations inside word definitions,
         ;; but not other word definitions.
         (= form quot-open)
         (do
           (-d "reading quotation within word/quotation")
           (recur rdr eof delimiter (conj definition (->Quotation (read-delimited rdr eof quot-close)))))

         :else
         (recur rdr eof delimiter (conj definition form)))

       definition))))

(defn interpret*
  [rdr stack env]
  (let [eof (Object.)
        read' (fn [] (edn/read {:eof eof} rdr))]
    (let [form (read')]
      (-d "interpret form: " form (type form) stack env)
      (if (= form eof)
        [:OK stack]
        (cond
          (= form def-open)
          (let [word-name (read')
                _ (-d "reading word" word-name)
                definition (read-delimited rdr eof def-close)]
            (recur rdr stack (assoc env word-name (->Word word-name definition))))

          (= form quot-open)
          (let [definition (read-delimited rdr eof quot-close)]
            (-d "read quotation with definition" definition)
            (recur rdr (conj stack (->Quotation definition)) env))

          :else
          (let [{:keys [stack env]} (eval-form form stack env)]
            (recur rdr stack env)))))))

(defn interpret
  ([rdr] (interpret rdr []))
  ([rdr stack] (interpret rdr stack {}))
  ([rdr stack env]
   (interpret* rdr stack env)))

(defn go
  ([] (go "beeswax.bx"))
  ([beeswax-resource] (go beeswax-resource {}))
  ([beeswax-resource {:keys [debug?] :as opts}]
   (with-open [rdr (LineNumberingPushbackReader.
                    (PushbackReader.
                     (io/reader (io/resource beeswax-resource))))
               ]
     (let [tokens rdr]
       (interpret tokens)))))
