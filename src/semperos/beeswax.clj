(ns semperos.beeswax
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clj-yaml.core :as yaml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.reader.edn :as edn])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io PushbackReader]))

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

(defn ar1 [f]
  (fn [stack env]
    (let [x (peek stack)
          stack (pop stack)]
      (conj stack (f x)))))

(def count** (ar1 count))

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

(def http-request** (ar1 http/request))

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

(def parse-json** (ar1 json/parse-string))

(def parse-yaml** (ar1 yaml/parse-string))

(defn print**
  [stack _]
  (let [x (peek stack)
        stack (pop stack)]
    (print x)
    stack))

(defn print-stack**
  [stack _]
  (print "Stack of" (count stack) "items:" (pr-str stack))
  stack)

(defn interrupt**
  [stack _]
  (conj stack :bx/interrupt))

(def read-file** (ar1 slurp))

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

(defn clj**
  [stack _]
  (conj stack :bx/clj))

(declare invoke**)
(def builtin-words
  {
   'append       append**
   'clj          clj**
   'count        count**
   'dup          dup**
   'eq           eq**
   'get          get**
   'http-request http-request**
   'if           if**
   'interrupt    interrupt**
   'invoke       invoke**
   'parse-json   parse-json**
   'parse-yaml   parse-yaml**
   'print        print**
   'print-stack  print-stack**
   'read-file    read-file**
   'regex-match  regex-match**
   'rot          rot**
   'set          set**
   'swap         swap**
   'type         type**
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

(defn invoke**
  [stack env]
  (let [a (peek stack)]
    (invoke a stack env)))

(defn resolve-word
  [form env]
  (-d "resolve word?" form env)
  (or (get env form)
      (get builtin-words (word-set form))
      (and (instance? semperos.beeswax.Invocable form)
           form)))

(defn eval-form [form stack env]
  (let [word (resolve-word form env)]
    (cond
      word
      (do
        (-d "evaling" word)
        {:stack (invoke word stack env)
         :env env})

      (symbol? form)
      (throw (ex-info (str "Cannot resolve symbol " form ", you either misspelled it or forgot to implement a word.")
                      {:symbol form
                       :error :unresolved-symbol}))

      :else
      (do
        (-d "adding to stack" form (resolve-word form env))
        {:stack (conj stack form)
         :env env}))))

(def ^:const def-open '<def)
(def ^:const def-close 'def>)
(def ^:const quot-open '<<)
(def ^:const quot-close '>>)

(defn read-delimited
  ([tokens delimiter] (read-delimited tokens delimiter []))
  ([tokens delimiter definition]
   (if-let [form (first tokens)]
     (let [tokens (next tokens)]
       (if-not (= form delimiter)
         (cond
           ;; Support quotations inside word definitions,
           ;; but not other word definitions.
           (= form quot-open)
           (let [_ (-d "parsing quotation within word/quotation")
                 [quotation-definition tokens] (read-delimited tokens quot-close)
                 quotation (->Quotation definition)]
             (recur tokens delimiter (conj definition quotation)))

           :else
           (recur tokens delimiter (conj definition form)))

         [definition tokens]))
     (throw (ex-info (str "Reached end of program before finding closing " (pr-str delimiter)))))))

(defn interpret*
  [tokens stack env]
  (if-let [form (first tokens)]
    (let [_ (-d "form:" form)
          orig-tokens tokens
          tokens (next tokens)]
      (cond
        (= form def-open)
        (let [word-name (first tokens)
              _ (-d "parsing word definition" word-name)
              tokens (next tokens)
              [definition tokens] (read-delimited tokens def-close)]
          (recur tokens stack (assoc env word-name (->Word word-name definition))))

        (= form quot-open)
        (let [[definition tokens] (read-delimited tokens quot-close)]
          (recur tokens (conj stack (->Quotation definition)) env))

        (#{'clj :bx/clj} form)
        (let [x (eval (first tokens))]
          (println "WHOA" (class x))
          (recur (next tokens)
                 (if (fn? x)
                   (let [item (peek stack)
                         stack (pop stack)]
                     (conj stack (x item)))
                   (conj stack x))
                 env))

        :else
        (let [{:keys [stack env]} (eval-form form stack env)]
          (if (= (peek stack) :bx/interrupt)
            {:rest-of-program orig-tokens
             :stack (pop stack)}
            (recur tokens stack env)))))
    {:rest-of-program []
     :stack stack}))

(defn interpret
  ([tokens] (interpret tokens []))
  ([tokens stack] (interpret tokens stack {}))
  ([tokens stack env]
   (-d "Starting interpeter")
   (interpret* tokens stack env)))

(def i interpret)

(defn go
  ([] (go "beeswax.bx"))
  ([beeswax-resource] (go beeswax-resource {}))
  ([beeswax-resource {:keys [debug?] :as opts}]
   (with-open [rdr (LineNumberingPushbackReader.
                    (PushbackReader.
                     (io/reader (io/resource beeswax-resource))))]
     (let [eof (Object.)
           read' (fn [] (edn/read {:eof eof} rdr))
           tokens (persistent!
                   (loop [form (read') tokens (transient [])]
                     (if (= form eof)
                       tokens
                       (recur (read') (conj! tokens form)))))]
       (interpret tokens)))))
