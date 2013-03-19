(ns task01.core
  (:require [pl.danieljanus.tagsoup :refer :all])
  (:gen-class))

(defn get-vector-href[data]
  ;; find href
  (map (fn[[first second]](:href second)) (filter (fn[[first second]](= :a first)) data)))

(defn get-vector-href-struct[data]
  ;; find href struct
  (filter (fn[[first second]] (and  (= :h3 first) (= {:class "r"} second))) data))

(defn get-vector[data acc]
  ;; build linear vector structure
  (if (nil? data)
       acc
  (let [d data]
     (let[fdata (first d)]
      (if(vector? fdata)
        ;; hello scala
        (concat (get-vector fdata (conj acc fdata)) (get-vector (next d) ()))
       (get-vector (next d) acc))))))

(defn get-links []
" 1) Find all elements containing {:class \"r\"}.

Example:
[:h3 {:class \"r\"} [:a {:shape \"rect\", :class \"l\",
                         :href \"https://github.com/clojure/clojure\",
                         :onmousedown \"return rwt(this,'','','','4','AFQjCNFlSngH8Q4cB8TMqb710dD6ZkDSJg','','0CFYQFjAD','','',event)\"}
                     [:em {} \"clojure\"] \"/\" [:em {} \"clojure\"] \" · GitHub\"]]

   2) Extract href from the element :a.

The link from the example above is 'https://github.com/clojure/clojure'.

  3) Return vector of all 10 links.

Example: ['https://github.com/clojure/clojure', 'http://clojure.com/', . . .]
"
  (let [data (parse "clojure_google.html")]
   (vec (get-vector-href ( get-vector (get-vector-href-struct (get-vector data ()))())))))

(defn -main []
  (println (str "Found " (count (get-links)) " links!")))


