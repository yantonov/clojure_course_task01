(ns task01.core
  (:require [pl.danieljanus.tagsoup :as ts])
  (:gen-class))

(defn- tag?
  "Checks whether given node is a tag."
  [node]
  (vector? node))

(defn- children
  "If node is a tag returns children, otherwise returns nil."
  [node]
  (if (tag? node)
    (ts/children node)
    '()))

(defn- attributes
  "If node is a tag returns attribute map, otherwise returns nil."
  [node]
  (if (tag? node)
    (ts/attributes node)
    nil))

(defn- submap?
  "Checks whether given submap is contained inside map."
  [submap map]
  (= submap (select-keys map (keys submap))))

(defn- matcher
  "Returns function-matcher. Returns functions take node and returns bool to indicate if mathing by tag and attr map occured."
  [tag-to-match attr-map-to-match]
  (fn [element]
    (and (tag? element)
         (= tag-to-match (ts/tag element))
         (submap? attr-map-to-match (ts/attributes element)))))

(defn- apply-handlers
  "Apply node-hanlders to given tree"
  ([node node-handlers]
     (apply-handlers node node-handlers {}))
  ([node node-handlers result]
     (if-let [handlers (seq node-handlers)]
       (let [{matcher :matcher f :fn} (first handlers)
             match (matcher node)
             rest-handlers (if match (rest handlers) handlers)
             val (if (and match (not (nil? f))) (f node result) result)]
         (reduce #(apply-handlers %2 rest-handlers %1)
                 val
                 (children node))
         )
       result))
  )

(def ^:private task-selectors
  [{:matcher (matcher :h3 {:class "r"})}
   {:matcher (matcher :a {:class "l"})
    :fn (fn [node result] (assoc result
                            :links
                            (cons (:href (attributes node))
                                  (get result :links '()))))}])

(defn- ad-hoc-tree-walk-solution
  [data]
  (let [v (vec (:links (apply-handlers data
                                       task-selectors)))]
    (do (println v) v)))

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
  (let [data (ts/parse "clojure_google.html")]
    (ad-hoc-tree-walk-solution data)))

(defn -main []
  (println (str \F\o\u\n\d" " (count (get-links)) " links!")))
