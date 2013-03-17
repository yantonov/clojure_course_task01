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

(defn- matcher
  "Returns function-matcher. Returns functions take node and returns bool to indicate if mathing by tag and attr map occured."
  [tag-to-match attr-map-to-match]
  (fn [element]
    (and (tag? element)
         (= tag-to-match (ts/tag element))
         (let [attrs (ts/attributes element)
               attrs-to-match (seq attr-map-to-match)]
           (every? #(= (attrs (first %))
                       (attr-map-to-match (first %)))
                   attrs-to-match)))))

(defn- apply-handlers
  "Apply node-hanlders to given tree"
  [node node-handlers reduce-node-fn reduce-children-fn]
  (let [h-seq (seq node-handlers)]
    (if h-seq
      (let [{matcher :matcher f :fn} (first h-seq)]
        (if (matcher node)
          (reduce-node-fn f node
                          (reduce-children-fn
                           (map #(apply-handlers %
                                                 (rest h-seq)
                                                 reduce-node-fn
                                                 reduce-children-fn)
                                (children node))))
          (reduce-children-fn (map #(apply-handlers %
                                                    h-seq
                                                    reduce-node-fn
                                                    reduce-children-fn)
                                   (children node)))))
      nil)))

(defn- task-selectors []
  [{:matcher (matcher :h3 {:class "r"})}
   {:matcher (matcher :a {:class "l"})
    :fn (fn [node] (:href (attributes node)))}])

(defn- reduce-node-fn
  "Combine result for given node with results for children"
  [f node children-results]
  (if (nil? f)
    children-results
    (cons (f node) children-results)))

(defn- reduce-children-fn
  "Reduces result of applying handlers for children."
  [children-result]
  (apply concat children-result)
  )

(defn- ad-hoc-tree-walk-solution
  [data]
  (vec (apply-handlers data
                       (task-selectors)
                       reduce-node-fn
                       reduce-children-fn)))

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
