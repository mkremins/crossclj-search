(ns crossclj-search.core
  (:require [clj-http.client :as http]
            [clojure.string :as str]))

(defn tokenize
  "Splits a project name or search query (e.g. \"org.clojure/core.async\" or
  \"core-match\") into a set of constituent parts."
  [s]
  (let [s (str/lower-case s)]
    (set (concat (str/split s #"\W+") (str/split s #"\/")))))

(defn parse-project
  "Parses a 6-tuple of strings representing a Clojure project fetched from
  CrossClj and returns an equivalent project map."
  [[popularity fullname version ns name description]]
  {:project fullname :version version :name name :description description
   :popularity (read-string popularity) ; integer 1-7, greater = more dependents
   :tags (tokenize fullname)})

(def projects
  "A seq of maps representing popular Clojure projects, fetched from CrossClj."
  (->> (-> (http/get "http://crossclj.info/api/v1/prefetch-home")
           :body
           (str/replace "\"" "") ; remove quotation marks at the start and end
           (str/split #"\|"))
       (partition 6)
       (map parse-project)))

(defn search
  "Given a `query` string to search, returns a seq of project maps matching the
  query. Results are sorted first by relevance and then by popularity."
  [query]
  (let [query (tokenize query)
        relevance #(count (clojure.set/intersection (:tags %) query))]
    (->> projects
         (filter (comp pos? relevance))
         (sort-by relevance >))))
