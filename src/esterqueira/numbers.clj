(ns esterqueira.numbers)

(defn parse-number [rows]
  (for [[i row] (map-indexed list rows)
        [j cell] (map-indexed list row)
        :when (= cell \#)]
    [(- j 1) (- 2 i)]))

(def number-centers
  (mapv parse-number
        [["###"
          "# #"
          "# #"
          "# #"
          "###"]

         [" # "
          " # "
          " # "
          " # "
          " # "]

         ["###"
          "  #"
          "###"
          "#  "
          "###"]

         ["###"
          "  #"
          " ##"
          "  #"
          "###"]

         ["# #"
          "# #"
          "###"
          "  #"
          "  #"]

         ["###"
          "#  "
          "###"
          "  #"
          "###"]

         ["###"
          "#  "
          "###"
          "# #"
          "###"]

         ["###"
          "  #"
          " ##"
          "  #"
          "  #"]

         ["###"
          "# #"
          "###"
          "# #"
          "###"]

         ["###"
          "# #"
          "###"
          "  #"
          "  #"]]))
