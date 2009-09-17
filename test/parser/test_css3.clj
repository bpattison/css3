(use 'clojure.contrib.repl-utils)
(use 'clojure.contrib.seq-utils)

(ns test.parser.test_css3)

(use :reload 'parser.css3)

(def pl (comp css3parse seq) )
(defn pass [t] (if (= t nil) false (= (:test t) (:data t) ) ) )
(defn css3-test [tl]
  (let [t (first tl)]
    (prn)
    (if (pass t)
      (do (println (.concat "PASS: " (:name t) ) ) (css3-test (rest tl) ) )
      (if (not= t nil )
        (do
          (println (.concat "FAIL: " (:name t) ) )
          (println "EXPECTED:")
          (prn (:data t) )
          (println "RECEIVED:")
          (prn (:test t) )
        )
      ))))

(defn def-test [e d]
  (hash-map :name e, :test (pl e), :data d) )

(def test-list
  (list

    (def-test "body { }"
      [{:type :ruleset, :selector "body" :data [] }])

    (def-test "body { padding-left : 11em }"
      [{:type :ruleset, :selector "body" :data 
         [{:type :declaration, :data
           [{:type :dimension, :data "11em"}], :property "padding-left"}]
       }])

    (def-test "body { padding-left : 11em; }"
      [{:type :ruleset, :selector "body" :data 
         [{:type :declaration, :data
           [{:type :dimension, :data "11em"}], :property "padding-left"}]
       }])

    (def-test "body { padding-left : 11em; color: purple; }"
      [{:type :ruleset, :selector "body" :data
        [{:type :declaration, :property "padding-left",
          :data [{:type :dimension, :data "11em"}]}
         {:type :declaration, :property "color",
          :data [{:type :ident, :data "purple"}]}]}])

    (def-test (str "body {"
              "  padding-left : 11em;"
              "  color: purple;"
              "  font-family: Georgia, \"Times New Roman\", Times, serif;"
              "}")
      [{:type :ruleset, :selector "body" :data
        [{:type :declaration, :data
          [{:type :dimension, :data "11em"}], :property "padding-left"}
         {:type :declaration, :data
          [{:type :ident, :data "purple"}], :property "color"}
         {:type :declaration, :data
          [{:type :ident, :data "Georgia"}
           {:type :scalar, :data "Times New Roman"}
           {:type :ident, :data "Times"}
           {:type :ident, :data "serif"}], :property "font-family"}]}] )
 
    (def-test (str "body {"
              "  padding-left : 11em;"
              "  color: purple;"
              "  font-family: Georgia  \"Times New Roman\"  Times serif;"
              "}")
      [{:type :ruleset, :selector "body" :data
        [{:type :declaration, :data
          [{:type :dimension, :data "11em"}], :property "padding-left"}
         {:type :declaration, :data
          [{:type :ident, :data "purple"}], :property "color"}
         {:type :declaration, :data
          [{:type :ident, :data "Georgia"}
           {:type :scalar, :data "Times New Roman"}
           {:type :ident, :data "Times"}
           {:type :ident, :data "serif"}], :property "font-family"}]}] )

    (def-test "body { background-color: #d8da3d }"
        [{:type :ruleset, :selector "body" :data
          [{:type :declaration, :data
            [{:type :hash, :data "#d8da3d"}], :property "background-color"}]}])

    (def-test ".font-face { font-family : Times; }"
      [{:type :ruleset, :selector ".font-face" :data
        [{:type :declaration, :data
          [{:type :ident, :data "Times"}], :property "font-family"}]}])

    (def-test ".favorite:before { content : url(\"icons/heart.png\"); }" 
      [{:type :ruleset, :selector ".favorite:before", :data
        [{:type :declaration, :data
          [{:type :uri, :data "icons/heart.png"}], :property "content"}] }] )

    (def-test ".favorite:before { content : url(icons/heart.png); }" 
      [{:type :ruleset, :selector ".favorite:before", :data
        [{:type :declaration, :data
          [{:type :uri, :data "icons/heart.png"}], :property "content"}] }] )

    (def-test (str ".my_CSS3_class {"
                   "  font-family: SketchRockwell;"
                   "  font-size: 3.2em;"
                   "  letter-spacing: 1px;"
                   "  text-align: center;"
                   "  text-shadow: 3px, 3px, 7px, #111;"
                   "}")
      [{:type :ruleset, :selector ".my_CSS3_class" :data
        [{:type :declaration, :data
          [{:type :ident, :data "SketchRockwell"}], :property "font-family"}
           {:type :declaration, :data
            [{:type :dimension, :data "3.2em"}], :property "font-size"}
           {:type :declaration, :data
            [{:type :dimension, :data "1px"}], :property "letter-spacing"}
         {:type :declaration, :data
          [{:type :ident, :data "center"}], :property "text-align"}
         {:type :declaration, :data
          [{:type :dimension, :data "3px"}
           {:type :dimension, :data "3px"}
           {:type :dimension, :data "7px"}
           {:type :hash, :data "#111"}], :property "text-shadow"}]}])

    (def-test (str ".favorite:before { content : url(icons/heart.png); }\n" 
                   ".my_CSS3_class {\n"
                   "  font-family: SketchRockwell;\n"
                   "  font-size: 3.2em;\n"
                   "  letter-spacing: 1px;\n"
                   "  text-align: center;\n"
                   "  text-shadow: 3px, 3px, 7px, #111;\n"
                   "}\n")
      [{:type :ruleset, :selector ".favorite:before", :data
        [{:type :declaration, :data
          [{:type :uri, :data "icons/heart.png"}], :property "content"}] }
       {:type :ruleset, :selector ".my_CSS3_class" :data
        [{:type :declaration, :data
          [{:type :ident, :data "SketchRockwell"}], :property "font-family"}
         {:type :declaration, :data
          [{:type :dimension, :data "3.2em"}], :property "font-size"}
         {:type :declaration, :data
          [{:type :dimension, :data "1px"}], :property "letter-spacing"}
         {:type :declaration, :data
          [{:type :ident, :data "center"}], :property "text-align"}
         {:type :declaration, :data
          [{:type :dimension, :data "3px"}
           {:type :dimension, :data "3px"}
           {:type :dimension, :data "7px"}
           {:type :hash, :data "#111"}], :property "text-shadow"}]}])

;   (def-test "@font-face { font-family : Times; }" nil)
(comment
  "@media all and (min-width: 640px)"
  "{ #media-queries-1 { background-color: #0f0; } }"
  "@media screen and (max-width: 2000px)"
  "{ #media-queries-2 { background-color: #0f0; } }"
)

))

(css3-test test-list)

(def d
  [{:type :ruleset, :selector ".favorite:before", :data
     [{:type :declaration, :data
       [{:type :uri, :data "icons/heart.png"}], :property "content"}] }
   {:type :ruleset, :selector ".my_CSS3_class" :data
       [{:type :declaration, :data
          [{:type :ident, :data "SketchRockwell"}], :property "font-family"}
        {:type :declaration, :data
          [{:type :dimension, :data "3.2em"}], :property "font-size"}
        {:type :declaration, :data
          [{:type :dimension, :data "1px"}], :property "letter-spacing"}
        {:type :declaration, :data
          [{:type :ident, :data "center"}], :property "text-align"}
        {:type :declaration, :data
          [{:type :dimension, :data "3px"}
           {:type :dimension, :data "3px"}
           {:type :dimension, :data "7px"}
           {:type :hash, :data "#111"}], :property "text-shadow"}]}])

(pr (get-ruleset ".favorite:before" d))
(println)
(pr (get-ruleset #"CSS3" d))
(println)
(pr (get-ruleset 0 d))
(println)
(pr (get-property 0 (get-ruleset 0 d)))
(println)
(println (get-data (first (get-property "font-size" (get-ruleset #"CSS3" d)))))
