(ns carneades.editor.controller.listeners
  (:use clojure.contrib.def
        clojure.contrib.pprint
        carneades.editor.view.editorapplication
        carneades.engine.lkif.import
        ;; temporary tests:
        carneades.engine.statement
        carneades.engine.argument))

(defvar- *lkif-content* (atom {}))

(defn on-open-file [event view]
  (let [not-property (str "Post, by pursuing the fox,"
                          "did not acquire property in the fox.")
        possession-required (str "Property rights in wild animals may "
                                 "be acquired only by possession.")
        foxes-are-wild "Foxes are wild animals."
        no-possession (str "Post did not have possession of the fox.")
        pursuit-not-sufficient (str "Pursuit is not sufficient"
                                    "to acquire possession.")
        justinian "Justinian's Institutes"
        fleta "Fleta"
        bracton "Bracton"
        actual-possession-required "Actual corporal possession is required."
        puffendorf "Puffendorf"
        bynkershoek "Bynkershoek"
        mortally-wounded-deemed-possessed (str "Pursuit is sufficient to obtain"
                                               " possession when the animal "
                                               "is mortally wounded.")
        barbeyrac "Barbeyrac"
        grotius "Grotius"
        mortally-wounded "The fox was mortally wounded."
        land-owner-has-possession (str "The owner of land pursuing a "
                                       "livelihood with animals on his land is "
                                       "deemed to have possession of "
                                       "the animals.")
        livelihood-on-own-land "Post was pursing his livelihood on his own land"
        keeble "Keeble"
        certainty (str "A bright-line rule creates legal certainty, "
                       "preserving peace and order.")
        order "Peace and order is an important social value."
        a1 (make-arg a1 (pro not-property
                        (pm possession-required)
                        (pm no-possession)
                        (pm foxes-are-wild)))
        a2 (make-arg a2 (pro no-possession (pm pursuit-not-sufficient)))
        a3 (make-arg a3 (pro pursuit-not-sufficient (am justinian)))
        a4 (make-arg a4 (pro pursuit-not-sufficient (am fleta)))
        a5 (make-arg a5 (pro pursuit-not-sufficient (am bracton)))
        a6 (make-arg a6 (pro no-possession (pm actual-possession-required)))
        a7 (make-arg a7 (pro actual-possession-required (am puffendorf)))
        a8 (make-arg a8 (pro puffendorf (am bynkershoek)))
        a9 (make-arg a9 (con actual-possession-required 
                        (pm mortally-wounded-deemed-possessed)
                        (pm mortally-wounded)))
        a10 (make-arg a10 (pro mortally-wounded-deemed-possessed (am grotius)))
        a11 (make-arg a11 (pro mortally-wounded-deemed-possessed (am barbeyrac)))
        a12 (make-arg a12 (con actual-possession-required
                          (pm land-owner-has-possession)
                          (pm livelihood-on-own-land)))
        a13 (make-arg a13 (pro land-owner-has-possession (am keeble)))
        a14 (make-arg a14 (pro actual-possession-required 
                          (pm certainty)
                          (pm order)))
        args1 (argument-graph (list a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
                                    a14))
        facts1 (list foxes-are-wild possession-required certainty order)
        tompkins (accept args1 facts1)]
    ;; (display-graph view tompkins statement-formatted)
    (prn "ask-lkif-file-to-open...")
    (when-let [file (ask-lkif-file-to-open view)]
      (when-let [content (lkif-import (.getPath file))]
        (reset! *lkif-content* content)
        (pprint content)
        (display-graphs-list view (map :id (:ags (deref *lkif-content*))))))))

(defn on-select-graph-id [event view]
  (when-not (.getValueIsAdjusting event)
    (let [list (.getSource event)
        model (.getModel list)
        indices (seq (. list getSelectedIndices))
        ids (set (map (fn [i] (.getElementAt model i)) indices))]
    
    (prn "indices")
    (prn indices)
    (prn "ids")
    (prn ids)
    (let [ags (filter identity
               (map (fn [ag]
                      (when (contains? ids (:id ag))
                        ag))
                    (:ags (deref *lkif-content*))))]
      (prn "ags")
      (doseq [ag ags]
        (prn "ag")
        (prn ag)
        (display-graph view ag statement-formatted))))))

