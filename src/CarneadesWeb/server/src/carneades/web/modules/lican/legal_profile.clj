(ns carneades.web.modules.lican.legal-profile
  (:require [carneades.database.legal-profile :as lp]))

(defn create-markos-legal-profile-db
  []
  (lp/create-db "test-markos2" "root" "pw1")
  (lp/with-db "test-markos2" "root" "pw1"
    (lp/create-profile+ {:default true
                         :metadata {:title "Default"}
                         :rules [{:ruleid 'default-licensing-rule
                                  :value 1.0}
                                 {:ruleid 'reciprocity-rule
                                  :value 1.0}
                                 {:ruleid 'compatible-reflexive-rule
                                  :value 1.0}
                                 {:ruleid 'fsf-theory-of-linking
                                  :value 1.0}
                                 {:ruleid 'linked-library
                                  :value 1.0}
                                 {:ruleid 'dynamically-linked-library
                                  :value 1.0}
                                 {:ruleid 'statically-linked-library
                                  :value 1.0}]})
    (lp/create-profile+ {:default false
                         :metadata {:title "Lawrence Rosen"}
                         :rules [{:ruleid 'default-licensing-rule
                                  :value 1.0}
                                 {:ruleid 'reciprocity-rule
                                  :value 1.0}
                                 {:ruleid 'compatible-reflexive-rule
                                  :value 1.0}
                                 {:ruleid 'fsf-theory-of-linking
                                  :value 0.0}
                                 {:ruleid 'linked-library
                                  :value 1.0}
                                 {:ruleid 'dynamically-linked-library
                                  :value 1.0}
                                 {:ruleid 'statically-linked-library
                                  :value 1.0}]})))
