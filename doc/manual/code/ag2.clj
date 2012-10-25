{:header nil,
 :language
 {(should-do (and (not a) (not b) (not c)) j1)
  urn:uuid:0952c684-a7a9-4b32-9256-8979807ab0c8,
  (promotes c (and vs vi))
  urn:uuid:c6cb93be-ef06-4457-97b4-c3a84eb44444,
  (realizes (and a b c) c)
  urn:uuid:970b9cdb-eb34-4736-a92e-cef644d25fc5,
  (results-in (and (not a) (not b) (not c)) j1 (and a b c))
  urn:uuid:c2de2e0b-5f0c-47f2-b175-56f96b7aed50,
  (circumstances (and (not a) (not b) (not c)))
  urn:uuid:f6e794ea-0352-421c-8168-7456632f3e1f,
  c urn:uuid:d817c728-cc14-4f30-a5f6-f1b42c8cd3ec,
  b urn:uuid:9197a5e3-5cb4-4fab-95c2-cb38d8df8dd3,
  a urn:uuid:2bfd89dd-a575-4e0f-a873-67c1c19c757c},
 :statement-nodes
 {urn:uuid:0952c684-a7a9-4b32-9256-8979807ab0c8
  {:id urn:uuid:0952c684-a7a9-4b32-9256-8979807ab0c8,
   :atom (should-do (and (not a) (not b) (not c)) j1),
   :header nil,
   :weight nil,
   :value nil,
   :standard :pe,
   :main false,
   :text {},
   :premise-of #{},
   :pro #{urn:uuid:ac6d1017-f369-48e5-9f4b-b7fece85a482},
   :con #{}},
  urn:uuid:c6cb93be-ef06-4457-97b4-c3a84eb44444
  {:id urn:uuid:c6cb93be-ef06-4457-97b4-c3a84eb44444,
   :atom (promotes c (and vs vi)),
   :header nil,
   :weight 1.0,
   :value nil,
   :standard :pe,
   :main false,
   :text
   {:en
    "Realizing the goal of making more content searchable on
     the Internet would promote the values vs) open access
     to research and vi) balancing stakeholder interests."},
   :premise-of #{urn:uuid:ac6d1017-f369-48e5-9f4b-b7fece85a482},
   :pro #{},
   :con #{}},
  urn:uuid:970b9cdb-eb34-4736-a92e-cef644d25fc5
  {:id urn:uuid:970b9cdb-eb34-4736-a92e-cef644d25fc5,
   :atom (realizes (and a b c) c),
   :header nil,
   :weight 1.0,
   :value nil,
   :standard :pe,
   :main false,
   :text
   {:en
    "If a) the law is clearer, b) copyright owners benefit
     from increased marketing of their works and c) more
     content is searchable on then Internet, then the goal
     of making more content searchable on the Internet will
     have been achieved."},
   :premise-of #{urn:uuid:ac6d1017-f369-48e5-9f4b-b7fece85a482},
   :pro #{},
   :con #{}},

  ...  ;; remaining statements elided to save space
  }
 :argument-nodes
 {urn:uuid:ac6d1017-f369-48e5-9f4b-b7fece85a482
  {:id urn:uuid:ac6d1017-f369-48e5-9f4b-b7fece85a482,
   :header nil,
   :scheme pras1,
   :strict false,
   :weight 0.5,
   :value nil,
   :conclusion urn:uuid:0952c684-a7a9-4b32-9256-8979807ab0c8,
   :pro true,
   :premises
   ({:statement urn:uuid:f6e794ea-0352-421c-8168-7456632f3e1f,
     :positive true,
     :role "circumstances",
     :implicit false}
    {:statement urn:uuid:c2de2e0b-5f0c-47f2-b175-56f96b7aed50,
     :positive true,
     :role "action",
     :implicit false}
    {:statement urn:uuid:970b9cdb-eb34-4736-a92e-cef644d25fc5,
     :positive true,
     :role "goal",
     :implicit false}
    {:statement urn:uuid:c6cb93be-ef06-4457-97b4-c3a84eb44444,
     :positive true,
     :role "value",
     :implicit false})}},
 :references {},
 :namespaces {}}
