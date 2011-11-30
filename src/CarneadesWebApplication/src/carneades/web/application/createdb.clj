(ns carneades.web.application.createdb)

(def dbname "db6")

(use 'carneades.database.db)
(use 'carneades.engine.statement)
(use 'carneades.engine.argument)

(def db (make-database-connection dbname "root" "pw1"))
(create-argument-database dbname "root" "pw1")

(def p1 (make-statement :atom "p1"))
(def p2 (make-statement :atom "p2"))
(def c1 (make-statement :atom "c1"))
(def arg1 (make-argument :conclusion c1))


(create-statement db p1)