(ns carneades.agws.database)
(require '[clojure.java [jdbc :as jdbc]])

(defn make-db  
  "Returns a map describing a database with the given name and password."
  [db-name passwd]
  (let [db-protocol "tcp"            ; "file|mem|tcp"
        db-host     "localhost:9092"] ; "path|host:port"
    
    {:classname   "org.h2.Driver" ; must be in classpath
     :subprotocol "h2"
     :subname (str db-protocol "://" db-host "/" db-name)
     ; Any additional keys are passed to the driver
     ; as driver-specific properties.
     :user     "root"
     :password passwd}))
  
(defn- init-db
  "Initialize the database by creating the tables."
  [db]
  (jdbc/with-connection db
      (jdbc/create-table :statement 
          [:id "int primary key not null"]
          [:weight "double default 0.50"]
          [:standard "tinyint default 0"]   ; 0=pe, 1=cce, 2=brd, 3=dv 
          [:wff "varchar"]          
          [:content "int"]
          ["foreign key(content) references string(id)"])
                        
      (jdbc/create-table :argument
          [:id "int primary key not null"]
          [:conclusion "int not null"]
          [:graph "int not null"]
          [:weight "double default 0.50"]
          [:scheme "int"]
          [:direction "boolean default true"]  ; true=pro, false=con
          [:title "int"]
          ["foreign key(conclusion) references statement(id)"]
          ["foreign key(graph) references graph(id)"]
          ["foreign key(scheme) references scheme(id)"]
          ["foreign key(title) references string(id)"])
                        
      (jdbc/create-table :premise
          [:id "int primary key not null"]
          [:argument "int not null"]
          [:statement "int not null"]
          [:polarity "boolean default true"]    ; true=positive, false=negative
          [:role "varchar"]
          ["foreign key(argument) argument(id)"]
          ["foreign key(statement) statement(id)"]
          ["foreign key(role) string(id)"])                         
           
      (jdbc/create-table :graph
          [:id "int primary key not null"]
          [:main_issue "int"]
          [:title "int"]
          ["foreign key(main_issue) references statement(id)"]
          ["foreign key(title) references string(id)"])
                        
      (jdbc/create-table :string
          [:id "int primary key not null"]
          [:en "clob not null"]   ; English
          [:de "clob"]            ; German
          [:nl "clob"]            ; Dutch
          [:fr "clob"]            ; French
          [:it "clob"]            ; Italian
          [:sp "clob"])           ; Spanish
          ; and so on for other langauges

      (jdbc/create-table :source
         [:id "int primary key not null"]
         [:argument "int"]
         [:coverage "varchar"]
         [:date "varchar"]       ; http://www.w3.org/TR/NOTE-datetime                         
         [:description "int"]
         [:format "varchar"]     ; A list of MIME types, comma separated
         [:identifier "varchar"] 
         [:language "varchar"]
         [:publisher "varchar"]
         [:relation "varchar"]
         [:rights "varchar"]
         [:source "int"]
         [:subject "varchar"]
         [:title "varchar"]
         [:type "varchar"]       ; see: http://dublincore.org/documents/dcmi-type-vocabulary/
         ["foreign key(argument) references argument(id)"]
         ["foreign key(description) references string(id)"]
         ["foreign key(source) references source(id)"])
                        
      (jdbc/create-table :source_has_contributor
         [:source "int"]
         [:author "int"]
         ["foreign key(source) references source(id)"]     
         ["foreign key(author) references author(id)"])
                                           
      (jdbc/create-table :source_has_creator
         [:source "int"]
         [:author "int"]
         ["foreign key(source) references source(id)"]     
         ["foreign key(author) references author(id)"])
                        
      (jdbc/create-table :author
         [:id "int primary key not null"]
         [:name "varchar"])
         ; other properties of authors?
                        
      (jdbc/create-table :namespace
         [:prefix "varchar[64] primary key not null"]
         [:uri    "varchar"])
                        
      (jdbc/create-table :scheme   ; argumentation scheme
         [:id "int primary key not null"]
         [:uri "varchar"]
         [:title "int"]
         ["foreign key(title) references string(id)"])))


                        
                        
         
      
                        
      
         
         
         
         

                         
      
          
                         
                      
  