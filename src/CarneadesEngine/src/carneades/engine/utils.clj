;;; Copyright (c) 2010 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Utilities functions to manipulate sequences, files and more."}
  carneades.engine.utils
  (:use clojure.java.io
        clojure.pprint)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.walk :as w])
  (:import java.security.MessageDigest
           java.util.zip.ZipOutputStream
           java.util.zip.ZipInputStream
           java.util.zip.ZipEntry
           java.io.ByteArrayInputStream
           java.io.ByteArrayOutputStream
           java.io.ByteArrayInputStream
           java.io.BufferedOutputStream
           java.io.FileOutputStream
           java.io.InputStreamReader))

(defn boolean? [x]
  (instance? Boolean x))

(defn nonemptyseq?
  "Returns true if x is a nonempty sequence"
  [x]
  (and (seq? x) (not (empty? x))))

(defn conj-ifnot
  "Conjoins col and x if the binary predicate pred is false
   for all values in col else returns col"
  [pred col x]
  (if (some #(pred x %) col)
    col
    (conj col x)))

(defn union-if
  "Returns the union of s1 and s2, (pred x y) is true for each
   (x y) couple in the union"
  ([pred] #{})
  ([pred s1] s1)
  ([pred s1 s2]
     (if (< (count s1) (count s2))
       (reduce #(conj-ifnot pred %1 %2) s1 s2)
       (reduce #(conj-ifnot pred %1 %2) s2 s1))))

(defn interleaveall
  "Returns a lazy seq of the first item in each coll, then the second etc.
   If the sequences do not have the same length, the remaining elements are
   appended and interleaved at the end of the returned sequence. "
  ([c1] c1)
  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (cond (and s1 s2)
              (cons (first s1) (cons (first s2)
                                     (interleaveall (rest s1) (rest s2))))
              :else (or s1 s2)))))
  ([c1 c2 & colls]
     (lazy-seq
      (let [ss (map seq (conj colls c2 c1))]
        (when (some identity ss)
          (concat (map first (filter not-empty ss))
                  (let [notemptyrest (filter not-empty (map rest ss))]
                    (if (empty? notemptyrest)
                      '()
                      (apply interleaveall notemptyrest)))))))))

(defn mapinterleave
  "Returns the result of applying interleaveall to the result of applying map
   to f and colls. If the result of applying map is a sequence with one element
   then the sequence is returned"
  [f & colls]
  (let [col (apply map f colls)]
    (cond (empty? col) col
          (= (count col) 1) (first col)
          :else (apply interleaveall col))))

;; safe get
(defmacro sget
  "Like get but if *assert* is true, throws an exception if the key is
   not present "
  [map key]
  (if *assert*
    `(let [notfound# (gensym)
           v# (get ~map ~key notfound#)]
       (if (= v# notfound#)
         (throw (Exception. (format "Key '%s' not found" ~key)))
         v#))
    `(get ~map ~key)))

(defn conjoin
  "Returns a predicate that returns true when all of the predicates return true.

   Translated from the book Ansi Common Lisp, Prentice Hall, Paul Graham"
  [f & fs]
  (if (empty? fs)
    f
    (let [conjoined (apply conjoin fs)]
      (fn [& args]
        (and (apply f args) (apply conjoined args))))))

(defn disjoin
  "Returns a predicate that returns true when one of the predicates return true.

   Translated from the book Ansi Common Lisp, Prentice Hall, Paul Graham"
  [f & fs]
  (if (empty? fs)
    f
    (let [conjoined (apply conjoin fs)]
      (fn [& args]
        (or (apply f args) (apply conjoined args))))))

(defn nilify
  "Returns a predicate that returns true when f returns true, nil otherwise"
  [f]
  (fn [& args]
    (or (apply f args) nil)))

;; copied from old contrib
(defn #^String str-drop
  "Drops first n characters from s. Returns an empty string if n is
greater than the length of s."
  [n #^String s]
  (if (< (count s) n)
    ""
    (.substring s n)))

(defn split-str
  "Splits string s in strings of length n"
  [s n]
  (loop [[s r] [s s]
         sq []]
    (if (empty? r)
      sq
      (let [f (subs 0 n r)]
        (recur [f (str-drop n r)] (conj sq f))))))

;;; files

(def ^{:doc "the platform dependant file separator"}
  file-separator java.io.File/separator)

(defn same-directory?
  "Returns true when filename and filename2 have the same parent directory"
  [filename filename2]
  (= (.getParent (file filename)) (.getParent (file filename2))))

(defn make-relative
  "Makes path relative to relative-to"
  [path relative-to]
  (let [f (file path)
        f2 (file relative-to)
        dirsize (count (.getPath f2))
        dirsize (+ dirsize (count file-separator))]
    (subs (.getPath f) dirsize)))

(defn create-path
  "Creates a Path from the (string) segments"
  [& segments]
  (.getPath (file (str/join file-separator segments))))

(defn make-absolute
  "Makes relative path absolute to relative-to"
  [path relative-to]
  (.getPath (file (str relative-to file-separator path))))

(defn in-directory?
  "Returns true if path is directly under or below directory dir"
  [path dir]
  (.startsWith path dir))

(defn absolute?
  "Returns true if pathname is absolute"
  [pathname]
  (.isAbsolute (file pathname)))

(defn absolute
  "Makes pathname absolute"
  [pathname]
  (.getPath (.getAbsoluteFile (file pathname))))

(defn parent
  "Returns the parent of pathname"
  [pathname]
  (.getParent (file pathname)))

(defn exists?
  "Returns true if pathname exists on the file system"
  [pathname]
  (.exists (file pathname)))

(defn url?
  "Returns true if s is a well-formed URL"
  [s]
  (try
    (java.net.URL. s)
    true
    (catch java.net.MalformedURLException e
      false)))

(defn extension
  "Returns the extension of pathname"
  [pathname]
  (last (re-find #".*\.(.*)" pathname)))

(defn add-extension [pathname ext]
  (if (= (last pathname) \.)
    (str pathname ext)
    (str pathname "." ext)))

(defn last-segment
  "Returns the last segment of pathname"
  [pathname]
  (last (str/split pathname (re-pattern file-separator))))

(defn last-uri-segment
  "Returns the last segment of uri"
  [uri]
  (last (str/split uri (re-pattern "/"))))

;;; strings
(defn break-line
  "Breaks line in chunk of line-length length."
  [s line-length]
  (let [len (count s)
        lastpartstart (* (quot len line-length) line-length)
        sparts (if (<= len line-length)
                 [[0 len]]
                 (concat (partition 2 1 (range 0 len line-length))
                         [[lastpartstart len]]))]
    (map (fn [[start end]]
           (subs s start end)) sparts)))

(defn delete-file-recursively
  "Delete file f. If it's a directory, recursively delete all its contents.
Raise an exception if any deletion fails unless silently is true."
  [f & [silently]]
  (let [f (file f)]
    (if (.isDirectory f)
      (doseq [child (.listFiles f)]
        (delete-file-recursively child silently)))
    (delete-file f silently)))

;;; exceptions

(defn first-cause
  ([exception]
     (first-cause exception (.getCause exception)))
  ([exception cause]
     (if (nil? cause)
       exception
       (recur cause (.getCause exception)))))

(defmacro unwrap-exceptions
  "Catchs any exception and rethrows its first initial cause"
  [& body]
  `(try
     ~@body
     (catch Exception e#
       (throw (first-cause e#)))))

;; resources
(defn get-resource
  "Get the resource identified by name"
  [name]
  (-> (Thread/currentThread)
      (.getContextClassLoader)
      (.getResource name)))

(defn get-resource-as-stream
  "Returns the resource identified by 'name' as a stream"
  [name]
  (-> (Thread/currentThread)
      (.getContextClassLoader)
      (.getResourceAsStream name)))

(defn safe-read-string
  [s]
  (if (empty? s)
    nil
    (binding [*read-eval* false]
      (read-string s))))

;; the sha functions are taken from https://gist.github.com/1698245
(def ^{:dynamic true} *default-hash* "SHA-256")

;; take from http://ideone.com/MoNJ14Z8 which was, in turn, gathered from
;; Tom Lee's post "Clojure and MessageDigest"
;;     (http://tomlee.co/2009/06/clojure-and-messagedigest/)
(defn hexdigest
  "Returns the hex digest of an object. Expects a string as input."
  ([input] (hexdigest input *default-hash*))
  ([input hash-algo]
     (if (string? input)
       (let [hash (MessageDigest/getInstance hash-algo)]
         (. hash update (.getBytes input))
         (let [digest (.digest hash)]
           (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
       (do
         (println "Invalid input! Expected string, got" (type input))
         nil))))

(defn compare-sha256 [obj ref-hash]
  "Compare an object to a hash; true if (= (hash obj) ref-hash)."
  (= ref-hash (hexdigest obj "SHA-256")))

(def sha256 (fn [input] (hexdigest input "SHA-256")))

(defn- zip-entry-path
  [f fdir]
  (let [rel (make-relative (.getPath f) (.getPath fdir))]
   (if (.isDirectory f)
     (str (.getName fdir) "/" rel "/")
     (str (.getName fdir) "/" rel))))

(defn zip-dir-helper
  [dirpath stream]
  (let [out (ZipOutputStream. stream)
        fdir (io/file dirpath)]
    (doseq [f (file-seq (io/file dirpath))]
      (when (not (.equals f fdir))
        (let [entry (ZipEntry. (zip-entry-path f fdir))]
          (.putNextEntry out entry)
          (when (.isFile f)
            (let [buffer (.getBytes (slurp f))]
              (.write out buffer))))))
    (.close out)
    out))

(defn zip-dir
  "Creates a zip archive from the content of a directory. If zippath
is specified the zip archive is saved to a file, otherwise a
ByteArrayInputStream is returned."
  ([dirpath zippath]
     (with-open [stream (io/output-stream zippath)]
       (zip-dir-helper dirpath stream)))
  ([dirpath]
     (with-open [stream (ByteArrayOutputStream.)]
       (zip-dir-helper dirpath stream)
       (ByteArrayInputStream. (.toByteArray stream)))))

(defn- pathify
  [paths]
  (str/join file-separator paths))

(defn- write-zip-entry
  [zip-stream entry out-file]
  (let [out (BufferedOutputStream. (FileOutputStream. out-file) 1024)
        data (byte-array 1024)]
    (loop [len (.read zip-stream data 0 1024)]
      (if (not (= -1 len))
        (do (.write out data 0 len)
            (recur (.read zip-stream data 0 1024)))))
    (.flush out)
    (.close out)))

(defn- copy-zip-entry
  [zip-stream entry destination]
  (let [out-file (file (pathify [destination (.getName entry)]))]
    (if (.isDirectory entry)
      (.mkdirs out-file)
      (write-zip-entry zip-stream entry out-file))
    (.getNextEntry zip-stream)))

(defn- unzip-stream
  [zip-stream destination]
  (loop [entry (.getNextEntry zip-stream)]
    (if entry
      (recur (copy-zip-entry zip-stream entry destination))))
  (.close zip-stream))

(defn unzip
  [zippath destination]
  (let [zip-stream (ZipInputStream. (io/input-stream zippath))]
    (unzip-stream zip-stream destination)))

(defn serialize-atom
  [atom]
  ;; this is a hack for the https://github.com/drlivingston/kr
  ;; library which returns binding having symbol not readable
  ;; by the Clojure reader, such as kb/ProgrammingLanguage/1
  (str/replace (str atom) #"/([0-9]+)" "/_$1"))

(defn restore-kr-symbol
  [sexp]
  (if (symbol? sexp)
    (symbol (str/replace (str sexp) #"_([0-9]+)" "$1"))
    sexp))

(defn unserialize-atom
  [s]
  (w/postwalk restore-kr-symbol (safe-read-string s)))

(defn unrecordify
  "Recursively convert the records inside coll into plain maps."
  [m]
  (reduce-kv (fn [m k v]
               (if (map? v)
                 (assoc m k (into {} v))
                 m))
             m
             m))
