(ns impact.web.jetty
  (:use ring.adapter.jetty)
  (:import org.mortbay.jetty.webapp.WebAppContext
           (org.mortbay.jetty.handler AbstractHandler)
           (org.mortbay.jetty Server Request Response)
           (org.mortbay.jetty.bio SocketConnector)
           (org.mortbay.jetty.security SslSocketConnector)
           (javax.servlet.http HttpServletRequest HttpServletResponse))
  )

(defn add-war [server contextpath warpath]
  (let [wac (WebAppContext.)]
    (.setWar wac warpath)
    (.setContextPath wac contextpath)
    (.setServer wac server)
    (.addHandler server wac)))

(defn ^Server run-jetty-with-wars
  [handler options wars]
  (let [server (run-jetty handler options)]
    (doseq [w wars]
      (add-war server (:context-path w) (:war-path w)))
    (.stop server)
    (.start server)
    server))
