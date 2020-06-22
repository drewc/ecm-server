(import :drewc/ftw)
(export #t)

(def ecm-server-address "127.0.0.1:4202")
(def ecm-server #f)

(def (start-ecm-server!)
  (set! ecm-server (start-ftw-http-server! ecm-server-address)))
