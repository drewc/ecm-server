(import :drewc/ftw :ecm/server/postgresql :std/db/dbi :ecm/server/request-user)

(define-endpoint session "^/json/session$")

(def (session/GET (s "{}"))
  (let (o (assget "Origin" (http-request-headers*)))
    (with-request-user ()
      (let (sess s #;(query return: 'single
                        (string-append
                         "SELECT jsi.login_session('"
                         (symbol->string (current-ecm-login)) "');")))
        (http-response-write* 200 `(("Content-Type" . "application/json")
                                    ("Access-Control-Allow-Origin" . ,(or o "*"))
                                    ("Access-Control-Allow-Credentials" . "true"))
                              sess)))))
