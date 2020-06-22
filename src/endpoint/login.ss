(import :drewc/ftw :ecm/server/postgresql :std/db/dbi :ecm/server/request-user
        :std/text/utf8 :std/text/json)

(define-endpoint login "^/json/login$")

(def (login/GET)
  (let ((ps (GET-parameters*))
        (o (assget "Origin" (http-request-headers*))))
        (http-response-write* 200 `(("Content-Type" . "application/json")
                                    ("Access-Control-Allow-Origin" . ,(or o "*"))
                                    ("Access-Control-Allow-Credentials" . "true"))
                              (with-output-to-string "" (cut write ps)))))

(def (login/OPTIONS)
 (let (o (assget "Origin" (http-request-headers*)))
   (http-response-write*
    200 `(("Access-Control-Allow-Origin" . ,(or o "*"))
          ("Access-Control-Allow-Credentials" . "true")
          ("Access-Control-Allow-Methods" . "POST, GET, OPTIONS") 
          ("Access-Control-Allow-Headers" . "Content-Type"))
    #f)))

(def (login/POST)
  (let* ((data (with-input-from-u8vector (http-request-body*) read-json))
         (user (hash-get data 'u))
         (o (assget "Origin" (http-request-headers*)))
         (pw (hash-get data 'p))
         (string-obj (with-ecm-sql () (query return: 'single
                                      "SELECT login.login_user($1, $2)"
                                      user pw)))
         (restr (or string-obj (with-output-to-string
                        "" (cut write-json
                             (list->hash-table
                              '((error . "Invalid Username and/or Password)"))))))))
  (http-response-write* 200 `(("Content-Type" . "application/json")
                              ("Access-Control-Allow-Origin" . ,(or o "*"))
                              ("Access-Control-Allow-Credentials" . "true"))
                        restr)))
