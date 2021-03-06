(import :drewc/ftw :ecm/server/postgresql :std/db/dbi :ecm/server/request-user
        :std/text/utf8 :std/text/json)


(def (/OPTIONS)
  (let (o (assget "Origin" (http-request-headers*)))
    (http-response-write*
     200 `(("Access-Control-Allow-Origin" . ,(or o "*"))
           ("Access-Control-Allow-Credentials" . "true")
           ("Access-Control-Allow-Methods" . "POST, GET, OPTIONS")
           ("Access-Control-Allow-Headers" . "Content-Type"))
     #f)))

(def (respond/json json)
  (let ((o (assget "Origin" (http-request-headers*))))
    (http-response-write* 200 `(("Content-Type" . "application/json")
                                ("Access-Control-Allow-Origin" . ,(or o "*"))
                                ("Access-Control-Allow-Credentials" . "true"))
                          json)))

(define-endpoint person "^/json/person$")

(def person/OPTIONS /OPTIONS)

(def (person-crux-query id)
  (query return: 'single
         "SELECT jso.person($1::int)" id))

(def (person/GET)
  (let* ((id (string->number (GET-parameter* "id")))
         (crux (with-ecm-sql () (person-crux-query id))))
    (respond/json crux)))

(define-endpoint person-autocomplete "^/json/person/autocomplete")

(def person-autocomplete/OPTIONS /OPTIONS)

(def (search-person q)
  (query return: 'single
         "SELECT json_agg(jso.corpus(person))
            FROM search_person($1) person" q))

(def (person-autocomplete/GET)
  (let* ((q (GET-parameter* "q"))
         (res (with-ecm-sql () (search-person q))))
    (respond/json res)))
