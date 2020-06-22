(import :drewc/ftw :ecm/server/postgresql :std/db/dbi :ecm/server/request-user
        :std/text/utf8 :std/text/json :gerbil/gambit/exceptions :std/sugar)

(define-endpoint transaction-edit "^/json/transaction/edit$")

(def (/OPTIONS)
  (let (o (assget "Origin" (http-request-headers*)))
    (http-response-write*
     200 `(("Access-Control-Allow-Origin" . ,(or o "*"))
           ("Access-Control-Allow-Credentials" . "true")
           ("Access-Control-Allow-Methods" . "POST, GET, OPTIONS")
           ("Access-Control-Allow-Headers" . "Content-Type"))
     #f)))

(def (respond/json code: (code 200) json )
  (let ((o (assget "Origin" (http-request-headers*))))
    (http-response-write* code `(("Content-Type" . "application/json")
                                ("Access-Control-Allow-Origin" . ,(or o "*"))
                                ("Access-Control-Allow-Credentials" . "true"))
                          json)))


(def transaction-edit/OPTIONS /OPTIONS)

(def (transaction-select-values)
  (query return: 'single
         "SELECT jso.transaction_select_values()"))

(def (transaction-edit/GET)
  (let ((o (assget "Origin" (http-request-headers*)))
        (v (with-ecm-sql () (transaction-select-values))))
    (with-request-user () (respond/json v))))


(define-endpoint transaction-create "^/json/transaction/create$")

(def transaction-create/OPTIONS /OPTIONS)

(def (insert-transaction tr)
  (with-request-user-db ()
    (query return: 'single
           "SELECT jso.insert_transaction($1)" tr)))

(def (transaction-create/POST)
  (with-request-user ()
   (try
    (let* ((req-tranny (http-request-body*))
           (new-tranny (insert-transaction req-tranny)))
      (respond/json tr))
    (catch (e)
      (respond/json
       (json-object->string
        (plist->hash-table
         [error: (call-with-output-string
                  (cut display-exception e <>))])))))))
