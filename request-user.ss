(import :drewc/ftw :ecm/server/postgresql :std/db/dbi  :std/db/conpool
        :std/srfi/19 :std/text/json :std/ref :std/sugar)
(export #t)

(defstruct request-user (session))
(def current-request-user (make-parameter #f))
(def current-ecm-login (make-parameter #f))


(def active-request-users (make-hash-table-eq))

(def (call-with-request-user-db user fn)
  (with-ecm-sql (c)
    (try
     (let (role (ref (request-user-session user)
                     'request_user_record 'db_role))
       (query "RESET SESSION AUTHORIZATION")
       (query (with-output-to-string "SET SESSION AUTHORIZATION " (cut write role)))
       (fn role))
     (finally (query "RESET SESSION AUTHORIZATION")))))

(defrules with-request-user-db ()
  ((_ () body ...)
   (call-with-request-user-db (current-request-user) (lambda (_) body ...))))


(def (call-with-request-user
      fn otherwise: (otherwise
                     (lambda (req)
                       (let (o (assget "Origin" (http-request-headers req)))
                          (http-response-write* 401
                          `(("Content-Type" . "text/plain")
                            ("Access-Control-Allow-Origin" . ,(or o "*"))
                            ("Access-Control-Allow-Credentials" . "true"))
                          "No Valid Request User")))))
  (let* ((id (assget "ecm-login" (http-request-cookies*)))
         (id (and id (string->symbol id)))
         (ru (and id (hash-get active-request-users id)))
         (call/ru (lambda (ru)
                    (parameterize ((current-request-user ru)
                                   (current-ecm-login id))
                      (fn ru)))))

    (cond ((not id) (otherwise (current-http-request)))
          ((not ru)
           (let* (sess
                  (with-ecm-sql ()
                    (query return: 'json
                           (string-append "SELECT jsi.login_session('"
                                          (symbol->string id) "');"))))
             (if (not sess) (otherwise (current-http-request))
                 (let (ru (request-user sess))
                   (set! (ref active-request-users id) ru)
                   (call/ru ru)))))
          (#t (call/ru ru)))))


(defrules with-request-user ()
  ((_ () body ...)
   (call-with-request-user (lambda (_) body ...)))
  ((_ (user) body ...)
   (call-with-request-user (lambda (user) body ...))))
