(import (prefix-in :drewc/postgresql/connect pg_)
        :drewc/postgresql/postgresql
        :std/db/dbi
        :std/db/conpool
        :gerbil/gambit/exceptions
        :ecm/server/conf
        :std/sugar
        :std/iter
        :std/text/json
        :std/srfi/1)
(export #t)

(def (ecm-sql-connect)
  (let (m (conf-value database: master:))
    (pg_connect db: (pgetq db: m)
                port: (or (pgetq port: m) 5432)
                user: (or (pgetq user: m) "ecm")
                host: (or (pgetq host: m) "localhost"))))

(def current-ecm-sql-connection (make-parameter #f))
(def ecm-conpool (make-conpool ecm-sql-connect))

(def (call-with-ecm-conpool-connection fn pool: (conpool ecm-conpool))
  (let (conn #f)
      (set! conn (conpool-get conpool))
      (try
       (parameterize ((current-ecm-sql-connection conn))
         (fn conn))
        (catch ((? (not sql-error?)) e)
          (conpool-release conpool conn)
          (set! conn #f)
          (display-exception e)
          (error (call-with-output-string (cut display-exception e <>))))
        (finally (when conn (conpool-put conpool conn))))))

(defrules with-ecm-sql ()
  ((_ (con) body ...) (call-with-ecm-conpool-connection
                       (lambda (con) body ...)))
  ((_ () body ...) (with-ecm-sql (_) body ...)))


(def (row->vector r) (if (vector? r) r (vector r)))
(def (row->list r) (if (vector? r) (vector->list r) (list r)))
(def (row->plist row keys)
  (let lp ((k (map string->keyword keys)) (r (row->list row)))
    (if (null? k) k
        (cons* (car k) (car r) (lp (cdr k) (cdr r))))))

(def (row->alist row keys)
  (for/collect ((c (row->vector row)) (k keys)) (cons k c)))

(def (row->json row)
  (if (not row) row
      (with-input-from-string 
          (if (vector? row) (vector-ref row 0) row)
    read-json)))


(def query-return-types
  `((single . ,(lambda (rows _) (car rows)))
    (vectors . ,(lambda (rows _) (map row->vector rows)))
    (plists . ,(lambda (rows cols) (let (keys (map string->keyword cols))
                                (map (cut row->plist <> cols) rows))))
    (plist . ,(lambda (rows cols)
               (row->plist (car rows) cols)))
    (json . ,(lambda (rs _) (row->json (car rs))))))


(def (query connection: (con #f)
            return: (ret 'identity)
            sql . args)
  (let* ((conn (or con (current-ecm-sql-connection) (ecm-sql-connect)))
         (stmt (if (string? sql) (sql-prepare conn sql)))
         (names (sql-columns stmt)))
    (try (unless (null? args)
           (apply sql-bind stmt args))
         (let (r ((if (null? names) sql-exec sql-query) stmt))
           (unless (null? names)
             (if (eq? ret 'identity) r
             ((assget ret query-return-types) r names))))
         (finally sql-finalize stmt))))
