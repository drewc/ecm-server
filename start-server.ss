(import :ecm/server/web-server
        :ecm/server/json-server
        :std/logger)

(set! std/logger#current-logger (make-parameter #f))
(def error-log (open-output-string))

(start-logger! error-log)

(start-ecm-server!)
(get-output-string error-log)
