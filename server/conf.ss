(export #t)

(def conf-file-path "~/.ecm/ecm.conf")

(def (read-conf-file (path conf-file-path))
  (with-input-from-file path read))

(def conf #f)

(def (conf-value key . keys)
  (def (cv c key . keys)
    (let (v (pgetq key c))
      (if (null? keys) v (if v (apply cv v keys) v))))

  (unless conf (set! conf (read-conf-file)))
  (apply cv conf key keys))
