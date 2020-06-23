#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("server/conf" "server/postgresql" "server/web-server" "server/request-user"
    "server/endpoint/login" "server/endpoint/session" "server/endpoint/user"
    "server/endpoint/claim" "server/endpoint/policy" "server/endpoint/person"
    "server/endpoint/transaction"
    "server/json-server"
    ))


