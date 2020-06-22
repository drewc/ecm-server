#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("src/conf" "src/postgresql" "src/web-server" "src/request-user"
    "src/endpoint/login" "src/endpoint/session" "src/endpoint/user"
    "src/endpoint/claim" "src/endpoint/policy" "src/endpoint/person"
    "src/endpoint/transaction"
    "src/json-server"
    ))


