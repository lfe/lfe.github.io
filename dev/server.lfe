#! /usr/bin/env lfescript
;; -*- mode: lfe -*-
;;! -smp enable -sname factorial -mnesia debug verbose

(defun main
  ((`(,base_path))
    (io:format "Got path: ~p~n" (list base_path))
    (io:format "Joined path: ~p~n" (list (filename:join base_path "dev")))
    (inets:start 'permanent)
    (inets:start 'httpd `(;; mandatory
                          #(port 4000)
                          #(server_name "localhost")
                          #(server_root ,(filename:join base_path "dev"))
                          #(document_root ,base_path)
                          ;; communication properties
                          #(bind_address any)
                          #(socket_type ip_comm)
                          #(ipfamily inet)
                          ;; administrative properties
                          #(mime_types (#("html" "text/html")
                                        #("css" "text/css")))
                          ;; log properties
                          #(error_log "logs/error_log")
                          #(transfer_log "logs/access_log")
                          #(security_log "logs/auth_log")))
   (receive (_ ""))))
