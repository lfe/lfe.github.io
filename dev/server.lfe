#! /usr/bin/env lfescript
;; -*- mode: lfe -*-
;;! -smp enable -sname devserver

(defun main
  ((`(,base-path))
    (inets:start 'permanent)
    (case (start-httpd base-path)
      (`#(ok ,pid)
       (print-help pid)
       (wait))
      (start-error
       (lfe_io:format "~p~n" `(,start-error))))))

(defun start-httpd (base-path)
  (inets:start 'httpd `(;; mandatory
                        #(port 4000)
                        #(server_name "localhost")
                        #(server_root ,(filename:join base-path "dev"))
                        #(document_root ,base-path)
                        ;; communication properties
                        #(bind_address any)
                        #(socket_type ip_comm)
                        #(ipfamily inet)
                        ;; administrative properties
                        #(mime_types (#("html" "text/html")
                                      #("css" "text/css")))
                        ;; URL aliasing properties
                        #(directory_index ("index.html"))
                        ;; log properties
                        #(error_log "logs/error_log")
                        #(transfer_log "logs/access_log")
                        #(security_log "logs/auth_log"))))

(defun print-help (pid)
  (let ((cfg-data (httpd:info pid)))
    (lfe_io:format "Running dev server with the following configuration:~n~n" '())
    (lfe_io:format "~p~n~n" `(,cfg-data))
    (lfe_io:format "View the site by loading the following in your browser:~n~n" '())
    (lfe_io:format "    http://~s:~p/~n~n" `(,(proplists:get_value 'server_name cfg-data)
                                             ,(proplists:get_value 'port cfg-data)))
    (lfe_io:format "To stop the server, type ^c.~n~n" '())
    'ok))

(defun wait ()
  (receive (_ "")))

