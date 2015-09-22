#!/usr/bin/env lfe
;;;; This script runs a local web server serving up ./ as the document root. A
;;;; script was used to do this due to the following:
;;;;  * this project has no source code other than static HTML
;;;;  * we did not want to add modules which would then need a compile stage
;;;;  * we wanted to be able to bring up the server with a single command
;;;;    and display help to stdout about usage, once it was up
;;;;  * we wanted to closely match the user/dev experience that others might
;;;;    be familar with from working with such projects as Jekyll, etc.
;;;;

;;; Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun default-port () "4000")
(defun log-dir () "dev")
(defun config (base-path port)
  `(;; mandatory
    #(port ,port)
    #(server_name "localhost")
    #(server_root ,(filename:join base-path (log-dir)))
    #(document_root ,base-path)
    ;; communication properties
    #(bind_address any)
    #(socket_type ip_comm)
    #(ipfamily inet)
    ;; administrative properties
    #(mime_types (#("html" "text/html")
                  #("css" "text/css")
                  #("js" "application/javascript")))
    ;; URL aliasing properties
    #(directory_index ("index.html"))
    ;; log properties
    #(error_log "logs/error_log")
    #(transfer_log "logs/access_log")
    #(security_log "logs/auth_log")))

;;; Entry point ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  (main script-args))

(defun main
  ((`(,base-path))
   (main base-path (default-port)))
  ((`(,base-path ,port))
   (main base-path (list_to_integer port))))

(defun main (base-path port)
  (inets:start 'permanent)
  (case (start-httpd base-path port)
    (`#(ok ,pid)
     (print-help pid)
     (wait))
    (start-error
     (lfe_io:format "~p~n" `(,start-error)))))

;;; Support functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-httpd (base-path port)
  (inets:start 'httpd (config base-path port)))

(defun print-help (pid)
  (let ((cfg-data (httpd:info pid)))
    (lfe_io:format (++ "~nYou are running the development web server as ~p with "
                       "~nthe following configuration:~n~n~p~n~nView the site "
                       "by loading the following in your browser:~n~n"
                       "    http://~s:~p/~n~nIn a separate terminal window, you "
                       "can connect to this node by~nexecuting 'make connect' "
                       "from the same directory.~n~n"
                       "To stop the server, type ^c^c.~n")
                   `(,(node)
                     ,cfg-data
                     ,(proplists:get_value 'server_name cfg-data)
                     ,(proplists:get_value 'port cfg-data))))
  'ok)

(defun wait ()
  (receive (_ "")))

;;; Execute the main function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(main)
