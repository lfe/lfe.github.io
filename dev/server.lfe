#!/usr/bin/env lfe

;;; Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun default-port () "4000")
(defun log-dir () "dev")
(defun config (base-path port)
  `(;; mandatory
    #(port ,port)
    #(server_name "localhost")
    #(server_root ,(filename:join base-path "dev"))
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
    (lfe_io:format (++ "Running the development web server as ~p with the "
                       "~nfollowing configuration:~n~n~p~n~nView the site by "
                       "loading the following in your browser:~n~n"
                       "    http://~s:~p/~n~nTo stop the server, type ^c^c.~n")
                   `(,(node)
                     ,cfg-data
                     ,(proplists:get_value 'server_name cfg-data)
                     ,(proplists:get_value 'port cfg-data))))
  'ok)

(defun wait ()
  (receive (_ "")))

;;; Execute the main function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(main)