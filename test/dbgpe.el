;;; dbgpe.el --- DBGp engine implementation for testing.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017

;; Author: sg2002 <sg2002@gmx.com>
;; Keywords: DBGp, debugger, PHP, Xdebug, Perl, Python, Ruby, Tcl, Komodo

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(defvar dbgpe-sessions '())

(defun dbgpe-get-fileuri (session-id)
  (concat "file:///" (cdr (assoc 'filename (car (cdr (assq 'stack (assq session-id dbgpe-sessions))))))))

(defun dbgpe-get-lineno (session-id)
  (number-to-string (cdr (assoc 'lineno (car (cdr (assq 'stack (assq session-id dbgpe-sessions))))))))

(defun dbgpe-stack-step (session-id)
  (push (cons
         session-id
         (cons
          (cons 'stack
                (cdr (cdr (assq 'stack (assq session-id dbgpe-sessions)))))
          (assq session-id dbgpe-sessions)))
        dbgpe-sessions))

(defvar dbgpe-encoding 'iso-8859-1)

(defun dbgpe-init (session)
  `((init
     ((xmlns . "urn:debugger_protocol_v1")
      (xmlns:xdebug . "http://xdebug.org/dbgp/xdebug")
      (fileuri . ,(cdr (assoc 'filename (car (cdr (assq 'stack session))))))
      (language . "PHP")
      (protocol_version . "1.0")
      (appid . "18408")
      (idekey . "xdebug"))
     (engine ((version . "2.4.0RC2")) "Xdebug")
     (author nil "Derick Rethans")
     (url nil "http://xdebug.org")
     (copyright nil "Copyright (c) 2002-2015 by Derick Rethans"))))

(defun dbgpe-connect (stack context)
  (let* ((connection (open-network-stream "dbgpe" "*dbgpe*" "localhost" 9000))
         (session `((stack . ,stack)
                    (context . ,context)
                    (status 'starting))))
    (push (cons connection session) dbgpe-sessions)
    (fset
     'dbgpe-process-filter
     (lambda (process string)
       (let ((response (dbgpe-parse-command
                        connection
                        (split-string (substring string 0 (- (length string) 1))))))
         (when response
           (dbgpe-send-string process (dbgpe-build-string (dbgpe-to-xml response)))))))
    (set-process-filter connection 'dbgpe-process-filter)
    (dbgpe-send-string connection (dbgpe-build-string (dbgpe-to-xml (dbgpe-init session))))
    connection))

(defun dbgpe-sentinel (process event)
  (when (equal event "connection broken by remote peer\n")
    (assq-delete-all process dbgpe-sessions)
    (delete-process process)))


(defun dbgpe-disconnect (connection)
  (delete-process connection)
  (assq-delete-all connection dbgpe-sessions))

;; * Command processing
(defun dbgpe-list-to-alist (list)
  (pcase list
    (`(,k ,v . ,rest)
      `((,(intern k) . ,v)
        ,@(dbgpe-list-to-alist rest)))
    (_ nil)))

(defun dbgpe-parse-command (session-id command-split)
  (let* ((command-name-dashed (replace-regexp-in-string "_" "-" (car command-split)))
         (fn-name (intern (concat "dbgpe-command-" command-name-dashed))))
    (funcall fn-name session-id (dbgpe-list-to-alist (cdr command-split)))))

(defun dbgpe-to-xml(form)
  (with-temp-buffer
    (xml-print form)
    (buffer-string)))

(defun dbgpe-build-string (xml)
  (concat (number-to-string (length xml)) "\0" xml "\0"))

(defun dbgpe-send-string(process string)
  (process-send-string process string))

;; * Commands
(defun dbgpe-command-parse-flag (session-id parsers flags flag)
  (delq nil (apply 'append (mapcar (lambda (parser) (funcall parser session-id flags flag)) parsers))))

(defun dbgpe-command-parse-flags (session-id parsers flags)
  (apply 'append (mapcar (apply-partially 'dbgpe-command-parse-flag session-id parsers flags) flags)))

(defun dbgpe-command-common-flag-parser (session-id flags flag)
  (pcase (car flag)
    (`-i (progn
           (push (cons session-id (cons (cons 'transaction-id (cdr flag)) (assq session-id dbgpe-sessions)))
                 dbgpe-sessions)
           `((transaction_id . ,(cdr flag)))))
    (_ nil)))

(defun dbgpe-command-feature-set-flag-parser (session-id flags flag)
  (pcase (car flag)
    (`-n `((feature . ,(cdr flag))))
    (`-v `((success . "1")))
    (_ nil)))

(defun dbgpe-command-common ()
  '((xmlns . "urn:debugger_protocol_v1")
    (xmlns:xdebug . "http://xdebug.org/dbgp/xdebug")))

(defun dbgpe-command-feature-set (session-id flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "feature_set"))
              (dbgpe-command-parse-flags
               session-id
               '(dbgpe-command-common-flag-parser
                 dbgpe-command-feature-set-flag-parser) flags)))))

(defun dbgpe-command-feature-get (session-id flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "feature_get") (cons 'supported "1"))
              (dbgpe-command-parse-flags
               session-id
               '(dbgpe-command-common-flag-parser
                 dbgpe-command-feature-set-flag-parser) flags))
     ,(if (equal (cdr (assq '-n flags)) "encoding")
          (symbol-name dbgpe-encoding)
          "line conditional call return exception"))))

(defun dbgpe-command-feature-stdout-flag-parser (session-id flags flag)
  (pcase (car flag)
    (`-c `((success . "1")))
    (_ nil)))

(defun dbgpe-command-stdout (session-id flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "stdout"))
              (dbgpe-command-parse-flags
               session-id
               '(dbgpe-command-common-flag-parser
                 dbgpe-command-feature-stdout-flag-parser) flags)))))

(defun dbgpe-command-stderr-flag-parser (session-id flags flag)
  (pcase (car flag)
    (`-c `((success . "0")))
    (_ nil)))

(defun dbgpe-command-stderr (session-id flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "stderr"))
              (dbgpe-command-parse-flags
               session-id
               '(dbgpe-command-common-flag-parser
                 dbgpe-command-stderr-flag-parser) flags)))))

(defun dbgpe-command-context-names (session-id flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "context_names"))
              (dbgpe-command-parse-flags
               session-id
               '(dbgpe-command-common-flag-parser) flags))
     (context ((name . "Locals")(id . "0")))
     (context ((name . "Superglobals")(id . "1")))
     (context ((name . "User defined constants")(id . "2"))))))

(defun dbgpe-command-status (session-id flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "status")
                    (cons 'status "starting")
                    (cons 'reason "ok"))
              (dbgpe-command-parse-flags
               session-id
               '(dbgpe-command-common-flag-parser) flags)))))

(defun dbgpe-command-step-into (session-id flags)
  (dbgpe-stack-step session-id)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "step_into")
                    (cons 'status "break")
                    (cons 'reason "ok"))
              (dbgpe-command-parse-flags
               session-id
               '(dbgpe-command-common-flag-parser) flags))
     (xdebug:message ((filename . ,(dbgpe-get-fileuri session-id))
                      (lineno . ,(dbgpe-get-lineno session-id)))))))

(defun dbgpe-command-step-over (session-id flags)
  (dbgpe-stack-step session-id)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "step_into")
                    (cons 'status "break")
                    (cons 'reason "ok"))
              (dbgpe-command-parse-flags
               session-id
               '(dbgpe-command-common-flag-parser) flags))
     (xdebug:message ((filename . ,(dbgpe-get-fileuri session-id))
                      (lineno . ,(dbgpe-get-lineno session-id)))))))


(defun dbgpe-command-stack-get (session-id flags)
  (if (eq (cdr (assq 'status (assq session-id dbgpe-sessions))) 'stopped)
      (progn (dbgpe-disconnect session-id)
             nil)
    `((response
       ,(append (dbgpe-command-common)
                (list (cons 'command "stack_get"))
                (dbgpe-command-parse-flags
                 session-id
                 '(dbgpe-command-common-flag-parser) flags))
       (stack ((where . "{main}")(level . "0")(type . "file")
               (filename . ,(dbgpe-get-fileuri session-id))
               (lineno . ,(dbgpe-get-lineno session-id))))))))

(defun dbgpe-command-source-flag-parser (session-id flags flag)
  (pcase (car flag)
    (`-f (list
          (base64-encode-string
           (with-temp-buffer
             (insert-file-contents
              ;; Normally geben sends filename prefixed with file:///
              ;; but geben-find-file uses file:// prefix.
              (replace-regexp-in-string "file:[/]*" "" (cdr flag)))
             (buffer-string)))))
    (_ nil)))

(defun dbgpe-command-source (session-id flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "source")
                    (cons 'encoding "base64"))
              (dbgpe-command-parse-flags
               session-id
               '(dbgpe-command-common-flag-parser) flags))
     ,@(dbgpe-command-parse-flags session-id
       '(dbgpe-command-source-flag-parser) flags))))

(defun dbgpe-command-context-flag-parser (session-id flags flag)
  (pcase (car flag)
    (`-c `((context . ,(cdr flag))))
    (_ nil)))


(defun dbgpe-command-context-get (session-id flags)
  (if (eq (cdr (assq 'status (assq session-id dbgpe-sessions))) 'stopped)
      (progn (dbgpe-disconnect)
             nil)
    `((response
       ,(append (dbgpe-command-common)
                (list (cons 'command "context_get"))
                (dbgpe-command-parse-flags
                 session-id
                 '(dbgpe-command-common-flag-parser
                   dbgpe-command-context-flag-parser) flags))
       ,@(cdr (assq 'context (assq session-id dbgpe-sessions)))))))

(defun dbgpe-command-run (session-id flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "run")
                    (cons 'status "stopping")
                    (cons 'reason "ok"))
              (dbgpe-command-parse-flags
               session-id
               '(dbgpe-command-common-flag-parser) flags)))))

(defun dbgpe-command-stop (session-id flags)
  (push (cons session-id (cons (cons 'status 'stopped) (assq session-id dbgpe-sessions)))
        dbgpe-sessions)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "stop")
                    (cons 'status "stopped")
                    (cons 'reason "ok"))
              (dbgpe-command-parse-flags
               session-id
               '(dbgpe-command-common-flag-parser) flags)))))

;; * DBGPE proxy
;; ** Single session utility functions
;; Functions here have session-aware analogs higher up, for the debugger engine itself.
(defun dbgpe-proxy-parse-command (command-split)
  "Parse command COMMAND-SPLIT.
This function works similarly to DBGPE-PARSE-COMMAND, only it does not take SESSION an argument."
  (let* ((command-name-dashed (replace-regexp-in-string "_" "-" (car command-split)))
         (fn-name (intern (concat "dbgpe-command-" command-name-dashed))))
    (funcall fn-name (dbgpe-list-to-alist (cdr command-split)))))

(defun dbgpe-proxy-command-parse-flag (parsers flags flag)
  (delq nil (apply 'append (mapcar (lambda (parser) (funcall parser flags flag)) parsers))))

(defun dbgpe-proxy-command-parse-flags (parsers flags)
  (apply 'append (mapcar (apply-partially 'dbgpe-proxy-command-parse-flag parsers flags) flags)))
;; ** Debugger side
(defvar dbgpe-proxy-debugger-listener nil)

(defvar dbgpe-proxy-debugger-listener-port 9001)

(defvar dbgpe-proxy-debuggers ())

(defun dbgpe-proxy-debugger-init-process-filter (process string)
  (let ((response (dbgpe-proxy-parse-command (split-string string))))
    (when response
      (process-send-string process (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                                           (dbgpe-to-xml response)))))
  (delete-process process))

(defun dbgpe-command-proxyinit-flag-parser (flags flag)
  (pcase (car flag)
    (`-k `((idekey . ,(cdr flag))))
    (`-a `((address . ,(car (split-string (cdr flag) ":")))
           (port . ,(cadr (split-string (cdr flag) ":")))))
    (_ nil)))

(defun dbgpe-proxy-reply (flags)
  `((proxyinit ((success . "1")
                 ,@(dbgpe-proxy-command-parse-flags
                    '(dbgpe-command-proxyinit-flag-parser) flags)))))

(defun dbgpe-command-proxyinit (flags)
  (add-to-list 'dbgpe-proxy-debuggers
               `(,(cdr (assoc '-k flags)) .
                 ((address ,(car (split-string (cdr (assoc '-a flags)) ":")))
                  (port ,(cadr (split-string (cdr (assoc '-a flags)) ":"))))))
  (dbgpe-proxy-reply flags))

(defun dbgpe-command-proxystop (flags)
  (setq dbgpe-proxy-debuggers
        (delq (assoc (cdr (assoc '-k flags)) dbgpe-proxy-debuggers)
              dbgpe-proxy-debuggers))
  (dbgpe-proxy-reply flags))

;; ** Engine side
(defvar dbgpe-proxy-processes ())

(defvar dbgpe-proxy-engine-listener nil)

(defvar dbgpe-proxy-engine-listener-port 9000)

(defun dbgpe-proxy-engine-sentinel (process event)
  (when (not (process-live-p process))
    (when (process-live-p (cdr (assoc process dbgpe-proxy-processes)))
      (delete-process (cdr (assoc process dbgpe-proxy-processes))))
    (setq dbgpe-proxy-processes (delq (assoc process dbgpe-proxy-processes) dbgpe-proxy-processes))))

(defun dbgpe-proxy-debugger-sentinel (process event)
  (when (not (process-live-p process))
    (when (process-live-p (cdr (rassoc process dbgpe-proxy-processes)))
      (delete-process (cdr (rassoc process dbgpe-proxy-processes))))
    (setq dbgpe-proxy-processes (delq (rassoc process dbgpe-proxy-processes) dbgpe-proxy-processes))))

(defun dbgpe-proxy-engine-process-filter (process string)
  (if (assoc process dbgpe-proxy-processes)
      (process-send-string (cdr (assoc process dbgpe-proxy-processes)) string))
  (dbgpe-proxy-create-process process string))

(defun dbgpe-proxy-debugger-process-filter (process string)
  (process-send-string (car (rassoc process dbgpe-proxy-processes)) string))

(defun dbgpe-proxy-create-process (engine-process init)
  (let* ((xml (with-temp-buffer
             (insert init)
             (xml-parse-region)))
         (idekey (cdr (assoc 'idekey (car (cdr (car xml))))))
         (debugger (cdr (assoc idekey dbgpe-proxy-debuggers)))
         (debugger-process
          (if debugger
              (make-network-process
               :name "dbgpe-proxy-debugger-process"
               :host (cadr (assoc 'address
                                  debugger))
               :service (string-to-number
                         (cadr (assoc 'port debugger)))
               :sentinel 'dbgpe-proxy-debugger-sentinel
               :filter 'dbgpe-proxy-debugger-process-filter)
            nil)))
    (when debugger-process
      (add-to-list 'dbgpe-proxy-processes
                   (cons engine-process debugger-process))
      (process-send-string
       debugger-process
       (dbgpe-build-string
        (dbgpe-to-xml
         (cons
          (cons
           (car (car xml))
           (cons
            (cons
             (cons 'proxied (car (process-contact engine-process)))
             (car (cdr (car xml))))
            (cdr (cdr (car xml)))))
          (cdr xml))))))))

;; ** Control
(defun dbgpe-proxy-start ()
  (setq dbgpe-proxy-engine-listener
        (make-network-process :name "dbgpe-proxy-engine-listener"
                              :server t
                              :service dbgpe-proxy-engine-listener-port
                              :sentinel 'dbgpe-proxy-engine-sentinel
                              :filter 'dbgpe-proxy-engine-process-filter)
        dbgpe-proxy-debugger-listener
        (make-network-process :name "dbgpe-proxy-debugger-listener"
                              :server t
                              :service dbgpe-proxy-debugger-listener-port
                              :filter 'dbgpe-proxy-debugger-init-process-filter)))

(defun dbgpe-proxy-stop ()
  (delete-process dbgpe-proxy-engine-listener)
  (delete-process dbgpe-proxy-debugger-listener)
  (setq dbgpe-proxy-debuggers ())
  (mapc (lambda (elt) (delete-process (car elt))
          (delete-process (cdr elt))) dbgpe-proxy-processes)
  (setq dbgpe-proxy-processes ()))

(provide 'dbgpe)
;;; dbgpe.el ends here
