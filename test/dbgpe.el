;;; dbgpe.el --- DBGp engine implementation for testing.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016

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

(defvar dbgpe-connection nil)

(defvar dbgpe-filename nil)

(defun dbgpe-get-fileuri ()
  (concat "file:///" dbgpe-filename))

(defvar dbgpe-context nil)

(defvar dbgpe-transaction-id 30000)

(defvar dbgpe-status 'starting)

(defvar dbgpe-encoding 'iso-8859-1)

(defun dbgpe-init (fileuri)
  `((init
     ((xmlns . "urn:debugger_protocol_v1")
      (xmlns:xdebug . "http://xdebug.org/dbgp/xdebug")
      (fileuri . ,dbgpe-filename)
      (language . "PHP")
      (protocol_version . "1.0")
      (appid . "18408")
      (idekey . "xdebug"))
     (engine ((version . "2.4.0RC2")) "Xdebug")
     (author nil "Derick Rethans")
     (url nil "http://xdebug.org")
     (copyright nil "Copyright (c) 2002-2015 by Derick Rethans"))))

(defun dbgpe-connect (filename context)
  (dbgpe-disconnect)
  (setq dbgpe-connection (open-network-stream "dbgpe" "*dbgpe*" "localhost" 9000))
  (set-process-filter dbgpe-connection 'dbgpe-process-filter)
  (setq dbgpe-filename filename)
  (setq dbgpe-context context)
  (setq dbgpe-status 'starting)
  (dbgpe-send-string (dbgpe-build-string (dbgpe-to-xml (dbgpe-init filename)))))

(defun dbgpe-disconnect ()
  (when (bound-and-true-p dbgpe-connection)
    (delete-process dbgpe-connection)
    (setq dbgpe-transaction-id 30000)
    (setq dbgpe-filename nil)
    (setq dbgpe-context nil)))

;; * Command processing
(defun dbgpe-list-to-alist (list)
  (pcase list
    (`(,k ,v . ,rest)
      `((,(intern k) . ,v)
        ,@(dbgpe-list-to-alist rest)))
    (_ nil)))

(defun dbgpe-parse-command (command-string)
  (let* ((command-split (split-string (substring command-string 0 (- (length command-string) 1))))
         (command-name-dashed (replace-regexp-in-string "_" "-" (car command-split)))
         (fn-name (intern (concat "dbgpe-command-" command-name-dashed))))
    (funcall fn-name (dbgpe-list-to-alist (cdr command-split)))))

(defun dbgpe-to-xml(form)
  (with-temp-buffer
    (xml-print form)
    (buffer-string)))

(defun dbgpe-build-string (xml)
  (concat (number-to-string (length xml)) "\0" xml "\0"))

(defun dbgpe-send-string(string)
  (process-send-string dbgpe-connection string))

(defun dbgpe-process-filter (process string)
  ""
  (let ((response (dbgpe-parse-command string)))
    (when response
      (dbgpe-send-string (dbgpe-build-string (dbgpe-to-xml response))))))

;; * Commands
(defun dbgpe-command-parse-flag (parsers flags flag)
  (delq nil (mapcar (lambda (parser) (funcall parser flags flag)) parsers)))

(defun dbgpe-command-parse-flags (parsers flags)
  (apply 'append (mapcar (apply-partially 'dbgpe-command-parse-flag parsers flags) flags)))

(defun dbgpe-command-common-flag-parser (flags flag)
  (pcase (car flag)
    (`-i (progn (setq dbgpe-transaction-id (cdr flag))
                (cons 'transaction_id (cdr flag))))
    (_ nil)))

(defun dbgpe-command-feature-set-flag-parser (flags flag)
  (pcase (car flag)
    (`-n (cons 'feature (cdr flag)))
    (`-v (cons 'success "1"))
    (_ nil)))

(defun dbgpe-command-common ()
  '((xmlns . "urn:debugger_protocol_v1")
    (xmlns:xdebug . "http://xdebug.org/dbgp/xdebug")))

(defun dbgpe-command-feature-set (flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "feature_set"))
              (dbgpe-command-parse-flags
               '(dbgpe-command-common-flag-parser
                 dbgpe-command-feature-set-flag-parser) flags)))))

(defun dbgpe-command-feature-get (flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "feature_get") (cons 'supported "1"))
              (dbgpe-command-parse-flags
               '(dbgpe-command-common-flag-parser
                 dbgpe-command-feature-set-flag-parser) flags))
     ,(if (equal (cdr (assq '-n flags)) "encoding")
          (symbol-name dbgpe-encoding)
          "line conditional call return exception"))))

(defun dbgpe-command-feature-stdout-flag-parser (flags flag)
  (pcase (car flag)
    (`-c (cons 'success "1"))
    (_ nil)))

(defun dbgpe-command-stdout (flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "stdout"))
              (dbgpe-command-parse-flags
               '(dbgpe-command-common-flag-parser
                 dbgpe-command-feature-stdout-flag-parser) flags)))))

(defun dbgpe-command-stderr-flag-parser (flags flag)
  (pcase (car flag)
    (`-c (cons 'success "0"))
    (_ nil)))

(defun dbgpe-command-stderr (flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "stderr"))
              (dbgpe-command-parse-flags
               '(dbgpe-command-common-flag-parser
                 dbgpe-command-stderr-flag-parser) flags)))))

(defun dbgpe-command-context-names (flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "context_names"))
              (dbgpe-command-parse-flags
               '(dbgpe-command-common-flag-parser) flags))
     (context ((name . "Locals")(id . "0")))
     (context ((name . "Superglobals")(id . "1")))
     (context ((name . "User defined constants")(id . "2"))))))

(defun dbgpe-command-status (flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "status")
                    (cons 'status "starting")
                    (cons 'reason "ok"))
              (dbgpe-command-parse-flags
               '(dbgpe-command-common-flag-parser) flags)))))

(defun dbgpe-command-step-into (flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "step_into")
                    (cons 'status "break")
                    (cons 'reason "ok"))
              (dbgpe-command-parse-flags
               '(dbgpe-command-common-flag-parser) flags))
     (xdebug:message ((filename . ,(dbgpe-get-fileuri)) (lineno . "2"))))))

(defun dbgpe-command-stack-get (flags)
  (if (eq dbgpe-status 'stopped)
      (dbgpe-disconnect)

    `((response
       ,(append (dbgpe-command-common)
                (list (cons 'command "stack_get"))
                (dbgpe-command-parse-flags
                 '(dbgpe-command-common-flag-parser) flags))
       (stack ((where . "{main}")(level . "0")(type . "file")
               (filename . ,(dbgpe-get-fileuri))(lineno . "2")))))))

(defun dbgpe-command-source (flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "source")
                    (cons 'encoding "base64"))
              (dbgpe-command-parse-flags
               '(dbgpe-command-common-flag-parser) flags))
     ,(base64-encode-string (with-temp-buffer
         (insert-file-contents dbgpe-filename)
         (buffer-string))))))

(defun dbgpe-command-context-flag-parser (flags flag)
  (pcase (car flag)
    (`-c (cons 'context (cdr flag)))
    (_ nil)))

(defun dbgpe-command-context-get (flags)
  (if (eq dbgpe-status 'stopped)
      (dbgpe-disconnect)

    `((response
       ,(append (dbgpe-command-common)
                (list (cons 'command "context_get"))
                (dbgpe-command-parse-flags
                 '(dbgpe-command-common-flag-parser
                   dbgpe-command-context-flag-parser) flags))
       ,@dbgpe-context))))


(defun dbgpe-command-run (flags)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "run")
                    (cons 'status "stopping")
                    (cons 'reason "ok"))
              (dbgpe-command-parse-flags
               '(dbgpe-command-common-flag-parser) flags)))))

(defun dbgpe-command-stop (flags)
  (setq dbgpe-status 'stopped)
  `((response
     ,(append (dbgpe-command-common)
              (list (cons 'command "stop")
                    (cons 'status "stopped")
                    (cons 'reason "ok"))
              (dbgpe-command-parse-flags
               '(dbgpe-command-common-flag-parser) flags)))))

(provide 'dbgpe)
;;; dbgpe.el ends here
