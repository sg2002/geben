;;; geben-test.el --- Unit tests for geben  -*- lexical-binding: t; -*-

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
(defun geben-test-get-test-directory ()
  (concat (file-name-directory (locate-library "geben")) "test/"))

(add-to-list 'load-path (geben-test-get-test-directory))

(require 'dbgpe)

(defun geben-test-dbgpe-generic (&optional stack context)
  "Connects to php/generic.php, which is very basic php file for testing."
  (let ((default-context '((property ((name . "$x")(fullname . "$x")
                                      (type . "uninitialized")))))
        (default-stack `(((filename . ,(concat (geben-test-get-test-directory)
                                             "php/generic.php"))
                          (lineno . 1))
                         ((filename . ,(concat (geben-test-get-test-directory)
                                             "php/generic.php"))
                          (lineno . 1))
                         ((filename . ,(concat (geben-test-get-test-directory)
                                             "php/generic-fns.php"))
                          (lineno . 1))
                         ((filename . ,(concat (geben-test-get-test-directory)
                                             "php/generic.php"))
                          (lineno . 1)))))
      (dbgpe-connect (if stack stack default-stack)
                     (if context context default-context))))

(defun geben-test-fixture (body)
  (unwind-protect
      (progn (geben 1)
             (funcall body))
    (geben-where)
    (geben-run)
    (geben 64)))

;; * Tests
(ert-deftest geben-test-context ()
  "Tests whether it's possible to show context window."
  (geben-test-fixture
   (lambda ()
     (geben-test-dbgpe-generic)
     (sit-for 3)
     (geben-where)
     (geben-display-context))))

(defun buffer-contains-substring (string)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t)
      (search-forward string nil t))))

(defun geben-test-dbgpe-context-unicode ()
  "Connects to php/generic.php, which is very basic php file for testing."
  (let ((array-name (encode-coding-string
                    (decode-coding-string "$массив" 'iso-8859-1) 'utf-8))
        (array-key (encode-coding-string
                    (decode-coding-string "ключ" 'iso-8859-1) 'utf-8)))
    (geben-test-dbgpe-generic
     nil
     `((property ((name . ,array-name)(fullname . ,array-name)(address . "92932256")
                  (type . "array")(children . "1")(numchildren . "1")(page . "0")
                  (pagesize . "32"))
                 (property ((name . ,array-key)
                            (fullname . ,(concat "$array[&apos;"
                                                 array-key "&apos;]"))
                            (type . "string")(size . "16")
                            (encoding . "base64"))
                           "0LfQvdCw0YfQtdC90LjQtQ=="))))))

(ert-deftest geben-test-context-array-unicode-keys ()
  "Geben used to freeze when you opened a context with an
array that had unicode keys if the process coding system
was utf-8. Also we check that both the key and value
were decoded propertly."
  (geben-test-fixture
   (lambda ()
     (geben-test-dbgpe-context-unicode)
     (sleep-for 1)
     (geben-where)
     (sleep-for 1)
     (geben-display-context)
     (sleep-for 1)
     (save-excursion
       (save-match-data
         (goto-char (point-min))
         (widget-button-press (- (should (search-forward
                                          "массив" nil t)) 9))
         (should
          (search-forward "ключ" nil t))
         (should
          (search-forward "значение" nil t)))))))

(ert-deftest geben-test-find-this-file ()
  "Check whether geben-find-this-file works."
  (geben-test-fixture
   (lambda ()
     ;; We create multiple windows since the default
     ;; fallback logic is capable of switching to buffer
     ;; in the single existing window, but
     ;; this won't help when there's more than 1 window
     (split-window-right)
     (geben-test-dbgpe-generic)
     (sleep-for 1)
     (find-file (concat (geben-test-get-test-directory)
                        "php/generic-fns.php"))
     (let ((window (selected-window)))
       (geben-find-this-file)
       (sleep-for 1)
       (should (equal (file-name-base (buffer-file-name (window-buffer window))) "generic-fns"))
       (with-current-buffer (window-buffer window)
         (should geben-mode))))))


(ert-deftest geben-test-window-replace-when-session-moves ()
  "Check the ability to replace the current debug buffer with buffer
containing the new session position, when the session moves."
  (geben-test-fixture
   (lambda ()
     ;; We create multiple windows since the default
     ;; fallback logic is capable of switching to buffer
     ;; in the single existing window, but
     ;; this won't help when there's more than 1 window
     (split-window-right)
     (geben-test-dbgpe-generic)
     (sleep-for 1)
     (with-current-buffer (window-buffer (selected-window))
       (geben-step-into)
       (sleep-for 1)
       (should (equal (file-name-base (buffer-file-name (window-buffer (selected-window)))) "generic-fns"))
       (should geben-mode)
       (geben-step-over)
       (sleep-for 1)
       (should (equal (file-name-base (buffer-file-name (window-buffer (selected-window)))) "generic"))
       (should geben-mode)))))

(provide 'geben-test)
;;; geben-test.el ends here
