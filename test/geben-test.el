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

(defun geben-test-dbgpe-generic (&optional context)
  "Connects to php/generic.php, which is very basic php file for testing."
  (let ((default-context '((property ((name . "$x")(fullname . "$x")
                                      (type . "uninitialized"))))))
      (dbgpe-connect (concat (geben-test-get-test-directory) "php/generic.php")
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
     (sit-for 3)
     (geben-where)
     (geben-display-context)
     (save-excursion
       (save-match-data
         (goto-char (point-min))
         (sit-for 1)
         (widget-button-press (- (should (search-forward
                                          "массив" nil t)) 9))
         (should
          (search-forward "ключ" nil t))
         (should
          (search-forward "значение" nil t)))))))

(provide 'geben-test)
;;; geben-test.el ends here
