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

(defun geben-test-dbgpe-index-php ()
  "Connects to php/generic.php, which is very basic php file for testing."
  (dbgpe-connect (concat (geben-test-get-test-directory) "php/generic.php")
                 '((property ((name . "$x")(fullanem . "$x")(type . "uninitialized"))))))

(defun geben-test-fixture (body)
  (unwind-protect
      (progn (geben 1)
             (funcall body))
    (geben-where)
    (geben-run)
    (geben 64)))

;; * Tests
(ert-deftest geben-test-show-context ()
  "Tests whether it's possible to show context window."
  (geben-test-fixture
   (lambda ()
     (geben-test-dbgpe-index-php)
     (sit-for 3)
     (geben-where)
     (geben-display-context))))

(provide 'geben-test)
;;; geben-test.el ends here
