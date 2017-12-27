;;; cakecrumbs-tests.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  onohiroko

;; Author: onohiroko <onohiroko@kuanyenMBP>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun tcakecrumbs-pcp ()
  "print current point. for writing test."
  (interactive)
  (message "%s" (point)))
(global-set-key (kbd "<f12>") 'tcakecrumbs-pcp)


(defmacro tcakecrumbs-deftest (test-name major-mode-list file-path &rest test-list)
  `(ert-deftest ,(intern (format "tcakecrumbs-test-%s" test-name)) ()
     ,@(mapcar (lambda (pair)
                 `(should (equal ,(car pair) ,(cadr pair))))
               test-list))
  )
(jade-mode)
(pug-mode)
(yajade-mode)
(scss-mode)
(less-css-mode)
(defun tcakecrumbs-run-with (in-major-mode)
  (with-temp-buffer
    (yajade-mode))
  )
(with-temp-buffer
  (insert-file-contents fPath)
  (goto-char 1))
(tcakecrumbs-deftest "jade"
                     ((+ 1 1) 2)
                     ((+ 3 3) 6)
                     )


(ert-deftest tcakecrumbs-jade ()
  "Tests cakecrumbs + jade file"
  (should (equal (pp-to-string '(quote quote)) "'quote"))
  (should (equal (pp-to-string '((quote a) (quote b))) "('a 'b)\n"))
  (should (equal (pp-to-string '('a 'b)) "('a 'b)\n")))


(provide 'cakecrumbs-tests)
;;; cakecrumbs-tests.el ends here
