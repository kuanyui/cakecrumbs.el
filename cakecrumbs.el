;;; cakecrumbs.el ---                               -*- lexical-binding: t; -*-

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

(defvar-local cakecrumbs--parents-list '())
(defvar-local cakecrumbs--refresh-timer nil
  "Buffer-local timer.")
(defvar-local cakecrumbs--original-head-line-format nil
  "The value of `header-line-format' before calling `cakecrumbs-install-header'")

(defvar cakecrumbs-separator " | ")
(setq cakecrumbs-ellipsis "[...] ")


(defface cakecrumbs-ellipsis
  '((((class color) (background light)) (:inherit font-lock-comment-face))
    (((class color) (background dark)) (:inherit font-lock-comment-face)))
  "Ellipsis" :group 'cakecrumbs-faces)

(defface cakecrumbs-separator
  '((((class color) (background light)) (:inherit font-lock-comment-face))
    (((class color) (background dark)) (:inherit font-lock-comment-face)))
  "Seperator between each level" :group 'cakecrumbs-faces)

(defface cakecrumbs-tag
  '((((class color) (background light)) (:inherit font-lock-function-name-face))
    (((class color) (background dark)) (:inherit font-lock-function-name-face)))
  "HTML/CSS tag" :group 'cakecrumbs-faces)

(defface cakecrumbs-id
  '((((class color) (background light)) (:inherit font-lock-keyword-face))
    (((class color) (background dark)) (:inherit font-lock-keyword-face)))
  "HTML/CSS #id" :group 'cakecrumbs-faces)

(defface cakecrumbs-class
  '((((class color) (background light)) (:inherit font-lock-type-face))
    (((class color) (background dark)) (:inherit font-lock-type-face)))
  "HTML/CSS .class" :group 'cakecrumbs-faces)

(defface cakecrumbs-pseudo
  '((((class color) (background light)) (:inherit font-lock-constant-face))
    (((class color) (background dark)) (:inherit font-lock-constant-face)))
  "CSS :pseudo selector" :group 'cakecrumbs-faces)

(defface cakecrumbs-attr
  '((((class color) (background light)) (:inherit font-lock-variable-name-face))
    (((class color) (background dark)) (:inherit font-lock-variable-name-face)))
  "CSS [attribute=] selector" :group 'cakecrumbs-faces)

(defface cakecrumbs-preprocessor
  '((((class color) (background light)) (:inherit font-lock-preprocessor-face))
    (((class color) (background dark)) (:inherit font-lock-preprocessor-face)))
  "SCSS/LESS/Stylus @.+ or CSS @media" :group 'cakecrumbs-faces)


(defun cakecrumbs-matched-positions-all (regexp string &optional subexp-depth)
  "Return a list of matched positions for REGEXP in STRING.
SUBEXP-DEPTH is 0 by default."
  (if (null subexp-depth)
      (setq subexp-depth 0))
  (save-match-data
    (let ((pos 0) result)
      (while (and (string-match regexp string pos)
                  (< pos (length string)))
        (let ((m (match-end subexp-depth)))
          (push (cons (match-beginning subexp-depth) (match-end subexp-depth)) result)
          (setq pos (match-end 0))))
      (nreverse result))))

(setq ex "span.col-md-3.col-xs-6#test-hello")
(setq cs "span .col-md-3.col-xs-6 > #test-hello[disabled=true] :not(:nth-child(42))")

(setq cakecrumbs-re-tag "^[^ .#:&@()]+")
(setq cakecrumbs-re-class "[.][-A-z0-9_]+")
(setq cakecrumbs-re-id "#[-A-z0-9_]+")
(setq cakecrumbs-re-attr "\\[[-A-z0-9_]+.*\\]")
(setq cakecrumbs-re-pseudo "::?[-A-z0-9_]+")
(setq cakecrumbs-re-preprocessor "@[-A-z0-9_]+")

;; (setq cakecrumbs-ignored-patterns '("\\.col-[a-z0-9-]+"))  ; [FIXME] When parent only has .col-*
(setq cakecrumbs-ignored-patterns '())

(cakecrumbs-matched-positions-all cakecrumbs-re-tag cs 0)
(cakecrumbs-matched-positions-all cakecrumbs-re-id cs 0)
(cakecrumbs-matched-positions-all cakecrumbs-re-class cs 0)
(cakecrumbs-matched-positions-all cakecrumbs-re-attr cs 0)
(cakecrumbs-matched-positions-all cakecrumbs-re-pseudo cs 0)

(defun cakecrumbs-propertize-string (level-str)
  "Input is single-level string"
  (mapc (lambda (patt)
          (setq level-str (replace-regexp-in-string patt "" level-str)))
        cakecrumbs-ignored-patterns)
  (let ((m (car (cakecrumbs-matched-positions-all cakecrumbs-re-tag level-str)))) ; tag
    (if m (set-text-properties (car m) (cdr m) '(face cakecrumbs-tag) level-str)))
  (let ((m (car (cakecrumbs-matched-positions-all cakecrumbs-re-id level-str)))) ; id
    (if m (set-text-properties (car m) (cdr m) '(face cakecrumbs-id) level-str)))
  (let ((m (cakecrumbs-matched-positions-all cakecrumbs-re-class level-str))) ; class
    (if m (mapc (lambda (pair)
                  (set-text-properties (car pair) (cdr pair) '(face cakecrumbs-class) level-str))
                m)))
  (let ((m (cakecrumbs-matched-positions-all cakecrumbs-re-attr level-str))) ; attr
    (if m (mapc (lambda (pair)
                  (set-text-properties (car pair) (cdr pair) '(face cakecrumbs-attr) level-str))
                m)))
  (let ((m (cakecrumbs-matched-positions-all cakecrumbs-re-pseudo level-str))) ; pseudo
    (if m (mapc (lambda (pair)
                  (set-text-properties (car pair) (cdr pair) '(face cakecrumbs-pseudo) level-str))
                m)))
  (let ((m (cakecrumbs-matched-positions-all cakecrumbs-re-preprocessor level-str))) ; preprocessor
    (if m (mapc (lambda (pair)
                  (set-text-properties (car pair) (cdr pair) '(face cakecrumbs-preprocessor) level-str))
                m)))
  level-str)

(defun cakecrumbs-format-parents (parents)
  "PARENTS is a (no-propertized) string list"
  (mapconcat #'cakecrumbs-propertize-string
             parents
             (propertize cakecrumbs-separator 'face 'cakecrumbs-separator)))

(defun cakecrumbs-generate-header-string ()
  ""
  (let ((parents (cakecrumbs-get-parents)))
    (if (null parents)
        (propertize "(cakecrumbs idle)" 'face 'cakecrumbs-ellipsis)
      (let* ((fin (cakecrumbs-format-parents parents))
             (ellipsis-str (or cakecrumbs-ellipsis "[...]"))
             (ellipsis-len (length ellipsis-str))
             (need-ellipsis nil))
        (while (> (+ (length fin) (if need-ellipsis ellipsis-len 0))
                  (window-body-width))
          (setq need-ellipsis t)
          (pop parents)
          (setq fin (cakecrumbs-format-parents parents)))
        (if need-ellipsis
            (concat (propertize ellipsis-str 'face 'cakecrumbs-ellipsis) fin)
          fin)))))

(defun zzz () (interactive) (message (cakecrumbs-generate-header-string)))

(defun cakecrumbs-install-header ()
  (setq cakecrumbs--original-head-line-format header-line-format)
  (setq header-line-format '((:eval (cakecrumbs-generate-header-string)))))

(defun cakecrumbs-uninstall-header ()
  (setq header-line-format cakecrumbs--original-head-line-format))

(defun cakecrumbs-current-line-string ()
  (substring (thing-at-point 'line t) 0 -1))

;; ======================================================
;; header-line
;; ======================================================

;; ( "" (:propertize which-func-current local-map
;;                   (keymap (mode-line keymap (mouse-3 . end-of-defun)
;;                                      (mouse-2 . #[nil "" [1 narrow-to-defun] 2 nil nil])
;;                                      (mouse-1 . beginning-of-defun)
;;                                      ))
;;                   face which-func mouse-face mode-line-highlight help-echo
;;                   "mouse-1: go to beginning
;; mouse-2: toggle rest visibility
;; mouse-3: go to end") "")

;; (add-hook 'prog-mode-hook 'which-func-setup-header t t)
;; (add-hook 'html-mode-hook 'which-func-setup-header t t)


;; (format "point == %s , %s" (point) (parse-partial-sexp (point-min) (point-max) nil nil (syntax-ppss)))
(save-excursion (syntax-after (point))
                (skip-syntax-forward "^()" 3)
                (syntax-ppss))

(defun cakecrumbs-string-match (regexp num string)
  (save-match-data
    (if (string-match regexp string)
        (match-string num string)
      nil)))

(defun cakecrumbs-get-parents (&optional point)
  (cond ((memq major-mode '(scss-mode less-css-mode css-mode))
         (cakecrumbs-scss-get-parents point))
        ((memq major-mode '(html-mode web-mode nxml-mode sgml-mode))
         (cakecrumbs-html-get-parents point))
        ((memq major-mode '(html-mode yajade-mode jade-mode pug-mode))
         (cakecrumbs-jade-get-parents point))))

;; ======================================================
;; SCSS / LESS
;; ======================================================
(defun cakecrumbs-scss-get-parents (&optional point)
  "Return a string list. Each string is the selectors at its level."
  (save-excursion
    (let* ((parent-pos-list (nth 9 (syntax-ppss point)))
           (parent-list (mapcar (lambda (pos)
                                  (goto-char pos)
                                  (let* ((to (point))
                                         (from (progn (re-search-backward "[,;{}^\n]" nil t)
                                                      (1+ (point)))))
                                    (string-trim (buffer-substring from to)))) parent-pos-list)))
      parent-list
      )))

;; ======================================================
;; HTML
;; ======================================================

(defun s () (interactive) (message "%s" (syntax-ppss)))

(defun cakecrumbs-html-get-parent (&optional point)
  "return list. (PARENT-TAG PARENT-POS IN-TAG-ITSELF).
string PARENT-TAG has been formatted as CSS/Jade/Pug-liked.
bool IN-TAG-ITSELF "
  (interactive)
  (save-excursion
    (if point (goto-char point))
    (save-match-data
      (let (back-until-tag-name tag-pos fin-selector)
        ;; Why not just `re-search-back' "<" then check if in paren via `syntax-ppss'?
        ;; `web-mode' redefined `syntax-table', which makes `syntax-ppss' unable to check if in <...> paren || in attr string.
        ;; I don't want to follow web-mode's rapidly development.
        (while (let* ((tag-pos (re-search-backward "< *\\(\\(?:.\\|\n\\)*?\\) *>" 0 t))
                      (raw-tag-body (if tag-pos (match-string-no-properties 1)))  ;; raw string in <(...)>
                      (is-comment (if tag-pos (string-prefix-p "!--" raw-tag-body)))
                      (slash (cond ((string-prefix-p "/" raw-tag-body) 'left)
                                   ((string-suffix-p "/" raw-tag-body) 'right)
                                   (t nil)))
                      (raw-classes (if tag-pos (cakecrumbs-string-match " class ?= ?[\"']\\(.+?\\)[\"']" 1 raw-tag-body)))
                      (formatted-classes (if raw-classes (concat "." (string-join (split-string raw-classes) "."))))
                      (raw-id (if tag-pos (cakecrumbs-string-match " id ?= ?[\"']\\(.+?\\)[\"']" 1 raw-tag-body)))
                      (formatted-id (if raw-id (concat "#" (string-trim raw-id)))))
                 (if tag-pos
                     (setq tag-name (cond ((eq slash 'left)  (cakecrumbs-string-match "^/ *\\([^>< ]+\\)" 1 raw-tag-body))
                                          ((eq slash 'right) (cakecrumbs-string-match "[^>< ]+ *$" 0 raw-tag-body))
                                          (t                 (cakecrumbs-string-match "^[^>< ]+" 0 raw-tag-body)))))
                 (cond (is-comment t)  ; t: continue
                       ((null tag-pos) nil) ; nil: terminate while loop
                       ((eq slash 'right) t) ; self-closing tag (<hr/>)
                       ((eq slash 'left) ; closing-tag (</title>)
                        (setq back-until-tag-name tag-name)
                        t)
                       (back-until-tag-name
                        (if (string-equal back-until-tag-name tag-name) ; if equal, set to nil && continue. else, continue.
                            (progn (setq back-until-tag-name nil) t))
                        t) ; always continue while loop
                       ((string-match "^\\(link\\|a\\|img\\)\\b" raw-tag-body) t) ; self-closing tag (<img/> or <img>)
                       (t ;; found parent!
                        (setq fin-selector
                              (if (equal tag-name "div")
                                  (if (or formatted-id formatted-classes)
                                      (concat formatted-id formatted-classes)
                                    "div")
                                (concat tag-name formatted-id formatted-classes)))
                        nil)
                       ))) ; WHILE
        (list fin-selector tag-pos "[unimplemented]")
        ))))


(defun cakecrumbs-html-get-parents (&optional point)
  (let ((fin '())
        (last-parent-pos (or point (point))))
    (while (let ((parent-obj (cakecrumbs-html-get-parent last-parent-pos)))
             (if (or (null (car parent-obj))
                     (null (nth 1 parent-obj)))
                 nil ; break
               (prog1 t ; continue
                 (push (car parent-obj) fin)
                 (setq last-parent-pos (nth 1 parent-obj))
                 ))))
    fin))

(defun h () (interactive) (message "%s" (cakecrumbs-html-get-parent)))
(defun hhh () (interactive) (message "%s" (cakecrumbs-html-get-parents)))

;; ======================================================
;; Jade / Pug
;; ======================================================

(defun cakecrumbs-invisible-line-p ()
  (string-match "^[ \t]*$" (cakecrumbs-current-line-string)))

(defun cakecrumbs-jade-get-parent (&optional point)
  ;; [TODO] li: sapn()
  "return value (parent-tag parent-tag-point in-tag-itself).
Find backward lines up to parent"
  (save-excursion
    (if point (goto-char point))
    (let* ((parent-indent-must-less-than (if (cakecrumbs-invisible-line-p)
                                             (prog1 (current-column) (forward-line -1))
                                           (current-indentation)))
           (last-indent parent-indent-must-less-than)
           (tag-name nil)
           (TAG-PATT "^ +\\([.#A-z0-9_-]+\\)")
           (in-parenthesis (nth 9 (syntax-ppss))))
      (while (progn
               (cond ((bobp) nil)  ; break
                     ((cakecrumbs-invisible-line-p) t)  ; continue
                     ((string-match "^[\ t]+|" (cakecrumbs-current-line-string)) t)  ; continue
                     (in-parenthesis
                      (goto-char (car in-parenthesis)) ;; goto beginning of current parenthesis
                      (setq tag-name (cakecrumbs-string-match TAG-PATT 1 (cakecrumbs-current-line-string)))
                      nil) ; break
                     ((> (current-column) (current-indentation))  ; (ex: | is cursor pos) ===>  span() |
                      (setq tag-name (cakecrumbs-string-match TAG-PATT 1 (cakecrumbs-current-line-string)))
                      nil) ; break
                     ((and (not in-parenthesis)
                           (>= (current-indentation) parent-indent-must-less-than)) ; absolutely not parent
                      t) ; continue
                     ((>= (current-indentation) last-indent) t) ; (absolutely not parent) continue
                     (t
                      (progn
                        (setq last-indent (min (current-indentation) last-indent))
                        (cond ((eq last-indent 0) ; root
                               nil) ; break
                              (t  ; a tag!
                               (setq tag-name (cakecrumbs-string-match TAG-PATT 1 (cakecrumbs-current-line-string)))
                               nil) ; break
                              )))))
        (forward-line -1)
        (back-to-indentation)
        (setq in-parenthesis (nth 9 (syntax-ppss))))
      (if tag-name
          (list tag-name
                (progn (back-to-indentation) (point))
                (if in-parenthesis t))
        nil
        ))
    ))

(defun cakecrumbs-jade-get-parents (&optional point)
  (save-excursion
    (if point (goto-char point))
    (let ((fin '())
          (pos point))
      (while (let ((parent (cakecrumbs-jade-get-parent pos)))
               (if parent
                   (prog1 t ; continue WHILE
                     (setq pos (nth 1 parent))
                     (push (car parent) fin))
                 nil))) ; break WHILE
      fin)))

(defun ppss () (interactive) (message "%s" (syntax-ppss)))

(defun j ()
  (interactive)
  (message (format "(%s),  %s" (point) (cakecrumbs-jade-get-parent))))

(defun jjj ()
  (interactive)
  (message"%s" (cakecrumbs-jade-get-parents)))

(defun ggg ()
  (interactive)
  (message"%s" (cakecrumbs-get-parents)))

;; (defun ttt ()
;;   (interactive)
;;   (save-excursion
;;     (message "%s" (parse-partial-sexp (point-min) (point-max) nil nil (syntax-ppss)))))

;; (defun sss ()
;;   (interactive)
;;   (message (format "POS ==> %s, CONTEXT ==> %s" (point) (save-excursion (sgml-get-context)))))

;; In nxml-mode,  `nxml-backward-up-element' to go up to parent.
;; In web-mode, C-c C-e u parent element (up) (`web-mode-element-parent-position' (point))



;; ======================================================
;; Idle Timer
;; ======================================================

;; (defun cakecrumbs-timer-handler (buffer)
;;   (when (buffer-live-p buffer)
;;     (with-current-buffer buffer
;;       (cakecrumbs-refresh)
;;       )))
;;
;; (add-hook 'cakecrumbs-mode-hook
;;           (lambda ()
;;             (setq cakecrumbs--refresh-timer
;;                   (run-with-idle-timer 0.8 t 'cakecrumbs-timer-handler (current-buffer)))))
;;
;; (add-hook 'kill-buffer-hook
;;           (lambda ()
;;             (when (timerp cakecrumbs--refresh-timer)
;;               (cancel-timer cakecrumbs--refresh-timer))))
;;

;; ======================================================
;; Minor Mode
;; ======================================================
(define-minor-mode cakecrumbs-mode
  "doc"
  :init-value nil
  :lighter " cakecrumbs"
  ;; :keymap cakecrumbs-mode-map
  :global nil
  (if cakecrumbs-mode
      (cakecrumbs-install-header)
    (cakecrumbs-uninstall-header)))

(provide 'cakecrumbs)
;;; cakecrumbs.el ends here
