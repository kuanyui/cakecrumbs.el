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

(setq cakecrumbs-jade-major-modes   '(yajade-mode jade-mode pug-mode))
(setq cakecrumbs-html-major-modes   '(html-mode web-mode nxml-mode sgml-mode))
(setq cakecrumbs-scss-major-modes   '(scss-mode less-css-mode css-mode))
(setq cakecrumbs-stylus-major-modes '(stylus-mode sass-mode))


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



(defun cakecrumbs-show-full ()
  (interactive)
  (let ((parents (cakecrumbs-get-parents)))
    (if (null parents)
        (message (propertize "[Cakecrumbs] Not in a supported area!" 'face 'cakecrumbs-ellipsis))
      (message (cakecrumbs-format-parents parents)))))

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
  "return string list, containing parents."
  (cond ((memq major-mode cakecrumbs-scss-major-modes)
         (cakecrumbs-scss-get-parents point))
        ((memq major-mode cakecrumbs-html-major-modes)
         (cakecrumbs-html-get-parents point))
        ((memq major-mode cakecrumbs-jade-major-modes)
         (cakecrumbs-jade-get-parents point))))

(defun cakecrumbs-get-parent ()
  "return a list: (PARENT-SELECTOR PARENT-POS IN-TAG-ITSELF)"
  (cond ((memq major-mode cakecrumbs-scss-major-modes)
         (cakecrumbs-scss-get-parent point))
        ((memq major-mode cakecrumbs-html-major-modes)
         (cakecrumbs-html-get-parent point))
        ((memq major-mode cakecrumbs-jade-major-modes)
         (cakecrumbs-jade-get-parent point))))

;; ======================================================
;; SCSS / LESS
;; ======================================================
(defun cakecrumbs-scss-extract-selector-from-pos (&optional pos)
  "Search backward. Use with `cakecrumbs-scss-get-parent'"
  (save-excursion
    (if pos (goto-char pos))
    (let* ((to pos)
           (from (progn (re-search-backward "[,;{}^\n]" nil t)
                        (1+ (point)))))
      (string-trim (buffer-substring from to)))))

(defun cakecrumbs-scss-get-parents (&optional point)
  "Return a string list. Each string is the selectors at its level."
  (save-excursion
    (let* ((parent-pos-list (nth 9 (syntax-ppss point))))
      (mapcar #'cakecrumbs-scss-extract-selector-from-pos parent-pos-list))))

(defun cakecrumbs-scss-get-parent (&optional from-pos)
  "return a list if found parent: (PARENT-SELECTOR PARENT-POS IN-TAG-ITSELF);
otherwise, nil.
IN-TAG-ITSELF is always nil."
  (let ((left-paren-pos (car (last (nth 9 (syntax-ppss from-pos))))))
    (if left-paren-pos
        (list
         (cakecrumbs-scss-extract-selector-from-pos left-paren-pos)
         left-paren-pos
         nil))))

(defun c () (interactive) (message "%s" (cakecrumbs-scss-get-parent)))

;; ======================================================
;; HTML
;; ======================================================

(defun s () (interactive) (message "%s" (syntax-ppss)))

(defun cakecrumbs-cursor-within-string (&optional pos)
  (nth 3 (syntax-ppss pos)))

(defun cakecrumbs-html-search-backward-< (&optional pos)
  "return the position of backwardly nearest syntactic < (not in
string && not in comment) from POS. If not found, return nil

`syntax-ppss' cannot detect comment in web-mode, so use such way."
  ;; Don't just check wether in paren via `syntax-ppss' because
  ;; `web-mode' redefined `syntax-table', which makes `syntax-ppss'
  ;; unable to check if in <...> paren || in attr string.
  (if (memq major-mode cakecrumbs-html-major-modes)
      (save-excursion
        (if pos (goto-char pos))
        (let ((fin nil))
          (while (progn
                   (setq fin (re-search-backward "<"  nil :no-error))
                   (cond ((null fin) nil)
                         ((eq (point-min) (point)) nil) ; break
                         ((not (memq major-mode cakecrumbs-html-major-modes)) nil)  ; break. Is this condition possible in mmm-mode?
                         ((cakecrumbs-cursor-within-string) t) ; continue
                         ((equal (buffer-substring-no-properties (point) (+ 4 (point))) "<!--") t) ; continue
                         (t nil))) ; found
            (setq fin nil))
          fin))))

(defun cakecrumbs-html-search-forward-> (&optional pos)
  "return the position of forwardly nearest syntactic > (not in
string && not in comment) from POS. If not found, return nil"
  (if (memq major-mode cakecrumbs-html-major-modes)
      (save-excursion
        (if pos (goto-char pos))
        (let ((fin nil))
          (while (progn
                   (setq fin (re-search-forward ">"  nil :no-error))
                   (cond ((null fin) nil)
                         ((eq (point-max) (point)) nil) ; break
                         ((not (memq major-mode cakecrumbs-html-major-modes)) nil)  ; break. Is this condition possible in mmm-mode?
                         ((cakecrumbs-cursor-within-string) t) ; continue
                         ((equal (buffer-substring-no-properties (- (point) 3) (point)) "-->") t) ; continue
                         (t nil))) ; found
            (setq fin nil))
          fin))))

(defun cakecrumbs-html-search-nearest-tag (&optional pos)
  "Get position of the nearest tag from POS (or `point' when POS is nil).
Always search backwardly, and comment tag never involved.

If not found (no valid HTML tag existed before POS), returns nil;
else, returns a list with following elements:

0. int,    the position of the found tag begins from.
1. int,    the position of the found tag ends at.
2: bool,   if current point is within this HTML tag, t.
3. symbol, type: `self-closing-tag', `start-tag', `end-tag'
4. string, tag name.
5. string, id name.
6. list,   class nqames.

"
  (let* ((begin (cakecrumbs-html-search-backward-< pos))
         (end (if begin (cakecrumbs-html-search-forward-> begin)))
         (in-tag (if end (eq end (cakecrumbs-html-search-forward-> pos)))))
    (if (or (null begin) (null end))
        nil
      (let* ((raw (replace-regexp-in-string "\\(?:\n\\| \\|\t\\)+" " " (buffer-substring-no-properties (1+ begin) (1- end))))
             (tag-role (cond ((string-match-p "^ */" raw) 'end-tag)
                             ((string-match-p "/ *$" raw) 'self-closing-tag)
                             (t 'start-tag)))
             (tag-name (if (eq tag-role 'end-tag)
                           (cakecrumbs-string-match "\\([A-z0-9_-]+\\)$" 1 raw)
                         (cakecrumbs-string-match "^\\([A-z0-9_-]+\\)" 1 raw)))
             (tag-id (if (memq tag-role '(self-closing-tag start-tag))
                         (cakecrumbs-string-match "[ \"']id ?= ?['\"]\\([A-z0-9 _-]+\\)['\"]" 1 raw)))
             (tag-classes (if (memq tag-role '(self-closing-tag start-tag))
                              (cakecrumbs-string-match "[ \"']class ?= ?['\"]\\([A-z0-9 _-]+\\)['\"]" 1 raw))))
        (if (and (eq tag-role 'start-tag)
                 (equal tag-name '("img" "link")))
            (setq tag-role 'self-closing-tag))
        (if tag-id (setq tag-id (string-trim tag-id)))
        (if tag-classes (setq tag-classes (split-string (string-trim tag-classes) " +")))
        (list begin end in-tag tag-role tag-name tag-id tag-classes)))))


(defun cakecrumbs-html-get-parent (&optional from-pos)
  "return list. (PARENT-SELECTOR PARENT-POS IN-TAG-ITSELF).
string PARENT-TAG has been formatted as CSS/Jade/Pug-liked.
bool IN-TAG-ITSELF "
  (save-excursion
    (let* ((pos (or from-pos (point)))
           (m (cakecrumbs-html-search-nearest-tag from-pos))
           (m-pos (nth 0 m))
           (init-in-paren (nth 2 m))
           (m-tag-role (nth 3 m))
           (m-tag-name (nth 4 m))
           (stack '())
           )
      (while (cond ((null m) nil) ; break
                   (init-in-paren nil) ; break (the tag currently within is just parent)
                   ((and (null stack) (eq m-tag-role 'start-tag)) nil) ; break (found parent)
                   (t t)) ; continue
        ;; WHILE BODY
        ;; stack manipulate
        (cond ((eq m-tag-role 'start-tag)
               (if (equal (car stack) m-tag-name)
                   (pop stack)))
              ((eq m-tag-role 'end-tag)
               (push m-tag-name stack))
              (t nil)) ; ignore
        (setq m (cakecrumbs-html-search-nearest-tag m-pos))
        (setq m-pos (nth 0 m))
        (setq m-tag-role (nth 3 m))
        (setq m-tag-name (nth 4 m)))
      (if m
          (let* ((-id (nth 5 m))
                 (id (if -id (concat "#" -id)))
                 (-kls (nth 6 m))
                 (kls (if -kls (mapconcat (lambda (s) (concat "." s)) -kls "")))
                 (-name (nth 4 m))
                 (name (if (and kls (equal -name "div"))
                           ""
                         -name)))
            (list
             (concat name id kls)
             (nth 0 m)
             (nth 2 m)))))))


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


(defun a () (interactive) (re-search-backward "< *\\(\\(?:.\\|\n\\)*?\\) *>" 0 t))
(defun h () (interactive) (message "%s, %s" (point) (cakecrumbs-html-search-nearest-tag)))
(defun hh () (interactive) (message "%s" (cakecrumbs-html-get-parent)))
(defun hhh () (interactive) (message "%s" (cakecrumbs-html-get-parents)))

;; ======================================================
;; Jade / Pug
;; ======================================================


(defun cakecrumbs-invisible-line-p ()
  (string-match-p "^[ \t]*$" (cakecrumbs-current-line-string)))

(setq cakecrumbs-jade-invalid-tag-pattern
      ;; In fact, I tested tag `if-xxx', `each-xxx' with pug/jade compiler, but they will cause error..
      (concat "^[ \t]*"
              (regexp-opt
               '("if" "else" "for" "in" "each" "case" "when" "default" "block" "extends" "var"
                 "append" "prepend" "include" "yield" "mixin"))))

(defun cakecrumbs-jade-comment-line-p ()
  (string-match-p "^[ \t]*//" (cakecrumbs-current-line-string)))

(defun cakecrumbs-jade-search-nearest-nested-tag-in-current-line (&optional pos)
  "Get the nearest nested-tag from POS (or `point' when POS is nil) within current line.
return a list: (ELEM-SELECTOR TAG-POS INIT-IN-PAREN)

<ex> the `span' and `i' is nested-tag, `a' is plain-tag
    a(href='/:8080'): span(): i hello

If current line is comment, return nil. Always search backwardly."
  (save-excursion
    (if pos (goto-char pos))
    (if (cakecrumbs-jade-comment-line-p)
        nil
      (let* ((bol-pos (save-excursion (beginning-of-line) (point)))
             (eol-pos (save-excursion (end-of-line) (point)))
             (m nil)
             (ppss (syntax-ppss))
             (init-in-paren (not (eq ppss 0))))
        (re-search-forward "[( ]" eol-pos :no-error)
        (setq m (re-search-backward ": +\\([#.A-z0-9_-]+\\)" bol-pos :no-error))
        (while (cond ((null m) nil) ; break (not found)
                     ((nth 3 ppss) t) ; continue (cursor in string)
                     ((not (zerop (nth 0 ppss))) t) ; continue (cursor in parenthesis)
                     (t nil)  ; break (found)
                     )
          (setq m (re-search-backward ": +\\([#.A-z0-9_-]+\\)" bol-pos :no-error))
          (setq ppss (syntax-ppss)))
        (if m
            (list (match-string 1)
                  (point)
                  init-in-paren))))))

(defun cakecrumbs-jade-search-nearest-plain-tag (&optional pos)
  "Get position of the nearest plain-tag from POS (or `point' when POS is nil).
return a list: (ELEM-SELECTOR TAG-POS INIT-IN-PAREN)

Always search backwardly, and comment tag never involved."
  (save-excursion
    (if pos (goto-char pos))
    (let* ((ppss (syntax-ppss))
           (init-in-parenthesis-of-parent (nth 9 ppss))
           (init-cursor-column (current-column))
           (init-indentation (if (cakecrumbs-invisible-line-p)  ; parent's indentation must less than this
                                 (prog1 (current-column) (forward-line -1))
                               (progn (current-indentation))))
           (in-parenthesis init-in-parenthesis-of-parent)
           (TAG-PATT "^ *\\([.#A-z0-9_-]+\\)")
           (m (progn (if (> init-cursor-column init-indentation)
                         (end-of-line))
                     (re-search-backward TAG-PATT nil :no-error))))
      (while (cond ((null m) nil) ; break (not found)
                   ((not (zerop (nth 0 ppss))) t) ; continue (cursor in parenthesis)
                   (t nil)  ; break (found)
                   )
        (setq m (re-search-backward TAG-PATT nil :no-error))
        (setq ppss (syntax-ppss)))
      (if m
          (list (match-string 1)
                (progn (back-to-indentation) (point))
                init-in-parenthesis-of-parent
                )))))

(defun cakecrumbs-jade-get-parent (&optional point)
  ;; [TODO] li: sapn()
  "return value (PARENT-SELECTOR PARENT-POS IN-TAG-ITSELF).
Find backward lines up to parent"
  (save-excursion
    (if point (goto-char point))
    (let* ((init-in-parenthesis-of-parent (nth 9 (syntax-ppss)))
           (init-cursor-column (current-column))
           (init-indentation (if (cakecrumbs-invisible-line-p)  ; parent's indentation must less than this
                                 (prog1 (current-column) (forward-line -1))
                               (progn (current-indentation))))
           (in-parenthesis init-in-parenthesis-of-parent)
           (in-string nil)
           (found-parent nil)
           (TAG-PATT "^ *\\([.#A-z0-9_-]+\\)"))
      (if (or (and (eq 0 init-cursor-column) (eq 0 init-indentation))
              (and (eq 0 init-indentation) (not init-in-parenthesis-of-parent)))
          nil
        (progn (while (cond ((bobp) nil)  ; break
                            ((cakecrumbs-invisible-line-p) t)  ; continue
                            ;; current
                            ((>= (current-indentation) init-indentation)
                             (if (not in-parenthesis) t  ; continue (absolutly not parent)
                               (progn ; in-parenthesis
                                 (goto-char (car in-parenthesis)) ;; goto beginning of current parenthesis
                                 (back-to-indentation)
                                 (if (>= (current-indentation) init-indentation) t ; continue (absolutly not parent)
                                   (prog1 nil (setq found-parent t)))))) ; parent found! break
                            ((string-match "^[ \t]*\\(|\\|-\\|//\\)" (cakecrumbs-current-line-string)) t)  ; continue
                            ((string-match cakecrumbs-jade-invalid-tag-pattern (cakecrumbs-current-line-string)) t)  ; continue
                            (t
                             (setq found-parent t)
                             nil) ; break
                            )  ; WHILE TEST ends here
                 ;; WHILE BODY
                 (setq init-in-parenthesis-of-parent nil)
                 (let ((ppss (syntax-ppss)))
                   (setq in-string (nth 3 ppss))
                   (setq in-parenthesis (nth 9 ppss)))
                 (if found-parent
                     (list (cakecrumbs-string-match TAG-PATT 1 (cakecrumbs-current-line-string))
                           (progn (back-to-indentation) (point))
                           (if init-in-parenthesis-of-parent t))
                   nil
                   )))))))

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
  (message (format "(%s),  %s" (point) (cakecrumbs-jade-search-nearest-nested-tag-in-current-line))))

(defun jj ()
  (interactive)
  (message (format "(%s),  %s" (point) (cakecrumbs-jade-search-nearest-plain-tag))))

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

(defalias 'cakecrumbs 'cakecrumbs-mode)

(provide 'cakecrumbs)
;;; cakecrumbs.el ends here
