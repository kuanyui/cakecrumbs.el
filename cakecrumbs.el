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

(defvar-local cakecrumb--parents-list '())
(defvar-local cakecrumb-refresh-timer nil "Buffer-local timer.")

(defface cakecrumbs-separator
  '((((class color) (background light)) (:inherit font-lock-comment-face))
    (((class color) (background dark)) (:inherit font-lock-comment-face)))
  "Seperator between each level" :group 'cakecrumbs-faces)

(defface cakecrumbs-tag
  '((((class color) (background light)) (:inherit font-lock-type-face))
    (((class color) (background dark)) (:inherit font-lock-type-face)))
  "HTML tag" :group 'cakecrumbs-faces)

(defface cakecrumbs-id
  '((((class color) (background light)) (:inherit font-lock-keyword-face))
    (((class color) (background dark)) (:inherit font-lock-keyword-face)))
  "HTML #id" :group 'cakecrumbs-faces)

(defface cakecrumbs-class
  '((((class color) (background light)) (:inherit font-lock-type-face))
    (((class color) (background dark)) (:inherit font-lock-type-face)))
  "HTML .class" :group 'cakecrumbs-faces)

(defface cakecrumbs-pseudo
  '((((class color) (background light)) (:inherit font-lock-constant-face))
    (((class color) (background dark)) (:inherit font-lock-constant-face)))
  "CSS pseudo selector" :group 'cakecrumbs-faces)

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
(setq cs "span .col-md-3.col-xs-6 > #test-hello :not(:nth-child(42))")

(setq cakecrumbs-re-tag "^[^ .#:()]+")
(setq cakecrumbs-re-class "[.][-A-z0-9_]+")
(setq cakecrumbs-re-id "#[-A-z0-9_]+")
(setq cakecrumbs-re-pseudo ":[-A-z0-9_]+?")

(cakecrumbs-matched-positions-all cakecrumbs-re-tag cs 0)
(cakecrumbs-matched-positions-all cakecrumbs-re-id cs 0)
(cakecrumbs-matched-positions-all cakecrumbs-re-class cs 0)
(cakecrumbs-matched-positions-all cakecrumbs-re-pseudo cs 0)



(defun cakecrumbs-set-faces (level-str)
  "Input is single-level string"
  (let ((m (car (cakecrumbs-matched-positions-all cakecrumbs-re-tag ex)))) ; tag
    (if m (set-text-properties (car m) (cdr m) '(face cakecrumbs-tag) level-str)))
  (let ((m (car (cakecrumbs-matched-positions-all cakecrumbs-re-id ex)))) ; id
    (if m (set-text-properties (car m) (cdr m) '(face cakecrumbs-id) level-str)))
  (let ((m (cakecrumbs-matched-positions-all cakecrumbs-re-class ex))) ; class
    (if m (mapc (lambda (pair)
                  (set-text-properties (car pair) (cdr pair) '(face cakecrumbs-class) level-str))
                m)))
  (let ((m (cakecrumbs-matched-positions-all cakecrumbs-re-pseudo ex))) ; pseudo
    (if m (mapc (lambda (pair)
                  (set-text-properties (car pair) (cdr pair) '(face cakecrumbs-pseudo) level-str))
                m)))
  level-str)



(defun cakecrumbs-make-header ()
  ""
  (let* ((full-header (abbreviate-file-name buffer-file-name))
         (header (file-name-directory full-header))
         (drop-str "[...]"))
    (if (> (length full-header)
           (window-body-width))
        (if (> (length header)
               (window-body-width))
            (progn
              (concat (cakecrumbs-with-faces drop-str
                                             :background "blue"
                                             :weight 'bold
                                             )
                      (cakecrumbs-with-faces (substring header
                                                        (+ (- (length header)
                                                              (window-body-width))
                                                           (length drop-str))
                                                        (length header))
                                             ;; :background "red"
                                             :weight 'bold
                                             )))
          (concat (cakecrumbs-with-faces header
                                         ;; :background "red"
                                         :foreground "#8fb28f"
                                         :weight 'bold
                                         )))
      (concat (cakecrumbs-with-faces header
                                     ;; :background "green"
                                     ;; :foreground "black"
                                     :weight 'bold
                                     :foreground "#8fb28f"
                                     )
              (cakecrumbs-with-faces (file-name-nondirectory buffer-file-name)
                                     :weight 'bold
                                     ;; :background "red"
                                     )))))

(defun cakecrumbs-display-header ()
  (setq header-line-format
        '("" ;; invocation-name
          (:eval (if (buffer-file-name)
                     (cakecrumbs-make-header)
                   "%b")))))

(add-hook 'buffer-list-update-hook 'cakecrumbs-display-header)


;; ======================================================
;; header-line
;; ======================================================

( "" (:propertize which-func-current local-map
                  (keymap (mode-line keymap (mouse-3 . end-of-defun)
                                     (mouse-2 . #[nil "" [1 narrow-to-defun] 2 nil nil])
                                     (mouse-1 . beginning-of-defun)
                                     ))
                  face which-func mouse-face mode-line-highlight help-echo
                  "mouse-1: go to beginning
mouse-2: toggle rest visibility
mouse-3: go to end") "")


(setq header-line-format '(t "hello"))

(add-hook 'prog-mode-hook 'which-func-setup-header t t)
(add-hook 'html-mode-hook 'which-func-setup-header t t)


;; (format "point == %s , %s" (point) (parse-partial-sexp (point-min) (point-max) nil nil (syntax-ppss)))
(save-excursion (syntax-after (point))
                (skip-syntax-forward "^()" 3)
                (syntax-ppss))

(defun cakecrumbs-string-match (regexp num string)
  (save-match-data
    (if (string-match regexp string)
        (match-string num string)
      nil)))



;; ======================================================
;; SCSS / LESS
;; ======================================================
(defun cakecrumbs-scss-get-parents (&optional point)
  (save-excursion
    (let* ((parent-pos-list (nth 9 (syntax-ppss)))
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
(defun cakecrumbs-html-get-parent (&optional point)
  (interactive)
  (save-excursion
    (if point (goto-char point))
    (save-match-data
      (let (back-until-tag-name tag-name tag-pos)
        (while (let* ((tag-pos (re-search-backward "< *\\(\\(?:.\\|\n\\)*?\\) *>" 0 t))
                      (raw-tag (and tag-pos (match-string-no-properties 1)))
                      (slash (cond ((string-prefix-p "/" raw-tag) 'left)
                                   ((string-suffix-p "/" raw-tag) 'right)
                                   (t nil))))
                 (if tag-pos
                     (setq tag-name (cond ((eq slash 'left)  (cakecrumbs-string-match "^/ *\\([^>< ]+\\)" 1 raw-tag))
                                          ((eq slash 'right) (cakecrumbs-string-match "[^>< ]+ *$" 0 raw-tag))
                                          (t                 (cakecrumbs-string-match "^[^>< ]+" 0 raw-tag)))))
                 (cond ((null tag-pos) nil) ; nil: terminate while loop
                       (back-until-tag-name
                        (if (string-equal back-until-tag-name tag-name) ; if equal, set to nil && continue. else, continue.
                            (progn (setq back-until-tag-name nil) t))
                        t) ; always continue while loop
                       ((string-match "^\\(link\\|a\\|img\\)\\b" raw-tag) t) ; self-closing tag (<img/> or <img>)
                       ((and slash (eq slash 'right)) t) ; self-closing tag (<hr/>)
                       ((and slash (eq slash 'left) raw-tag) ; closing-tag (</title>)
                        (setq back-until-tag-name tag-name)
                        t)
                       (t nil)  ;; found parent!
                       ))) ; WHILE
        (message (format "%s -- %s" (point) tag-name)))
      )))

(defun cakecrumbs-html-get-parents (&optional point)
  (let ((fin '())
        (last-parent-pos (or point (point))))
    (while (let ((parent (funcall func-to-get-parent last-parent-pos)))
             (if (null parent)
                 nil ; break
               (prog1 t ; continue
                 (push parent fin)
                 ))))
    fin))

;; ======================================================
;; Jade / Pug
;; ======================================================
(defun cakecrumbs-jade-get-parent (&optional point)
  ;; [TODO] li: span()
  "return value (parent-tag-name parent-tag-point in-tag-itself)"
  (save-excursion
    (if point (goto-char point))
    (let ((last-indent (current-indentation))
          (parent-indent-must-less-than (current-indentation))  ; unless in-parenthesis is non-nil
          (tag-name nil)
          (TAG-PATT "^ +\\([.#A-z0-9_-]+\\)")
          (in-parenthesis (nth 9 (syntax-ppss))))
      (while (progn
               ;; (back-to-indentation)
               (cond (in-parenthesis
                      (goto-char (car in-parenthesis)) ;; goto beginning of current parenthesis
                      (setq tag-name (cakecrumbs-string-match TAG-PATT 1 (thing-at-point 'line t)))
                      nil) ; break
                     ((> (current-column) (current-indentation))  ; (ex: | is cursor pos) ===>  span() |
                      (setq tag-name (cakecrumbs-string-match TAG-PATT 1 (thing-at-point 'line t)))
                      nil) ; break
                     ((and (not in-parenthesis)
                           (>= (current-indentation) parent-indent-must-less-than)) ; absolutely not parent
                      (forward-line -1)
                      t) ; continue
                     ((>= (current-indentation) last-indent) ; absolutely not parent
                      (forward-line -1)
                      t) ; continue
                     (t
                      (progn
                        (setq last-indent (min (current-indentation) last-indent))
                        (cond ((eq last-indent 0) ; root
                               nil) ; break
                              (t  ; a tag!
                               (setq tag-name (cakecrumbs-string-match TAG-PATT 1 (thing-at-point 'line t)))
                               nil) ; break
                              ))))))
      (if tag-name
          (list tag-name
                (progn (back-to-indentation) (point))
                (if in-parenthesis t))
        nil
        ))
    ))

(defun jjj ()
  (interactive)
  (message (format "(point) ==> %s,  %s" (point) (cakecrumbs-jade-get-parent))))

(defun ttt ()
  (interactive)
  (save-excursion
    (message "%s" (parse-partial-sexp (point-min) (point-max) nil nil (syntax-ppss)))))

(defun sss ()
  (interactive)
  (message (format "POS ==> %s, CONTEXT ==> %s" (point) (save-excursion (sgml-get-context)))))

;; In nxml-mode,  `nxml-backward-up-element' to go up to parent.
;; In web-mode, C-c C-e u parent element (up) (`web-mode-element-parent-position' (point))



;; ======================================================
;; Idle Timer
;; ======================================================

(defun cakecrumbs-timer-handler (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cakecrumbs-refresh)
      )))

(add-hook 'cakecrumbs-mode-hook
          (lambda ()
            (setq cakecrumbs-refresh-timer
                  (run-with-idle-timer 0.8 t 'cakecrumbs-timer-handler (current-buffer)))))

(add-hook 'kill-buffer-hook
          (lambda ()
            (when (timerp cakecrumbs-refresh-timer)
              (cancel-timer cakecrumbs-refresh-timer))))



(provide 'cakecrumbs)
;;; cakecrumbs.el ends here
