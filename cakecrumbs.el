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

(which-function-mode)
(setq which-func-format
      '( "" (:propertize which-func-current local-map
                         (keymap (mode-line keymap (mouse-3 . end-of-defun)
                                            (mouse-2 . #[nil "" [1 narrow-to-defun] 2 nil nil])
                                            (mouse-1 . beginning-of-defun)
                                            ))
                         face which-func mouse-face mode-line-highlight help-echo
                         "mouse-1: go to beginning
mouse-2: toggle rest visibility
mouse-3: go to end") ""))

(delete (assoc 'which-func-mode mode-line-misc-info) mode-line-misc-info)
(setq which-func-header-line-format '(which-func-mode ("" which-func-format)))

(defun which-func-setup-header ()
  (interactive)
  (when which-func-mode
    (delete (assoc 'which-func-mode mode-line-misc-info) mode-line-misc-info)
    (setq header-line-format which-func-header-line-format)))

(setq which-func-functions '(which-func-scss-fn))

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



(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun sl/make-header ()
  ""
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (sl/header (file-name-directory sl/full-header))
         (sl/drop-str "[...]"))
    (if (> (length sl/full-header)
           (window-body-width))
        (if (> (length sl/header)
               (window-body-width))
            (progn
              (concat (with-face sl/drop-str
                                 :background "blue"
                                 :weight 'bold
                                 )
                      (with-face (substring sl/header
                                            (+ (- (length sl/header)
                                                  (window-body-width))
                                               (length sl/drop-str))
                                            (length sl/header))
                                 ;; :background "red"
                                 :weight 'bold
                                 )))
          (concat (with-face sl/header
                             ;; :background "red"
                             :foreground "#8fb28f"
                             :weight 'bold
                             )))
      (concat (with-face sl/header
                         ;; :background "green"
                         ;; :foreground "black"
                         :weight 'bold
                         :foreground "#8fb28f"
                         )
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold
                         ;; :background "red"
                         )))))

(defun sl/display-header ()
  (setq header-line-format
        '("" ;; invocation-name
          (:eval (if (buffer-file-name)
                     (sl/make-header)
                   "%b")))))



(add-hook 'buffer-list-update-hook
          'sl/display-header)

(provide 'cakecrumbs)
;;; cakecrumbs.el ends here
