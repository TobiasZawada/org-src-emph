;;; org-src-emph.el --- Emphasis in Org Babel Source blocks  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  DREWOR020

;; Author: DREWOR020 <toz@smtp.1und1.de>
;; Keywords: outlines, wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide Org source block header argument :emph.
;; The value of :emph is a list '(BEG END FACE) consisting of two regexps BEG and END
;; and a face specification FACE.
;;
;; BEG and END are the regular expressions matching the beginning and the end
;; of the region to be emphasized with FACE.
;;
;; This package is inspired by the following two emacs.stackexchange.com questions:
;; https://emacs.stackexchange.com/questions/63306/emphasize-text-snippets-in-source-blocks
;; https://emacs.stackexchange.com/questions/63643/noweb-references-in-sh-blocks-breaks-the-syntax-highlighting?noredirect=1#comment102374_63643

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'ob)
(require 'org-element)
(require 'ox-html)

(defvar org-src-emph-info nil)

(defun org-src-emph-info-ad (data _info)
  "Store the source block info  in `org-src-emph-info'.
This is a :before advice for function `org-export-data'.
DATA and _INFO are the same arguments as for `org-export-data'."
  (when (eq (org-element-type data) 'src-block)
    (let* ((beg (org-element-property :begin data))
	   (info (save-excursion
		   (goto-char beg)
		   (org-babel-exp--at-source
		       (org-babel-get-src-block-info 'light)))))
      (setq org-src-emph-info info))))

(advice-add 'org-export-data :before #'org-src-emph-info-ad)

(defun org-src-emph-strip (emphasizers)
  "Replace emphasis markers by text properties in current buffer.
The emphasis markers are described by the list EMPHASIZERS.
Each marker is a list (RE-BEGIN RE-END FACE).
Non-greedy matches for \\(?1:RE-BEGIN\\)\\(?2:.*\\)\\(?3:RE-END\\) are searched.
The groups 1 and 3 are removed and group 1 is propertised by
:org+emph (MATCH1 MATCH2 FACE)."
  (dolist (emph emphasizers)
    ;;;;;;;;;;
    ;; The following cases of specifications for emph are converted to (begin end face):
    ;; - (beginAndEnd . face),
    ;; - (beginAndEnd  face)
    (cond
     ((null (listp (cdr emph)))
      (setq emph (list (car emph) (car emph) (cdr emph))))
     ((eq (length emph) 2)
      (setq emph (list (car emph) (car emph) (cadr emph)))))
    ;;;;;;;;;;
    (goto-char (point-min))
    (cl-multiple-value-bind
	(re-begin re-end face) emph
      (unless (stringp re-end)
	(setq face re-end
	      re-end re-begin))
      (let ((re (format "\\(?1:%s\\)\\(?2:\\(?:.\\|\n\\)*?\\)\\(?3:%s\\)" re-begin re-end)))
	(while (re-search-forward re nil t)
	  (put-text-property (match-beginning 2) (match-end 2)
			     :org+emph (list (match-string 1) (match-string 3) face))
	  (replace-match "" nil nil nil 3)
	  (replace-match "" nil nil nil 1))
	))))

(cl-defun org-src-emph-restore (&key (face-prop 'face) emph-marks)
  "Restore emphasis from text properties in current buffer.
See `org-src-emph-strip' for the structure of
emphasis markers and text properties.
If FACE-PROP is non-nil put the face with that property on the marked text.
If EMPH-MARKS is non-nil insert the emphasis markers
at their original positions."
  (declare (special int))
  (goto-char (point-min))
  (cl-loop for int being the intervals property :org+emph
	   for start = (car int)
	   for end = (cdr int)
	   for prop = (get-text-property start :org+emph)
	   when prop
	   when face-prop
	   do
	   (font-lock-append-text-property start end face-prop (nth 2 prop))
	   and when emph-marks
	   do
	   (goto-char end) (insert (nth 1 prop))
	   (goto-char start) (insert (nth 0 prop))))

(defun org-src-emph-html-fontify-code (fun code lang)
  "Consider emphasis markers in source blocks.
This is an around advice for `org-html-fontify-code' as FUN.
CODE and LANG are passed to FUN.
But before, emphasis markers are replaced by text properties in CODE."
  (if-let ((args (nth 2 org-src-emph-info))
	   (emph-arg (alist-get :emph args))
	   ((null (string-equal emph-arg "'%emph")))
	   (emph-exprs (eval (read emph-arg))))
      (let ((org-src-mode-hook (cons 'org-src-emph-restore org-src-mode-hook))
	    (new-code (with-temp-buffer
			(insert code)
			(org-src-emph-strip emph-exprs)
			(buffer-string))))
	(funcall fun new-code lang))
    (funcall fun code lang)))

(advice-add 'org-html-fontify-code :around #'org-src-emph-html-fontify-code)

(defun org-src-emph-transfer-props (org-buffer start)
  "Transfer font lock properties from current buffer to ORG-BUFFER.
In the ORG-BUFFER the text is at START."
  ;; This code segment is adapted from `org-src-font-lock-fontify-block'.
  (cl-loop
   for int being the intervals
   for pos = (car int)
   for next = (cdr int)
   do
   (dolist (prop (cons 'face font-lock-extra-managed-props))
     (let ((new-prop (get-text-property pos prop)))
       (put-text-property
	(+ start pos) (+ start next) prop new-prop
	org-buffer)))))

(defun org-src-emph-fontify-block (fun lang start end)
  "Consider the :emph source block header argument.
This is an around advice for `org-src-font-lock-fontify-block' as FUN.
The args LANG START END are the same as for `org-src-font-lock-fontify-block'."
  (save-match-data
    (if-let* ((buf (current-buffer))
	      (info (org-babel-get-src-block-info 'light))
	      (body (buffer-substring (1+ start) end))
	      (args (nth 2 info))
	      (emph-arg (alist-get :emph args))
	      ((null (string-equal emph-arg "%emph")))
	      (emph-exprs (eval (read emph-arg))))
	(with-temp-buffer
	  (insert body)
	  (org-src-emph-strip emph-exprs)
	  (funcall fun lang (point-min) (point-max))
	  (org-src-emph-restore :emph-marks t)
	  (org-src-emph-transfer-props buf start)
	  )
      (funcall fun lang start end))))

(advice-add 'org-src-font-lock-fontify-block :around #'org-src-emph-fontify-block)

(defun org-src-emph-spec-to-string (arg-list)
  "Filter-args advice removing :emph for tangling.
ARG-LIST contains the spec list for `org-babel-spec-to-string'.
See `org-src-emph-strip' for the meaning of :emph."
  (let* ((spec (car arg-list))
	 (body (gv-ref (nth 5 spec)))
	 (params (nth 4 spec))
	 (emph (alist-get :emph params nil nil #'string-equal)))
    (with-temp-buffer
      (insert (gv-deref body))
      (org-src-emph-strip emph)
      (setf (gv-deref body) (buffer-string))))
  arg-list)

(advice-add 'org-babel-spec-to-string :filter-args #'org-src-emph-spec-to-string)

(defun org-src-emph-template-with-emph (_backend)
  "Temporarily add :emph to `org-babel-exp-code-template'."
  (make-local-variable 'org-babel-exp-code-template)
  (setq org-babel-exp-code-template
	(replace-regexp-in-string
	 "\\(#\\+BEGIN_SRC[^\n]*\\)\n"
	 "\\1 :emph '%emph\n"
	 org-babel-exp-code-template)))

(add-hook 'org-export-before-processing-hook #'org-src-emph-template-with-emph)

(provide 'org-src-emph)
;;; org-src-emph.el ends here
