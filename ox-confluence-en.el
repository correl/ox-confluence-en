;;; ox-confluence-en.el --- Enhanced Confluence Wiki Back-End for Org Export -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018, Correl Roush

;; Author: Correl Roush <correl@gmail.com>
;; URL: https://github.com/correl/ox-confluence-en
;; Version: 1.0
;; Keywords: outlines, confluence, wiki
;; Package-Requires: ((emacs "24") (org "8.2") (s "1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; ox-confluence.el lets you convert Org files to confluence wiki
;; markup using the ox.el export engine.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;	 (require 'ox-confluence-en)
;;
;; Export Org files to confluence:
;; M-x ox-confluence-en-export-as-confluence
;; or use the org-export-dispatch UI.
;;
;;; Code:
(require 'ox)
(require 'ox-confluence)
(require 's)

(defgroup ox-confluence-en nil
  "Options for exporting to Confluence"
  :tag "Org Confluence (Enhanced)"
  :group 'org-export)

(defcustom ox-confluence-en-use-plantuml-macro t
  "Embed PlantUML graphs using the PlantUML macro.

When exporting the results of a PlantUML, dot, or ditaa source
block, use the Confluence PlantUML macro to render the graph
rather than linking to the resulting image generated locally.

Requires the free confluence PlantUML plugin to be installed:
https://marketplace.atlassian.com/plugins/de.griffel.confluence.plugins.plant-uml"
  :group 'ox-confluence-en
  :type 'boolean)

(org-export-define-derived-backend 'confluence-en 'confluence
  :translate-alist '((headline . ox-confluence-en-headline)
                     (paragraph . ox-confluence-en-paragraph)
                     (src-block . ox-confluence-en-src-block)
                     (quote-block . ox-confluence-en-quote-block)
                     (special-block . ox-confluence-en-special-block)
                     (verbatim . ox-confluence-en-verbatim)
                     (code . ox-confluence-en-verbatim)
                     (fixed-width . ox-confluence-en-verbatim)
                     (table-cell . ox-confluence-en-table-cell)
                     (item . ox-confluence-en-item)
                     (timestamp . ox-confluence-en-timestamp)
                     (property-drawer . ox-confluence-en-property-drawer))
  :menu-entry
  '(?C "Export to Confluence"
       ((?C "As Wiki buffer"
	    (lambda (a s v b)
              (ox-confluence-en-export-as-confluence a s v b))))))

(defun ox-confluence-en-headline (headline contents info)
  (let* ((low-level-rank (org-export-low-level-p headline info))
	 (text (org-export-data (org-element-property :title headline)
				info))
	 (todo (org-export-data (org-element-property :todo-keyword headline)
				info))
	 (level (org-export-get-relative-level headline info))
	 (todo-text (if (or (not (plist-get info :with-todo-keywords))
			    (string= todo ""))
			""
                      (let* ((todo-type (org-element-property :todo-type headline))
                             (status-color (cond
                                            ((equal todo-type 'todo) "red")
                                            (t "green"))))
                        (format "%s " (ox-confluence-en--macro "status" nil `((color . ,status-color) (title . ,todo))))))))
    ;; Else: Standard headline.
    (format "h%s. %s%s\n%s" level todo-text text
            (if (org-string-nw-p contents) contents ""))))

(defun ox-confluence-en-paragraph (paragraph contents info)
  "Strip newlines from paragraphs.

Confluence will include any line breaks in the paragraph, rather
than treating it as reflowable whitespace."
  (replace-regexp-in-string "\n" " " contents))

(defun ox-confluence-en-src-block (src-block contents info)
  "Embed source block results using available macros.

Currently, PlantUML, Graphviz, and Ditaa graphs will be embedded
using the macros provided by the PlantUML plugin, if it is
available."
  (let ((lang (org-element-property :language src-block))
        (code (org-export-format-code-default src-block info))
        (caption (if (org-export-get-caption src-block)
                     (org-trim (org-export-data (org-export-get-caption src-block) info)))))
    (if (and ox-confluence-en-use-plantuml-macro
             (member lang '("plantuml" "dot" "ditaa")))
        (ox-confluence-en--macro "plantuml" code `((type . ,lang)))
      (ox-confluence-en--block lang "Emacs" caption code))))

(defun ox-confluence-en--block (language theme caption contents)
  (concat "\{code:theme=" theme
          (when language (format "|language=%s" language))
          (when caption (format "|title=%s" caption))
          "}\n"
          contents
          "\{code\}\n"))

(defun ox-confluence-en-quote-block (quote-block contents info)
  (ox-confluence-en--macro "quote" contents))

(defun ox-confluence-en-special-block (special-block contents info)
  (let ((block-type (downcase (org-element-property :type special-block))))
    (if (member block-type '("info" "note" "warning"))
        (ox-confluence-en--macro block-type contents)
      (org-ascii-special-block special-block contents info))))

(defun ox-confluence-en-verbatim (verbatim contents info)
  (format "{{%s}}"
          (org-trim (org-element-property :value verbatim))))

(defun ox-confluence-en--macro (name contents &optional arguments)
  (let ((open-tag (concat "\{" name
                          (when arguments
                            (concat ":"
                                    (mapconcat (lambda (pair) (format "%s=%s"
                                                                      (car pair)
                                                                      (cdr pair)))
                                               arguments
                                               "|")))
                          "}"))
        (close-tag (concat "{" name "}")))
    (if contents (concat open-tag "\n" contents "\n" close-tag)
      open-tag)))

;;;###autoload
(defun ox-confluence-en-export-as-confluence
    (&optional async subtreep visible-only body-only ext-plist)
  "Export the current org-mode buffer as Confluence wiki markup.

The markup is exported to a temporary buffer, which you can use
to copy the content to be pasted within Confluence."
  (interactive)
  (let ((org-babel-default-header-args:plantuml '((:exports . "code")))
        (org-babel-default-header-args:dot '((:exports . "code")))
        (org-babel-default-header-args:ditaa '((:exports . "code"))))
    (org-export-to-buffer 'confluence-en "*org CONFLUENCE Export*"
      async subtreep visible-only body-only ext-plist (lambda () (text-mode)))))

(defun ox-confluence-en-table-cell  (table-cell contents info)
  "Wrap table cell contents in whitespace.

Without the extra whitespace, cells will collapse together thanks
to confluence's table header syntax being multiple pipes."
  (let ((table-row (org-export-get-parent table-cell)))
    (concat
     (when (org-export-table-row-starts-header-p table-row info)
       "|")
     " " contents " |")))

(defun ox-confluence-en--checkbox (item info)
  "Return checkbox string for ITEM or nil.
INFO is a plist used as a communication channel."
  (cl-case (org-element-property :checkbox item)
    (on "☑ ")
    (off "☐ ")
    (trans "☒ ")))

(defun ox-confluence-en-item (item contents info)
  (let ((as-table (or (org-export-read-attribute :attr_confluence
                                                 (org-export-get-parent item)
                                                 :as-table))))
    (if as-table (ox-confluence-en--item-as-table item contents info)
      (ox-confluence-en--item-as-list item contents info))))

(defun ox-confluence-en--item-as-list (item contents info)
  (let ((list-type (org-element-property :type (org-export-get-parent item)))
        (checkbox (ox-confluence-en--checkbox item info))
        (depth (1+ (ox-confluence-en--li-depth item info))))
    (cl-case list-type
      (descriptive
       (concat (make-string depth ?-) " "
               (org-export-data (org-element-property :tag item) info) ": "
               (org-trim contents)))
      (ordered
       (concat (make-string depth ?#) " "
               (org-trim contents)))
      (t
       (concat (make-string depth ?-)
               " "
               (org-trim contents))))))

(defun ox-confluence-en--item-as-table (item contents info)
  (let* ((list-type (org-element-property :type (org-export-get-parent item)))
         (checkbox (ox-confluence-en--checkbox item info))
         (depth (ox-confluence-en--li-depth item info))
         (info (plist-put info :ox-confluence-en-nested (1+ depth))))
    (cl-case list-type
      (descriptive
       (concat (s-repeat depth "| | ")
               "|| " (org-export-data (org-element-property :tag item) info)
               " |" (org-export-data (org-trim contents) info)))
      (t
       (concat (s-repeat depth "| | ")
               "|" (org-export-data (org-trim contents) info) " |")))))

(defun ox-confluence-en--li-depth (item info)
  (let ((nested (plist-get info :ox-confluence-en-nested)))
    (message "Nested: %s" nested)
    (- (org-confluence--li-depth item)
       (or nested 0))))

(defun ox-confluence-en-timestamp (timestamp contents info)
  (s-replace-all
   '(("[" . "")
     ("]" . ""))
   (org-ascii-timestamp timestamp contents info)))

(defun ox-confluence-en-property-drawer (property-drawer contents info)
  (and (org-string-nw-p contents)
       (ox-confluence-en--macro "info" contents)))

(provide 'ox-confluence-en)
;;; ox-confluence-en.el ends here
