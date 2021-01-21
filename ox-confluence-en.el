;;; ox-confluence-en.el --- Enhanced Confluence Wiki Back-End for Org Export -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018, Correl Roush

;; Author: Correl Roush <correl@gmail.com>
;; URL: https://github.com/correl/ox-confluence-en
;; Version: 1.0
;; Keywords: outlines, confluence, wiki
;; Package-Requires: ((emacs "24") (org "9.1") (s "1.12"))

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
  "Transcode a HEADLINE element from Org to Confluence wiki markup.

CONTENTS holds the contents of the element. INFO is a plist
holding contextual information."
  (let* ((text (org-export-data (org-element-property :title headline) info))
         (todo (org-export-data (org-element-property :todo-keyword headline) info))
         (custom-id (org-export-data (org-element-property :CUSTOM_ID headline) info))
         (level (org-export-get-relative-level headline info))
         (anchor (if (string= custom-id "") ""
                   (format "{anchor:%s}\n" custom-id)))
         (todo-text (if (or (not (plist-get info :with-todo-keywords))
                            (string= todo ""))
                        ""
                      (let* ((todo-type (org-element-property :todo-type headline))
                             (status-color (cond
                                            ((equal todo-type 'todo) "red")
                                            (t "green"))))
                        (format "%s " (ox-confluence-en--macro "status" nil `((color . ,status-color) (title . ,todo))))))))
    ;; Else: Standard headline.
    (format "%sh%s. %s%s\n%s" anchor level todo-text text
            (if (org-string-nw-p contents) contents ""))))

(defun ox-confluence-en-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Confluence wiki markup.

Unlike `org-confluence-table-cell', newlines are stripped from
PARAGRAPH. This is because Confluence will include any line
breaks in the paragraph, rather than treating it as reflowable
whitespace.

CONTENTS holds the contents of the element. INFO is a plist
holding contextual information."
  (replace-regexp-in-string "\n" " " contents))

(defun ox-confluence-en-src-block (src-block contents info)
  "Embed SRC-BLOCK results using available macros.

Currently, PlantUML, Graphviz, and Ditaa graphs will be embedded
using the macros provided by the PlantUML plugin, if it is
available.

CONTENTS holds the contents of the element. INFO is a plist
holding contextual information."
  (let ((lang (org-element-property :language src-block))
        (code (org-export-format-code-default src-block info))
        (caption (if (org-export-get-caption src-block)
                     (org-trim (org-export-data (org-export-get-caption src-block) info)))))
    (if (and ox-confluence-en-use-plantuml-macro
             (member lang '("plantuml" "dot" "ditaa")))
        (ox-confluence-en--macro "plantuml" code `((type . ,lang)))
      (ox-confluence-en--code-block lang code caption))))

(defun ox-confluence-en--macro (name contents &optional arguments)
  "Build a Confluence wiki macro block.

Inserts CONTENTS into a macro NAME. ARGUMENTS may be provided as
an alist."
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

(defun ox-confluence-en--code-block (language contents &optional caption theme)
  "Build code macro block wrapping CONTENTS as LANGUAGE.

A CAPTION may be provided to be used as a title to the code
block. If no THEME is provided, code will be highlighted using
Confluence's Emacs theme."
  (let ((arguments `((language . ,language)
                     (theme . ,(or theme "Emacs")))))
    (when caption (push `(title . ,caption) arguments))
    (ox-confluence-en--macro "code" contents arguments)))

(defun ox-confluence-en-quote-block (quote-block contents info)
  "Transcode QUOTE-BLOCK element from Org to Confluence wiki markup.

CONTENTS holds the contents of the element. INFO is a plist
holding contextual information."
  (ox-confluence-en--macro "quote" contents))

(defun ox-confluence-en-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Confluence wiki markup.

If the block type is info, note, or warning, it will be
transcoded using the matching Confluence wiki macro.

CONTENTS holds the contents of the element. INFO is a plist
holding contextual information."
  (let ((block-type (downcase (org-element-property :type special-block))))
    (cond ((string-equal block-type "info")
           (ox-confluence-en--macro "info" contents))
          ((member block-type '("note" "notes"))
           (ox-confluence-en--macro "note" contents))
          ((string-equal block-type "warning")
           (ox-confluence-en--macro "warning" contents))
          (t (org-ascii-special-block special-block contents info)))))

(defun ox-confluence-en-verbatim (verbatim contents info)
  "Transcode a VERBATIM element from Org to Confluence wiki markup.

CONTENTS holds the contents of the element. INFO is a plist
holding contextual information."
  (format "{{%s}}"
          (org-trim (org-element-property :value verbatim))))

;;;###autoload
(defun ox-confluence-en-export-as-confluence
    (&optional async subtreep visible-only body-only ext-plist)
  "Export the current buffer as Confluence wiki markup.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title and
table of contents from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org Confluence Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (let ((org-babel-default-header-args:plantuml '((:exports . "code")))
        (org-babel-default-header-args:dot '((:exports . "code")))
        (org-babel-default-header-args:ditaa '((:exports . "code"))))
    (org-export-to-buffer 'confluence-en "*Org Confluence Export*"
      async subtreep visible-only body-only ext-plist (lambda () (text-mode)))))

(defun ox-confluence-en-table-cell  (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to Confluence wiki markup.

Unlike `org-confluence-table-cell', table cell contents are
wrapped in whitespace.

Without the extra whitespace, cells would collapse together
thanks to confluence's table header syntax being multiple pipes.

CONTENTS holds the contents of the element. INFO is a plist
holding contextual information."
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
  "Transcode a list ITEM element from Org to Confluence wiki markup.

If the `:as-table' attribute is non-nil (set via
`#+ATTR_CONFLUENCE' on the list), the list item will be
transcoded as a table row.

CONTENTS holds the contents of the element. INFO is a plist
holding contextual information."
  (let ((as-table (or (org-export-read-attribute :attr_confluence
                                                 (org-export-get-parent item)
                                                 :as-table))))
    (if as-table (ox-confluence-en--item-as-table item contents info)
      (ox-confluence-en--item-as-list item contents info))))

(defun ox-confluence-en--item-as-list (item contents info)
  "Transcode a list ITEM element as a bulleted list item.

CONTENTS holds the contents of the element. INFO is a plist
holding contextual information."
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
  "Transcode a list ITEM as a table row.

CONTENTS holds the contents of the element. INFO is a plist
holding contextual information."
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
  "Return the depth of a list ITEM.

Used while building lists as tables, where the nested depth is
tracked as a property in INFO."
  (let ((nested (plist-get info :ox-confluence-en-nested)))
    (max 0 (- (org-confluence--li-depth item)
              (or nested 0)))))

(defun ox-confluence-en-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP from Org to Confluence wiki markup.

CONTENTS holds the contents of the element. INFO is a plist
holding contextual information."
  (s-replace-all
   '(("[" . "")
     ("]" . ""))
   (org-ascii-timestamp timestamp contents info)))

(defun ox-confluence-en-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER from Org to Confluence wiki markup.

The drawer's CONTENTS will be wrapped in an info macro.

CONTENTS holds the contents of the element. INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
       (ox-confluence-en--macro "info" contents)))

(provide 'ox-confluence-en)
;;; ox-confluence-en.el ends here
