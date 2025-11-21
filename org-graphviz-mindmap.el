;;; org-graphviz-mindmap.el --- Generate Graphviz mind maps from Org mode files -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Nowislewis
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1") (org "9.0"))
;; Keywords: outlines, hypermedia, org, graphviz
;; URL: https://github.com/yourusername/org-graphviz-mindmap

;;; Commentary:

;; This package generates Graphviz mind maps from Org mode files.
;; Features:
;; - Parse multi-level headings and generate hierarchical mind maps
;; - Support org-id links with dashed arrows
;; - Same-level headings have same height and color
;; - Top-to-bottom layout with level-based colors
;; - Export to PNG/SVG/PDF formats

;;; Code:

(require 'org)
(require 'org-element)

(defgroup org-graphviz-mindmap nil
  "Generate Graphviz mind maps from Org mode files."
  :group 'org
  :prefix "org-graphviz-mindmap-")

(defcustom org-graphviz-mindmap-output-format "svg"
  "Default output format for mind map images."
  :type '(choice (const "png") (const "svg") (const "pdf"))
  :group 'org-graphviz-mindmap)

(defcustom org-graphviz-mindmap-include-todo-keywords t
  "Whether to include TODO keywords in node labels."
  :type 'boolean
  :group 'org-graphviz-mindmap)

(defcustom org-graphviz-mindmap-level-colors
  '("#E3F2FD" "#BBDEFB" "#90CAF9" "#64B5F6" "#42A5F5" "#2196F3")
  "Colors for different heading levels (same level = same color)."
  :type '(repeat color)
  :group 'org-graphviz-mindmap)

(defcustom org-graphviz-mindmap-todo-colors
  '(("TODO" . "#D32F2F")
    ("DOING" . "#F57C00")
    ("DONE" . "#388E3C")
    ("CANCELLED" . "#757575")
    ("WAITING" . "#F9A825"))
  "Colors for TODO keywords in labels.
Each element is a cons cell (KEYWORD . COLOR).
The color is applied only to the TODO keyword text, not the entire node."
  :type '(alist :key-type string :value-type color)
  :group 'org-graphviz-mindmap)

(defcustom org-graphviz-mindmap-todo-bold t
  "Whether to make TODO keywords bold in labels."
  :type 'boolean
  :group 'org-graphviz-mindmap)

;;; Internal state

(defvar org-graphviz-mindmap--id-to-node (make-hash-table :test 'equal)
  "Map org-id to node identifier.")

(defvar org-graphviz-mindmap--node-counter 0
  "Counter for node identifiers.")

;;; Core functions

(defun org-graphviz-mindmap--reset ()
  "Reset internal state."
  (clrhash org-graphviz-mindmap--id-to-node)
  (setq org-graphviz-mindmap--node-counter 0))

(defun org-graphviz-mindmap--make-id ()
  "Generate unique node ID."
  (format "n%d" (cl-incf org-graphviz-mindmap--node-counter)))

(defun org-graphviz-mindmap--sanitize (text)
  "Sanitize TEXT for DOT labels."
  (setq text (replace-regexp-in-string "\"" "\\\\\"" text))
  (setq text (replace-regexp-in-string "\n" " " text))
  (if (> (length text) 50)
      (concat (substring text 0 47) "...")
    text))

(defun org-graphviz-mindmap--get-title (headline)
  "Get title from HEADLINE without TODO keyword."
  (org-element-property :raw-value headline))

(defun org-graphviz-mindmap--make-html-label (title todo)
  "Create HTML label with colored TODO keyword.
TITLE is the heading text, TODO is the optional TODO keyword."
  (if (and org-graphviz-mindmap-include-todo-keywords todo)
      (let* ((todo-color (or (cdr (assoc todo org-graphviz-mindmap-todo-colors)) "#000000"))
             (todo-tag (if org-graphviz-mindmap-todo-bold
                          (format "<B><FONT COLOR=\"%s\">%s</FONT></B>" todo-color todo)
                        (format "<FONT COLOR=\"%s\">%s</FONT>" todo-color todo)))
             (sanitized-title (org-graphviz-mindmap--sanitize title)))
        (format "<%s %s>" todo-tag sanitized-title))
    (org-graphviz-mindmap--sanitize title)))

(defun org-graphviz-mindmap--get-color (level)
  "Get color for LEVEL."
  (or (nth (1- level) org-graphviz-mindmap-level-colors)
      (car (last org-graphviz-mindmap-level-colors))))

(defun org-graphviz-mindmap--extract-id-links (headline)
  "Extract org-id links from HEADLINE's own content (excluding sub-headings)."
  (let ((begin (org-element-property :contents-begin headline))
        (end (org-element-property :contents-end headline))
        (links nil))
    (when (and begin end)
      (save-excursion
        (goto-char begin)
        ;; Find the first sub-heading (if any) to limit search
        (let* ((first-subheading (when (re-search-forward "^\\*+ " end t)
                                   (match-beginning 0)))
               (actual-end (if first-subheading
                               (min first-subheading end)
                             end)))
          (goto-char begin)
          (while (re-search-forward "\\[\\[id:\\([^]]+\\)\\]" actual-end t)
            (push (match-string 1) links)))))
    (nreverse links)))

(defun org-graphviz-mindmap--parse ()
  "Parse current Org buffer into heading list."
  (let (headings)
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (hl)
        (let* ((id (org-graphviz-mindmap--make-id))
               (org-id (org-element-property :ID hl))
               (level (org-element-property :level hl))
               (title (org-graphviz-mindmap--get-title hl))
               (todo (org-element-property :todo-keyword hl)))
          (when org-id (puthash org-id id org-graphviz-mindmap--id-to-node))
          (push (list :id id
                      :org-id org-id
                      :level level
                      :title title
                      :todo todo
                      :headline hl)
                headings))))
    (nreverse headings)))

(defun org-graphviz-mindmap--find-parent (headings current)
  "Find parent of CURRENT in HEADINGS."
  (let ((level (plist-get current :level))
        (idx (cl-position current headings)))
    (cl-loop for i from (1- idx) downto 0
             for h = (nth i headings)
             when (< (plist-get h :level) level)
             return h)))

(defun org-graphviz-mindmap--generate-dot (headings)
  "Generate DOT code from HEADINGS."
  (let ((nodes "")
        (edges ""))

    ;; Generate nodes with rank groups for same-level headings
    (let ((levels (make-hash-table)))
      ;; Group by level
      (dolist (h headings)
        (let ((lvl (plist-get h :level)))
          (push h (gethash lvl levels))))

      ;; Generate nodes grouped by level
      (maphash
       (lambda (lvl hs)
         (setq nodes (concat nodes (format "  // Level %d\n" lvl)))
         (setq nodes (concat nodes "  { rank=same;\n"))
         (dolist (h (nreverse hs))
           (let* ((id (plist-get h :id))
                  (title (plist-get h :title))
                  (todo (plist-get h :todo))
                  (label (org-graphviz-mindmap--make-html-label title todo))
                  (color (org-graphviz-mindmap--get-color lvl))
                  (label-attr (if (string-prefix-p "<" label)
                                 (format "label=%s" label)
                               (format "label=\"%s\"" label))))
             (setq nodes (concat nodes
                                (format "    %s [%s, fillcolor=\"%s\"];\n"
                                       id label-attr color)))))
         (setq nodes (concat nodes "  }\n\n")))
       levels))

    ;; Generate hierarchical edges
    (dolist (h headings)
      (let ((parent (org-graphviz-mindmap--find-parent headings h)))
        (when parent
          (setq edges (concat edges
                             (format "  %s -> %s;\n"
                                    (plist-get parent :id)
                                    (plist-get h :id)))))))

    ;; Generate org-id link edges
    (dolist (h headings)
      (dolist (link-id (org-graphviz-mindmap--extract-id-links (plist-get h :headline)))
        (let ((target (gethash link-id org-graphviz-mindmap--id-to-node)))
          (when target
            (setq edges (concat edges
                               (format "  %s -> %s [style=dashed, color=\"#2196F3\", constraint=false];\n"
                                      (plist-get h :id) target)))))))

    ;; Assemble
    (concat "digraph {\n"
            "  rankdir=TB;\n"
            "  node [shape=box, style=\"filled,rounded\", fontname=\"Arial\"];\n"
            "  edge [penwidth=2];\n"
            "  ranksep=1.0;\n\n"
            nodes
            "\n"
            edges
            "}\n")))

(defun org-graphviz-mindmap--render (dot-code output-file dot-file)
  "Render DOT-CODE to OUTPUT-FILE using DOT-FILE as intermediate."
  (let ((format (or (file-name-extension output-file) org-graphviz-mindmap-output-format)))
    (with-temp-file dot-file (insert dot-code))
    (let ((cmd (format "dot -T%s %s -o %s"
                      format
                      (shell-quote-argument dot-file)
                      (shell-quote-argument output-file))))
      (unless (zerop (shell-command cmd))
        (error "Failed to generate mind map"))
      (message "Mind map generated: %s" output-file)
      output-file)))

;;; Interactive commands

;;;###autoload
(defun org-graphviz-mindmap-create ()
  "Create mind map from current Org buffer.
Automatically saves to /tmp and opens the generated image."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org mode"))

  (org-graphviz-mindmap--reset)
  (let* ((base-name (file-name-base (or (buffer-file-name) "org-mindmap")))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (dot-file (expand-file-name (format "%s-%s.dot" base-name timestamp)
                                     temporary-file-directory))
         (output-file (expand-file-name (format "%s-%s.%s" base-name timestamp
                                                org-graphviz-mindmap-output-format)
                                        temporary-file-directory))
         (headings (org-graphviz-mindmap--parse))
         (dot-code (org-graphviz-mindmap--generate-dot headings)))
    (org-graphviz-mindmap--render dot-code output-file dot-file)
    (find-file output-file)
    (message "Mind map: %s (DOT: %s)" output-file dot-file)))

;;;###autoload
(defun org-graphviz-mindmap-show-dot ()
  "Show generated DOT code for current Org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org mode"))

  (org-graphviz-mindmap--reset)
  (let* ((headings (org-graphviz-mindmap--parse))
         (dot-code (org-graphviz-mindmap--generate-dot headings)))
    (with-current-buffer (get-buffer-create "*Org Mind Map DOT*")
      (erase-buffer)
      (insert dot-code)
      (when (fboundp 'graphviz-dot-mode)
        (graphviz-dot-mode))
      (display-buffer (current-buffer)))))

(provide 'org-graphviz-mindmap)

;;; org-graphviz-mindmap.el ends here
