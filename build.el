(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(package-install 'dash)
(package-install 'htmlize)

;;----------------------------------------
;; Source: https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors

(require 'ox-publish)
(require 'easy-mmode)
(require 'dash)

(defun unpackaged/org-export-get-reference (datum info)
  "Like `org-export-get-reference', except uses heading titles instead of random numbers."
  (let ((cache (plist-get info :internal-references)))
    (or (car (rassq datum cache))
        (let* ((crossrefs (plist-get info :crossrefs))
               (cells (org-export-search-cells datum))
               ;; Preserve any pre-existing association between
               ;; a search cell and a reference, i.e., when some
               ;; previously published document referenced a location
               ;; within current file (see
               ;; `org-publish-resolve-external-link').
               ;;
               ;; However, there is no guarantee that search cells are
               ;; unique, e.g., there might be duplicate custom ID or
               ;; two headings with the same title in the file.
               ;;
               ;; As a consequence, before re-using any reference to
               ;; an element or object, we check that it doesn't refer
               ;; to a previous element or object.
               (new (or (cl-some
                         (lambda (cell)
                           (let ((stored (cdr (assoc cell crossrefs))))
                             (when stored
                               (let ((old (org-export-format-reference stored)))
                                 (and (not (assoc old cache)) stored)))))
                         cells)
                        (when (org-element-property :raw-value datum)
                          ;; Heading with a title
                          (unpackaged/org-export-new-title-reference datum cache))
                        ;; NOTE: This probably breaks some Org Export
                        ;; feature, but if it does what I need, fine.
                        (org-export-format-reference
                         (org-export-new-reference cache))))
               (reference-string new))
          ;; Cache contains both data already associated to
          ;; a reference and in-use internal references, so as to make
          ;; unique references.
          (dolist (cell cells) (push (cons cell new) cache))
          ;; Retain a direct association between reference string and
          ;; DATUM since (1) not every object or element can be given
          ;; a search cell (2) it permits quick lookup.
          (push (cons reference-string datum) cache)
          (plist-put info :internal-references cache)
          reference-string))))

(defun unpackaged/org-export-new-title-reference (datum cache)
  "Return new reference for DATUM that is unique in CACHE."
  (cl-macrolet ((inc-suffixf (place)
                  `(progn
                     (string-match (rx bos
                                       (minimal-match (group (1+ anything)))
                                       (optional "--" (group (1+ digit)))
                                       eos)
                                   ,place)
                     ;; HACK: `s1' instead of a gensym.
                     (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                (match-string 2 ,place)))
                             (suffix (if suffix
                                         (string-to-number suffix)
                                       0)))
                       (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
    (let* ((title (org-element-property :raw-value datum))
           (ref (url-hexify-string (substring-no-properties title)))
           (parent (org-element-property :parent datum)))
      (while (--any (equal ref (car it))
                    cache)
        ;; Title not unique: make it so.
        (if parent
            ;; Append ancestor title.
            (setf title (concat (org-element-property :raw-value parent)
                                "--" title)
                  ref (url-hexify-string (substring-no-properties title))
                  parent (org-element-property :parent parent))
          ;; No more ancestors: add and increment a number.
          (inc-suffixf ref)))
      ref)))

(define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
  "Attempt to export Org as HTML with useful link IDs.
Instead of random IDs like \"#orga1b2c3\", use heading titles,
made unique when necessary."
  :global t
  (if unpackaged/org-export-html-with-useful-ids-mode
      (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
    (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))

(unpackaged/org-export-html-with-useful-ids-mode 1)

;;----------------------------------------
;; Source: http://alhassy.com/emacs.d/#Clickable-Headlines
(defun my-org-html-format-headline-function (todo todo-type priority text tags info)
  "Format a headline with a link to itself."
  (let* ((headline (get-text-property 0 :parent text))
         (id (or (org-element-property :CUSTOM_ID headline)
                 (ignore-errors (org-export-get-reference headline info))
                 (org-element-property :ID headline)))
         (link (if id
                   (format "<a href=\"#%s\">🔗</a> %s"
                           id
                           text)
                 text)))
    (org-html-format-headline-default-function todo todo-type priority link tags info)))
;;--------------------------------------------------

(setf org-html-metadata-timestamp-format "%Y-%m-%d")
(setq org-html-head-include-scripts nil)
(setq org-html-head-include-default-style nil)

(setq org-publish-project-alist
      `(("index"
         :with-toc nil
         :with-sub-superscript nil
         :section-numbers nil
         :html-postamble nil
         :html-doctype "html5"
         :html-html5-fancy t
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"/>\n"
         :recursive nil
         :include ("index.org")
         :base-directory "./org"
         :publishing-directory "./public"
         :publishing-function org-html-publish-to-html)
        ("blog" ; unique
         :html-format-headline-function my-org-html-format-headline-function
         :auto-sitemap t
         :sitemap-format-entry
         ,(lambda (entry style project)
            (format "%s [[file:%s][%s]]"
                    (format-time-string "%m/%y" (org-publish-find-date entry project))
                    entry
                    (org-publish-find-title entry project)))
         :sitemap-function
         ,(lambda (title list)
            (concat (format "#+TITLE: %s\n" title)
                    "#+OPTIONS: html-postamble:nil html-preamble:nil\n"
                    (org-list-to-org list)))
         :sitemap-filename "index.org"
         :sitemap-title "Blog"
         :sitemap-sort-files anti-chronologically
         :recursive t
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../style.css\"/>\n"
         :html-postamble t
         :html-postamble-format
         (("en" ,(concat "<hr/>"
                         "<p class=\"date\">Creation Date: %d</p>"
                         "<p class=\"date\">Last Modified: %C</p>"
                         "<a href=\"../index.html\">Back</a>")))
         :html-doctype "html5"
         :html-html5-fancy t
         :base-directory "./org/blog" ;; .org
         :publishing-directory "./public/blog" ;; created
         :publishing-function org-html-publish-to-html
         :with-title t
         :with-date t
         :with-toc t
         :with-sub-superscript nil
         :section-numbers nil)
        ("azimut.github.io"
         :components ("blog" "index"))))

(org-publish-all t) ; t for NO cache

(message "Build complete")
