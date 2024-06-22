(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install 'htmlize)

;;--------------------------------------------------

(require 'ox-publish)

(setf org-html-metadata-timestamp-format "%Y-%m-%d")
(setq org-html-validation-link nil) ;; rm Validate link
(setq org-html-head-include-scripts nil)
(setq org-html-head-include-default-style nil)
(setq org-html-head
      (concat "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\"/>"
              "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"/>"))
(setq org-publish-project-alist
      (list
       (list
        "Notes" ; unique
        :with-date t
        :auto-sitemap t
        :sitemap-filename "index.org"
        :sitemap-title "Notes"
        :sitemap-ignore-case t
        :recursive t
        :base-directory "./content" ;; .org
        :publishing-directory "./public" ;; created
        :publishing-function 'org-html-publish-to-html
        :with-author nil
        :with-creator t ; emacs and org versions
        :with-toc t
        :section-numbers t
        :time-stamp-file nil)))

(org-publish-all t) ; t for NO cache

(message "Build complete")
