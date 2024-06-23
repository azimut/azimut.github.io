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

(setq org-html-postamble t)
(setq org-html-postamble-format
      `(("en" ,(concat "<p>Date: %d</p>"
                       "<p>Last Modified: %C</p>"))))
(setf org-html-metadata-timestamp-format "%Y-%m-%d")
(setq org-html-validation-link nil) ;; rm Validate link
(setq org-html-head-include-scripts nil)
(setq org-html-head-include-default-style nil)
(setq org-html-head
      (concat "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\"/>"
              "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"/>"))

(setq org-publish-project-alist
      `(("Notes" ; unique
         :auto-sitemap t
         :sitemap-format-entry
         ,(lambda (entry style project)
            (format "%s [[file:%s][%s]]"
                    (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
                    entry
                    (org-publish-find-title entry project)))
         :sitemap-function
         ,(lambda (title list)
            (concat (format "#+TITLE: %s\n" title)
                    "#+OPTIONS: html-postamble:nil html-preamble:nil\n"
                    (org-list-to-org (print list))))
         :sitemap-filename "index.org"
         :sitemap-title "Notes"
         :sitemap-sort-files anti-chronologically
         :with-date t
         :recursive t
         :base-directory "./content" ;; .org
         :publishing-directory "./public" ;; created
         :publishing-function org-html-publish-to-html
         :with-author nil
         :with-title t
         :with-toc t
         :section-numbers t
         :time-stamp-file nil)))

(org-publish-all t) ; t for NO cache

(message "Build complete")
