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
(package-install 'lua-mode)
(package-install 'gleam-ts-mode)
(package-install 'elm-mode)
(package-install 'tuareg)
(package-install 'haskell-mode)
(package-install 'rust-mode)
(package-install 'arduino-mode)


;;----------------------------------------
;; Source: https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors

(require 'ox-publish)
(require 'easy-mmode)
(require 'dash)
(require 'gleam-ts-mode)
(require 'elm-mode)
(require 'tuareg)
(require 'haskell-mode)
(require 'rust-mode)
(require 'lua-mode)
(require 'arduino-mode)

(gleam-ts-install-grammar)

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
                   (format "<a href=\"#%s\">%s</a>" id text)
                 text)))
    (org-html-format-headline-default-function todo todo-type priority link tags info)))
;;--------------------------------------------------
;; Doc: https://ogp.me/
(defun meta-name (name content)
  (format "<meta name=\"%s\" content=\"%s\"/>" name content))

(defun meta-twitter (type content)
  (meta-name (format "twitter:%s" type) content))

(defun meta-og (type content)
  (format "<meta property=\"og:%s\" content=\"%s\"/>" type content))
;;--------------------------------------------------

(setq org-html-metadata-timestamp-format "%d %b %Y")
(setq org-html-head-include-scripts nil)
(setq org-html-head-include-default-style nil)

(defun unlines (&rest rest)
  (string-join rest "\n"))

(defvar html-head-extra-article ; needs dynamic og:url
  (unlines
   (meta-og      "type"         "article")
   (meta-og      "locale"       "en_US")
   (meta-og      "site_name"    "azimut's webpage")
   (meta-og      "image"        "https://azimut.github.io/apple-touch-icon.png")
   (meta-twitter "image"        "https://azimut.github.io/apple-touch-icon.png")
   (meta-twitter "card"         "summary")
   (meta-twitter "image:width"  "180")
   (meta-twitter "image:height" "180")))

(defvar html-head-extra-index
  (unlines
   (meta-name    "robots"       "index,follow")
   (meta-name    "description"  "azimut's personal website")
   (meta-og      "description"  "azimut's personal website")
   (meta-og      "site_name"    "azimut's webpage")
   (meta-og      "type"         "website")
   (meta-og      "url"          "https://azimut.github.io/")
   (meta-og      "image"        "https://azimut.github.io/apple-touch-icon.png")
   (meta-twitter "image"        "https://azimut.github.io/apple-touch-icon.png")
   (meta-twitter "card"         "summary")
   (meta-twitter "image:width"  "180")
   (meta-twitter "image:height" "180")))

(defvar html-head
  (unlines
   "<script type=\"text/javascript\" src=\"/script.js\"></script>"
   "<link rel=\"stylesheet\" type=\"text/css\" href=\"/style.css\"/>"
   "<link rel=\"stylesheet\" type=\"text/css\" href=\"/photoswipe.min.css\"/>"
   "<link rel=\"icon\" href=\"/favicon.ico\" type=\"image/x-icon\" />"
   "<link rel=\"apple-touch-icon\" href=\"/apple-touch-icon.png\" />"
   "<script type=\"module\">"
   "import PhotoSwipeLightbox from \'/photoswipe-lightbox.esm.min.js\';
    const lightbox = new PhotoSwipeLightbox({
      // may select multiple galleries
      gallery: \'#mygallery\',

      // Elements within gallery (slides)
      children: \'a\',

      // setup PhotoSwipe Core dynamic import
      pswpModule: () => import(\'/photoswipe.esm.min.js\')
    });
    lightbox.init();"
   "</script>"))

(defvar cloudflare-script
  (unlines
   "<!-- Cloudflare Web Analytics -->"
   "<script defer src=\"https://static.cloudflareinsights.com/beacon.min.js\" data-cf-beacon='{\"token\": \"a6847e40b42c4009813b1f275831b258\"}' ></script>"
   "<!-- End Cloudflare Web Analytics -->"))

(defvar navigation-html
  (unlines
   "<hr/>"
   "<ul>"
   "<li><a href=\"#\" onclick=\"history.back()\">⬅ Back</a></li>"
   "<li><a href=\"../../\">⌂ Home</a></li>"
   "<li><a href=\"#\" onclick=\"window.scrollTo(0,0)\">⬆ Top</a></li>"
   "</ul>"))

;; Modified to add new headers
(defun org-html-meta-tags-default (info)
  "A default value for `org-html-meta-tags'.

Generate a list items, each of which is a list of arguments that can
be passed to `org-html--build-meta-entry', to generate meta tags to be
included in the HTML head.

Use document's plist INFO to derive relevant information for the tags."
  (let ((author (and (plist-get info :with-author)
                     (let ((auth (plist-get info :author)))
                       ;; Return raw Org syntax.
                       (and auth (org-element-interpret-data auth)))))
        (title (and (plist-get info :title)
                    (org-element-interpret-data
                     (plist-get info :title)))))
    (list
     (when (org-string-nw-p title)
       (list "property" "og:title" title))
     (when (org-string-nw-p title)
       (list "name" "twitter:title" title))
     (when (org-string-nw-p author)
       (list "name" "author" author))
     (when (org-string-nw-p (plist-get info :description))
       (list "name" "description"
             (plist-get info :description)))
     (when (org-string-nw-p (plist-get info :description)) ;; <-
       (list "property" "og:description"
             (plist-get info :description)))
     (when (org-string-nw-p (plist-get info :keywords))
       (list "name" "keywords" (plist-get info :keywords)))
     (when (plist-get info :date) ;; <-
       (list "property" "article:published_time"
             (org-export-get-date info "%FT%T%z")))
     (when (plist-get info :date) ;; <-
       (list "property" "article:modified_time"
             (format-time-string
              "%FT%T%z"
              (file-attribute-modification-time
               (file-attributes buffer-file-name)))))
     '("name" "generator" "Org Mode"))))

(defun make-section (section-name)
  `(,section-name ; unique
    :author "azimut"
    :html-format-headline-function my-org-html-format-headline-function
    :auto-sitemap t
    :headline-levels 4 ; aka h5 - default = 3
    :sitemap-format-entry
    ,(lambda (entry style project)
       (format "*%s*[[file:%s][%s]]"
               (format-time-string "%m/%y" (org-publish-find-date entry project))
               entry
               (org-publish-find-title entry project)))
    :sitemap-function
    ,(lambda (title list)
       (unlines (concat "#+TITLE: " title)
                "#+OPTIONS: title:t html-preamble:nil html-postamble:t"
                "#+HTML_HEAD_EXTRA: <link rel=\"stylesheet\" type=\"text/css\" href=\"/sitemap.css\" />"
                (org-list-to-org (if (string= title "Blog") ; FIXME: skip drafts/
                                     (print (butlast list))
                                   list))))
    :sitemap-filename "index.org"
    :sitemap-title ,(capitalize section-name)
    :sitemap-sort-files anti-chronologically
    :recursive t
    :html-head ,html-head
    :html-head-extra ,html-head-extra-article
    :html-preamble t
    :html-preamble-format
    (("en" ,(unlines
             "<h1>%t</h1>"
             "<div id=\"progress-bar\"></div>"
             "<div class=\"created\">Created: %d</div>"
             "<div class=\"updated\">Updated: %C</div>")))
    :html-postamble t
    :html-postamble-format
    (("en" ,(unlines navigation-html cloudflare-script)))
    :html-doctype "html5"
    :html-html5-fancy t
    :base-directory ,(concat "./org/" section-name) ;; .org
    :publishing-directory ,(concat "./public/" section-name) ;; created
    :publishing-function org-html-publish-to-html
    :with-title nil
    :with-date t
    :with-toc t
    :with-sub-superscript nil
    :section-numbers nil))

(defun make-base (name &optional navigation-p)
  `(,name
    :author "azimut"
    :with-toc nil
    :with-sub-superscript nil
    :section-numbers nil
    :html-postamble t
    :html-postamble-format
    (("en" ,(if navigation-p
                (concat navigation-html cloudflare-script)
              cloudflare-script)))
    :html-doctype "html5"
    :html-html5-fancy t
    :html-head ,html-head
    :html-head-extra ,html-head-extra-index
    :recursive nil
    :base-extension "xyz"  ; non existent extension, then whitelist
    :include (,(concat name ".org"))
    :base-directory "./org"
    :publishing-directory "./public"
    :publishing-function org-html-publish-to-html))

(setq org-publish-project-alist
      `(,(make-base "index")
        ,(make-base "about" t)
        ,(make-base "404" t)
        ,(make-section "blog")
        ,(make-section "notes")
        ("azimut.github.io"
         :components ("blog" "notes" "index" "about" "404"))))

(org-publish-all t) ; t for NO cache

(message "Build complete")
