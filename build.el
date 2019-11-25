(require 'ox-tufte)
(require 'f)
(require 'ht)
(require 'mustache)
(require 'ts)

(defcustom input-file "./index.org"
  "Input org file to transform")

(defcustom output-file "./docs/index.html"
  "Output html")

(defcustom output-atom-file "./docs/atom.xml"
  "Atom feed file")

(defcustom root-title "paper-reading | vernacular.ai"
  "Page title")

(defcustom root-url "https://vernacular-ai.github.io/paper-reading"
  "Main url for page")

(defvar atom-template "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <title>{{ root-title }}</title>
  <link href=\"{{ root-url }}\"/>
  <link href=\"{{ root-url }}/atom.xml\" rel=\"self\"/>
  <updated>{{ root-date }}</updated>
  <author><name>{{ root-author }}</name></author>
  {{#entry}}
  <entry>
    <title>{{ title }}</title>
    <updated>{{ date }}</updated>
    <content type=\"xhtml\">
      <div xmlns=\"http://www.w3.org/1999/xhtml\">
        {{{ content }}}
      </div>
    </content>
  </entry>
  {{/entry}}
</feed>"
  "Template for atom.xml file")

(defun parse-hl-content (hl)
  (let* ((section (car (org-element-contents hl)))
         (org-text (buffer-substring-no-properties (org-element-property :contents-begin section)
                                                   (org-element-property :contents-end section))))
    (with-temp-buffer
      (org-mode)
      (insert org-text)
      (let ((org-export-show-temporary-export-buffer t))
        (org-html-export-as-html nil nil nil t)
        (buffer-string)))))

(defun parse-entries ()
  "Parser entries to make feeds of from the current buffer."
  (mapcar #'ht<-alist
          (org-element-map (org-element-parse-buffer) 'headline
            (lambda (hl)
              (let ((title (org-element-property :raw-value hl)))
                `(("title" . ,title)
                  ("content" . ,(parse-hl-content hl))
                  ("date" . ,(ts-format "%FT%T%z" (ts-parse title)))))))))

(defun format-atom ()
  (mustache-render atom-template
                   (ht ("root-title" root-title)
                       ("root-author" "Vernacular.ai")
                       ("root-url" root-url)
                       ("root-date" (ts-format "%FT%T%z" (ts-now)))
                       ("entry" (parse-entries)))))

(with-current-buffer (find-file-noselect input-file)
  (let ((org-export-show-temporary-export-buffer t))
    (org-tufte-export-to-buffer)
    (f-write-text (buffer-string) 'utf-8 output-file)))

(with-current-buffer (find-file-noselect input-file)
  (f-write-text (format-atom) 'utf-8 output-atom-file))
