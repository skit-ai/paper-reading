(require 'ox-tufte)
(require 'f)

(defcustom input-file "./index.org"
  "Input org file to transform")

(defcustom output-file "./docs/index.html"
  "Output html")

(with-current-buffer (find-file-noselect input-file)
  (let ((org-export-show-temporary-export-buffer t))
    (org-tufte-export-to-buffer)
    (f-write-text (buffer-string) 'utf-8 output-file)))
