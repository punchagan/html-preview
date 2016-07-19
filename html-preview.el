;;; html-preview.el --- Preview html files inside Emacs' webkit xwidget -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Puneeth Chaganti

;; Author: Puneeth Chaganti <punchagan@muse-amuse.in>
;; Created: 2016 July 15
;; Keywords: html, org-mode, preview
;; URL: <https://github.com/punchagan/html-preview>

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Dependencies:
;; Requires Emacs to be compiled with xwidgets support

;;; Commentary:

;; Provides `html-preview' which is a function to preview current file exported
;; inside Emacs. Uses xwidgets as opposed to `org-eww' which uses `eww' make
;; changes. This allows viewing Reveal.js presentations, for instance.

(require 's)
(require 'xwidget)

(defvar html-preview-generator-name 'ox-reveal
  "Name of the html generator function to use.")

(defvar html-preview-buffer-name "*html-preview-buffer*"
  "Name of the buffer in which previews are shown")

(defvar html-preview-generate-function-alist
  '((ox-html . html-preview--generate-ox-html)
    (ox-reveal . html-preview--generate-ox-reveal))
  "Alist mapping a name to the html generate functions.  Each of
these functions hast to return the path to the HTML file after
generating it.")

(defvar html-preview-xwidget-minor-mode-map (make-sparse-keymap)
  "Keymap when `html-preview-xwidget-minor-mode' is active.")

(defvar html-preview--reveal-trigger-key-js "Reveal.triggerKey(%s)")

(defvar html-preview--reveal-keys
  '("n" "SPC"
    "p"
    ("<left>" 37) "h"
    ("<right>" 39) "l"
    ("<up>" 38) "k"
    ("<down>" 40) "j"
    ("<home>" 36)
    ("<end>" 35)
    "b" "."
    "f"
    "ESC" "o")
  "Reveal keybindings that we trigger using reveal keys

N, SPACE    Next slide
P           Previous slide
←, H       Navigate left
→, L       Navigate right
↑, K        Navigate up
↓, J        Navigate down
Home        First slide
End         Last slide
B, .        Pause
F           Fullscreen
ESC, O      Slide overview
")

(defmacro html-preview-define-key (key)
  "Define key for preview mode"
  `(let* ((key* (kbd (if (stringp ,key) key (car ,key))))
          (code (if (stringp key*) (string-to-char (s-upcase key*)) (cadr ,key))))
     (define-key html-preview-xwidget-minor-mode-map key*
       (lambda ()
         (interactive)
         (xwidget-webkit-execute-script-rv
          (xwidget-webkit-current-session)
          (format html-preview--reveal-trigger-key-js code))))))

(dolist (key html-preview--reveal-keys)
  (html-preview-define-key key))

(define-minor-mode html-preview-xwidget-minor-mode
  "Minor mode for the html-preview xwidget buffer."
  :init-value nil
  :keymap html-preview-xwidget-minor-mode-map)

(define-minor-mode html-preview-minor-mode
  "Minor mode for the html-preview source buffer.

Enabling this mode just sets up a save hook to run html-preview
on every save."
  :init-value nil
  :lighter " html-preview-src"
  (if html-preview-minor-mode
      (add-hook 'after-save-hook #'html-preview nil t)
    (remove-hook 'after-save-hook #'html-preview t)))

(defun html-preview--generate-default ()
  (buffer-file-name))

(defun html-preview--generate-ox-reveal ()
  (require 'ox-reveal)
  (org-reveal-export-to-html))

(defun html-preview--generate-ox-html ()
  (require 'ox-html)
  (org-html-export-to-html))

(defun html-preview--generate ()
  "Generate html from the appropriate function"
  (let ((f (cdr (assoc html-preview-generator-name html-preview-generate-function-alist))))
    (if (not (null f))
        (funcall f)
      (message (concat "No html generate function for "
                       (symbol-name html-preview-generator-name)
                       ". Assuming file is html."))
      (html-preview--generate-default))))

(defun html-preview--xwidget-webkit-callback (xwidget xwidget-event-type)
  ;; TODO: Allow for other window placements?
  (if (not (equal xwidget-event-type 'document-load-finished))
      (xwidget-webkit-callback xwidget xwidget-event-type)
    (switch-to-buffer html-preview--src-buffer)
    (delete-other-windows)
    (switch-to-buffer-other-window (xwidget-buffer xwidget))
    (xwidget-webkit-adjust-size-dispatch)
    (if (equal 'ox-reveal
               (buffer-local-value
                'html-preview-generator-name
                html-preview--src-buffer))
        (html-preview-xwidget-minor-mode 1)
      (html-preview-xwidget-minor-mode -1))))

(defun html-preview--browse-url (url)
  "Browse the URL using our own xwidget."
  (let* ((preview-buffer (get-buffer html-preview-buffer-name))
         (xw (and (buffer-live-p preview-buffer)
                  (with-current-buffer preview-buffer
                    (xwidget-at (point-min))))))
    (if xw
        (progn
          ;; FIXME: `document-load-finished' is not fired when URL is same as
          ;; the one opened before, but only fragment changes. Also, scroll
          ;; doesn't work in this case!
          (xwidget-webkit-goto-uri xw "")
          (xwidget-webkit-goto-uri xw url))
      (xwidget-webkit-new-session url #'html-preview--xwidget-webkit-callback)
      (with-current-buffer (xwidget-buffer (xwidget-webkit-last-session))
        (rename-buffer html-preview-buffer-name)))))

(defun html-preview--get-url-fragment (html-path)
  (save-excursion
    ;; Headline parser assumes we are at beginning of headline
    (if (org-at-heading-p)
        (beginning-of-line)
      (save-restriction
        (widen)
        (org-previous-visible-heading 1)))
    (let ((headline (org-element-headline-parser (point-max)))
          (info (org-export-get-environment))
          (generator-name html-preview-generator-name)
          ;; FIXME: Works only for ox-reveal and ox-html generators
          (id-re "<\\(section\\|div\\) id=\"\\(.*?\\)\".*?>\n<h.*?>%s</h.>")
          text fragment)
      (while (org-export-low-level-p headline info)
        (org-up-element)
        (setq headline (org-element-headline-parser (point-max))))
      (setq text (nth 4 (org-heading-components)))

      (with-temp-buffer
        (insert-file-contents html-path)
        (save-match-data
          (when (re-search-forward (format id-re text) nil t)
            (setq fragment (match-string 2))))
        (or fragment "")))))

(defun html-preview (&optional beginning)
  "Use xwidgets and get a reveal preview"
  (interactive "p")
  (let* ((html-path (expand-file-name (html-preview--generate)))
         (full-path (format "file://%s" html-path))
         (fragment (and (not (equal 4 beginning))
                        (ignore-errors (html-preview--get-url-fragment html-path))))
         (url (if fragment (format "%s#%s" full-path fragment) full-path))
         xw)
    (setq html-preview--src-buffer (current-buffer))
    (html-preview--browse-url url)))

(provide 'html-preview)
