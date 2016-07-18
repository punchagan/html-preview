;; -*- lexical-binding: t -*-
;;; html-preview.el -- Preview html files inside Emacs' webkit xwidget

;; Copyright (C) 2016 Puneeth Chaganti

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

(defvar html-preview-minor-mode-map (make-sparse-keymap)
  "Keymap html-preview-mode is active.")

(defvar html-preview-generator-name 'ox-reveal
  "Name of the html generator function to use.")

(defvar html-preview-generate-function-alist
  '((ox-html . html-preview--generate-ox-html)
    (ox-reveal . html-preview--generate-ox-reveal))
  "Alist mapping a name to the html generate functions")

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
     (define-key html-preview-minor-mode-map key*
       (lambda ()
         (interactive)
         (xwidget-webkit-execute-script-rv
          (xwidget-webkit-current-session)
          (format html-preview--reveal-trigger-key-js code))))))

(dolist (key html-preview--reveal-keys)
  (html-preview-define-key key))

(defun html-preview--generate-default ()
  (buffer-file-name))

(defun html-preview--generate-ox-reveal ()
  (require 'ox-reveal)
  (org-reveal-export-to-html))

(defun html-preview--generate-ox-html ()
  (require 'ox-html)
  (org-html-export-to-html))

(define-minor-mode html-preview-minor-mode
  "Minor mode to simulate buffer local keybindings."
  :init-value nil
  :keymap html-preview-minor-mode-map)

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
    (rename-buffer "*html-preview-buffer*")
    (xwidget-webkit-adjust-size-dispatch)
    (if (equal 'ox-reveal
               (buffer-local-value
                'html-preview-generator-name
                html-preview--src-buffer))
        (html-preview-minor-mode 1)
      (html-preview-minor-mode -1))))

(defun html-preview ()
  "Use xwidgets and get a reveal preview"
  (interactive)
  (let* ((html-path (expand-file-name (html-preview--generate)))
         xw)
    (setq html-preview--src-buffer (current-buffer))
    (xwidget-webkit-browse-url (format "file://%s" html-path))
    (setq xw (car (get-buffer-xwidgets xwidget-webkit-last-session-buffer)))
    ; FIXME: Could this possibly create a race condition?
    (xwidget-put xw 'callback #'html-preview--xwidget-webkit-callback)
    ;; TODO: Jump to the correct section, get section id using org-rules.
    ))

(provide 'html-preview)

;; TODO: Submit patch to xwidgets.el that uses calls callback from 'callback
;; property on widget, instead of global method
