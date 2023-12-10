;;; switch-mode.el --- Switch buffer  -*- coding: utf-8; lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defvar switch-mode-methods '("local" "toolbox"))

;;;###autoload(autoload 'tramp-completion-handle-file-name-all-completions "tramp")
(declare-function tramp-completion-handle-file-name-all-completions "tramp")

(defun switch-mode--get-remote-localname (&optional buffer)
  (when-let ((name (buffer-file-name buffer)))
    (if-let ((remote-ident (file-remote-p name t)))
        (let ((file-name (tramp-dissect-file-name name)))
          (when (member (tramp-file-name-method file-name) switch-mode-methods)
            (cons remote-ident (tramp-file-name-localname file-name))))
      (cons nil name))))

(defun switch-mode--get-method-remotes (method)
  (seq-uniq
   (tramp-completion-handle-file-name-all-completions
    (format "/%s:" method) "")))

(defun switch-mode--get-remotes (localname)
  (seq-uniq
   (apply 'append (list nil)
          (mapcar 'car (seq-filter
                        (lambda (x) (equal (cdr x) localname))
                        (mapcar 'switch-mode--get-remote-localname
                                (cons
                                 nil
                                 (when (boundp 'uniquify-managed)
                                   (mapcar 'uniquify-item-buffer uniquify-managed))))))
          (mapcar 'switch-mode--get-method-remotes
                  switch-mode-methods))))

(defun switch-mode--has-conflict-p (localname)
  (when (and (boundp 'uniquify-managed) uniquify-managed)
    (seq-some (lambda (buffer)
                (and (not (eq buffer (current-buffer)))
                     (equal localname
                            (cdr (switch-mode--get-remote-localname buffer)))))
              (mapcar 'uniquify-item-buffer uniquify-managed))))

(defun switch-mode--switch-to (file-name)
  (if-let ((buffer (find-buffer-visiting file-name)))
      (switch-to-buffer (or (buffer-base-buffer buffer) buffer))
    (set-visited-file-name file-name nil t)
    (force-mode-line-update t)))

;;;###autoload
(defun switch-mode-switch ()
  (interactive)
  (when-let ((remote-local-name (switch-mode--get-remote-localname)))
    (let* ((remotes
            (mapcar
             (lambda (x) (concat "[" x "]"))
             (seq-filter
              (lambda (x) (not (equal x (car remote-local-name))))
              (switch-mode--get-remotes (cdr remote-local-name)))))
           (prefix (completing-read "Switch to: "
                                    remotes nil t "[")))
      (switch-mode--switch-to
       (concat (string-remove-prefix "[" (string-remove-suffix "]" prefix))
               (cdr remote-local-name))))))


(defun switch-mode-menu (&optional buffer)
  (interactive "@")
  (let* ((localname
          (if (file-remote-p buffer-file-name)
              (tramp-file-name-localname (tramp-dissect-file-name buffer-file-name))
            buffer-file-name))
         (remotes (switch-mode--get-remotes localname)))
    (popup-menu
     (cons "Switch"
           (mapcar
            (lambda (prefix)
              (let ((file-name (concat prefix localname)))
                (vector
                 (concat "[" prefix "]")
                 (lambda () (interactive) (switch-mode--switch-to file-name))
                 :active (not (equal file-name buffer-file-name))
                 :style 'toggle
                 :selected (find-buffer-visiting file-name))))
            remotes)))))

(defvar switch-mode-line-map (make-sparse-keymap))
(keymap-set switch-mode-line-map "<mode-line> <mouse-1>" 'switch-mode-menu)
(keymap-set switch-mode-line-map "<mode-line> <mouse-2>" 'ignore)
(keymap-set switch-mode-line-map "<mode-line> <mouse-3>" 'ignore)
(keymap-set switch-mode-line-map "<header-line> <mouse-1>" 'switch-mode-menu)
(keymap-set switch-mode-line-map "<header-line> <mouse-2>" 'ignore)
(keymap-set switch-mode-line-map "<header-line> <mouse-3>" 'ignore)

(defun switch-mode-line ()
  (when-let ((remote-localname (switch-mode--get-remote-localname)))
    (propertize (concat "[" (car remote-localname) "]")
                'face (when (switch-mode--has-conflict-p (cdr remote-localname)) 'error)
                'help-echo "Reopen\nmouse-1: shows a menu"
                'mouse-face 'mode-line-highlight
                'local-map switch-mode-line-map)))

(defvar switch-mode-line-format '(:eval (switch-mode-line)))
(put 'switch-mode-line-format 'risky-local-variable t)

;;;###autoload
(define-minor-mode switch-mode
  "Reopen file"
  :global t
  (cond
   (switch-mode
    (setq-default
     mode-line-remote
     (append
      mode-line-remote
      '(switch-mode-line-format)))
    (force-mode-line-update t))
   (t
    (setq-default
     mode-line-remote
     (remove 'switch-mode-line-format
             (default-value 'mode-line-remote)))
    (force-mode-line-update t))))

(provide 'switch-mode)
;;; switch-mode.el ends here
