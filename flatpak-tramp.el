;;; flatpak-tramp.el --- Tramp connection to flatpak instances  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author: Brian Cully <bjc@kublai.com>
;; Maintainer: "洪筱冰" <hxb@localhost.localdomain>
;; Version: 0.1

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

;;; 最初是从tramp-container.el里抄来的

;; Allow Tramp access to environments provided by Flatpak
;;
;; ## Usage
;;
;; Open a file on a Flatpak application
;;
;;     C-x C-f /flatpak:APPLICATION:/path/to/file
;;
;; Where:
;;     APPLICATION     is the application to connect to
;;
;; Open a file on a Flatpak instance
;;
;;     C-x C-f /flatpak:INSTANCE@:/path/to/file
;;
;; Where:
;;     INSTANCE        is the instance to connect to

;;; Code:

(require 'tramp)

(defun flatpak-tramp--list-instances ()
  (mapcar
   'string-split
   (process-lines flatpak-tramp-program "ps" "--columns=instance,application")))

(defun flatpak-tramp--list-applications ()
  (process-lines flatpak-tramp-program "list" "--app" "--columns=application"))

;;;###autoload
(defun flatpak-tramp--completion-function (&optional _filename)
  (append
   (flatpak-tramp--list-instances)
   (mapcar
    (lambda (app) (list nil app)) (flatpak-tramp--list-applications))))

(defvar flatpak-tramp--override-p nil)

;;;###autoload
(defun flatpak-tramp--run-host-a (fun &rest args)
  (if flatpak-tramp--override-p
      (apply fun args)
    (cl-letf* ((flatpak-tramp--override-p t)
               (make-process (symbol-function 'make-process))
               (call-process (symbol-function 'call-process))
               ((symbol-function 'make-process)
                (lambda (&rest args)
                  (apply make-process
                         (plist-put args :command
                                    (append '("flatpak-spawn"
                                              "--host" "--watch-bus" "--")
                                            (plist-get args :command))))))
               ((symbol-function 'call-process)
                (lambda (program &optional infile destination display &rest args)
                  (apply call-process
                         "flatpak-spawn"
                         infile destination display
                         "--host" "--watch-bus" "--" program args))))
      (apply fun args))))

;;;###autoload
(defun flatpak-tramp--ignore-history-a (method)
  (not (equal method flatpak-tramp-method)))

;;;###autoload
(defun flatpak-tramp--get-completion-user-host-a (fun method partial-user partial-host user host)
  (if (not (equal method flatpak-tramp-method))
      (funcall fun method partial-user partial-host user host)
    (cond
     ((and partial-user partial-host)
      (unless (and (equal partial-user user)
                   host (string-prefix-p partial-host host))
        (setq user nil))
      (setq host ""))
     (partial-user
      (if (and user (string-prefix-p partial-user user))
          (setq host "")
        (setq user nil
              host nil)))
     (partial-host
      (unless (and host (string-prefix-p partial-host host))
        (setq host nil
              user nil)))
     (t (setq user nil
              host nil)))
    (unless (zerop (+ (length user) (length host)))
      (concat
       (tramp-completion-make-tramp-file-name method user host nil)
       (when (and user host) ":")))))

;;;###tramp-autoload
(with-eval-after-load 'tramp-loaddefs
  (defcustom flatpak-tramp-program "flatpak"
    "Name of the Flatpak program"
    :group 'tramp
    :type '(choice (const "flatpak")
                   (string)))

  (defconst flatpak-tramp-method "flatpak"
    "Tramp method name to use to connect to Flatpak instances.")

  (tramp--with-startup
   (add-to-list 'tramp-methods
                `(,flatpak-tramp-method
                  (tramp-login-program ,flatpak-tramp-program)
                  (tramp-login-args (("enter" "%u" "%l")
                                     ("run" "-p"
                                      ,(concat "--command="
                                               tramp-default-remote-shell)
                                      "%h" "-c" "%l")))
                  (tramp-direct-async t)
                  (tramp-remote-shell ,tramp-default-remote-shell)
                  (tramp-remote-shell-args ("-c"))))
   (add-to-list 'tramp-default-host-alist `(,flatpak-tramp-method nil ""))
   (tramp-set-completion-function
    flatpak-tramp-method
    '((flatpak-tramp--completion-function ""))))

  (advice-add 'tramp-parse-connection-properties
              :before-while 'flatpak-tramp--ignore-history-a)
  (advice-add 'tramp-get-completion-user-host
              :around 'flatpak-tramp--get-completion-user-host-a)

  (when (string-equal (getenv "container") "flatpak")
    (dolist (fun '(tramp-file-name-handler
                   tramp-completion-handle-file-name-all-completions))
      (advice-add fun :around 'flatpak-tramp--run-host-a))))

(add-hook 'tramp-unload-hook
          (lambda ()
            (unload-feature 'flatpak-tramp 'force)))

(provide 'flatpak-tramp)
;;; flatpak-tramp.el ends here
