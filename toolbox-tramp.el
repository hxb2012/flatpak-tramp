;;; toolbox-tramp.el --- Tramp connection to toolbox containers  -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author: Brian Cully <bjc@kublai.com>
;; Maintainer: "洪筱冰" <hxb@localhost.localdomain>

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

;; Allow Tramp access to environments provided by Toolbox
;;
;; ## Usage
;;
;; Open a file on a Toolbox container
;;
;;     C-x C-f /toolbox:CONTAINER:/path/to/file
;;
;; Where:
;;     CONTAINER     is the container to connect to

;;; Code:

(require 'tramp)

(defun toolbox-tramp--list-containers ()
  (process-lines tramp-podman-program "container" "list" "-a"
                 "-f=label=com.github.containers.toolbox=true"
                 "--format" "{{.Names}}"))

;;;###autoload
(defun toolbox-tramp--completion-function (&optional _filename)
  (cons
   (list nil "")
   (mapcar
    (lambda (name) (list nil name))
    (toolbox-tramp--list-containers))))

;;;###tramp-autoload
(with-eval-after-load 'tramp-loaddefs
  (defcustom toolbox-tramp-program "toolbox"
    "Name of the Toolbox program"
    :group 'tramp
    :type '(choice (const "toolbox")
                   (string)))

  (defconst toolbox-tramp-method "toolbox"
    "Tramp method name to use to connect to Toolbox instances.")

  (tramp--with-startup
   (add-to-list 'tramp-methods
                `(,toolbox-tramp-method
                  (tramp-login-program ,toolbox-tramp-program)
                  (tramp-login-args (("run")
                                     ("-c" "%h")
                                     ("--")
                                     ("%l")))
                  (tramp-direct-async t)
                  (tramp-remote-shell ,tramp-default-remote-shell)
                  (tramp-remote-shell-args ("-c"))))
   (add-to-list 'tramp-default-host-alist `(,toolbox-tramp-method nil ""))
   (tramp-set-completion-function
    toolbox-tramp-method
    '((toolbox-tramp--completion-function "")))))

(add-hook 'tramp-unload-hook
          (lambda ()
            (unload-feature 'toolbox-tramp 'force)))

(provide 'toolbox-tramp)
;;; toolbox-tramp.el ends here
