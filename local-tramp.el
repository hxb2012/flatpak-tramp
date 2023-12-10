;;; local-tramp.el --- Tramp connection to localhost  -*- coding: utf-8; lexical-binding: t; -*-

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

;; Allow Tramp access to localhost
;;
;; ## Usage
;;
;; Open a file on localhost
;;
;;     C-x C-f /local::/path/to/file
;;

;;; Code:

(require 'tramp)


;;;###autoload
(defun local-tramp--get-completion-user-host-a (fun method partial-user partial-host user host)
  (if (not (equal method local-tramp-method))
      (funcall fun method partial-user partial-host user host)
    (concat
       (tramp-completion-make-tramp-file-name method "" "" nil)
       ":")))


;;;###tramp-autoload
(with-eval-after-load 'tramp-loaddefs
  (defconst local-tramp-method "local"
    "Tramp method name to use to connect to localhost.")

  (when (string-equal (getenv "container") "flatpak")
    (tramp--with-startup
     (add-to-list 'tramp-methods
                  `(,local-tramp-method
                    (tramp-direct-async t))))
    (advice-add 'tramp-get-completion-user-host
                :around 'local-tramp--get-completion-user-host-a)))

(add-hook 'tramp-unload-hook
          (lambda ()
            (unload-feature 'local-tramp 'force)))

(provide 'local-tramp)
;;; local-tramp.el ends here
