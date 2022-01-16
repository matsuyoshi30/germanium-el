;;; germanium.el --- Generate image from source code using germanium -*- lexical-binding: t -*-

;; Author: Masaya Watanabe
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/matsuyoshi30/germanium-el

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tool for generating image from source code using germanium
;; <https://github.com/matsuyoshi30/germanium>.  It requires you
;; to have the `germanium' command-line tool installed on your path.

;;; Code:

(require 'subr-x)

(defgroup germanium nil
  "tool for generating image from source code"
  :group 'tools)

(defcustom germanium-executable-path "germanium"
  "Executable command path for gernamium."
  :type 'string
  :group 'germanium)

(defcustom germanium-background-color "#aaaaff"
  "Set background color to resulting PNG by default."
  :type 'string
  :group 'germanium)

(defcustom germanium-show-line-number t
  "Add line numbers to resulting PNG by default."
  :type 'boolean
  :group 'germanium)

(defcustom germanium-show-window-access-bar t
  "Add window access bar to the resulting PNG by default."
  :type 'boolean
  :group 'germanium)

(defcustom germanium-default-style "dracula"
  "Set default style for generated PNG."
  :type 'string
  :group 'germanium)

(defcustom germanium-check-options-each-execute-command t
  "Set Whether to check options each time the command is executed."
  :type 'boolean
  :group 'germanium)

(defcustom germanium-remove-extra-indentation t
  "Set Whether to remove extra indentation when the command executed is for region."
  :type 'boolean
  :group 'germanium)

(defcustom germanium-completion-function 'ido-completing-read
  "Function to use for completion.

Function needs to have a signature similar to `ido-completing-read', for example `ivy-completing-read'."
  :type 'function
  :group 'germanium)

(defvar germanium-available-styles nil
  "List of available germanium styles.")

(when (commandp germanium-executable-path)
  (setq-default germanium-available-styles
                (split-string (shell-command-to-string
                               (mapconcat #'shell-quote-argument
                                          (list germanium-executable-path "--list-styles")
                                          " ")))))

(defun germanium--indent-length (str)
  "Function to count indent in STR."
  (letrec ((f (lambda (lst acc)
                 (if (eq 32 (car lst)) ;; only for whitespace
                     (funcall f (cdr lst) (1+ acc))
                   acc))))
    (funcall f (coerce str 'list) 0)))

(defun germanium--remove-extra-indentation (contents)
  "Remove extra indentation from CONTENTS."
  (if (and contents germanium-remove-extra-indentation)
      (let* ((lines (split-string contents "\n"))
             (minidx (apply #'min
                         (mapcar #'(lambda (x) (germanium--indent-length x))
                                 (remove-if (lambda (x) (string= x "")) lines)))))
        (mapconcat #'identity
                   (mapcar #'(lambda (x) (if (>= (length x) minidx) (substring x minidx)))
                           lines)
                   "\n"))
    contents))

(defun germanium--build-command-options-string (&rest args)
  "Build germanium command options string from ARGS.

Supported options are `:line-number', `:window-access-bar' and `style'"
  (let* ((background-color (or (plist-get args :background-color) germanium-background-color))
         (show-line-number (and (plist-get args :line-number) germanium-show-line-number))
         (show-window-access-bar (and (plist-get args :window-access-bar) germanium-show-window-access-bar))
         (style (or (plist-get args :style) germanium-default-style)))
    (seq-remove #'null
                `(,(when (not show-line-number) "--no-line-number")
                  ,(when (not show-window-access-bar) "--no-window-access-bar")
                  ,(when background-color (format "--background=%s" background-color))
                  ,(when style (format "--style=%s" style))))))

(defun germanium--build-exec-command (file-path temp-file-path options)
  "Build germanium execute command.
It is from FILE-PATH or CONTENTS and TEMP-FILE-PATH with OPTIONS.

Output file name is based on FILE-PATH default."
  (let ((output
          (concat (file-name-base file-path) ".png")))
    (if temp-file-path
        (mapconcat #'identity
                   (append
                    (list "cat"
                          temp-file-path
                          "|"
                          germanium-executable-path
                          "--output" output
                          "-l" (file-name-extension file-path))
                    options)
                   " ")
      (mapconcat #'shell-quote-argument
                 (append
                  (list germanium-executable-path
                        "--output" output
                        file-path)
                  options)
                 " "))))

(defun germanium--exec-command (file-path temp-file-path)
  "Execute germanium command with FILE-PATH, CONTENTS and TEMP-FILE-PATH."
  (interactive)
  (let ((command-string
          (if germanium-check-options-each-execute-command
              (let ((style
                     (funcall germanium-completion-function
                              "Style: "
                              germanium-available-styles
                              nil
                              germanium-available-styles
                              germanium-default-style))
                    (background-color
                     (read-string "Background color (RGB): "
                                  germanium-background-color))
                    (show-line-number (yes-or-no-p "Add line number? "))
                    (show-window-access-bar (yes-or-no-p "Add window access bar? ")))
                (germanium--build-exec-command file-path temp-file-path
                                               (germanium--build-command-options-string :style style
                                                                                        :background-color background-color
                                                                                        :line-number show-line-number
                                                                                        :window-access-bar show-window-access-bar)))
            (germanium--build-exec-command file-path temp-file-path (germanium--build-command-options-string)))))
    (shell-command command-string)))

;;;###autoload
(defun germanium-install ()
  "Install `germanium' via `go'."
  (interactive)
  (unless (yes-or-no-p "Install `germanium' via go?")
    (user-error "Abort install"))
  (unless (executable-find "go")
    (user-error "Missing `go'.  Please ensure Emacs's PATH and is installed"))
  (shell-command "go install github.com/matsuyoshi30/germanium/cmd/germanium@latest"))

;;;###autoload
(defun germanium-region-to-png (start end)
  "Generate a PNG file from current region between START and END."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (not (commandp germanium-executable-path))
      (user-error "`germanium' executable path not found")
    (if (and start end)
        (if-let* ((file-name (buffer-file-name))
                  (file-path (expand-file-name file-name))
                  (temp-file-path (make-temp-file "germanium-region"))
                  (contents
                   (germanium--remove-extra-indentation (replace-regexp-in-string "\n$" "" (buffer-substring-no-properties start end)))))
            (progn
              (write-region contents nil temp-file-path 'append)
              (germanium--exec-command file-path temp-file-path)
              (delete-file temp-file-path))
          (user-error "Current buffer is not associated with any file"))
      (user-error "Need to select region"))))

;;;###autoload
(defun germanium-buffer-to-png ()
  "Generate a PNG file from current buffer."
  (interactive)
  (if (not (commandp germanium-executable-path))
      (user-error "`germanium' executable path not found")
    (if-let* ((file-name (buffer-file-name))
              (file-path (expand-file-name file-name)))
        (germanium--exec-command file-path nil)
      (user-error "Current buffer is not associated with any file"))))

(provide 'germanium)
;;; germanium.el ends here
