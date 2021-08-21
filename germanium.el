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

(defun germanium--exec-command (file-path contents)
  "Build germanium execute command from FILE-PATH or CONTENTS.

Output file name is based on FILE-PATH default."
  (let ((output
          (concat (file-name-base file-path) ".png")))
    (if contents
        (mapconcat #'identity
                   (list "echo"
                           (shell-quote-argument contents)
                           "|"
                           germanium-executable-path
                           "--output" output
                           "-l" (file-name-extension file-path))
                   " ")
        (mapconcat #'shell-quote-argument
                   (list germanium-executable-path
                         "--output" output
                         file-path)
                   " "))))

;;;###autoload
(defun germanium-install ()
  "Install `germanium' via `go'."
  (interactive)
  (unless (yes-or-no-p "Install `germanium' via go?")
    (error "Abort install"))
  (unless (executable-find "go")
    (error "Missing `go'.  Please ensure Emacs's PATH and the installing"))
  (shell-command "go install github.com/matsuyoshi30/germanium/cmd/germanium@latest"))

;;;###autoload
(defun germanium-region-to-png (start end)
  "Generate a PNG file from current region between START and END."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (not (commandp germanium-executable-path))
      (error "`germanium' executable path not found")
    (if (and start end)
         (if-let* ((file-name (buffer-file-name))
                   (file-path (expand-file-name file-name))
                   (contents (buffer-substring-no-properties start end)))
             (let* ((command-string
                     (germanium--exec-command file-path contents)))
               (if (not (= 0 (shell-command command-string)))
                   (error "Failed to generate image"))))
      (error "Need to select region"))))

;;;###autoload
(defun germanium-buffer-to-png ()
  "Generate a PNG file from current buffer."
  (interactive)
  (if (not (commandp germanium-executable-path))
      (error "`germanium' executable path not found")
    (if-let* ((file-name (buffer-file-name))
              (file-path (expand-file-name file-name)))
        (let* ((command-string
                (germanium--exec-command file-path nil)))
          (if (not (= 0 (shell-command command-string)))
              (error "Failed to generate image")))
    (error "Current buffer is not associated with any file"))))

(provide 'germanium)
;;; germanium.el ends here
