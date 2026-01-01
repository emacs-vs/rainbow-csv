;;; rainbow-csv.el --- Highlight CSV and TSV files in different rainbow colors  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/rainbow-csv
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (csv-mode "1.22"))
;; Keywords: convenience csv

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
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Highlight CSV and TSV files in different rainbow colors.
;;

;;; Code:

(require 'csv-mode)

(defgroup rainbow-csv nil
  "Highlight CSV and TSV spreadsheet files in different rainbow colors."
  :prefix "rainbow-csv-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/emacs-vs/rainbow-csv"))

(defcustom rainbow-csv-colors
  (cond
   ((eq 'light (frame-parameter nil 'background-mode))
    '("#333333"
      "#A96329"
      "#233286"
      "#AD66AA"
      "#317CB5"
      "#732301"
      "#4A3F87"
      "#B1364F"
      "#A96329"
      "#0BB8B8"))
   (t
    '("#CCCCCC"
      "#569CD6"
      "#DCCD79"
      "#529955"
      "#CE834A"
      "#8CDCFE"
      "#B5C078"
      "#4EC9B0"
      "#569CD6"
      "#F44747")))
  "List of colors to use."
  :type '(list color)
  :group 'rainbow-csv)

(defcustom rainbow-csv-separators nil
  "Alist map mode to separator."
  :type '(list (cons symbol character))
  :group 'rainbow-csv)
(make-obsolete 'rainbow-csv-separators 'csv-separators "1.0.0")

(defvar rainbow-csv--old-csv-font-lock-keywords nil
  "Store the old value for variable `csv-font-lock-keywords'.")

;;
;; (@* "Entry" )
;;

(defun rainbow-csv--enable ()
  "Enable `rainbow-csv' in current buffer."
  (rainbow-csv-highlight))

(defun rainbow-csv--disable ()
  "Disable `rainbow-csv' in current buffer."
  (rainbow-csv-revert-font-lock))

;;;###autoload
(define-minor-mode rainbow-csv-mode
  "Minor mode `rainbow-csv-mode'."
  :lighter " RainbowCSV"
  :group rainbow-csv
  (if rainbow-csv-mode (rainbow-csv--enable) (rainbow-csv--disable)))

;;
;; (@* "Core" )
;;

(defun rainbow-csv--revert-font-lock-keywords ()
  "Revert to default font lock rules."
  (unless rainbow-csv--old-csv-font-lock-keywords
    (setq rainbow-csv--old-csv-font-lock-keywords csv-font-lock-keywords))
  (setq csv-font-lock-keywords rainbow-csv--old-csv-font-lock-keywords))

(defun rainbow-csv-revert-font-lock ()
  "Revert to default font lock rules interactively."
  (interactive)
  (rainbow-csv--revert-font-lock-keywords)
  (font-lock-refresh-defaults))

;;;###autoload
(defun rainbow-csv-highlight (&optional separator)
  "Highlight CSV with rainbow colors, optionally select SEPARATOR."
  (interactive (list (when current-prefix-arg (read-char "Separator: "))))
  (rainbow-csv--revert-font-lock-keywords)
  (font-lock-mode 1)
  (when-let* ((separator (or separator
                             (csv-guess-separator ; From `csv-guess-set-separator'
                              (buffer-substring-no-properties
                               (point-min) (min 8192 (point-max)))
                              2048)))
              (fields
               (save-excursion
                 (goto-char (point-min))
                 (seq-filter (lambda (s) (not (string-empty-p s))) (csv--collect-fields (line-end-position))))))
    ;; Is the first field quoted?
    (let* ((quote-char (string-match-p (format "^\\([%s]\\).*\\1$" (string-join csv-field-quotes)) (car fields)))
           (quote-char (and quote-char (substring (car fields) nil 1)))) ; Get the first char
      (dotimes (i (length fields))
        (let* ((r (if quote-char
                      (format "^\\(%s[^%s]*%s[%c\n]\\)\\{%d\\}"
                              quote-char quote-char quote-char separator (1+ i))
                    (format "^\\([^%c\n]*[%c\n]\\)\\{%d\\}"
                            separator separator (1+ i))))
               (len (length rainbow-csv-colors))
               (color (nth (% i len) rainbow-csv-colors)))
          (setq csv-font-lock-keywords
                (append csv-font-lock-keywords
                        `((,r (1 '(face (:foreground ,color)) prepend t)))))))))
  (font-lock-refresh-defaults))


(provide 'rainbow-csv)
;;; rainbow-csv.el ends here
