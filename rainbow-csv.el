;;; rainbow-csv.el --- Highlight CSV and TSV files in different rainbow colors  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Shen, Jen-Chieh

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
  '("#CCCCCC"
    "#569CD6"
    "#DCCD79"
    "#529955"
    "#CE834A"
    "#8CDCFE"
    "#B5C078"
    "#4EC9B0"
    "#569CD6"
    "#F44747")
  "List of colors to use."
  :type 'list
  :group 'rainbow-csv)

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
  "Not documented (SEPARATOR)."
  (interactive (list (when current-prefix-arg (read-char "Separator: "))))
  (rainbow-csv--revert-font-lock-keywords)
  (font-lock-mode 1)
  (let* ((separator (or separator (cl-case major-mode
                                    (`tsv-mode ?\t)
                                    (`csv-mode ?\,)
                                    (t ?\,))))
         (n (save-excursion
              (goto-char (point-min))
              (search-forward "," nil t)
              (count-matches (string separator)
                             (line-beginning-position) (line-end-position))))
         (n (1+ n)))
    (dotimes (i n)
      (let* ((r (format "^\\([^%c\n]+%c\\)\\{%d\\}"
                        separator separator (1+ i)))
             (len (length rainbow-csv-colors))
             (color (nth (% i len) rainbow-csv-colors)))
        (setq csv-font-lock-keywords
              (append csv-font-lock-keywords
                      `((,r (1 '(face (:foreground ,color)) prepend t))))))))
  (font-lock-refresh-defaults))

(provide 'rainbow-csv)
;;; rainbow-csv.el ends here
