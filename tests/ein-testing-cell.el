;;; ein-testing-cell.el --- Testing utilities for cell module

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-testing-cell.el is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; ein-testing-cell.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-testing-cell.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'json)

(defun ein:testing-codecell-data (&optional input prompt-number outputs)
  (list :cell_type "code"
        :input (or input "")
        :language "python"
        :outputs outputs
        :collapsed json-false
        :prompt_number prompt-number))

(defun ein:testing-textcell-data (&optional source cell-type)
  (list :cell_type cell-type
        :source (or source "")))

(defun ein:testing-markdowncell-data (&optional source)
  (ein:testing-textcell-data source "markdown"))

(defun ein:testing-rawcell-data (&optional source)
  (ein:testing-textcell-data source "raw"))

(defun ein:testing-htmlcell-data (&optional source)
  (ein:testing-textcell-data source "html"))

(defun ein:testing-headingcell-data (&optional source level)
  (append (ein:testing-textcell-data source "heading")
          (list :level (or level 1))))

(provide 'ein-testing-cell)

;;; ein-testing-cell.el ends here