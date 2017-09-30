;;; verilog-parse.el --- Verilog/SystemVerilog parser and IDE -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2017 Chuck McClish

;; Author:           Chuck McClish <huntedbysparrwos@gmail.com>
;;                   Tim McClish <>
;; Maintainer:       Chuck McClish <huntedbysparrwos@gmail.com>
;;                   Tim McClish <>
;; URL:              http://github.com/zettawatt/verilog-parse
;; Version:          0.0.0
;; Created:          30 Sep 2017
;; Keywords:         verilog, SystemVerilog, ide, asic, hdl
;; Compatibility:    GNU 25.x
;; Package-Requires: ((emacs "25") (verilog-mode))

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; The goal of this project is to create a useful IDE for Verilog
;; and SystemVerilog written entirely in Emacs Lisp. This will
;; replace the other verilog parser system written in Perl called
;; 'verparse'.

;; 'verlog-parse' will utilize 'verilog-mode' functions for parsing
;; verilog files.

;;; Configuration:

;; FIXME: User configuration info here

;;; Code:

;; Required Packages
(require 'verilog-mode)

(defvar *verilog-parse-database* nil
  "Hash table of all information parsed by 'verparse-parse-source-files'")

(defun verparse-parse-source-files files
  "Parse all verilog files in FILES and update '*verilog-parse-database*'")

(provide 'verilog-parse)
;;; verilog-parse.el ends here
