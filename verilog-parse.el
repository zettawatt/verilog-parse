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
(require 'eieio-base)
(require 'eieio-speedbar)

(defvar *svlog-defines* nil
  "Hash table of all `defines in the design")

(defvar *svlog-modules* nil
  "List of all modules in the design")

(defvar *svlog-objects-file* nil
  "File to store all instances of SV objects")

;; Base class of all objects
(defclass svlog-object (eieio-instance-tracker eieio-persistent)
  ((buffer
    :initarg :buffer
    :accessor buffer
    :documentation "The buffer containing the SV object. If buffer is closed,
open the file indicated by 'svlog-file'")
   (comment
    :initarg :comment
    :accessor comment
    :documentation "Comment header found describing the SV object")
   (svlog-file
    :initarg :svlog-file
    :initform (error "Must supply a file name of where to find the object.")
    :accessor svlog-file
    :documentation "The file in which to find the SV object.")
   (file
    :initarg :file
    :initform *svlog-objects-file*
    :accessor file
    :allocation :class
    :documentation "The file in which to save SV objects")
   (name
    :initarg :name
    :initform (error "Must supply an object name.")
    :accessor name
    :documentation "The name of the SV object.")))

(defgeneric svlog-goto-defun (object)
  "Go to the SV object definition or assignment.")

(defgeneric svlog-goto-load (object)
  "List the SV object instances or loads.")

;; Block type classes
(defclass svlog-block (svlog-object eieio-speedbar-directory-button)
  ()
  "Block type classes of SV objects")

(defclass svlog-module (svlog-block)
  ((parent-module
    :initarg :parent-module
    :accessor parent-module
    :documentation "The parent module this module is instantiated in.")
   (child-modules
    :initarg :child-modules
    :accessor child-modules
    :documentation "Associative array of child modules that are instantiated in this module.")
   (ports
    :initarg :ports
    :accessor ports
    :documentation "Associative array of all ports of this module.")
   (signals
    :initarg :signals
    :accessor signals
    :documentation "Associative array of all local signals of this module.")
   (tracker-symbol
    :initarg :tracker-symbol
    :initform '*svlog-modules*
    :accessor tracker-symbol
    :documentation "List of all module objects."))
  "SV 'module' object")

(defclass svlog-package (svlog-block)
  ()
  "SV 'package' object")

(defclass svlog-interface (svlog-block)
  ()
  "SV 'interface' object")

(defclass svlog-class (svlog-block)
  ()
  "SV 'class' object")

(defclass svlog-task-function (svlog-block)
  ()
  "SV 'function' and 'task' object")


;; Signal type classes
(defclass svlog-signal (svlog-object eieio-speedbar-file-button)
  ()
  "Signal type classes of SV objects")

(defclass svlog-input (svlog-signal)
  ()
  "Module 'input' object")

(defclass svlog-output (svlog-signal)
  ()
  "Module 'output' object")

(defclass svlog-inout (svlog-input svlog-output)
  ()
  "Module 'inout' object")

(defclass svlog-var (svlog-signal)
  ()
  "Module local variable or wire object")

(defclass svlog-struct (svlog-signal)
  ()
  "SV 'struct' or 'enum' objects")


;; Constant type classes
(defclass svlog-parameter (svlog-signal)
  ()
  "SV 'parameter' object")

(defclass svlog-constant (svlog-signal)
  ()
  "SV 'const' or 'localparam' objects")


(defun verparse-parse-source-files files
  "Parse all verilog files in FILES and update '*verilog-parse-database*'")

(provide 'verilog-parse)
;;; verilog-parse.el ends here
