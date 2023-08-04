;;; defcapture.el --- A convenience macro for the Doct DSL       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Abraham Aguilar

;; Author: Abraham Aguilar <a.aguilar@ciencias.unam.mx>
;; URL: https://github.com/aggu4/defcapture
;; Keywords: convenience org
;; Description: A defun analog for org-capture templates
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (doct "3.0") (org "9.4"))
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple convenience macro for defining org capture templates.
;; It uses the Doct DSL for declarations. It has a dedicated namespace
;; for capture templates and provides convenient functions to remove
;; capture templates.
;;
;; Example of usage:
;;
;; (defcapture parent () "Parent Capture"
;;  :keys "p"
;;  :file "~/example.org"
;;  :prepend t
;;  :template ("* %{todo-state} %^{Description}"
;;             ":PROPERTIES:"
;;             ":Created: %U"
;;             ":END:"
;;             "%?"))
;;
;; (defcapture childa (parent) "First Child"
;;   :keys "a"
;;   :headline "One"
;;   :todo-state "TODO"
;;   :hook (lambda () (message "\" First Child\" selected.")))
;;
;; (defcapture childb (parent) "Second Child"
;;   :keys "b"
;;   :headline "Two"
;;   :todo-state "NEXT")
;;
;; (defcapture childc ("Parent") "Third Child"
;;   :keys "c"
;;   :headline "Three"
;;   :todo-state "MAYBE")

;;; Code:

(require 'org)
(require 'doct)
(require 'cl-lib)

;; A structure for captures
;; PARENTS is a list of strings which are the names of the capture's parents.
;; IS-CHILD is t if the capture is a child of another and nil if it isn't.
;; DECLARATION is the declaration of the capture in doct.
(cl-defstruct defcapture--capture
  parents
  is-child
  declaration)

;; The capture namespace.
(defvar defcapture--capture-namespace (make-hash-table))

(defun defcapture--capture-boundp (name)
  "Return t if NAME is bound to a capture and nil if it isn't."
  (if (and (gethash name defcapture--capture-namespace) t)
      t
    (progn
      (message "The capture %s isn't defined" name)
      nil)))

(defun defcapture--compute-children (name)
  "Search the `defcapture--capture-namespace' for the children of the capture NAME.
Return them as a list."
  (cl-loop for k being each hash-key of defcapture--capture-namespace
           using (hash-value v)
           appending
           (when (cl-member name (defcapture--capture-parents v) :test #'equal)
             (list k))))


(defun defcapture--make-declaration (name)
  "Return the declaration for defcapture--capture NAME collecting it's children."
  (let ((children (defcapture--compute-children name)))
    (if children
        (append (defcapture--capture-declaration
                  (gethash name defcapture--capture-namespace))
                (list
                 :children (mapcar #'defcapture--make-declaration
                                   children)))
      (defcapture--capture-declaration
        (gethash name defcapture--capture-namespace)))))


(defun defcapture--sync-org-captures ()
  "Sync the captures in `defcapture--capture-namespace' with `org-capture-templates'."
  (setq org-capture-templates
        (doct (cl-loop for name being each hash-key of defcapture--capture-namespace
                       using (hash-value capture)
                       appending
                       (unless (defcapture--capture-is-child capture)
                         (list (defcapture--make-declaration name)))))))


;;;###autoload
(cl-defmacro defcapture (name (&rest parents) desc &body body)
  "Define a capture NAME and, optionally, add it to the children of PARENTS.
DESC must be a string. BODY is the capture template's declarations in doct
style. After binding NAME to the capture, update org captures."
  `(if (cl-every (lambda (x) (and x t))
                 (mapcar #'defcapture--capture-boundp ',parents))
     (progn
       (puthash ',name
               (make-defcapture--capture
                :parents ',parents
                :is-child ,(and parents t)
                :declaration ',(cons desc body))
               defcapture--capture-namespace)
       (defcapture--sync-org-captures))
     (progn
       (message "The parent %s is not defined."
        (cl-find-if #'defcapture--capture-boundp ',parents)))))

;;;###autoload
(defun defcapture-generate-defcapture (name)
  "Return capture NAME's definition if NAME is `defcapture--capture-boundp'.
Otherwise return nil."
  (when (defcapture--capture-boundp name)
    (let ((capture (gethash name defcapture--capture-namespace)))
      `(defcapture ,name (,@(defcapture--capture-parents capture))
         ,@(defcapture--capture-declaration capture)))))

;;;###autoload
(defmacro defcapture-remove-capture (name)
  "Remove the capture NAME if NAME is `defcapture--capture-boundp'.
Then, sync `org-capture-templates'. Return nil if NAME is not
`defcapture--capture-boundp'."
  `(when (defcapture--capture-boundp ,name)
     (remhash ,name defcapture--capture-namespace)
     (defcapture--sync-org-captures)))


(provide 'defcapture)
;;; defcapture.el ends here
