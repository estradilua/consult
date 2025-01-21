;;; consult-flymake.el --- Provides the command `consult-flymake' -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Provides the command `consult-flymake'.  This is an extra package,
;; to allow lazy loading of flymake.el.  The `consult-flymake' command
;; is autoloaded.

;;; Code:

(require 'consult)
(require 'flymake)
(eval-when-compile (require 'cl-lib))

(defconst consult-flymake--narrow
  '((?e . "Error")
    (?w . "Warning")
    (?n . "Note")))

(defun consult-flymake--candidates (diags)
  "Return Flymake errors from DIAGS as formatted candidates.
DIAGS should be a list of diagnostics as returned from `flymake-diagnostics'."
  (cl-loop
   for diag in diags
   for buffer = (flymake-diagnostic-buffer diag)
   for file = (if (bufferp buffer)
                  (buffer-file-name buffer)
                buffer)
   for (line . col) =
   (cond* (;; has live overlay, use overlay for position
           (buffer-live-p buffer)
           (with-current-buffer buffer
             (save-excursion
               (without-restriction
                 (goto-char (flymake-diagnostic-beg diag))
                 (cons (line-number-at-pos)
                       (- (point)
                          (line-beginning-position)))))))
         (;; diagnostic not annotated, maybe foreign, check for cons
          (match* (cons line col) (flymake-diagnostic-beg diag))
          (cons line (1- col)))
         (;; may still be a valid foreign diagnostic
          (match* (cons line col) (flymake--diag-orig-beg diag))
          (cons line (1- col))))
   for type = (flymake-diagnostic-type diag)
   when (and line col) maximize (length (file-name-nondirectory file)) into buffer-width
   when (and line col) maximize (length (number-to-string line)) into line-width
   when (and line col) collect
   (list (file-name-nondirectory file)
         file
         line
         col
         type
         (flymake-diagnostic-oneliner diag t)
         (pcase (flymake--lookup-type-property type 'flymake-category)
           ('flymake-error ?e)
           ('flymake-warning ?w)
           (_ ?n)))
   into diags
   finally return
   (if diags
       (let ((fmt (format "%%-%ds %%-%dd %%-7s %%s" buffer-width line-width)))
         (message "%d and %d" buffer-width line-width)
         (mapcar
           (pcase-lambda (`(,buffer ,file ,line ,col ,type ,text ,narrow))
             (propertize (format fmt buffer line
                                 (propertize (format "%s" (flymake--lookup-type-property
                                                           type 'flymake-type-name type))
                                             'face (flymake--lookup-type-property
                                                    type 'mode-line-face 'flymake-error))
                                 text)
                         'consult--candidate (list file line col)
                         'consult--type narrow))
           ;; Sort by buffer, severity and position.
           (sort diags
                 (pcase-lambda (`(,b1 _ ,l1 ,c1 ,t1 _ _) `(,b2 _ ,l2 ,c2 ,t2 _ _))
                   (let ((s1 (flymake--severity t1))
                         (s2 (flymake--severity t2)))
                     (or (string-lessp b1 b2)
                         (and (string-equal b1 b2)
                              (or (> s1 s2)
                                  (and (= s1 s2)
                                       (or (< l1 l2)
                                           (and (= l1 l2)
                                                (< c1 c2))))))))))))
     (user-error "No flymake errors (Status: %s)"
                 (if (seq-difference (flymake-running-backends)
                                     (flymake-reporting-backends))
                     'running 'finished)))))

(defun consult--flymake-position (cand &optional find-file)
  (pcase cand
    (`(,file ,line ,col)
     (consult--marker-from-line-column
       (funcall (or find-file #'consult--file-action) file)
       line col))))

(defun consult--flymake-state ()
  "Flymake state function."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (action cand)
      (unless cand
        (funcall open))
      (funcall jump action (consult--flymake-position
                            cand
                            (unless (eq action 'return) open))))))


;;;###autoload
(defun consult-flymake (&optional project)
  "Jump to Flymake diagnostic.
When PROJECT is non-nil then prompt with diagnostics from all
buffers in the current project instead of just the current buffer."
  (interactive "P")
  (consult--forbid-minibuffer)
  (consult--read
   (consult-flymake--candidates
     (if-let ((project (and project (project-current))))
         (flymake--project-diagnostics project)
       (flymake-diagnostics)))
   :prompt "Flymake diagnostic: "
   :category 'consult-flymake-error
   :history t ;; disable history
   :require-match t
   :sort nil
   :group (consult--type-group consult-flymake--narrow)
   :narrow (consult--type-narrow consult-flymake--narrow)
   :lookup #'consult--lookup-candidate
   :state (consult--flymake-state)))

(provide 'consult-flymake)
;;; consult-flymake.el ends here
