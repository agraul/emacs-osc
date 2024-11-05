;;; osc.el --- An osc porcelain inside Emacs -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;; Magit-inspired osc procelain
;;; Code:

(require 'magit-section)
(require 'transient)
(require 'with-editor)

(define-derived-mode osc-mode magit-section-mode "osc"
  "Mode for interacting with `osc'.")
(defvar osc--buffer-name "*osc-status*")

(defun osc--kill-buffer (buffer-or-name)
  "Kill BUFFER-OR-NAME if it exists."
  (when (bufferp (get-buffer buffer-or-name))
    (kill-buffer buffer-or-name)))

(defclass osc-status-section (magit-section)
  ((files :initform nil)))

;;;Commands
(defun osc-cmd-status (&optional dir)
  "Run \"osc status\" in the current package directory.

Pass the universal-argument to prompt for an alternative directory.
In Lisp code, DIR can be passed instead."
  (interactive)
  (let ((osc-dir (cond (dir dir)
                       (current-prefix-arg (osc--read-directory))
                       (t default-directory))))
    (if (or (not osc-dir) (not (osc-package osc-dir)))
        (error "%s is not an osc package directory" osc-dir)
      (osc--kill-buffer osc--buffer-name)
      (switch-to-buffer osc--buffer-name)
      ;; (erase-buffer)
      (insert (string-join (osc-info osc-dir) "\n"))
      (magit-insert-section (osc-status-section)
        (magit-insert-heading nil "Files")
        (insert (osc-run "status" osc-dir "-v")))
      (osc-mode))))

;;; Functions
;;; Running commands
(defun osc-run (subcmd &optional directory &rest args)
  "Run an osc command synchronously, specified as SUBCMD.

DIRECTORY  can be used to change the working directory for the call.
Any ARGS given will be appended to the command."
  (let ((default-directory (or directory
                               default-directory)))
    (shell-command-to-string (apply #'osc--format-cmd subcmd args))))


(defun osc-run-with-editor (subcmd buf &optional directory &rest args)
  "Run an osc command asynchronously using with-editor, specified as SUBCMD.

with-editor is used to enable editing changelogs, checkin
messages and the like.

BUF is the buffer that the output is written to.
DIRECTORY  can be used to change the working directory for the call.
Any ARGS given will be appended to the command."
  (let ((default-directory (or directory default-directory)))
    (with-editor-async-shell-command (apply #'osc--format-cmd subcmd args) buf)))

(defun osc--format-cmd (subcmd &rest args)
  "Format an osc command built from SUBCMD and ARGS."
  ;;FIXME: quote args
  (format "osc %s" (mapconcat #'identity (cons subcmd args) " ")))

;;;project / package information
;;TODO: cache per-directory
(defun osc-info (&optional dir)
  "Return information about the active osc package or project checkout.

Pass DIR to set the checkout directory. Defaults to \"default-directory\"."
  (string-split (osc-run "info" dir) "\n"))

(defun osc-package (&optional dir)
  "Return the name of the active osc package.

DIR is the directory of the active osc package. Defaults to
 \"default-directory\"."
  (when-let ((name-str (car (seq-filter (lambda (s) (string-prefix-p "Package name:" s))
                                        (osc-info dir)))))
    (nth 2 (string-split name-str))))

(defun osc-project (&optional dir)
  "Return the name of the active osc project.

DIR is the directory of the active osc package. Defaults to
 \"default-directory\"."
  (when-let ((name-str (car (seq-filter (lambda (s) (string-prefix-p "Project name:" s))
                                        (osc-info dir)))))
    (nth 2 (string-split name-str))))

(defun osc--project-p (dir)
  "Return t if DIR is a project directory."
  (and (osc-project dir)
       (not (osc-package dir))))

;; TODO: add completions
(defun osc--read-directory ()
  "Read a directory (string)."
  (read-string "Directory: " default-directory))

;;; old functions to update
(defun osc-run-results ()
  "Run `osc results' in the current package directory."
  (interactive)
  (if (osc-package)
      (progn
        (osc--cleanup-results-buffer)
        (osc-run "results" osc--results-buffer-name osc-dir)
        (switch-to-buffer osc--results-buffer-name)
        (osc-results-mode))
    (message "Not in an osc package directory.")))

(defun osc-run-vc (&optional edit-only)
  "Edit changlog interactively.

Set EDIT-ONLY to avoid creating a new entry at the top."
  (interactive "P")
  (if-let ((osc-dir (osc--find-osc-working-directory default-directory)))
      (when (osc--package-directory-p osc-dir)
        (if edit-only
            (osc-run-with-editor "vc" "*osc*" nil "--just-edit"))
        (osc-run-with-editor "vc" "*osc*"))
    (message "Not in an osc package directory.")))

(defun osc--run-remotebuildlog (project package repository architecture &optional flavor)
  "WIP porcelain for `osc remotebuildlog' will be passed through."
  (osc--cleanup-buildlog-buffer)
  (osc-run "remotebuildlog"
           osc--buildlog-buffer-name
           nil
           project
           (if flavor
               (format "%s:%s" package flavor)
             package)
           repository
           architecture)
  (switch-to-buffer osc--buildlog-buffer-name)
  (osc-buildlog-mode))

(defun osc-run-rdiff (oldprj oldpack newprj &optional newpack)
  "Run `osc rdiff'.

Like `osc', OLDPRJ OLDPACK and NEWPRJ are required. NEWPACK
defaults to OLDPACK if omitted."
  (interactive "sOld Project: \nsOld Package: \nsNew Project: \nsNew Package: ")
  (osc--cleanup-rdiff-buffer)
  (osc-run "rdiff"
           osc--rdiff-buffer-name
           nil
           oldprj
           oldpack
           newprj
           (or newpack oldpack))
  (switch-to-buffer osc--rdiff-buffer-name)
  (osc-rdiff-mode))

(defun osc-run-update ()
  "Run `osc update' in the current package directory."
  (interactive)
  (if-let ((osc-dir (osc--find-osc-working-directory default-directory)))
      (when (osc--package-directory-p osc-dir)
        (osc-run "update" "*osc*" osc-dir))))

(defun osc-add-at-point (filename)
  "Add a file to list of files tracked by `osc'.

When called interactively, use the filename at point.

Otherwise pass the file as FILENAME."
  (interactive
   (list (thing-at-point 'filename)))
  (osc-run-add filename))

(defun osc-run-add (file)
  "Add FILE to the list of files tracked by `osc'.

Must be in a package directory or a subdirectory thereof."
  (interactive "fFile: ")
  (if-let ((osc-dir (osc--find-osc-working-directory default-directory)))
      (when (osc--package-directory-p osc-dir)
        (osc-run "add" "*osc*" osc-dir file))
    (message "Not in an osc package directory.")))


(transient-define-prefix osc--transient-rdiff ()
  ["Server-side \"pretty\" diff of two packages\n"
   ("-r" "Revision N:M, n=old M=new" "--revision=")
   ("p" "Two packages" osc-run-rdiff)
   ("t" "Revision of this package" osc-run-rdiff)
   ("m" "Project meta" osc-run-rdiff)])

;; TODO: Add arguments
(transient-define-prefix osc-dispatch ()
  "Invoke an osc command."
  [("s" "status" osc-run-status)
   ("r" "results" osc-run-results)
   ("D" "rdiff" osc--transient-rdiff)
   ("u" "update" osc-run-update)])

(provide 'osc)
;;; osc.el ends here
