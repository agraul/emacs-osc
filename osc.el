;;; osc.el --- An osc porcelain inside Emacs -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;; Magit-inspired osc procelain
;;; Code:

(require 'transient)

;; REVIEW: are different modes for different outputs needed?
(define-derived-mode osc-status-mode special-mode "osc-status"
  (setq buffer-read-only t))
(defvar osc--status-buffer-name "*osc-status*")
(defun osc--cleanup-status-buffer ()
  "Kill osc-status buffer if it exists."
  (when (bufferp (get-buffer osc--status-buffer-name))
    (kill-buffer osc--status-buffer-name)))

(define-derived-mode osc-results-mode special-mode "osc-results"
  (setq buffer-read-only t))
(defvar osc--results-buffer-name "*osc-results*")
(defun osc--cleanup-results-buffer ()
  "Kill osc-results buffer if it exists."
  (when (bufferp (get-buffer osc--results-buffer-name))
    (kill-buffer osc--results-buffer-name)))

(define-derived-mode osc-rdiff-mode diff-mode "osc-rdiff"
  (setq buffer-read-only t))
(defvar osc--rdiff-buffer-name "*osc-rdiff*")
(defun osc--cleanup-rdiff-buffer ()
  "Kill osc-rdiff buffer if it exists."
  (when (bufferp (get-buffer osc--rdiff-buffer-name))
    (kill-buffer osc--rdiff-buffer-name)))

(define-derived-mode osc-buildlog-mode special-mode "osc-buildlog"
  (setq buffer-read-only t))
(defvar osc--buildlog-buffer-name "*osc-buildlog*")
(defun osc--cleanup-buildlog-buffer ()
  "Kill osc-buildlog buffer if it exists."
  (when (bufferp (get-buffer osc--buildlog-buffer-name))
    (kill-buffer osc--buildlog-buffer-name)))


;; TODO: display status using magit-section
(defun osc-run-status ()
  "Run `osc status' in the current package directory.

Also works if the current working directory is a subdirectory of a package
directory."
  (interactive)
  (if-let ((osc-dir (osc--find-osc-working-directory default-directory)))
      (when (osc--package-directory-p osc-dir)
        (osc--cleanup-status-buffer)
        (osc-run "status" osc--status-buffer-name osc-dir)
        (switch-to-buffer osc--status-buffer-name)
        (osc-status-mode))
    (message "Not in an osc package directory.")))

(defun osc-run-results ()
  "Run `osc results' in the current package directory."
  (interactive)
  (if-let ((osc-dir (osc--find-osc-working-directory default-directory)))
      (when (osc--package-directory-p osc-dir)
        (osc--cleanup-results-buffer)
        (osc-run "results" osc--results-buffer-name osc-dir)
        (switch-to-buffer osc--results-buffer-name)
        (osc-results-mode))
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
  "Run `osc rdiff'."
  (interactive "sOld Project: \nsOld Package: \nsNew Project: \nsNew Package:")
  (osc--cleanup-rdiff-buffer)
  (osc-run "rdiff"
           osc--rdiff-buffer-name
           nil
           oldprj
           oldpack
           newprj
           (or newpack oldpack))
  (switch-to-buffer osc-rdiff-buffer-name)
  (osc-rdiff-mode))

(defun osc-run-update ()
  "Run `osc update' in the current package directory."
  (interactive)
  (if-let ((osc-dir (osc--find-osc-working-directory default-directory)))
      (when (osc--package-directory-p osc-dir)
        (osc-run "update" "*osc-log*" osc-dir))))

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
        (osc-run "add" "*osc-log*" osc-dir file))
    (message "Not in an osc package directory.")))

(defun osc-run (subcmd buf &optional directory &rest args)
  "Run an osc command synchronously, specified as SUBCMD.

BUF is the buffer that the output is written to.
DIRECTORY  can be used to change the working directory for the call.
Any ARGS given will be appended to the command."
  (let ((default-directory (or directory
                               default-directory))
        (shell-command-buffer-name buf))
    (shell-command (format "osc %s" (mapconcat #'identity (cons subcmd args) " ")))))

(defun osc-package ()
  "Return the name of the active osc package."
  (if-let ((osc-dir (osc--find-osc-working-directory default-directory)))
      (when (osc--package-directory-p osc-dir)
        (with-temp-buffer
          (insert-file-contents
           (expand-file-name "_package"
                             (expand-file-name ".osc" osc-dir)))
          (string-trim (buffer-string))))
    (message "Not in an osc package directory.")))

(defun osc-project ()
  "Return the name of the active osc project."
  (if-let ((osc-dir (osc--find-osc-working-directory default-directory)))
   (with-temp-buffer
     (insert-file-contents
      (expand-file-name "_project"
                        (expand-file-name ".osc" osc-dir)))
     (string-trim (buffer-string)))
   (message "Not in an osc directory.")))

(defun osc--working-directory-p (dir)
  "Return whether the passed DIR is an osc working directory or not."
  (and (file-exists-p (expand-file-name ".osc" dir))
       dir))

(defun osc--package-directory-p (osc-dir)
  "Return whether the passed OSC-DIR is an osc package working directory or not.

\(osc--working-directory-p osc-dir) must be true."
  (file-exists-p (expand-file-name "_package" (expand-file-name ".osc" osc-dir))))

(defun osc--project-directory-p (osc-dir)
  "Return whether the passed OSC-DIR is an osc project working directory or not.

\(osc--working-directory-p osc-dir) must be true."
  (not (osc--package-directory-p osc-dir)))

(defun osc--find-osc-working-directory (dir)
  "Recursively find an osc working directory.

The function walks the directory tree up to ~/.

The starting point for finding the directory is specified by DIR."
  (if (osc--working-directory-p dir)
      dir
    (let ((parent (file-name-directory (directory-file-name dir))))
        (if (string= parent "/home/")
            nil
          (osc--find-osc-working-directory parent)))))

;; TODO: Add arguments
(transient-define-prefix osc-dispatch ()
  "Invoke an osc command."
  [("s" "status" osc-run-status)
   ("r" "results" osc-run-results)
   ("D" "rdiff"  osc-run-rdiff)
   ("u" "update" osc-run-update)])

(provide 'osc)
;;; osc.el ends here
