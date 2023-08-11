;;; osc.el --- An osc porcelain inside Emacs -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;; Magit-inspired osc procelain
;;; Code:

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

(defun osc-run-status ()
  "Run osc status in the current package directory.

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
  (interactive)
  (if-let ((osc-dir (osc--find-osc-working-directory default-directory)))
      (when (osc--package-directory-p osc-dir)
        (osc--cleanup-results-buffer)
        (osc-run "results" osc--results-buffer-name osc-dir)
        (switch-to-buffer osc--results-buffer-name)
        (osc-results-mode))
    (message "Not in an osc package directory.")))

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
                               default-directory)))
    (if args
        (progn
          (message "%s" args)
          (call-process "osc" nil buf nil subcmd (mapconcat 'identity args " ")))
      (call-process "osc" nil buf nil subcmd))))

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

(provide 'osc)
;;; osc.el ends here
