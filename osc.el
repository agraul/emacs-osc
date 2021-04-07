;;; osc.el --- An osc porcelain inside Emacs -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;; Magit-inspired osc procelain
;;; Code:

(defun osc-run (subcmd &optional directory &rest args)
  "Run an osc command synchronously, specified as SUBCMD.

DIRECTORY  can be used to change the working directory for the call.
Any ARGS given will be appended to the command."
  (let ((default-directory (or directory
                               default-directory)))
    (if args
        (call-process "osc" nil "*osc-out*" nil subcmd (format "%s" args))
      (call-process "osc" nil "*osc-out*" nil subcmd))))

(defun osc-run-status ()
  "Run osc status in the current package directory.

Also works if the current working directory is a subdirectory of a package directory."
  (if-let ((osc-dir (osc--find-osc-working-directory default-directory)))
    (when (osc--package-directory-p osc-dir)
      (osc-run "status" osc-dir))
    (message "Not in an osc package directory.")))

(defun osc--working-directory-p (dir)
  "Return wether the passed DIR is an osc working directory or not."
  (and (file-exists-p (expand-file-name ".osc" dir))
       dir))

(defun osc--package-directory-p (osc-dir)
  "Return wether the passed OSC-DIR is an osc package working directory or not.

\(osc--working-directory-p osc-dir) must be true."
  (file-exists-p (expand-file-name "_package" (expand-file-name ".osc" osc-dir))))

(defun osc--project-directory-p (osc-dir)
  "Return wether the passed OSC-DIR is an osc project working directory or not.

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
