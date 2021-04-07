;;; osc.el --- An osc porcelain inside Emacs -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;; Magit-inspired osc procelain
;;; Code:

(defun run-osc (subcmd &optional directory &rest args)
  "Run an osc command, specified as SUBCMD.

DIRECTORY  can be used to change the working directory for the call.
Any ARGS given will be appended to the command."
  (let ((default-directory (or directory
                               default-directory)))
    (if args
        (call-process "osc" nil "*osc-out*" nil subcmd (format "%s" args))
      (call-process "osc" nil "*osc-out*" nil subcmd))))

(defun run-osc-status ()
  "Run osc status in the current package directory.

Also works if the current working directory is a subdirectory of a package directory."
  (when-let ((osc-dir (osc--find-osc-working-directory)))
    (when (osc--package-directory-p osc-dir)
      (run-osc "status" osc-dir)))

(defun osc--working-directory-p (dir)
  "Return wether the passed DIR is an osc working directory or not."
  (and (file-exists-p (expand-file-name ".osc" dir))
       dir))

(defun osc--package-directory-p (osc-dir)
  "Return wether the passed OSC-DIR is an osc package working directory or not."
  (file-exists-p (expand-file-name "_package" (expand-file-name ".osc" osc-dir))))

  ;; (let (( (expand-file-name ".osc" osc-dir)))
  ;;   (file-exists-p "_package")))

(defun osc--project-directory-p (osc-dir)
  "Return wether the passed OSC-DIR is an osc project working directory or not."
  (not osc--package-directory-p osc-dir))

(defun osc--find-osc-working-directory (&optional current)
  "Recursively find an osc working directory.

The function walks the directory tree up to ~/.

The optional CURRENT directory is used for recursive calls."
  (unless current
    (setq current default-directory))
  (if (osc--working-directory-p current)
      current
    (let ((parent (file-name-directory (directory-file-name current))))
        (if (string= parent "/home/")
            nil
          (osc--find-osc-working-directory parent)))))

;; primitive test cases
(let ((default-directory "~/src/OBS/home:agraul/gccemacs"))
  (run-osc "status"))

(let ((default-directory "~/src/OBS/home:agraul/gccemacs"))
  (run-osc "dependson" nil "openSUSE_Tumbleweed" "x86_64"))

(let ((default-directory "~/src/OBS/home:agraul/gccemacs/"))
  (osc--find-osc-working-directory))

(let ((default-directory "~/src/OBS/home:agraul/gccemacs/foo/"))
  (osc--find-osc-working-directory))

(let ((default-directory "~/src/dotfiles/"))
  (osc--find-osc-working-directory))

(let ((default-directory "~/src/dotfiles/"))
  (run-osc-status))
(let ((default-directory "~/src/OBS/home:agraul/gccemacs/foo/"))
  (run-osc-status))
(let ((default-directory "~/src/OBS/home:agraul/gccemacs/"))
  (run-osc-status))

(provide 'osc)
;;; osc.el ends here
