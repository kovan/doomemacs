;;; lisp/cli/gc.el --- clean up after profiles, packages, and logs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(doom-require 'doom-lib 'packages)


;;
;;; Variables

;; None yet!


;;
;;; Helpers

(defun doom-gc--guix-profile ()
  "Run garbage collection on Doom's Guix profile."
  (if (not (file-directory-p doom-guix-profile-dir))
      (prog1 0
        (print! (item "No Guix profile to clean")))
    (print! (start "Cleaning Guix profile..."))
    (print-group!
      (let ((result (doom-guix--call "gc"
                                     (format "--profile=%s" doom-guix-profile-dir))))
        (if (zerop (car result))
            (progn
              (print! (success "Guix garbage collection complete"))
              1)
          (print! (warn "Guix gc failed: %s") (cdr result))
          0)))))

(defun doom-gc--guix-old-generations ()
  "Delete old Guix profile generations, keeping only the current one."
  (if (not (file-directory-p doom-guix-profile-dir))
      (prog1 0
        (print! (item "No Guix generations to purge")))
    (print! (start "Purging old Guix profile generations..."))
    (print-group!
      (let ((result (doom-guix--call "package"
                                     (format "--profile=%s" doom-guix-profile-dir)
                                     "--delete-generations")))
        (if (zerop (car result))
            (progn
              (print! (success "Old generations purged"))
              1)
          (print! (warn "Failed to purge generations: %s") (cdr result))
          0)))))

(defun doom-gc--elpa ()
  (let ((dirs (doom-files-in package-user-dir :type t :depth 0)))
    (if (not dirs)
        (prog1 0
          (print! (item "No ELPA packages to purge")))
      (print! (start "Purging ELPA packages..."))
      (dolist (path dirs (length dirs))
        (condition-case e
            (print-group!
              (if (file-directory-p path)
                  (delete-directory path 'recursive)
                (delete-file path))
              (print! (success "Deleted %s") (filename path)))
          (error
           (print! (error "Failed to delete %s because: %s")
                   (filename path)
                   e)))))))

(defun doom-gc--eln ()
  (if-let* ((dirs
             (cl-delete (expand-file-name comp-native-version-dir doom-packages--eln-output-path)
                        (when (file-directory-p doom-packages--eln-output-path)
                          (directory-files doom-packages--eln-output-path t "^[^.]" t))
                        :test #'file-equal-p)))
      (progn
        (print! (start "Purging old native bytecode..."))
        (print-group!
          (dolist (dir dirs)
            (print! (item "Deleting %S") (relpath dir doom-packages--eln-output-path))
            (delete-directory dir 'recursive))
          (print! (success "Purged %d directory(ies)" (length dirs))))
        (length dirs))
    (print! (item "No ELN directories to purge"))
    0))

(defun doom-gc--channel ()
  "Clean up the Guix channel directory.
Only channels.scm is stored here now; it will be regenerated on next pull."
  (if (not (file-directory-p doom-guix-channel-dir))
      (prog1 0
        (print! (item "No Guix channel directory to clean")))
    (print! (item "Channel config will be regenerated on next pull"))
    0))

;;
;;; Commands

(defcli! (gc)
    ((noguix-p   ("-g" "--no-guix")    "Don't run Guix garbage collection")
     (noelpa-p   ("-p" "--no-elpa")    "Don't purge ELPA packages")
     (noeln-p    ("-e" "--no-eln")     "Don't purge old ELN bytecode")
     (nogens-p   ("-G" "--no-generations") "Don't purge old Guix profile generations"))
  "Deletes orphaned packages & repos, and compacts them.

Purges all installed ELPA packages (as they are considered temporary). Runs
Guix garbage collection on Doom's profile and purges old profile generations.

It is a good idea to occasionally run this command to ensure your package list
remains lean."
  :benchmark t
  :group 'emacs
  (require 'comp nil t)
  (doom-initialize-packages)
  (doom-packages--barf-if-incomplete)
  (print! (start "Purging orphaned packages (for the emperor)..."))
  (print-group!
    (delq
     nil (list
          (if noguix-p
              (ignore (print! (item "Skipping Guix gc")))
            (/= 0 (doom-gc--guix-profile)))
          (if nogens-p
              (ignore (print! (item "Skipping generation purge")))
            (/= 0 (doom-gc--guix-old-generations)))
          (if noelpa-p
              (ignore (print! (item "Skipping elpa packages")))
            (/= 0 (doom-gc--elpa)))
          (when (featurep 'native-compile)
            (if noeln-p
                (ignore (print! (item "Skipping native bytecode")))
              (doom-gc--eln))))))
  t)

(provide 'doom-cli-gc)
;;; gc.el ends here
