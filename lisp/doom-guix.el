;;; lisp/doom-guix.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Doom's package management backend using the Guix package manager. This
;; replaces straight.el with a system that generates per-package Guix
;; definitions in a local channel and manages them through a dedicated Guix
;; profile.
;;
;; Packages are declared in modules' packages.el files via the `package!'
;; macro, which populates `doom-packages'. At sync time, Doom translates these
;; declarations into Guix package definitions (Guile Scheme), writes them to a
;; local channel, and installs them into a Doom-specific Guix profile.
;;
;; The guix-emacs channel (a community fork mirroring MELPA) provides base
;; definitions for most Emacs packages. Doom's local channel overrides these
;; with pinned commits when `:pin' is specified.
;;
;; Interacting with this system is done through Doom's bin/doom script:
;;
;; - `doom install': sets up Guix channels and builds the initial profile.
;; - `doom sync': regenerates the local channel and rebuilds the profile.
;; - `doom upgrade': pulls channel updates and rebuilds.
;;
;;; Code:

;;
;;; Variables

(defvar doom-guix-profile-dir
  (file-name-concat doom-local-dir "guix-profile")
  "Path to Doom's dedicated Guix profile.
All Emacs packages managed by Doom are installed into this profile.")

(defvar doom-guix-channel-dir
  (file-name-concat doom-local-dir "guix-channel")
  "Path to Doom's local Guix channel.
Contains generated per-package definitions with pinned commits.")

(defvar doom-guix-emacs-channel-url
  "https://github.com/garrgravarr/guix-emacs"
  "URL of the guix-emacs channel that mirrors MELPA packages.")

(defvar doom-guix-emacs-channel-branch "main"
  "Branch of the guix-emacs channel to use.")

(defvar doom-guix-name-map
  '()
  "Alist mapping Doom package names (symbols) to Guix package name strings.
Most packages follow the `emacs-<name>' convention and don't need entries here.
Only add packages whose Guix name deviates from that pattern.")

(defvar doom-guix-hash-cache-file
  (file-name-concat doom-local-dir "guix-hash-cache.el")
  "File caching source hashes for pinned packages.
Maps (repo . commit) pairs to base32 NAR hashes.")

(defvar doom-guix--hash-cache nil
  "In-memory cache of source hashes. Loaded from `doom-guix-hash-cache-file'.")

(defvar doom-guix--retries 3
  "Number of times to retry failed guix operations.")


;;
;;; Core utilities

(defun doom-guix--call (&rest args)
  "Call guix with ARGS synchronously. Returns (STATUS . OUTPUT).
Signals an error if guix is not found."
  (doom-guix--ensure-guix)
  (apply #'doom-call-process "guix" args))

(defun doom-guix--call! (&rest args)
  "Like `doom-guix--call' but signals an error on non-zero exit."
  (let ((result (apply #'doom-guix--call args)))
    (unless (zerop (car result))
      (error "guix %s failed (%d): %s"
             (string-join args " ")
             (car result)
             (cdr result)))
    result))

(defun doom-guix--ensure-guix ()
  "Ensure the guix executable is available on PATH."
  (unless (executable-find "guix")
    (user-error "Guix is not installed or not on PATH. \
Doom requires Guix for package management. \
See https://guix.gnu.org/manual/en/html_node/Installation.html")))


;;
;;; Package name mapping

(defun doom-guix--package-name (doom-name)
  "Convert Doom package name DOOM-NAME (a symbol) to its Guix package name.
Checks `doom-guix-name-map' first, then the package's `:guix-name' property,
then falls back to the `emacs-<name>' convention."
  (or (alist-get doom-name doom-guix-name-map)
      (when-let* ((plist (cdr (assq doom-name doom-packages))))
        (plist-get plist :guix-name))
      (concat "emacs-" (symbol-name doom-name))))

(defun doom-guix--recipe-url (recipe)
  "Extract a git URL from a Doom package RECIPE plist.
Supports :host/:repo shorthand and direct :url."
  (or (plist-get recipe :url)
      (when-let* ((host (plist-get recipe :host))
                  (repo (plist-get recipe :repo)))
        (format "https://%s/%s"
                (pcase host
                  ('github "github.com")
                  ('gitlab "gitlab.com")
                  ('bitbucket "bitbucket.org")
                  ('codeberg "codeberg.org")
                  ('sourcehut "git.sr.ht")
                  (_ (symbol-name host)))
                repo))))


;;
;;; Source hash management

(defun doom-guix--load-hash-cache ()
  "Load the hash cache from disk."
  (setq doom-guix--hash-cache
        (or (doom-file-read doom-guix-hash-cache-file :by 'read :noerror t)
            (make-hash-table :test #'equal))))

(defun doom-guix--save-hash-cache ()
  "Save the hash cache to disk."
  (when doom-guix--hash-cache
    (make-directory (file-name-directory doom-guix-hash-cache-file) t)
    (with-temp-file doom-guix-hash-cache-file
      (prin1 doom-guix--hash-cache (current-buffer)))))

(defun doom-guix--source-hash (url commit)
  "Get or compute the NAR hash for a git source at URL pinned to COMMIT.
Results are cached in `doom-guix--hash-cache'."
  (unless doom-guix--hash-cache
    (doom-guix--load-hash-cache))
  (let ((key (cons url commit)))
    (or (gethash key doom-guix--hash-cache)
        (let* ((result (doom-guix--call
                        "hash" "--serializer=nar" "--format=base32"
                        "--exclude-vcs"
                        ;; Fetch and hash in one shot using a git checkout
                        (format "--git-url=%s" url)
                        (format "--git-commit=%s" commit)))
               (hash (and (zerop (car result))
                          (string-trim (cdr result)))))
          (unless hash
            ;; Fallback: clone to temp dir and hash
            (let ((tmpdir (make-temp-file "doom-guix-" t)))
              (unwind-protect
                  (progn
                    (doom-call-process "git" "clone" "--depth" "1" url tmpdir)
                    (let ((default-directory tmpdir))
                      (doom-call-process "git" "fetch" "origin" commit "--depth" "1")
                      (doom-call-process "git" "checkout" commit))
                    (let ((result2 (doom-guix--call "hash" "--serializer=nar"
                                                    "--format=base32"
                                                    "--exclude-vcs" tmpdir)))
                      (setq hash (and (zerop (car result2))
                                      (string-trim (cdr result2))))))
                (delete-directory tmpdir t))))
          (when hash
            (puthash key hash doom-guix--hash-cache))
          hash))))


;;
;;; Channel generation

(defun doom-guix--write-channel-metadata ()
  "Write the .guix-channel file for Doom's local channel."
  (let ((channel-file (file-name-concat doom-guix-channel-dir ".guix-channel")))
    (make-directory doom-guix-channel-dir t)
    (with-temp-file channel-file
      (insert ";; Doom Emacs local Guix channel\n")
      (insert ";; Auto-generated by doom sync. Do not edit.\n")
      (insert "(channel\n")
      (insert "  (version 0)\n")
      (insert "  (url \"file://" doom-guix-channel-dir "\"))\n"))))

(defun doom-guix--generate-package-definition (name recipe pin)
  "Generate a Guix package definition string for package NAME.
RECIPE is the Doom recipe plist. PIN is a commit hash string."
  (let* ((guix-name (doom-guix--package-name name))
         (url (or (doom-guix--recipe-url recipe)
                  ;; Default: assume it's on MELPA/GitHub
                  (format "https://github.com/emacs-straight/%s" (symbol-name name))))
         (hash (when pin (doom-guix--source-hash url pin)))
         (files (plist-get recipe :files))
         (build (plist-get recipe :build))
         (pre-build (plist-get recipe :pre-build))
         (branch (plist-get recipe :branch))
         (deps (plist-get recipe :depends)))
    (concat
     (format "(define-public %s\n" guix-name)
     (if pin
         (concat
          (format "  (let ((commit \"%s\")\n" pin)
          "        (revision \"1\"))\n"
          "    (package\n"
          (format "      (name \"%s\")\n" guix-name)
          "      (version (git-version \"0\" revision commit))\n"
          "      (source\n"
          "       (origin\n"
          "        (method git-fetch)\n"
          "        (uri (git-reference\n"
          (format "              (url \"%s\")\n" url)
          "              (commit commit)))\n"
          "        (file-name (git-file-name name version))\n"
          (if hash
              (format "        (sha256\n         (base32 \"%s\"))))\n" hash)
            "        (sha256\n         (base32 \"0000000000000000000000000000000000000000000000000000\"))))\n"))
       ;; Unpinned: inherit from guix-emacs channel
       (concat
        "  (package\n"
        (format "    (name \"%s\")\n" guix-name)
        "    (version \"latest\")\n"
        "    (source #f)\n"))
     ;; Build system
     "      (build-system emacs-build-system)\n"
     ;; Files filter
     (when files
       (format "      (arguments\n       '(#:include '(%s)))\n"
               (mapconcat (lambda (f) (format "\"%s\"" f))
                          (if (eq (car-safe files) :defaults)
                              (cdr files)
                            files)
                          " ")))
     ;; Dependencies
     (when deps
       (format "      (propagated-inputs\n       (list %s))\n"
               (mapconcat (lambda (d) (doom-guix--package-name d))
                          deps " ")))
     "      (home-page \"https://doomemacs.org\")\n"
     "      (synopsis \"Doom Emacs managed package\")\n"
     "      (description \"Package managed by Doom Emacs.\")\n"
     "      (license license:gpl3+))"
     (if pin "\n)" "")
     "\n")))

(defun doom-guix--generate-channel-module ()
  "Generate the Guile module file containing all package definitions.
Writes to doom-guix-channel-dir/doom/packages.scm."
  (let ((module-dir (file-name-concat doom-guix-channel-dir "doom")))
    (make-directory module-dir t)
    (with-temp-file (file-name-concat module-dir "packages.scm")
      (insert ";; Doom Emacs package definitions for Guix\n")
      (insert ";; Auto-generated by doom sync. Do not edit.\n\n")
      (insert "(define-module (doom packages)\n")
      (insert "  #:use-module (guix packages)\n")
      (insert "  #:use-module (guix git-download)\n")
      (insert "  #:use-module (guix build-system emacs)\n")
      (insert "  #:use-module ((guix licenses) #:prefix license:)\n")
      (insert "  #:use-module (guix utils))\n\n")
      ;; Generate a definition for each package that has a pin or custom recipe
      (dolist (package doom-packages)
        (cl-destructuring-bind (name &key recipe pin disable ignore type
                                     &allow-other-keys)
            package
          (unless (or disable ignore
                      (memq type '(built-in virtual))
                      (and (not pin) (not recipe)))
            (insert (doom-guix--generate-package-definition name recipe pin))
            (insert "\n")))))))

(defun doom-guix--generate-manifest ()
  "Generate a Guix manifest file listing all enabled packages.
Writes to doom-guix-channel-dir/doom-manifest.scm."
  (with-temp-file (file-name-concat doom-guix-channel-dir "doom-manifest.scm")
    (insert ";; Doom Emacs package manifest for Guix\n")
    (insert ";; Auto-generated by doom sync. Do not edit.\n\n")
    (insert "(use-modules (doom packages)\n")
    (insert "             (guix packages)\n")
    (insert "             (gnu packages emacs-xyz))\n\n")
    (insert "(packages->manifest\n")
    (insert "  (list\n")
    (dolist (package doom-packages)
      (cl-destructuring-bind (name &key disable ignore type &allow-other-keys)
          package
        (unless (or disable ignore (memq type '(built-in virtual)))
          (insert (format "    %s\n" (doom-guix--package-name name))))))
    (insert "  ))\n")))

(defun doom-guix--generate-channels-file ()
  "Generate a channels.scm that includes the guix-emacs channel and Doom's local channel."
  (with-temp-file (file-name-concat doom-guix-channel-dir "channels.scm")
    (insert ";; Doom Emacs Guix channels\n")
    (insert ";; Auto-generated by doom sync. Do not edit.\n\n")
    (insert "(list\n")
    ;; Default Guix channel
    (insert "  (channel\n")
    (insert "    (name 'guix)\n")
    (insert "    (url \"https://git.savannah.gnu.org/git/guix.git\"))\n")
    ;; guix-emacs channel for MELPA coverage
    (insert "  (channel\n")
    (insert "    (name 'guix-emacs)\n")
    (insert (format "    (url \"%s\")\n" doom-guix-emacs-channel-url))
    (insert (format "    (branch \"%s\"))\n" doom-guix-emacs-channel-branch))
    ;; Doom's local channel for pinned packages
    (insert "  (channel\n")
    (insert "    (name 'doom)\n")
    (insert (format "    (url \"file://%s\")))\n" doom-guix-channel-dir))))

(defun doom-guix-generate-channel ()
  "Generate the complete local Guix channel from `doom-packages'.
This writes channel metadata, package definitions, manifest, and channels file."
  (print! (start "Generating Guix channel..."))
  (print-group!
    (doom-guix--write-channel-metadata)
    (doom-guix--generate-channel-module)
    (doom-guix--generate-manifest)
    (doom-guix--generate-channels-file)
    (doom-guix--save-hash-cache)
    (print! (success "Generated Guix channel at %s") doom-guix-channel-dir)))


;;
;;; Profile management

(defun doom-guix--build-profile ()
  "Install all packages from the manifest into Doom's Guix profile."
  (print! (start "Building Guix profile..."))
  (let ((manifest (file-name-concat doom-guix-channel-dir "doom-manifest.scm"))
        (channels (file-name-concat doom-guix-channel-dir "channels.scm"))
        (n 0)
        result)
    (while (and (< n doom-guix--retries)
                (not (and result (zerop (car result)))))
      (setq result
            (doom-guix--call "package"
                             "--profile" doom-guix-profile-dir
                             "--manifest" manifest
                             "--channels" channels)
            n (1+ n))
      (unless (zerop (car result))
        (when (< n doom-guix--retries)
          (print! (warn "Guix build failed, retrying (%d/%d)...") n doom-guix--retries)
          (sleep-for 2))))
    (if (zerop (car result))
        (print! (success "Guix profile built at %s") doom-guix-profile-dir)
      (signal 'doom-package-error
              (list "guix profile build" (cdr result))))))

(defun doom-guix--profile-load-paths ()
  "Return the list of load-path entries from Doom's Guix profile."
  (let ((site-lisp (file-name-concat doom-guix-profile-dir
                                     "share/emacs/site-lisp")))
    (when (file-directory-p site-lisp)
      (cons site-lisp
            (cl-loop for dir in (directory-directories site-lisp)
                     when (file-directory-p dir)
                     collect dir)))))

(defun doom-guix--setup-load-path ()
  "Add Doom's Guix profile paths to `load-path'."
  (dolist (path (doom-guix--profile-load-paths))
    (add-to-list 'load-path path)))


;;
;;; Package queries

(defun doom-guix--package-installed-p (name)
  "Return non-nil if package NAME is installed in Doom's Guix profile."
  (let ((guix-name (doom-guix--package-name name)))
    (let ((result (doom-guix--call "package"
                                   "--profile" doom-guix-profile-dir
                                   "--list-installed" (concat "^" guix-name "$"))))
      (and (zerop (car result))
           (not (string-empty-p (cdr result)))))))

(defun doom-guix--package-load-path (name)
  "Return the load-path entry for package NAME from Doom's Guix profile."
  (let ((guix-name (doom-guix--package-name name)))
    (cl-loop for path in (doom-guix--profile-load-paths)
             when (string-match-p (regexp-quote guix-name)
                                  (file-name-nondirectory path))
             return path)))


;;
;;; Channel updates

(defun doom-guix-pull-channels ()
  "Pull latest updates for all configured Guix channels."
  (print! (start "Pulling Guix channel updates..."))
  (let ((channels (file-name-concat doom-guix-channel-dir "channels.scm")))
    (if (file-exists-p channels)
        (let ((result (doom-guix--call "pull"
                                       "--channels" channels)))
          (if (zerop (car result))
              (print! (success "Channels updated"))
            (print! (warn "Channel update failed: %s") (cdr result))))
      (print! (warn "No channels.scm found, skipping pull")))))


(provide 'doom-guix)
;;; doom-guix.el ends here
