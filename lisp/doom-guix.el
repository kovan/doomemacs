;;; lisp/doom-guix.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Doom's package management backend using the Guix package manager. This
;; replaces straight.el with a system that manages Emacs packages through a
;; dedicated Guix profile, built programmatically via `guix repl -t machine'.
;;
;; Packages are declared in modules' packages.el files via the `package!'
;; macro, which populates `doom-packages'. At sync time, Doom starts a Guix
;; REPL subprocess and defines packages inline (overriding sources for pinned
;; commits), then builds a profile derivation containing all packages.
;;
;; The guix-emacs channel (a community fork mirroring MELPA) provides base
;; definitions for most Emacs packages. Pinned packages override these at
;; build time via `(package (inherit ...) (source ...))' in the REPL session.
;;
;; Interacting with this system is done through Doom's bin/doom script:
;;
;; - `doom install': sets up Guix channels and builds the initial profile.
;; - `doom sync': rebuilds the profile with current pins via the REPL.
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
  "Path to Doom's local Guix channel directory.
Used to store the generated channels.scm for `guix pull'.")

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
;;; REPL subprocess management

(defvar doom-guix--repl-process nil
  "The guix repl -t machine subprocess, or nil if not running.")

(defun doom-guix--repl-start ()
  "Start a `guix repl -t machine' subprocess.
Returns the process object. Reads and discards the initial
repl-version header line."
  (when (and doom-guix--repl-process
             (process-live-p doom-guix--repl-process))
    (doom-guix--repl-stop))
  (doom-guix--ensure-guix)
  (let ((proc (make-process
               :name "doom-guix-repl"
               :command '("guix" "repl" "-t" "machine")
               :buffer (generate-new-buffer " *doom-guix-repl*")
               :connection-type 'pipe
               :noquery t)))
    ;; Wait for the repl-version header
    (with-current-buffer (process-buffer proc)
      (let ((deadline (+ (float-time) 30)))
        (while (and (< (float-time) deadline)
                    (process-live-p proc)
                    (< (buffer-size) 1)
                    (not (string-match-p "\n" (buffer-string))))
          (accept-process-output proc 0.1))
        (unless (string-match-p "(repl-version" (buffer-string))
          (let ((output (buffer-string)))
            (doom-guix--repl-stop)
            (error "guix repl failed to start. Output: %s" output)))
        ;; Clear the header
        (erase-buffer)))
    (setq doom-guix--repl-process proc)))

(defun doom-guix--repl-eval (expr-string)
  "Send EXPR-STRING to the guix repl and return the parsed result.
Parses (values VAL ...) responses into a list of values.
Signals an error for (exception ...) responses."
  (unless (and doom-guix--repl-process
               (process-live-p doom-guix--repl-process))
    (error "guix repl is not running"))
  (let ((proc doom-guix--repl-process)
        (buf (process-buffer doom-guix--repl-process)))
    (with-current-buffer buf
      (erase-buffer))
    ;; Send expression (must be a single line for machine protocol)
    (process-send-string proc (concat (string-replace "\n" " " expr-string) "\n"))
    ;; Read response
    (with-current-buffer buf
      (let ((deadline (+ (float-time) 120)))
        (while (and (< (float-time) deadline)
                    (process-live-p proc)
                    (not (string-match-p "\n" (buffer-string))))
          (accept-process-output proc 0.5))
        (doom-guix--parse-repl-response
         (string-trim (buffer-string)) proc)))))

(defun doom-guix--parse-repl-response (response &optional proc)
  "Parse a guix repl machine-protocol RESPONSE string.
Returns a list of values for (values ...) responses.
Signals an error for (exception ...) or unexpected responses.
PROC is the process object, used only for error messages."
  (cond
   ((string-empty-p response)
    (error "guix repl: no response (process %s)"
           (if (and proc (process-live-p proc)) "alive" "dead")))
   ((string-prefix-p "(values " response)
    ;; Parse (values VAL ...) — read the inner values
    (let ((inner (substring response 8 -1)))
      (condition-case nil
          (read (format "(%s)" inner))
        (error (list inner)))))
   ((string-prefix-p "(exception " response)
    (error "guix repl error: %s" response))
   (t
    (error "guix repl: unexpected response: %s" response))))

(defun doom-guix--repl-stop ()
  "Kill the guix repl subprocess."
  (when doom-guix--repl-process
    (when (process-live-p doom-guix--repl-process)
      (kill-process doom-guix--repl-process))
    (when-let ((buf (process-buffer doom-guix--repl-process)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    (setq doom-guix--repl-process nil)))

(defmacro doom-guix--with-repl (&rest body)
  "Execute BODY with an active guix repl, cleaning up afterward."
  (declare (indent 0))
  `(progn
     (doom-guix--repl-start)
     (unwind-protect
         (progn ,@body)
       (doom-guix--repl-stop))))


;;
;;; REPL-based profile building

(defun doom-guix--repl-load-modules ()
  "Send use-modules forms to load required Guix modules in the repl."
  (doom-guix--repl-eval
   "(use-modules (guix profiles) (guix store) (guix packages)
                 (guix monads) (guix derivations) (guix git-download)
                 (guix build-system emacs) ((guix licenses) #:prefix license:)
                 (guix utils) (gnu packages emacs-xyz)
                 (guix gexp))"))

(defun doom-guix--scheme-package-expr (name recipe pin)
  "Build a Scheme expression string defining package NAME with RECIPE and PIN.
Returns a (define ...) s-expression string for evaluation in the repl."
  (let* ((guix-name (doom-guix--package-name name))
         (var-name (concat "doom-" (symbol-name name)))
         (url (or (doom-guix--recipe-url recipe)
                  (format "https://github.com/emacs-straight/%s" (symbol-name name))))
         (hash (when pin (doom-guix--source-hash url pin))))
    (if pin
        (format
         (concat "(define %s"
                 " (package (inherit (specification->package \"%s\"))"
                 " (source (origin (method git-fetch)"
                 " (uri (git-reference (url \"%s\") (commit \"%s\")))"
                 " (file-name (git-file-name \"%s\" \"%s\"))"
                 " (sha256 (base32 \"%s\"))))))")
         var-name guix-name url pin guix-name
         (substring pin 0 (min 7 (length pin)))
         (or hash "0000000000000000000000000000000000000000000000000000"))
      ;; Unpinned: just reference from the channel
      (format "(define %s (specification->package \"%s\"))"
              var-name guix-name))))

(defun doom-guix--build-profile-via-repl ()
  "Build Doom's Guix profile using the repl.
Constructs package definitions inline and builds a profile derivation."
  (print! (start "Building Guix profile via repl..."))
  (doom-guix--with-repl
    ;; Load modules
    (doom-guix--repl-load-modules)
    ;; Open store connection
    (doom-guix--repl-eval "(define %doom-store (open-connection))")
    ;; Define each package
    (let (pkg-vars)
      (dolist (package doom-packages)
        (cl-destructuring-bind (name &key recipe pin disable ignore type
                                     &allow-other-keys)
            package
          (unless (or disable ignore (memq type '(built-in virtual)))
            (let* ((var-name (concat "doom-" (symbol-name name)))
                   (expr (doom-guix--scheme-package-expr name recipe pin)))
              (condition-case err
                  (progn
                    (doom-guix--repl-eval expr)
                    (push var-name pkg-vars))
                (error
                 (print! (warn "Failed to define %s: %s") name (error-message-string err))))))))
      (when pkg-vars
        ;; Build manifest from all defined packages
        (let ((manifest-expr
               (format "(define %%doom-manifest (packages->manifest (list %s)))"
                       (string-join (nreverse pkg-vars) " "))))
          (doom-guix--repl-eval manifest-expr))
        ;; Build the profile derivation
        (doom-guix--repl-eval
         (format
          "(define %%doom-drv (run-with-store %%doom-store (profile-derivation %%doom-manifest #:hooks '())))"
          ))
        ;; Build it
        (doom-guix--repl-eval
         "(build-derivations %doom-store (list %doom-drv))")
        ;; Get the output path and create the profile symlink
        (let* ((output (doom-guix--repl-eval
                        "(derivation->output-path %doom-drv)"))
               (drv-path (if (listp output) (car output) output)))
          (when (and drv-path (stringp drv-path))
            ;; Create the profile symlink
            (when (file-symlink-p doom-guix-profile-dir)
              (delete-file doom-guix-profile-dir))
            (make-symbolic-link drv-path doom-guix-profile-dir t)
            (print! (success "Guix profile built at %s -> %s")
                    doom-guix-profile-dir drv-path)))
        ;; Close store
        (doom-guix--repl-eval "(close-connection %doom-store)")))
    ;; Save hash cache for any new hashes computed
    (doom-guix--save-hash-cache)))

(defun doom-guix--repl-package-installed-p (name)
  "Return non-nil if package NAME is installed in Doom's Guix profile.
Queries the profile manifest via the repl."
  (let ((guix-name (doom-guix--package-name name)))
    (condition-case nil
        (doom-guix--with-repl
          (doom-guix--repl-eval
           "(use-modules (guix profiles) (guix store))")
          (let* ((expr (format
                        (concat "(let* ((profile \"%s\")"
                                " (man (profile-manifest profile))"
                                " (entries (manifest-entries man)))"
                                " (any (lambda (e) (string=? (manifest-entry-name e) \"%s\")) entries))")
                        doom-guix-profile-dir guix-name))
                 (result (doom-guix--repl-eval expr)))
            (and result (not (equal result '("#f"))))))
      (error nil))))


;;
;;; Channel management

(defun doom-guix--generate-channels-file ()
  "Generate a channels.scm for `guix pull'.
Includes the default Guix channel and the guix-emacs channel."
  (make-directory doom-guix-channel-dir t)
  (with-temp-file (file-name-concat doom-guix-channel-dir "channels.scm")
    (insert ";; Doom Emacs Guix channels\n")
    (insert ";; Auto-generated by doom sync. Do not edit.\n\n")
    (insert "(list\n")
    ;; Default Guix channel (introduction required for authentication)
    (insert "  (channel\n")
    (insert "    (name 'guix)\n")
    (insert "    (url \"https://git.savannah.gnu.org/git/guix.git\")\n")
    (insert "    (introduction\n")
    (insert "      (make-channel-introduction\n")
    (insert "        \"9edb3f66fd807b096b48283debdcddccfea34bad\"\n")
    (insert "        (openpgp-fingerprint\n")
    (insert "          \"BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA\"))))\n")
    ;; guix-emacs channel for MELPA coverage
    (insert "  (channel\n")
    (insert "    (name 'guix-emacs)\n")
    (insert (format "    (url \"%s\")\n" doom-guix-emacs-channel-url))
    (insert (format "    (branch \"%s\")))\n" doom-guix-emacs-channel-branch))))

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
                                   (format "--profile=%s" doom-guix-profile-dir)
                                   (format "--list-installed=%s" (concat "^" guix-name "$")))))
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
  "Pull latest updates for all configured Guix channels.
Generates channels.scm if it doesn't exist."
  (print! (start "Pulling Guix channel updates..."))
  (let ((channels (file-name-concat doom-guix-channel-dir "channels.scm")))
    (unless (file-exists-p channels)
      (doom-guix--generate-channels-file))
    (let ((result (doom-guix--call "pull"
                                   (format "--channels=%s" channels))))
      (if (zerop (car result))
          (print! (success "Channels updated"))
        (print! (warn "Channel update failed: %s") (cdr result))))))


(provide 'doom-guix)
;;; doom-guix.el ends here
