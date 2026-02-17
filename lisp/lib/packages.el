;;; lisp/lib/packages.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Doom's package management system uses Guix to create a declarative,
;; reproducible package management system. Packages are declared in modules'
;; packages.el files via `package!' and managed through a dedicated Guix
;; profile.
;;
;; Interacting with this package management system is done through Doom's
;; bin/doom script. Find out more about it by running 'doom help' (I highly
;; recommend you add the script to your PATH). Here are some highlights:
;;
;; - `doom install': a wizard that guides you through setting up Doom and your
;;   private config for the first time.
;; - `doom sync': your go-to command for making sure Doom is in optimal
;;   condition. It ensures all unneeded packages are removed, all needed ones
;;   are installed, and all metadata associated with them is generated.
;; - `doom upgrade': upgrades Doom Emacs and your packages to the latest
;;   versions. There's also 'bin/doom sync -u' for updating only your packages.
;;
;; How this works is: the system reads packages.el files located in each
;; activated module, your private config (`doom-user-dir'), and one in
;; `doom-core-dir'. These contain `package!' declarations that tell DOOM what
;; packages to install and where from.
;;
;;; Code:

(require 'comp nil t)
(require 'doom-guix)
(doom-require 'doom-lib 'modules)


;;
;;; Variables

;; DEPRECATED: Will be stored in the local profile in v3.0
(defvar doom-packages ()
  "A list of enabled packages. Each element is a sublist, whose CAR is the
package's name as a symbol, and whose CDR is the plist supplied to its
`package!' declaration. Set by `doom-initialize-packages'.")

;; DEPRECATED: Will be stored in the local profile in v3.0
(defvar doom-disabled-packages ()
  "A list of packages that should be ignored by `use-package!' and `after!'.")


;;
;;; Package management API

(defun doom--ensure-guix ()
  "Ensure Guix is installed and properly configured for Doom."
  (doom-guix--ensure-guix)
  (unless (executable-find "git")
    (user-error "Git isn't present on your system. Cannot proceed."))
  (let* ((version (cdr (doom-call-process "git" "version")))
         (version
          (and (string-match "\\_<[0-9]+\\.[0-9]+\\(\\.[0-9]+\\)\\_>" version)
               (match-string 0 version))))
    (if version
        (when (version< version "2.23")
          (user-error "Git %s detected! Doom requires git 2.23 or newer!"
                      version)))))

(defun doom--ensure-core-packages (packages)
  "Ensure core PACKAGES are available on the load-path.
During initial setup, these may need to be installed via Guix before the full
profile is built."
  (doom-log "Ensuring core packages")
  (dolist (package packages)
    (let* ((name (car package))
           (guix-name (doom-guix--package-name name)))
      (unless (doom-package-installed-p name)
        (print! (start "Installing core package %s...") name)
        (doom-guix--call! "package"
                          "--profile" doom-guix-profile-dir
                          "--install" guix-name))
      ;; Ensure it's on load-path
      (when-let* ((path (doom-guix--package-load-path name)))
        (add-to-list 'load-path path)))))

;;;###autoload
(defun doom-initialize-core-packages (&optional force-p)
  "Ensure Guix is available and core packages are installed."
  (when (or force-p (not doom-packages))
    (doom-log "Initializing Guix")
    (doom--ensure-guix)
    (let ((packages (doom-package-list '((:doom)))))
      (doom--ensure-core-packages
       (seq-filter (fn! (eq (plist-get (cdr %) :type) 'core))
                   packages)))))

;;;###autoload
(defun doom-initialize-packages (&optional force-p)
  "Process all packages, essential and otherwise, if they haven't already been.

If FORCE-P is non-nil, do it anyway.

This ensures `doom-packages' is populated and Guix profile paths are on the
load-path."
  (doom-initialize-core-packages force-p)
  (when (or force-p (not (bound-and-true-p package--initialized)))
    (doom-log "Initializing package.el")
    (require 'package)
    (package-initialize)
    (unless package--initialized
      (error "Failed to initialize package.el")))
  (when (or force-p (null doom-packages))
    (doom-log "Initializing Guix packages")
    (setq doom-disabled-packages nil
          doom-packages (doom-package-list))
    (dolist (package doom-packages)
      (cl-destructuring-bind
          (name &key disable ignore &allow-other-keys) package
        (cond (ignore nil)
              (disable
               (cl-pushnew name doom-disabled-packages))
              (t
               ;; Add package's Guix profile path to load-path
               (when-let* ((path (doom-guix--package-load-path name)))
                 (add-to-list 'load-path path))))))
    ;; Also add all paths from the Guix profile
    (doom-guix--setup-load-path)))

;;;###autoload
(defun doom-package-get (package &optional prop nil-value)
  "Returns PACKAGE's `package!' recipe from `doom-packages'."
  (let ((plist (cdr (assq package doom-packages))))
    (if prop
        (if (plist-member plist prop)
            (plist-get plist prop)
          nil-value)
      plist)))

;;;###autoload
(defun doom-package-set (package prop value)
  "Set PROPERTY in PACKAGE's recipe to VALUE."
  (setf (alist-get package doom-packages)
        (plist-put (alist-get package doom-packages)
                   prop value)))

;;;###autoload
(defun doom-package-recipe (package &optional prop nil-value)
  "Returns the recipe PACKAGE was registered with."
  (let ((plist (plist-get (alist-get package doom-packages) :recipe)))
    (if prop
        (if (plist-member plist prop)
            (plist-get plist prop)
          nil-value)
      plist)))

;;;###autoload
(defun doom-package-recipe-repo (package)
  "Resolve and return PACKAGE's (symbol) repo identifier."
  (if-let* ((recipe (doom-package-recipe package))
            (repo (plist-get recipe :repo)))
      (file-name-nondirectory repo)
    (symbol-name package)))

;;;###autoload
(defun doom-package-build-recipe (package &optional prop nil-value)
  "Returns the recipe PACKAGE was declared with."
  (let ((plist (plist-get (alist-get package doom-packages) :recipe)))
    (if prop
        (if (plist-member plist prop)
            (plist-get plist prop)
          nil-value)
      plist)))

;;;###autoload
(defun doom-package-dependencies (package &optional _recursive _noerror)
  "Return a list of dependencies for a package.
Queries Guix for the dependency tree."
  (cl-check-type package symbol)
  (let* ((guix-name (doom-guix--package-name package))
         (result (doom-guix--call "package"
                                  "--profile" doom-guix-profile-dir
                                  "--list-installed" (concat "^" guix-name "$"))))
    ;; Guix doesn't easily expose dependency trees via CLI
    ;; Return nil for now; dependency tracking is handled by Guix internally
    nil))

;;;###autoload
(defun doom-package-depending-on (package &optional _noerror)
  "Return a list of packages that depend on PACKAGE."
  (cl-check-type package symbol)
  ;; Guix handles dependency tracking internally
  nil)

;;; Predicate functions
;;;###autoload
(defun doom-package-built-in-p (package)
  "Return non-nil if PACKAGE (a symbol) is built-in."
  (or (assq package package--builtins)
      (eq (doom-package-get package :type) 'built-in)))

;;;###autoload
(defun doom-package-installed-p (package)
  "Return non-nil if PACKAGE (a symbol) is installed."
  (doom-guix--package-installed-p package))

;;;###autoload
(defun doom-package-is-type-p (package type)
  "TODO"
  (memq type (ensure-list (doom-package-get package :type))))

;;;###autoload
(defun doom-package-in-module-p (package category &optional module)
  "Return non-nil if PACKAGE was installed by the user's private config."
  (when-let (modules (doom-package-get package :modules))
    (or (and (not module) (assq :user modules))
        (member (cons category module) modules))))

;;;###autoload
(defun doom-package-backend (package)
  "Return 'guix, 'builtin, 'elpa or 'other, depending on how PACKAGE is
installed."
  (cond ((doom-guix--package-installed-p package)
         'guix)
        ((or (doom-package-built-in-p package)
             (assq package package--builtins))
         'builtin)
        ((assq package package-alist)
         'elpa)
        ((locate-library (symbol-name package))
         'other)))


;;; Package getters
(defun doom-packages--read (file &optional noeval noerror)
  (condition-case-unless-debug e
      (with-temp-buffer ; prevent buffer-local state from propagating
        (if (not noeval)
            (load file noerror 'nomessage 'nosuffix)
          (when (file-exists-p file)
            (insert-file-contents file)
            (with-syntax-table emacs-lisp-mode-syntax-table
              ;; Scrape `package!' blocks from FILE for a comprehensive listing of
              ;; packages used by this module.
              (while (search-forward "(package!" nil t)
                (let ((ppss (save-excursion (syntax-ppss))))
                  ;; Don't collect packages in comments or strings
                  (unless (or (nth 3 ppss)
                              (nth 4 ppss))
                    (goto-char (match-beginning 0))
                    (cl-destructuring-bind (_ name . plist)
                        (read (current-buffer))
                      (push (cons
                             name (plist-put
                                   plist :modules
                                   (list (doom-module-context-key doom-module-context))))
                            doom-packages)))))))))
    (user-error
     (user-error (error-message-string e)))
    (error
     (signal 'doom-package-error
             (list (doom-module-context-key doom-module-context)
                   file e)))))

;;;###autoload
(defun doom-package-list (&optional module-list)
  "Retrieve a list of explicitly declared packages from MODULE-LIST.

If MODULE-LIST is omitted, read enabled module list in configdepth order (see
`doom-module-set'). Otherwise, MODULE-LIST may be any symbol (or t) to mean read
all modules in `doom-modules-dir', including :doom and :user. MODULE-LIST may
also be a list of module keys."
  (let ((module-list (cond ((null module-list) (doom-module-list))
                           ((symbolp module-list) (doom-module-list 'all))
                           (module-list)))
        (packages-file doom-module-packages-file)
        doom-disabled-packages
        doom-packages)
    (letf! (defun read-packages (key)
             (with-doom-module key
               (when-let (file (doom-module-locate-path
                                key doom-module-packages-file))
                 (doom-packages--read file nil 'noerror))))
      (with-doom-context 'package
        (let ((user? (assq :user module-list)))
          (when user?
            ;; We load the private packages file twice to populate
            ;; `doom-disabled-packages' disabled packages are seen ASAP...
            (let (doom-packages)
              (read-packages (cons :user nil))))
          (mapc #'read-packages module-list)
          ;; ...Then again to ensure privately overriden packages are properly
          ;; overwritten.
          (if user? (read-packages (cons :user nil)))
          (nreverse doom-packages))))))

;;;###autoload
(defun doom-package-pinned-alist ()
  "Return an alist mapping package names (strings) to pinned commits (strings)."
  (let (alist)
    (dolist (package doom-packages alist)
      (cl-destructuring-bind (name &key disable ignore pin unpin &allow-other-keys)
          package
        (when (and (not ignore)
                   (not disable)
                   (or pin unpin))
          (setf (alist-get (doom-package-recipe-repo name)
                           alist nil 'remove #'equal)
                (unless unpin pin)))))))

;;;###autoload
(defun doom-package-recipe-alist ()
  "Return recipes for non-builtin, non-virtual packages."
  (let (recipes)
    (dolist (package doom-packages)
      (cl-destructuring-bind (name &key recipe type disable ignore &allow-other-keys)
          package
        (unless (or disable ignore
                    (memq type '(built-in virtual)))
          (push (cons name recipe) recipes))))
    (nreverse recipes)))

;;;###autoload
(defun doom-package-homepage (package)
  "Return the url to PACKAGE's homepage (usually a repo)."
  (doom-initialize-packages)
  (or (get package 'homepage)
      (put package 'homepage
           (cond ((when-let (location (locate-library (symbol-name package)))
                    (with-temp-buffer
                      (if (string-match-p "\\.gz$" location)
                          (jka-compr-insert-file-contents location)
                        (insert-file-contents (concat (file-name-sans-extension location) ".el")
                                              nil 0 4096))
                      (let ((case-fold-search t))
                        (when (re-search-forward " \\(?:url\\|homepage\\|website\\): \\(http[^\n]+\\)\n" nil t)
                          (match-string-no-properties 1))))))
                 ((when-let* ((recipe (doom-package-recipe package))
                              (url (doom-guix--recipe-url recipe)))
                    url))
                 ((or package-archive-contents
                      (progn (package-refresh-contents)
                             package-archive-contents))
                  (pcase (ignore-errors (package-desc-archive (cadr (assq package package-archive-contents))))
                    (`nil nil)
                    ("org" "https://orgmode.org")
                    ((or "melpa" "melpa-mirror")
                     (format "https://melpa.org/#/%s" package))
                    ("gnu"
                     (format "https://elpa.gnu.org/packages/%s.html" package))
                    (archive
                     (if-let* ((src (cdr (assoc package package-archives))))
                         (format "%s" src)
                       (user-error "%s isn't installed through any known source (%s)"
                                   package archive)))))
                 ((user-error "Can't get homepage for %S package" package))))))


;;
;;; Commands

;;;###autoload
(defun doom/reload-packages ()
  "Reload `doom-packages' and reinitialize."
  (interactive)
  (message "Reloading packages")
  (doom-initialize-packages t)
  (message "Reloading packages...DONE"))

(defun doom--package-merge-recipes (package plist)
  (doom-plist-merge
   (plist-get plist :recipe)
   (plist-get (cdr (assq package doom-packages)) :recipe)))

(defun doom--package-to-bump-string (package plist)
  "Return a PACKAGE and its PLIST in 'username/repo@commit' format."
  (format "%s@%s"
          (plist-get (doom--package-merge-recipes package plist) :repo)
          (substring-no-properties (plist-get plist :pin) 0 12)))

(defun doom--package-at-point (&optional point)
  "Return the package and plist from the (package! PACKAGE PLIST...) at point."
  (save-match-data
    (save-excursion
      (and point (goto-char point))
      (while (and (or (atom (sexp-at-point))
                      (doom-point-in-string-or-comment-p))
                  (search-backward "(" nil t)))
      (when (eq (car-safe (sexp-at-point)) 'package!)
        (cl-destructuring-bind (beg . end)
            (bounds-of-thing-at-point 'sexp)
          (let* ((doom-packages nil)
                 (buffer-file-name
                  (or buffer-file-name
                      (bound-and-true-p org-src-source-file-name)))
                 (package
                  (with-doom-context 'package
                    (with-doom-module (doom-module-from-path buffer-file-name)
                      (eval (sexp-at-point) t)))))
            (list :beg beg
                  :end end
                  :package (car package)
                  :plist (cdr package))))))))

;;;###autoload
(defun doom/bumpify-package-at-point ()
  "Convert `package!' call at point to a bump string."
  (interactive)
  (cl-destructuring-bind (&key package plist beg end)
      (doom--package-at-point)
    (when-let (str (doom--package-to-bump-string package plist))
      (goto-char beg)
      (delete-region beg end)
      (insert str))))

;;;###autoload
(defun doom/bumpify-packages-in-buffer ()
  "Convert all `package!' calls in buffer into bump strings."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "(package!" nil t)
      (unless (doom-point-in-string-or-comment-p)
        (doom/bumpify-package-at-point)))))

;;;###autoload
(defun doom/bump-package-at-point (&optional select)
  "Inserts or updates a `:pin' for the `package!' statement at point.
Grabs the latest commit id of the package using 'git'."
  (interactive "P")
  (doom-initialize-packages)
  (cl-destructuring-bind (&key package plist beg end)
      (or (doom--package-at-point)
          (user-error "Not on a `package!' call"))
    (let* ((recipe (doom--package-merge-recipes package plist))
           (branch (plist-get recipe :branch))
           (oldid (or (plist-get plist :pin)
                      (doom-package-get package :pin)))
           (url (doom-guix--recipe-url recipe))
           (id (or (when url
                     (cdr (doom-call-process
                           "git" "ls-remote" url
                           (unless select branch))))
                   (user-error "Couldn't find a recipe for %s" package)))
           (id (car (split-string
                     (if select
                         (completing-read "Commit: " (split-string id "\n" t))
                       id)))))
      (when (and oldid
                 (plist-member plist :pin)
                 (equal oldid id))
        (user-error "%s: no update necessary" package))
      (save-excursion
        (if (re-search-forward ":pin +\"\\([^\"]+\\)\"" end t)
            (replace-match id t t nil 1)
          (goto-char (1- end))
          (insert " :pin " (prin1-to-string id))))
      (cond ((not oldid)
             (message "%s: → %s" package (substring id 0 10)))
            ((< (length oldid) (length id))
             (message "%s: extended to %s..." package id))
            ((message "%s: %s → %s"
                      package
                      (substring oldid 0 10)
                      (substring id 0 10)))))))

;;;###autoload
(defun doom/bump-packages-in-buffer (&optional select)
  "Inserts or updates a `:pin' to all `package!' statements in current buffer.
If SELECT (prefix arg) is non-nil, prompt you to choose a specific commit for
each package."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (doom-initialize-packages)
    (let (packages)
      (while (search-forward "(package! " nil t)
        (unless (let ((ppss (syntax-ppss)))
                  (or (nth 4 ppss)
                      (nth 3 ppss)
                      (save-excursion
                        (and (goto-char (match-beginning 0))
                             (not (plist-member (sexp-at-point) :pin))))))
          (condition-case e
              (push (doom/bump-package-at-point select) packages)
            (user-error (message "%s" (error-message-string e))))))
      (if packages
          (message "Updated %d packages\n- %s" (length packages) (string-join packages "\n- "))
        (message "No packages to update")))))

;;;###autoload
(defun doom/bump-module (category &optional module select)
  "Bump packages in CATEGORY MODULE.
If SELECT (prefix arg) is non-nil, prompt you to choose a specific commit for
each package."
  (interactive
   (let* ((module (completing-read
                   "Bump module: "
                   (let ((modules (doom-module-list 'all)))
                     (mapcar (lambda (m)
                               (if (listp m)
                                   (format "%s %s" (car m) (cdr m))
                                 (format "%s" m)))
                             (append (delete-dups (mapcar #'car modules))
                                     modules)))
                   nil t nil nil))
          (module (split-string module " " t)))
     (list (intern (car module))
           (ignore-errors (intern (cadr module)))
           current-prefix-arg)))
  (mapc (lambda! (key)
          (if-let* ((packages-file (doom-module-locate-path key doom-module-packages-file)))
              (with-current-buffer
                  (or (get-file-buffer packages-file)
                      (find-file-noselect packages-file))
                (doom/bump-packages-in-buffer select)
                (save-buffer))
            (message "Module %s has no packages.el file" key)))
        (if module
            (list (cons category module))
          (cl-remove-if-not (lambda (m) (eq (car m) category))
                            (doom-module-list 'all)))))

;;;###autoload
(defun doom/bump-package (package)
  "Bump PACKAGE in all modules that install it."
  (interactive
   (list (intern (completing-read "Bump package: "
                          (mapcar #'car (doom-package-list 'all))))))
  (let* ((packages (doom-package-list 'all))
         (modules (plist-get (alist-get package packages) :modules)))
    (unless modules
      (user-error "This package isn't installed by any Doom module"))
    (dolist (module modules)
      (when (doom-module-locate-path module doom-module-packages-file)
        (doom/bump-module (car module) (cdr module))))))

;;;###autoload
(defun doom/bumpify-diff (&optional interactive)
  "Copy user/repo@hash -> user/repo@hash's of changed packages to clipboard.

Must be run from a magit diff buffer."
  (interactive (list 'interactive))
  (save-window-excursion
    (magit-diff-staged)
    (unless (eq major-mode 'magit-diff-mode)
      (user-error "Not in a magit diff buffer"))
    (goto-char (point-min))
    (letf! (defun read-package ()
             (let* ((file (magit-file-at-point))
                    (visited? (if file (get-file-buffer file))))
               (save-window-excursion
                 (call-interactively #'magit-diff-visit-file)
                 (unwind-protect
                     (and (or (looking-at-p "(package!")
                              (re-search-forward "(package! " (line-end-position) t)
                              (re-search-backward "(package! " nil t))
                          (let* ((buffer-file-name file)
                                 (plist (doom--package-at-point)))
                            (cons (plist-get plist :package)
                                  plist)))
                   (unless visited?
                     (kill-current-buffer))))))
      (let (targets
            before
            after
            lines
            errors)
        (save-excursion
          (while (re-search-forward "^modified +\\(.+\\)$" nil t)
            (cl-pushnew (doom-module-from-path (match-string 1)) targets
                        :test #'equal)))
        (save-excursion
          (while (re-search-forward "^-" nil t)
            (when-let (pkg (read-package))
              (cl-pushnew pkg before :test #'equal))))
        (save-excursion
          (while (re-search-forward "^+" nil t)
            (when-let (pkg (read-package))
              (cl-pushnew pkg after :test #'equal))))
        (unless (= (length before) (length after))
          (user-error "Uneven number of packages being bumped"))
        (dolist (p1 before)
          (when (and (listp p1) (plist-get (cdr p1) :package))
            (cl-destructuring-bind (package &key plist _beg _end &allow-other-keys) p1
              (let ((p2 (cdr (assq package after))))
                (if (null p2)
                    (push package errors)
                  (let ((bstr1 (doom--package-to-bump-string package plist))
                        (bstr2 (doom--package-to-bump-string package (plist-get p2 :plist))))
                    (cl-pushnew (format "%s -> %s" bstr1 bstr2) lines :test #'equal)))))))
        (if (null lines)
            (user-error "No bumps to bumpify")
          (prog1 (funcall (if interactive #'kill-new #'identity)
                          (format "bump: %s\n\n%s"
                                  (mapconcat (lambda (x)
                                               (mapconcat #'symbol-name x " "))
                                             (cl-loop with alist = ()
                                                      for (category . module) in (reverse targets)
                                                      do (setf (alist-get category alist)
                                                               (append (alist-get category alist) (list module)))
                                                      finally return alist)
                                             " ")
                                  (string-join (sort (reverse lines) #'string-lessp)
                                               "\n")))
            (when interactive
              (message "Copied to clipboard"))))))))

;;;###autoload
(defun doom/commit-bumps ()
  "Create a pre-filled magit commit for currently bumped packages."
  (interactive)
  (magit-commit-create
   (list "-e" "-m" (doom/bumpify-diff))))


;;
;;; CLI API

(defun doom-packages--same-commit-p (abbrev-ref ref)
  (and (stringp abbrev-ref)
       (stringp ref)
       (string-match-p (concat "^" (regexp-quote abbrev-ref))
                       ref)))

(defun doom-packages--abbrev-commit (commit &optional full)
  (if full commit (substring commit 0 7)))

(defun doom-packages--commit-log-between (start-ref end-ref &optional repo-dir)
  "Get git log between START-REF and END-REF in REPO-DIR."
  (let ((default-directory (or repo-dir default-directory)))
    (let ((result (doom-call-process
                   "git" "log" "--oneline" "--no-merges"
                   end-ref (concat "^" (regexp-quote start-ref)))))
      (if (zerop (car result))
          (string-trim-right (or (cdr result) ""))
        (format "ERROR: Couldn't collect commit list because: %s" (cdr result))))))

(defun doom-packages--barf-if-incomplete ()
  "Error if the Guix profile doesn't exist."
  (unless (file-directory-p doom-guix-profile-dir)
    (user-error "Package state is incomplete. Run 'doom sync' first")))

(defvar doom-packages--eln-output-expected nil)

(defvar doom-packages--eln-output-path (car (bound-and-true-p native-comp-eln-load-path)))

(defun doom-packages--eln-file-name (file)
  "Return the short .eln file name corresponding to `file'."
  (file-name-concat
   comp-native-version-dir
   (file-name-nondirectory
    (comp-el-to-eln-filename file))))

(defun doom-packages--eln-output-file (eln-name)
  "Return the expected .eln file corresponding to `eln-name'."
  (file-name-concat doom-packages--eln-output-path eln-name))

(defun doom-packages--eln-error-file (eln-name)
  "Return the expected .error file corresponding to `eln-name'."
  (file-name-concat doom-packages--eln-output-path eln-name ".error"))

(defun doom-packages--find-eln-file (eln-name)
  "Find `eln-name' on the `native-comp-eln-load-path'."
  (cl-some (fn! (file-exists-p! eln-name %))
           native-comp-eln-load-path))

(defun doom-packages--elc-file-outdated-p (file)
  "Check whether the corresponding .elc for `file' is outdated."
  (let ((elc-file (byte-compile-dest-file file)))
    ;; NOTE Ignore missing elc files, they could be missing due to
    ;;   `no-byte-compile'. Rebuilding unnecessarily is expensive.
    (when (and (file-exists-p elc-file)
               (file-newer-than-file-p file elc-file))
      (doom-log "packages:elc: %s is newer than %s" file elc-file)
      t)))

(defun doom-packages--eln-file-outdated-p (file)
  "Check whether the corresponding .eln for `file' is outdated."
  (when (file-exists-p file)
    (let* ((eln-name (doom-packages--eln-file-name file))
           (eln-file (doom-packages--find-eln-file eln-name))
           (error-file (doom-packages--eln-error-file eln-name)))
      (cond (eln-file
             (when (file-newer-than-file-p file eln-file)
               (doom-log "packages:eln: %s is newer than %s" file eln-file)
               t))
            ((file-exists-p error-file)
             (when (file-newer-than-file-p file error-file)
               (doom-log "packages:eln: %s is newer than %s" file error-file)
               t))))))

(defun doom-packages--native-compile-done-h (file)
  "Callback fired when an item has finished async compilation."
  (when file
    (let* ((eln-name (doom-packages--eln-file-name file))
           (eln-file (doom-packages--eln-output-file eln-name))
           (error-file (doom-packages--eln-error-file eln-name)))
      (if (file-exists-p eln-file)
          (doom-log "packages:nativecomp: Compiled %s" eln-file)
        (let ((error-dir (file-name-directory error-file)))
          (if (not (file-writable-p error-dir))
              (doom-log "packages:nativecomp: failed to write %s" error-file)
            (make-directory error-dir 'parents)
            (write-region "" nil error-file)
            (doom-log "packages:nativecomp: wrote %s" error-file)))))))

(defun doom-packages--wait-for-native-compile-jobs ()
  "Wait for all pending async native compilation jobs."
  (cl-loop with previous = 0
           with timeout = 30
           with timer = 0
           for pending = (+ (length comp-files-queue)
                            (if (functionp 'comp--async-runnings)
                                (comp--async-runnings)
                              (comp-async-runnings)))
           while (not (zerop pending))
           if (/= previous pending) do
           (print! (start "\rNatively compiling %d files...\033[1A" pending))
           (setq previous pending
                 timer 0)
           else do
           (let ((inhibit-message t))
             (if (> timer timeout)
                 (cl-loop for file-name being each hash-key of comp-async-compilations
                          for prc = (gethash file-name comp-async-compilations)
                          unless (process-live-p prc)
                          do (setq timer 0)
                          and do (print! (warn "Native compilation of %S timed out" (path file-name)))
                          and return (kill-process prc))
               (cl-incf timer 0.1))
             (sleep-for 0.1))))

(defun doom-packages--write-missing-eln-errors ()
  "Write .error files for any expected .eln files that are missing."
  (cl-loop for file in doom-packages--eln-output-expected
           for eln-name = (doom-packages--eln-file-name file)
           for eln-file = (doom-packages--eln-output-file eln-name)
           for error-file = (doom-packages--eln-error-file eln-name)
           for error-dir = (file-name-directory error-file)
           unless (or (file-exists-p eln-file)
                      (file-newer-than-file-p error-file file)
                      (not (file-writable-p error-dir)))
           do (make-directory error-dir 'parents)
           (write-region "" nil error-file)
           (doom-log "Wrote %s" error-file))
  (setq doom-packages--eln-output-expected nil))

(defun doom-packages--compile-site-files ()
  "Queue async compilation for all non-doom Elisp files."
  (cl-loop with paths = (cl-loop for path in load-path
                                 unless (file-in-directory-p path doom-local-dir)
                                 collect path)
           for file in (doom-files-in paths :match "\\.el\\(?:\\.gz\\)?$")
           if (and (file-exists-p (byte-compile-dest-file file))
                   (not (doom-packages--find-eln-file (doom-packages--eln-file-name file)))
                   (not (cl-some (fn! (string-match-p % file))
                                 native-comp-deferred-compilation-deny-list))) do
           (doom-log "Compiling %s" file)
           (native-compile-async file)))

(defun doom-packages-ensure (&optional force-p)
  "Ensure packages are installed and built via Guix."
  (doom-initialize-packages)
  (if (not (file-directory-p doom-guix-profile-dir))
      (print! (start "Installing all packages for the first time (this may take a while)..."))
    (if force-p
        (print! (start "Rebuilding all packages (this may take a while)..."))
      (print! (start "Ensuring packages are installed and built..."))))
  (print-group!
    ;; Generate the local Guix channel from doom-packages
    (doom-guix-generate-channel)
    ;; Build the profile
    (when force-p
      ;; Force rebuild by removing the profile
      (when (file-exists-p doom-guix-profile-dir)
        (delete-file doom-guix-profile-dir)))
    (doom-guix--build-profile)
    ;; Set up load paths
    (doom-guix--setup-load-path)
    ;; Handle native compilation
    (when (featurep 'native-compile)
      (add-hook 'native-comp-async-cu-done-functions #'doom-packages--native-compile-done-h)
      (doom-packages--compile-site-files)
      (doom-packages--wait-for-native-compile-jobs)
      (doom-packages--write-missing-eln-errors))
    (print! (success "Packages are up-to-date"))))

(defun doom-packages-update (&optional pinned-only-p)
  "Updates packages by regenerating the Guix channel and rebuilding the profile."
  (doom-initialize-packages)
  (doom-packages--barf-if-incomplete)
  (if pinned-only-p
      (print! (start "Updating pinned packages..."))
    (print! (start "Updating all packages (this may take a while)..."))
    ;; Pull channel updates when doing a full update
    (doom-guix-pull-channels))
  ;; Regenerate channel with current pins and rebuild
  (doom-guix-generate-channel)
  (doom-guix--build-profile)
  (doom-guix--setup-load-path)
  (print! (success "Packages updated")))

(provide 'doom-lib '(packages))
;;; packages.el ends here
