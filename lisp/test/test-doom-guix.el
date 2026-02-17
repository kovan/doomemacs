;;; lisp/test/test-doom-guix.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; ERT tests for doom-guix.el (Guix package management backend) and
;; lisp/lib/packages.el (package management API).
;;
;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load doom-guix if not already loaded.  In batch mode the full Doom
;; init may not have run, so we load it directly.
(unless (featurep 'doom-guix)
  (load (expand-file-name "lisp/doom-guix.el" doom-emacs-dir)
        nil 'nomessage 'nosuffix))


;;
;;; Package name mapping

(ert-deftest doom-guix-package-name/default-convention ()
  "Package names follow the emacs-<name> convention by default."
  (let ((doom-packages nil)
        (doom-guix-name-map nil))
    (should (equal "emacs-magit" (doom-guix--package-name 'magit)))
    (should (equal "emacs-evil" (doom-guix--package-name 'evil)))))

(ert-deftest doom-guix-package-name/guix-name-property ()
  "The :guix-name property in doom-packages overrides the default."
  (let ((doom-packages '((vterm :guix-name "emacs-vterm-next")))
        (doom-guix-name-map nil))
    (should (equal "emacs-vterm-next" (doom-guix--package-name 'vterm)))))

(ert-deftest doom-guix-package-name/name-map-override ()
  "doom-guix-name-map takes highest priority."
  (let ((doom-packages '((org :guix-name "emacs-org-from-plist")))
        (doom-guix-name-map '((org . "emacs-org-custom"))))
    (should (equal "emacs-org-custom" (doom-guix--package-name 'org)))))

(ert-deftest doom-guix-package-name/name-map-over-default ()
  "doom-guix-name-map overrides the default convention."
  (let ((doom-packages nil)
        (doom-guix-name-map '((pdf-tools . "emacs-pdf-tools-server"))))
    (should (equal "emacs-pdf-tools-server"
                   (doom-guix--package-name 'pdf-tools)))))


;;
;;; Recipe URL extraction

(ert-deftest doom-guix-recipe-url/github ()
  "GitHub :host/:repo produces the correct URL."
  (should (equal "https://github.com/user/repo"
                 (doom-guix--recipe-url '(:host github :repo "user/repo")))))

(ert-deftest doom-guix-recipe-url/gitlab ()
  "GitLab :host/:repo produces the correct URL."
  (should (equal "https://gitlab.com/user/repo"
                 (doom-guix--recipe-url '(:host gitlab :repo "user/repo")))))

(ert-deftest doom-guix-recipe-url/codeberg ()
  "Codeberg :host/:repo produces the correct URL."
  (should (equal "https://codeberg.org/user/repo"
                 (doom-guix--recipe-url '(:host codeberg :repo "user/repo")))))

(ert-deftest doom-guix-recipe-url/sourcehut ()
  "Sourcehut :host/:repo produces the correct URL."
  (should (equal "https://git.sr.ht/user/repo"
                 (doom-guix--recipe-url '(:host sourcehut :repo "user/repo")))))

(ert-deftest doom-guix-recipe-url/bitbucket ()
  "Bitbucket :host/:repo produces the correct URL."
  (should (equal "https://bitbucket.org/user/repo"
                 (doom-guix--recipe-url '(:host bitbucket :repo "user/repo")))))

(ert-deftest doom-guix-recipe-url/direct-url ()
  "Direct :url takes precedence over :host/:repo."
  (should (equal "https://example.com/repo.git"
                 (doom-guix--recipe-url '(:url "https://example.com/repo.git"
                                          :host github :repo "user/repo")))))

(ert-deftest doom-guix-recipe-url/no-url ()
  "Missing URL info returns nil."
  (should-not (doom-guix--recipe-url '(:files ("*.el"))))
  (should-not (doom-guix--recipe-url nil))
  (should-not (doom-guix--recipe-url '(:host github))))


;;
;;; Package definition generation

(ert-deftest doom-guix-generate-package-definition/pinned ()
  "Pinned packages produce a definition with git-fetch and commit."
  (let ((doom-packages '((magit :recipe (:host github :repo "magit/magit"))))
        (doom-guix-name-map nil)
        (doom-guix--hash-cache (make-hash-table :test #'equal)))
    ;; Pre-populate hash cache to avoid calling guix
    (puthash (cons "https://github.com/magit/magit" "abc123")
             "0fakehash00000000000000000000000000000000000000000000"
             doom-guix--hash-cache)
    (let ((def (doom-guix--generate-package-definition
                'magit '(:host github :repo "magit/magit") "abc123")))
      (should (stringp def))
      (should (string-match-p "define-public emacs-magit" def))
      (should (string-match-p "commit \"abc123\"" def))
      (should (string-match-p "git-fetch" def))
      (should (string-match-p "0fakehash" def)))))

(ert-deftest doom-guix-generate-package-definition/unpinned ()
  "Unpinned packages produce an inheriting definition."
  (let ((doom-packages nil)
        (doom-guix-name-map nil))
    (let ((def (doom-guix--generate-package-definition 'evil nil nil)))
      (should (stringp def))
      (should (string-match-p "define-public emacs-evil" def))
      (should (string-match-p "version \"latest\"" def))
      (should-not (string-match-p "git-fetch" def)))))

(ert-deftest doom-guix-generate-package-definition/custom-files ()
  "Custom :files filter produces an #:include argument."
  (let ((doom-packages nil)
        (doom-guix-name-map nil))
    (let ((def (doom-guix--generate-package-definition
                'foo '(:host github :repo "user/foo" :files ("foo.el" "bar.el"))
                nil)))
      (should (string-match-p "#:include" def))
      (should (string-match-p "\"foo.el\"" def))
      (should (string-match-p "\"bar.el\"" def)))))

(ert-deftest doom-guix-generate-package-definition/custom-depends ()
  "Custom :depends produce propagated-inputs."
  (let ((doom-packages nil)
        (doom-guix-name-map nil))
    (let ((def (doom-guix--generate-package-definition
                'foo '(:host github :repo "user/foo" :depends (bar baz))
                nil)))
      (should (string-match-p "propagated-inputs" def))
      (should (string-match-p "emacs-bar" def))
      (should (string-match-p "emacs-baz" def)))))

(ert-deftest doom-guix-generate-package-definition/build-system ()
  "All definitions use emacs-build-system."
  (let ((doom-packages nil)
        (doom-guix-name-map nil))
    (let ((def (doom-guix--generate-package-definition 'foo nil nil)))
      (should (string-match-p "emacs-build-system" def)))))


;;
;;; Channel generation

(ert-deftest doom-guix-write-channel-metadata ()
  "Channel metadata file is written correctly."
  (let* ((tmpdir (make-temp-file "doom-test-guix-" t))
         (doom-guix-channel-dir tmpdir))
    (unwind-protect
        (progn
          (doom-guix--write-channel-metadata)
          (let ((content (with-temp-buffer
                           (insert-file-contents
                            (file-name-concat tmpdir ".guix-channel"))
                           (buffer-string))))
            (should (string-match-p "(channel" content))
            (should (string-match-p "(version 0)" content))
            (should (string-match-p (regexp-quote tmpdir) content))))
      (delete-directory tmpdir t))))

(ert-deftest doom-guix-generate-channel-module/skips-disabled ()
  "Channel module generation skips disabled, ignored, and built-in packages."
  (let* ((tmpdir (make-temp-file "doom-test-guix-" t))
         (doom-guix-channel-dir tmpdir)
         (doom-guix-name-map nil)
         (doom-packages '((active-pkg :pin "aaa111" :recipe (:host github :repo "u/active"))
                          (disabled-pkg :disable t :pin "bbb222" :recipe (:host github :repo "u/disabled"))
                          (ignored-pkg :ignore t :pin "ccc333" :recipe (:host github :repo "u/ignored"))
                          (builtin-pkg :type built-in)))
         (doom-guix--hash-cache (make-hash-table :test #'equal)))
    ;; Pre-populate hash cache
    (puthash (cons "https://github.com/u/active" "aaa111")
             "0fakehash00000000000000000000000000000000000000000000"
             doom-guix--hash-cache)
    (unwind-protect
        (progn
          (doom-guix--generate-channel-module)
          (let ((content (with-temp-buffer
                           (insert-file-contents
                            (file-name-concat tmpdir "doom" "packages.scm"))
                           (buffer-string))))
            (should (string-match-p "emacs-active-pkg" content))
            (should-not (string-match-p "emacs-disabled-pkg" content))
            (should-not (string-match-p "emacs-ignored-pkg" content))
            (should-not (string-match-p "emacs-builtin-pkg" content))))
      (delete-directory tmpdir t))))

(ert-deftest doom-guix-generate-manifest/skips-disabled ()
  "Manifest generation skips disabled, ignored, and built-in packages."
  (let* ((tmpdir (make-temp-file "doom-test-guix-" t))
         (doom-guix-channel-dir tmpdir)
         (doom-guix-name-map nil)
         (doom-packages '((active-pkg)
                          (disabled-pkg :disable t)
                          (ignored-pkg :ignore t)
                          (builtin-pkg :type built-in))))
    (unwind-protect
        (progn
          (doom-guix--generate-manifest)
          (let ((content (with-temp-buffer
                           (insert-file-contents
                            (file-name-concat tmpdir "doom-manifest.scm"))
                           (buffer-string))))
            (should (string-match-p "emacs-active-pkg" content))
            (should-not (string-match-p "emacs-disabled-pkg" content))
            (should-not (string-match-p "emacs-ignored-pkg" content))
            (should-not (string-match-p "emacs-builtin-pkg" content))))
      (delete-directory tmpdir t))))

(ert-deftest doom-guix-generate-channels-file ()
  "Channels file includes guix, guix-emacs, and doom channels."
  (let* ((tmpdir (make-temp-file "doom-test-guix-" t))
         (doom-guix-channel-dir tmpdir)
         (doom-guix-emacs-channel-url "https://github.com/garrgravarr/guix-emacs")
         (doom-guix-emacs-channel-branch "main"))
    (unwind-protect
        (progn
          (doom-guix--generate-channels-file)
          (let ((content (with-temp-buffer
                           (insert-file-contents
                            (file-name-concat tmpdir "channels.scm"))
                           (buffer-string))))
            (should (string-match-p "'guix)" content))
            (should (string-match-p "'guix-emacs)" content))
            (should (string-match-p "'doom)" content))
            (should (string-match-p "savannah" content))
            (should (string-match-p "garrgravarr" content))))
      (delete-directory tmpdir t))))


;;
;;; Package queries (doom-package-get, doom-package-recipe)

(ert-deftest doom-package-get/retrieves-property ()
  "doom-package-get retrieves a specific property."
  (let ((doom-packages '((magit :pin "abc123" :recipe (:host github :repo "magit/magit")))))
    (should (equal "abc123" (doom-package-get 'magit :pin)))
    (should (equal '(:host github :repo "magit/magit")
                   (doom-package-get 'magit :recipe)))))

(ert-deftest doom-package-get/returns-full-plist ()
  "doom-package-get without prop returns the full plist."
  (let ((doom-packages '((evil :pin "def456"))))
    (should (equal '(:pin "def456") (doom-package-get 'evil)))))

(ert-deftest doom-package-get/nil-value-fallback ()
  "doom-package-get returns nil-value for missing properties."
  (let ((doom-packages '((evil :pin "abc"))))
    (should (equal 'fallback (doom-package-get 'evil :nonexistent 'fallback)))))

(ert-deftest doom-package-recipe/returns-recipe ()
  "doom-package-recipe returns the :recipe plist."
  (let ((doom-packages '((magit :recipe (:host github :repo "magit/magit")))))
    (should (equal '(:host github :repo "magit/magit")
                   (doom-package-recipe 'magit)))))

(ert-deftest doom-package-recipe/returns-recipe-prop ()
  "doom-package-recipe with prop returns a specific recipe property."
  (let ((doom-packages '((magit :recipe (:host github :repo "magit/magit")))))
    (should (equal 'github (doom-package-recipe 'magit :host)))))

(ert-deftest doom-package-recipe-repo/returns-repo-basename ()
  "doom-package-recipe-repo returns the basename of the repo."
  (let ((doom-packages '((magit :recipe (:host github :repo "magit/magit")))))
    (should (equal "magit" (doom-package-recipe-repo 'magit)))))

(ert-deftest doom-package-recipe-repo/falls-back-to-name ()
  "doom-package-recipe-repo falls back to package name."
  (let ((doom-packages '((evil))))
    (should (equal "evil" (doom-package-recipe-repo 'evil)))))


;;
;;; Package predicates

(ert-deftest doom-package-built-in-p/type-property ()
  "doom-package-built-in-p detects :type built-in."
  (require 'package)
  (let ((doom-packages '((org :type built-in))))
    (should (doom-package-built-in-p 'org))))

(ert-deftest doom-package-is-type-p/checks-type ()
  "doom-package-is-type-p checks the :type property."
  (let ((doom-packages '((foo :type core))))
    (should (doom-package-is-type-p 'foo 'core))
    (should-not (doom-package-is-type-p 'foo 'virtual))))


;;
;;; doom-package-set

(ert-deftest doom-package-set/sets-property ()
  "doom-package-set sets a property on an existing package."
  (let ((doom-packages '((evil :pin "old"))))
    (doom-package-set 'evil :pin "new")
    (should (equal "new" (doom-package-get 'evil :pin)))))

(ert-deftest doom-package-set/adds-property ()
  "doom-package-set adds a new property."
  (let ((doom-packages '((evil))))
    (doom-package-set 'evil :pin "abc123")
    (should (equal "abc123" (doom-package-get 'evil :pin)))))


;;
;;; Hash cache

(ert-deftest doom-guix-hash-cache/save-and-load ()
  "Hash cache can be saved to disk and loaded back."
  (let* ((tmpdir (make-temp-file "doom-test-cache-" t))
         (doom-guix-hash-cache-file (file-name-concat tmpdir "hash-cache.el"))
         (doom-guix--hash-cache (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (puthash '("https://example.com" . "abc123") "somehash"
                   doom-guix--hash-cache)
          (doom-guix--save-hash-cache)
          (should (file-exists-p doom-guix-hash-cache-file))
          ;; Clear and reload
          (setq doom-guix--hash-cache nil)
          (doom-guix--load-hash-cache)
          (should (hash-table-p doom-guix--hash-cache))
          (should (equal "somehash"
                         (gethash '("https://example.com" . "abc123")
                                  doom-guix--hash-cache))))
      (delete-directory tmpdir t))))

(ert-deftest doom-guix-hash-cache/load-missing-file ()
  "Loading a missing cache file creates an empty hash table."
  (let ((doom-guix-hash-cache-file "/tmp/doom-test-nonexistent-cache.el")
        (doom-guix--hash-cache nil))
    (doom-guix--load-hash-cache)
    (should (hash-table-p doom-guix--hash-cache))
    (should (zerop (hash-table-count doom-guix--hash-cache)))))


;;
;;; Profile load paths

(ert-deftest doom-guix-profile-load-paths/missing-profile ()
  "Returns nil when the profile directory doesn't exist."
  (let ((doom-guix-profile-dir "/tmp/doom-test-nonexistent-profile"))
    (should-not (doom-guix--profile-load-paths))))

(provide 'test-doom-guix)
;;; test-doom-guix.el ends here
