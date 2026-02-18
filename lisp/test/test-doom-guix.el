;;; lisp/test/test-doom-guix.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;
;; Tests for doom-guix.el (Guix package management backend) and
;; lisp/lib/packages.el (package management API).
;;
;;; Code:

;; Load doom-guix if not already loaded.  In batch mode the full Doom
;; init may not have run, so we load it directly.
(unless (featurep 'doom-guix)
  (load (expand-file-name "lisp/doom-guix.el" doom-emacs-dir)
        nil 'nomessage 'nosuffix))


;;
;;; Package name mapping

(describe "doom-guix--package-name"
  (it "follows the emacs-<name> convention by default"
    (let ((doom-packages nil)
          (doom-guix-name-map nil))
      (expect (doom-guix--package-name 'magit) :to-equal "emacs-magit")
      (expect (doom-guix--package-name 'evil) :to-equal "emacs-evil")))

  (it "uses :guix-name property when set"
    (let ((doom-packages '((vterm :guix-name "emacs-vterm-next")))
          (doom-guix-name-map nil))
      (expect (doom-guix--package-name 'vterm) :to-equal "emacs-vterm-next")))

  (it "gives doom-guix-name-map highest priority"
    (let ((doom-packages '((org :guix-name "emacs-org-from-plist")))
          (doom-guix-name-map '((org . "emacs-org-custom"))))
      (expect (doom-guix--package-name 'org) :to-equal "emacs-org-custom")))

  (it "overrides default convention via name-map"
    (let ((doom-packages nil)
          (doom-guix-name-map '((pdf-tools . "emacs-pdf-tools-server"))))
      (expect (doom-guix--package-name 'pdf-tools)
              :to-equal "emacs-pdf-tools-server"))))


;;
;;; Recipe URL extraction

(describe "doom-guix--recipe-url"
  (it "produces correct GitHub URL"
    (expect (doom-guix--recipe-url '(:host github :repo "user/repo"))
            :to-equal "https://github.com/user/repo"))

  (it "produces correct GitLab URL"
    (expect (doom-guix--recipe-url '(:host gitlab :repo "user/repo"))
            :to-equal "https://gitlab.com/user/repo"))

  (it "produces correct Codeberg URL"
    (expect (doom-guix--recipe-url '(:host codeberg :repo "user/repo"))
            :to-equal "https://codeberg.org/user/repo"))

  (it "produces correct Sourcehut URL"
    (expect (doom-guix--recipe-url '(:host sourcehut :repo "user/repo"))
            :to-equal "https://git.sr.ht/user/repo"))

  (it "produces correct Bitbucket URL"
    (expect (doom-guix--recipe-url '(:host bitbucket :repo "user/repo"))
            :to-equal "https://bitbucket.org/user/repo"))

  (it "prefers direct :url over :host/:repo"
    (expect (doom-guix--recipe-url '(:url "https://example.com/repo.git"
                                     :host github :repo "user/repo"))
            :to-equal "https://example.com/repo.git"))

  (it "returns nil when URL info is missing"
    (expect (doom-guix--recipe-url '(:files ("*.el"))) :to-be nil)
    (expect (doom-guix--recipe-url nil) :to-be nil)
    (expect (doom-guix--recipe-url '(:host github)) :to-be nil)))


;;
;;; Package definition generation

(describe "doom-guix--generate-package-definition"
  (it "produces a definition with git-fetch and commit for pinned packages"
    (let ((doom-packages '((magit :recipe (:host github :repo "magit/magit"))))
          (doom-guix-name-map nil)
          (doom-guix--hash-cache (make-hash-table :test #'equal)))
      (puthash (cons "https://github.com/magit/magit" "abc123")
               "0fakehash00000000000000000000000000000000000000000000"
               doom-guix--hash-cache)
      (let ((def (doom-guix--generate-package-definition
                  'magit '(:host github :repo "magit/magit") "abc123")))
        (expect def :to-be-truthy)
        (expect def :to-match "define-public emacs-magit")
        (expect def :to-match "commit \"abc123\"")
        (expect def :to-match "git-fetch")
        (expect def :to-match "0fakehash"))))

  (it "produces an inheriting definition for unpinned packages"
    (let ((doom-packages nil)
          (doom-guix-name-map nil))
      (let ((def (doom-guix--generate-package-definition 'evil nil nil)))
        (expect def :to-be-truthy)
        (expect def :to-match "define-public emacs-evil")
        (expect def :to-match "version \"latest\"")
        (expect def :not :to-match "git-fetch"))))

  (it "produces #:include for custom :files"
    (let ((doom-packages nil)
          (doom-guix-name-map nil))
      (let ((def (doom-guix--generate-package-definition
                  'foo '(:host github :repo "user/foo" :files ("foo.el" "bar.el"))
                  nil)))
        (expect def :to-match "#:include")
        (expect def :to-match "\"foo.el\"")
        (expect def :to-match "\"bar.el\""))))

  (it "produces propagated-inputs for custom :depends"
    (let ((doom-packages nil)
          (doom-guix-name-map nil))
      (let ((def (doom-guix--generate-package-definition
                  'foo '(:host github :repo "user/foo" :depends (bar baz))
                  nil)))
        (expect def :to-match "propagated-inputs")
        (expect def :to-match "emacs-bar")
        (expect def :to-match "emacs-baz"))))

  (it "uses emacs-build-system"
    (let ((doom-packages nil)
          (doom-guix-name-map nil))
      (let ((def (doom-guix--generate-package-definition 'foo nil nil)))
        (expect def :to-match "emacs-build-system")))))


;;
;;; Channel generation

(describe "doom-guix channel generation"
  (it "writes channel metadata correctly"
    (let* ((tmpdir (make-temp-file "doom-test-guix-" t))
           (doom-guix-channel-dir tmpdir))
      (unwind-protect
          (progn
            (doom-guix--write-channel-metadata)
            (let ((content (with-temp-buffer
                             (insert-file-contents
                              (file-name-concat tmpdir ".guix-channel"))
                             (buffer-string))))
              (expect content :to-match "(channel")
              (expect content :to-match "(version 0)")
              (expect content :to-match (regexp-quote tmpdir))))
        (delete-directory tmpdir t))))

  (it "skips disabled, ignored, and built-in packages in channel module"
    (let* ((tmpdir (make-temp-file "doom-test-guix-" t))
           (doom-guix-channel-dir tmpdir)
           (doom-guix-name-map nil)
           (doom-packages '((active-pkg :pin "aaa111" :recipe (:host github :repo "u/active"))
                            (disabled-pkg :disable t :pin "bbb222" :recipe (:host github :repo "u/disabled"))
                            (ignored-pkg :ignore t :pin "ccc333" :recipe (:host github :repo "u/ignored"))
                            (builtin-pkg :type built-in)))
           (doom-guix--hash-cache (make-hash-table :test #'equal)))
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
              (expect content :to-match "emacs-active-pkg")
              (expect content :not :to-match "emacs-disabled-pkg")
              (expect content :not :to-match "emacs-ignored-pkg")
              (expect content :not :to-match "emacs-builtin-pkg")))
        (delete-directory tmpdir t))))

  (it "skips disabled, ignored, and built-in packages in manifest"
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
              (expect content :to-match "emacs-active-pkg")
              (expect content :not :to-match "emacs-disabled-pkg")
              (expect content :not :to-match "emacs-ignored-pkg")
              (expect content :not :to-match "emacs-builtin-pkg")))
        (delete-directory tmpdir t))))

  (it "includes guix, guix-emacs, and doom channels"
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
              (expect content :to-match "'guix)")
              (expect content :to-match "'guix-emacs)")
              (expect content :to-match "'doom)")
              (expect content :to-match "savannah")
              (expect content :to-match "garrgravarr")))
        (delete-directory tmpdir t)))))


;;
;;; Package queries (doom-package-get, doom-package-recipe)

(describe "doom-package-get"
  (it "retrieves a specific property"
    (let ((doom-packages '((magit :pin "abc123" :recipe (:host github :repo "magit/magit")))))
      (expect (doom-package-get 'magit :pin) :to-equal "abc123")
      (expect (doom-package-get 'magit :recipe)
              :to-equal '(:host github :repo "magit/magit"))))

  (it "returns the full plist without prop"
    (let ((doom-packages '((evil :pin "def456"))))
      (expect (doom-package-get 'evil) :to-equal '(:pin "def456"))))

  (it "returns nil-value for missing properties"
    (let ((doom-packages '((evil :pin "abc"))))
      (expect (doom-package-get 'evil :nonexistent 'fallback)
              :to-equal 'fallback))))

(describe "doom-package-recipe"
  (it "returns the :recipe plist"
    (let ((doom-packages '((magit :recipe (:host github :repo "magit/magit")))))
      (expect (doom-package-recipe 'magit)
              :to-equal '(:host github :repo "magit/magit"))))

  (it "returns a specific recipe property"
    (let ((doom-packages '((magit :recipe (:host github :repo "magit/magit")))))
      (expect (doom-package-recipe 'magit :host) :to-equal 'github))))

(describe "doom-package-recipe-repo"
  (it "returns the basename of the repo"
    (let ((doom-packages '((magit :recipe (:host github :repo "magit/magit")))))
      (expect (doom-package-recipe-repo 'magit) :to-equal "magit")))

  (it "falls back to package name"
    (let ((doom-packages '((evil))))
      (expect (doom-package-recipe-repo 'evil) :to-equal "evil"))))


;;
;;; Package predicates

(describe "doom-package-built-in-p"
  (it "detects :type built-in"
    (require 'package)
    (let ((doom-packages '((org :type built-in))))
      (expect (doom-package-built-in-p 'org) :to-be-truthy))))

(describe "doom-package-is-type-p"
  (it "checks the :type property"
    (let ((doom-packages '((foo :type core))))
      (expect (doom-package-is-type-p 'foo 'core) :to-be-truthy)
      (expect (doom-package-is-type-p 'foo 'virtual) :not :to-be-truthy))))


;;
;;; doom-package-set

(describe "doom-package-set"
  (it "sets a property on an existing package"
    (let ((doom-packages '((evil :pin "old"))))
      (doom-package-set 'evil :pin "new")
      (expect (doom-package-get 'evil :pin) :to-equal "new")))

  (it "adds a new property"
    (let ((doom-packages '((evil))))
      (doom-package-set 'evil :pin "abc123")
      (expect (doom-package-get 'evil :pin) :to-equal "abc123"))))


;;
;;; Hash cache

(describe "doom-guix hash cache"
  (it "can be saved to disk and loaded back"
    (let* ((tmpdir (make-temp-file "doom-test-cache-" t))
           (doom-guix-hash-cache-file (file-name-concat tmpdir "hash-cache.el"))
           (doom-guix--hash-cache (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (puthash '("https://example.com" . "abc123") "somehash"
                     doom-guix--hash-cache)
            (doom-guix--save-hash-cache)
            (expect (file-exists-p doom-guix-hash-cache-file) :to-be-truthy)
            ;; Clear and reload
            (setq doom-guix--hash-cache nil)
            (doom-guix--load-hash-cache)
            (expect doom-guix--hash-cache :to-be-truthy)
            (expect (hash-table-p doom-guix--hash-cache) :to-be-truthy)
            (expect (gethash '("https://example.com" . "abc123")
                             doom-guix--hash-cache)
                    :to-equal "somehash"))
        (delete-directory tmpdir t))))

  (it "creates an empty hash table for missing cache file"
    (let ((doom-guix-hash-cache-file "/tmp/doom-test-nonexistent-cache.el")
          (doom-guix--hash-cache nil))
      (doom-guix--load-hash-cache)
      (expect (hash-table-p doom-guix--hash-cache) :to-be-truthy)
      (expect (hash-table-count doom-guix--hash-cache) :to-equal 0))))


;;
;;; Profile load paths

(describe "doom-guix--profile-load-paths"
  (it "returns nil when the profile directory doesn't exist"
    (let ((doom-guix-profile-dir "/tmp/doom-test-nonexistent-profile"))
      (expect (doom-guix--profile-load-paths) :to-be nil))))

(provide 'test-doom-guix)
;;; test-doom-guix.el ends here
