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
;;; Scheme package expression generation

(describe "doom-guix--scheme-package-expr"
  (it "produces an inheriting definition with git-fetch for pinned packages"
    (let ((doom-packages '((magit :recipe (:host github :repo "magit/magit"))))
          (doom-guix-name-map nil)
          (doom-guix--hash-cache (make-hash-table :test #'equal)))
      (puthash (cons "https://github.com/magit/magit" "abc123")
               "0fakehash00000000000000000000000000000000000000000000"
               doom-guix--hash-cache)
      (let ((sexpr (doom-guix--scheme-package-expr
                   'magit '(:host github :repo "magit/magit") "abc123")))
        (expect sexpr :to-be-truthy)
        (expect sexpr :to-match "define doom-magit")
        (expect sexpr :to-match "inherit")
        (expect sexpr :to-match "specification->package")
        (expect sexpr :to-match "git-fetch")
        (expect sexpr :to-match "abc123")
        (expect sexpr :to-match "0fakehash"))))

  (it "produces a specification->package reference for unpinned packages"
    (let ((doom-packages nil)
          (doom-guix-name-map nil))
      (let ((sexpr (doom-guix--scheme-package-expr 'evil nil nil)))
        (expect sexpr :to-be-truthy)
        (expect sexpr :to-match "define doom-evil")
        (expect sexpr :to-match "specification->package")
        (expect sexpr :not :to-match "git-fetch")
        (expect sexpr :not :to-match "inherit"))))

  (it "uses emacs-<name> guix name in specification->package"
    (let ((doom-packages nil)
          (doom-guix-name-map nil))
      (let ((sexpr (doom-guix--scheme-package-expr 'foo nil nil)))
        (expect sexpr :to-match "\"emacs-foo\"")))))


;;
;;; REPL response parsing

(describe "doom-guix--parse-repl-response"
  (it "parses (values ...) into a list"
    (expect (doom-guix--parse-repl-response
             "(values \"/gnu/store/abc-profile\")")
            :to-equal '("/gnu/store/abc-profile")))

  (it "parses multiple values"
    (expect (doom-guix--parse-repl-response "(values 1 2 3)")
            :to-equal '(1 2 3)))

  (it "parses boolean values"
    (expect (doom-guix--parse-repl-response "(values #t)")
            :to-equal '("#t")))

  (it "signals error for exception responses"
    (expect (doom-guix--parse-repl-response
             "(exception misc-error \"something broke\")")
            :to-throw 'error))

  (it "signals error for empty responses"
    (expect (doom-guix--parse-repl-response "")
            :to-throw 'error))

  (it "signals error for unexpected responses"
    (expect (doom-guix--parse-repl-response "garbage")
            :to-throw 'error)))


;;
;;; Channel management

(describe "doom-guix channel generation"
  (it "includes guix and guix-emacs channels"
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
              (expect content :to-match "savannah")
              (expect content :to-match "garrgravarr")
              ;; No local doom channel anymore
              (expect content :not :to-match "'doom)")))
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
