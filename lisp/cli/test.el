;;; lisp/cli/test.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; The heart of Doom's test DSL and framework. Powered by either ERT or
;; Buttercup, this extends testing frameworks to allow for isolated execution
;; contexts on several levels, a more sophisticated CLI for tests, and
;; integration with Doom's profiles system so testing environments can be
;; generated on-the-fly.
;;
;;; Code:

;;
;;; Variables

(defvar doom-test-backend 'ert
  "One of `ert' or `buttercup'.")

(defvar doom-test-isolation-level nil
  "Determines the testing strategy for tests.

Should be one of:

  nil    -- Run all tests in the same session.
  file   -- Run each test file in isolated sessions.
  group  -- Run each group of tests in isolated sessions.
  t      -- Run each individual test in isolated sessions (very slow).")


;;
;;; Commands

(defcli! test
    ((backend ("--ert" "--buttercup"))
     (jobs    ("-j" "--jobs" int))
     &rest targets)
  "Run Doom unit tests.

Discovers and runs ERT test files from lisp/test/ (core tests) and
modules/**/test/ (module tests).

If TARGETS are given, only test files whose path contains one of the target
strings are loaded. For example:

  doom test guix         ; runs lisp/test/test-doom-guix.el
  doom test lang/python  ; runs modules/lang/python/test/*.el

OPTIONS:
  --ert
    Use ERT as the test backend (default).
  --buttercup
    Use Buttercup as the test backend.
  -j, --jobs
    Number of threads for native compilation during tests."
  :benchmark t
  (when backend
    (setq doom-test-backend (intern (string-remove-prefix "--" backend))))
  (when jobs
    (setq native-comp-async-jobs-number (truncate jobs)))
  (let* ((test-files (doom-test--discover-files targets))
         (file-count (length test-files)))
    (unless test-files
      (print! (warn "No test files found%s")
              (if targets
                  (format " matching: %s" (string-join targets ", "))
                ""))
      (exit! 1))
    (print! (start "Running %d test file%s with %s...")
            file-count
            (if (= file-count 1) "" "s")
            doom-test-backend)
    (pcase doom-test-backend
      ('ert       (doom-test--run-ert test-files))
      ('buttercup (doom-test--run-buttercup test-files))
      (_ (print! (error "Unknown test backend: %s") doom-test-backend)
         (exit! 1)))))


;;
;;; Helpers

(defun doom-test--discover-files (&optional targets)
  "Discover test files, optionally filtered by TARGETS.
Core tests live in lisp/test/test-*.el, module tests in
modules/**/test/test-*.el. If TARGETS is non-nil, only files whose path
contains one of the target strings are returned."
  (let ((core-dir (file-name-concat doom-emacs-dir "lisp" "test"))
        (modules-dir doom-modules-dir)
        files)
    ;; Collect core test files
    (when (file-directory-p core-dir)
      (dolist (file (directory-files core-dir t "\\`test-.*\\.el\\'"))
        (push file files)))
    ;; Collect module test files
    (when (file-directory-p modules-dir)
      (dolist (category-dir (doom-test--subdirs modules-dir))
        (dolist (module-dir (doom-test--subdirs category-dir))
          (let ((test-dir (file-name-concat module-dir "test")))
            (when (file-directory-p test-dir)
              (dolist (file (directory-files test-dir t "\\`test-.*\\.el\\'"))
                (push file files)))))))
    (setq files (nreverse files))
    ;; Filter by targets if given
    (if targets
        (cl-remove-if-not
         (lambda (file)
           (cl-some (lambda (target)
                      (string-match-p (regexp-quote target) file))
                    targets))
         files)
      files)))

(defun doom-test--subdirs (dir)
  "Return a list of immediate subdirectories of DIR."
  (cl-remove-if-not #'file-directory-p
                    (directory-files dir t "\\`[^.]")))

(defun doom-test--run-ert (test-files)
  "Load TEST-FILES and run all ERT tests defined in them."
  (require 'ert)
  (print-group!
    (dolist (file test-files)
      (print! (item "Loading %s") (path file))
      (load file nil 'nomessage 'nosuffix))
    (print! (start "Running ERT tests..."))
    (let* ((stats (ert-run-tests-batch t))
           (unexpected (ert-stats-completed-unexpected stats))
           (total (ert-stats-total stats))
           (passed (- total unexpected)))
      (print! (if (zerop unexpected)
                  (success "All %d tests passed" total)
                (error "%d/%d tests failed" unexpected total)))
      (unless (zerop unexpected)
        (exit! 1)))))

(defun doom-test--run-buttercup (test-files)
  "Load TEST-FILES and run all Buttercup tests defined in them."
  (condition-case nil
      (require 'buttercup)
    (error
     (print! (error "Buttercup is not installed. Use --ert or install buttercup."))
     (exit! 1)))
  (print-group!
    (dolist (file test-files)
      (print! (item "Loading %s") (path file))
      (load file nil 'nomessage 'nosuffix))
    (print! (start "Running Buttercup tests..."))
    (let ((buttercup-reporter #'buttercup-reporter-batch))
      (buttercup-run))
    ;; Buttercup signals on failure, so reaching here means success
    (print! (success "All tests passed"))))

(provide 'doom-cli-test)
;;; test.el ends here
