;;; undo-io/undo-io.el -*- lexical-binding: t; -*-

;;; Miscellaneous commands and settings for developers at Undo.

(require 'cl-lib)
(require 'magit)
(require 'with-editor)

(defgroup Undo-io () "Support for development at Undo")

(defcustom Undo-default-repo (expand-file-name "~/undo")
  "Default directory for the repository root."
  :type 'file
  :group 'Undo-io)

(defvar Undo-release-components
  '(binaries changelogs docs-devel docs-engine-api docs-external docs-external-libundo
    docs-internal docs-java docs-manpages examples gdb gdb-patches generated-files
    python-packages static-files)
  "List of optional components in a release.")

(defcustom Undo-release-target '("x64" "release" ())
  "Specification of release to build and test against."
  :type `(list (radio :tag "Architecture"
                      (const :tag "(all)" nil)
                      (const "arm64")
                      (const "x32")
                      (const "x64")
                      (const "x64-x32"))
               (radio :tag "Build type"
                      (const "release")
                      (const "debug"))
               (set :tag "Components"
                    ,@(cl-loop for component in Undo-release-components
                               collect (list 'const component))))
  :group 'Undo-io)

(defun Undo-release-target-name ()
  "Name of release target to build and test against."
  (cl-destructuring-bind (arch type components) Undo-release-target
    (format "release%s%s"
            (if (string= type "debug") "-debug" "") ; see #19121
            (if arch (format "-%s" arch) ""))))

(defun Undo-repo-root ()
  "Return the directory of the current repository root (or default if not in any repo)."
  (or (magit-repository-local-repository) Undo-default-repo))

(defmacro Undo-at-repo-root (&rest body)
  "Evaluate body with default-directory set to the current repository root."
  `(let ((default-directory (Undo-repo-root))) ,@body))

(defun Undo-compile (command)
  "Run a compilation at the repository root."
  (interactive (list (compilation-read-command command)))
  (Undo-at-repo-root (compile command)))

(defun Undo-build-local-release ()
  "Build a local release."
  (interactive)
  (Undo-compile
   (cl-destructuring-bind (arch type components) Undo-release-target
     (format "build/build_release --colour=never --log-level=WARNING --build-type=%s %s %s"
             type (Undo-release-target-name)
             (cl-loop for component in components concat (format " -c %s" component))))))

;; (defun Undo-rebuild-tags ()
;;   "Rebuild TAGS table."
;;   (interactive)
;;   (Undo-compile "rm -f TAGS && ./make.py -j8 --emacs --colour=never --log-level=WARNING TAGS"))

(defun Undo-test-case-at-point ()
  "Return the test case enclosing point, if any, or NIL otherwise."
  (save-excursion
    (cl-loop do (back-to-indentation)
             when (looking-at "^def \\(test[0-9]+\\)(") return (match-string 1)
             while (beginning-of-defun))))

(defun Undo-format-shell-command (format &rest args)
  "Format string and arguments, quoting each argument for the shell."
  (apply #'format format (mapcar #'shell-quote-argument args)))

(defun Undo-run-test (testcase)
  "Run a test case using the local release."
  (interactive (list (read-from-minibuffer "Case: " (Undo-test-case-at-point))))
  (Undo-compile
   (Undo-format-shell-command
    "./make.py --emacs --colour=never --log-level=INFO --test-params=%s,unchanged=1 %s"
    (Undo-release-target-name) testcase)))

(defun Undo-run-jenkins (suite branch)
  "Trigger Jenkins to run a test suite on a branch."
  (interactive
   (list
    (completing-read "Suite: " '("pre-push" "pre-merge" "cross-machine" "python-static-analysis"
                                 "ubsan" "suite level-min-pre-release" "vscode" "website" "fatcat" "new-distros"))
    (magit-read-local-branch "Branch")))
  (Undo-at-repo-root
    (shell-command (format "infra/jenkins trigger %s %s" suite (shell-quote-argument branch)))))

(defun Undo-add-changelog-entry (issue)
  "Add a single changelog entry for ISSUE using Emacs as the editor."
  (interactive (list (read-from-minibuffer "Issue: ")))
  (Undo-at-repo-root
     (with-editor-async-shell-command
      (Undo-format-shell-command "src/changelogs/add %s" issue)  "*Changelog Script Output*")))


;; Key bindings for Undo commands, using the "C-c u" prefix.

(global-set-key (kbd "C-c u j") #'Undo-run-jenkins)
(global-set-key (kbd "C-c u r") #'Undo-build-local-release)
(global-set-key (kbd "C-c u t") #'Undo-run-test)
;; (global-set-key (kbd "C-c u T") #'Undo-rebuild-tags)
(global-set-key (kbd "C-c u c") #'Undo-add-changelog-entry)


;; git-link configuration. For version earlier than v0.8.1 git-link wrongly
;; downcases the repo name, for example "Undo/core" becomes "undo/core",
;; requiring correction.

(defmacro Undo-git-link-handler (name handler)
  "Wrap a git-link handler function, specifying the correct repository name."
  `(defun ,name (hostname dirname &rest args)
     (let ((corrected-dirname (replace-regexp-in-string "^undo/" "Undo/" dirname t)))
       (apply ,handler hostname corrected-dirname args))))

(Undo-git-link-handler Undo-git-link #'git-link-github)
(Undo-git-link-handler Undo-git-link-commit #'git-link-commit-github)

(defun git-link-sourceware (hostname dirname filename branch commit start end)
  "Git-link handler function for projects on sourceware.org."
  (let* ((split-dirname (split-string dirname "/"))
         (directory (car split-dirname))
         (project (string-join (cdr split-dirname) "/")))
    (format "https://sourceware.org/%s/?p=%s.git;a=blob;f=%s;hb=%s%s"
            directory project filename commit (if start (format "#l%s" start) ""))))

(eval-after-load 'git-link
  '(progn
     (add-to-list 'git-link-remote-alist '("git\\.undoers\\.io" Undo-git-link))
     (add-to-list 'git-link-commit-remote-alist '("git\\.undoers\\.io" Undo-git-link-commit))
     (add-to-list 'git-link-remote-alist '("sourceware\\.org" git-link-sourceware))))

(provide 'undo-io)
