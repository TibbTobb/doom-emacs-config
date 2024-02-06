;;; undo-io/undo-io-cloud.el -*- lexical-binding: t; -*-
(require 's)
(require 'dash)

(require 'undo-io)

(defvar Undo-cloud-machines-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'Undo-cloud-ssh)
    (define-key map (kbd "g") 'Undo-cloud-machines)
    (define-key map (kbd "u") 'Undo-cloud-user-machines)
    (define-key map (kbd "t") 'Undo-cloud-test-from-name))
  map)

(define-derived-mode Undo-cloud-machines-mode tabulated-list-mode "Undo cloud machines"
  "Major mode for managing Undo's cloud VMs"
  (setq tabulated-list-format [("#" 3 t)
                               ("name" 55 t)
                               ("initiator" 35 t)
                               ("user@ip" 20 t)
                               ("distro" 20 t)
                               ("mem" 5 t)
                               ("cpu" 4 t)
                               ("disk" 4 t)
                               ("uptime" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "initiator" nil))
  (tabulated-list-init-header))

(defvar Undo-cloud-images-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'Undo-cloud-images)
    (define-key map (kbd "t") 'Undo-cloud-test-from-slug))
  map)

(define-derived-mode Undo-cloud-images-mode tabulated-list-mode "Undo cloud images"
  "Major mode for managing images available to Undo's cloud VMs"
  (setq tabulated-list-format [("#" 3 t)
                               ("id" 25 t)
                               ("region" 10 t)
                               ("slug" 30 t)
                               ("distro" 12 t)
                               ("version" 10 t)
                               ("arch" 5 t)
                               ("disk" 4 t)
                               ("date" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "slug" nil))
  (tabulated-list-init-header))

(defun Undo-cloud--run-cloudctl (arg-string)
  "Run 'python -m infra.cloudctl' with the arguments given in ARG-STRING"
  (s-trim (shell-command-to-string (format "python -m infra.cloudctl %s" arg-string))))

(defun Undo-cloud-ssh (machine)
  "SSH to a machine running in Undo's cloud"
  (interactive (list (read-from-minibuffer "Name: " (tabulated-list-get-id))))
  (shell (format "*Undo ssh %s*" machine))
  (insert (concat "python -m infra.cloudctl -s " machine))
  (comint-send-input))

(defun Undo-cloud-test-from-slug (slug testcase)
  "Run the test case given by TESTCASE on an aws cloud machine,
as specified by SLUG."
  (interactive (list (read-from-minibuffer "Slug: " (tabulated-list-get-id))
                     (read-from-minibuffer "Case: ")))
  (Undo-compile
   (Undo-format-shell-command
    "./make.py --emacs --colour=never --log-level=INFO f:aws-%s-%s:%s,%s"
    slug (user-login-name) testcase (Undo-release-target-name))))

(defun Undo-cloud-test-from-name (name testcase)
  "Run the test case given by TESTCASE on an aws cloud machine,
as specified by SLUG."
  (interactive (list (read-from-minibuffer "name: " (tabulated-list-get-id))
                     (read-from-minibuffer "Case: ")))
  (Undo-compile
   (Undo-format-shell-command
    "./make.py --emacs --colour=never --log-level=INFO f:%s,reuse:%s,%s"
    name testcase (Undo-release-target-name))))

(defun Undo-cloud--is-header-line (x)
  "Returns true when given the header line from infra/cloudctl.py, otherwise nil"
  (or (s-starts-with? "-" x)
      (not (s-numeric? (nth 0 (s-split-words x))))))

(defun Undo-cloud--line-split (x)
  "A helper function for parsing text output from infra/cloudctl.py into a list"
  (-map 's-trim (s-split "|" x)))

(defun Undo-cloud--line-convert (id-index data)
  "A helper function for converting lists into tabular data for
tabulated-list-mode.

DATA should be a list of lists containing the output of an
infra/cloudctl.py command. ID-INDEX should the the zero indexed
column of the line DATA to be used as the tabulated list id (see
tabulated-list-get-id)."
  (list (nth id-index x) (apply 'vector x)))

(defun Undo-cloud--tabulated-data (arg-string id-index)
  "Run Undo's cloudctl script with the arguments in ARG-STRING,
parse, and return the data in a format appropriate for
tabulated-data-mode. ID-INDEX should be the index of the column
to use as the tabulated-list-id"
  (cd (Undo-repo-root))
  (let* ((raw-lines (s-lines (Undo-cloud--run-cloudctl arg-string)))
         (no-header-lines (-drop-while 'Undo-cloud--is-header-line raw-lines))
         (split-lines (-map 'Undo-cloud--line-split no-header-lines))
         (tabulated-data (-map '(lambda (x) (Undo-cloud--line-convert id-index x)) split-lines)))
    tabulated-data))

(defun Undo-cloud-machines ()
  "Display a table of machines running in Undo's cloud."
  (interactive)
  (pop-to-buffer "*Undo cloud machines*" nil)
  (Undo-cloud-machines-mode)
  (setq tabulated-list-entries (Undo-cloud--tabulated-data "-ls" 1))
  (tabulated-list-print))

(defun Undo-cloud-user-machines ()
  "Display a table of machines running in Undo's cloud."
  (interactive)
  (pop-to-buffer "*Undo cloud machines*" nil)
  (Undo-cloud-machines-mode)
  (setq tabulated-list-entries (Undo-cloud--tabulated-data "-lus" 1))
  (tabulated-list-print))

(defun Undo-cloud-images ()
  "Display a table of images that can be run in Undo's cloud."
  (interactive)
  (pop-to-buffer "*Undo cloud images*" nil)
  (Undo-cloud-images-mode)
  (setq tabulated-list-entries (Undo-cloud--tabulated-data "-lmi" 3))
  (tabulated-list-print))

(provide 'undo-io-cloud)
