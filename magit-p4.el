;;; magit-p4.el --- git-p4 plug-in for Magit

;; Copyright (C) 2014 Damian T. Dobroczyński
;;
;; Author: Damian T. Dobroczy\\'nski <qoocku@gmail.com>
;; Keywords: vc tools
;; Package: magit-p4

;; Magit-p4 is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit-p4 is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit-p4.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This plug-in provides git-p4 functionality as a separate component
;; of Magit.

;;; Code:

(require 'magit)

(eval-when-compile
  (require 'cl-lib)
  (require 'find-lisp)
  (require 'p4))

(declare-function find-lisp-find-files-internal 'find-lisp)

;;; Options

(defgroup magit-p4 nil
  "Git-p4 support for Magit."
  :group 'magit-extensions)

;;; Commands

;;;###autoload
(defun magit-p4-clone (depot-path &optional target-dir)
  "Clone given depoth path.

   The first argument is P4 depot path to clone. The second optional argument
   is directory which will hold the Git repository."
  (interactive
   (append (list (p4-read-arg-string "Depot path: " "//" 'filespec))
           (if (and (not (search "destination=" magit-custom-options))
                    current-prefix-arg)
             (read-directory-name "Target directory: ")
             nil)))
  (magit-run-git-async "p4" "clone" (cons depot-path magit-custom-options)))

;;;###autoload
(defun magit-p4-sync (&optional depot-path)
  "Synchronize with default and/or given depoth path.

   The optional argument is P4 depot path which will be synchronised with.
   If not present, git-p4 will try to synchronize with default depot path which
   has been cloned to before."
  (interactive
   (when current-prefix-arg
     (list (p4-read-arg-string "With (another) depot path: " "//" 'filespec))))
  (magit-run-git-async "p4" "sync"
                       (cond (depot-path
                              (cons depot-path magit-custom-options))
                             (t magit-custom-options))))

;;;###autoload
(defun magit-p4-rebase ()
  "Runs git-p4 rebase."
  (interactive)
  (magit-run-git-async "p4" "rebase" magit-custom-options))

;;;###autoload
(defun magit-p4-submit ()
  "Runs git-p4 submit."
  (interactive)
  ;; git-p4 invokes editor using values of P4EDITOR or GIT_EDITOR variables
  ;; here we temporarily set P4EDITOR (it has precedence in git-p4) to "emacsclient"
  (let ((p4editor (getenv "P4EDITOR")))
    (setenv "P4EDITOR" "emacsclient")
    (magit-run-git-async "p4" "submit" magit-custom-options)
    (setenv "P4EDITOR" p4editor)))

;;; Utilities

(defun magit-p4-enabled ()
  t)

;;; Keymaps

(easy-menu-define magit-p4-extension-menu
  nil
  "Git P4 extension menu"
  '("Git P4"
    :visible magit-p4-mode
    ["Clone" magit-p4-clone (magit-p4-enabled)]
    ["Sync" magit-p4-sync (magit-p4-enabled)]
    ["Rebase" magit-p4-rebase (magit-p4-enabled)]
    ["Submit" magit-p4-submit (magit-p4-enabled)]))

(easy-menu-add-item 'magit-mode-menu
                    '("Extensions")
                    magit-p4-extension-menu)

;;; Add Perfoce group and its keys
(let ((p4-groups '((p4 (actions (("c" "Clone" magit-key-mode-popup-p4-clone)
                                 ("r" "Rebase" magit-key-mode-popup-p4-rebase)
                                 ("S" "Submit" magit-key-mode-popup-p4-submit)
                                 ("s" "Sync" magit-key-mode-popup-p4-sync))))
                   (p4-sync  (actions (("s" "Sync" magit-p4-sync)))
                             (switches (("-b" "Branch" "--branch")
                                        ("-db" "Detect branches" "--detect-branches")
                                        ("-s" "Silent" "--silent")
                                        ("-ib" "import Labels" "--import-labels")
                                        ("-il" "import Local" "--import-local")
                                        ("-p" "Keep path" "--keep-path")
                                        ("-c" "Client spec" "--use-client-spec")))
                             (arguments (("=c" "Changes files" "--changesfile=" read-from-minibuffer)
                                         ("=m" "Max changes" "--max-changes=" read-from-minibuffer))))
                   (p4-clone (actions (("c" "Clone" magit-p4-clone)))
                             (switches (("-b" "Bare clone" "--bare")))
                             (arguments (("=d" "Destination directory" "--destination=" read-directory-name)
                                         ("=/" "Exclude depot path" "-/ " read-from-minibuffer))))
                   (p4-submit (actions (("s" "Submit all" magit-p4-submit)))
                              (switches (("-M" "Detect renames" "-M")
                                         ("-u" "Preserve user" "--preserve-user")
                                         ("-l" "Export labels" "--export-labels")
                                         ("-n" "Dry run" "--dry-run")
                                         ("-p" "Prepare P4 only" "--prepare-p4-only")))
                              (arguments (("=o" "Upstream location to submit" "--origin=" read-from-minibuffer)
                                          ("=c" "Conflict resolution (ask|skip|quit)" "--conflict="
                                           (lambda (prompt)
                                             (first (completing-read-multiple prompt '("ask" "skip" "quit")))))
                                          ("=b" "Sync with branch after" "--branch=" read-from-minibuffer))))
                   (p4-rebase (actions (("r" "Rebase" magit-p4-rebase)))
                              (switches (("-l" "Import labels" "--import-labels")))))))
  (dolist (group-def p4-groups)
    (let* ((group (first group-def))
           (keys (cdr group-def))
           (switches (append '(("-g" "GIT_DIR" "--git-dir")
                               ("-v" "Verbose" "--verbose"))
                             (second (assoc 'switches keys))))
           (arguments (second (assoc 'arguments keys)))
           (actions (second (assoc 'actions keys))))
      (message "group: %s %s" group actions)
      ;; (re-)create the group
      (magit-key-mode-add-group group)
      ;; magit so far is buggy here 'cause "arguments" group is not created by default
      (let ((options (assoc group magit-key-mode-groups)))
        (setcdr (last options) (list (list 'arguments)))
        (dolist (switch switches)
          (apply 'magit-key-mode-insert-switch (cons group switch)))
        (assoc 'arguments options)
        (dolist (argument arguments)
          (apply 'magit-key-mode-insert-argument (cons group argument)))
        (dolist (action actions)
          (apply 'magit-key-mode-insert-action (cons group action))))
        ;; generate and bind the menu popup function
      (magit-key-mode-generate group)))
    (magit-key-mode-insert-action 'dispatch "4" "git-p4" 'magit-key-mode-popup-p4))

(defvar magit-p4-mode-map
  (make-sparse-keymap))

;;; Mode

;;;###autoload
(define-minor-mode magit-p4-mode
  "P4 support for Magit"
  :lighter " P4"
  :require 'magit-p4
  :keymap 'magit-p4-mode-map
  (or (derived-mode-p 'magit-mode)
      (user-error "This mode only makes sense with magit"))
  (when (called-interactively-p 'any)
    (magit-refresh)))

(provide 'magit-p4)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-p4.el ends here
