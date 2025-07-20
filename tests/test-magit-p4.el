;;; -*- lexical-binding: t; -*-

(eval-and-compile
  (require 'buttercup)
  (require 'cl-lib)
  (require 'magit)
  (require 'magit-p4)
  (load "helpers"))

(describe "Magit-P4 Magit dispatch menu integration"
  (before-each
    (set-up-temp-dir)
    (set-up-git-repo))

  (after-each
    (tear-down-temp-dir))

  (it "should hook into Magit's dispatch menu"
    (magit-status *test-git-repo*)
    (magit-dispatch)
    (expect (buffer-contains-p "4 Git P4" " *transient*"))))

;; This isn't actually exercising these popups, but it should catch the "doesn't even
;; open" type of errors and incompatibilities that creep in
(describe "Popup should open without errors:"
  (before-each
    (set-up-temp-dir)
    (set-up-git-repo))

  (after-each
    (tear-down-temp-dir))

  (cl-loop for popup in '(magit-p4-clone-popup
                          magit-p4-sync-popup
                          magit-p4-submit-popup)
           ;; Ensure variable capture for `popup'
           do (let ((popup popup))
                (it (format "%s" popup)
                  (without-transient-debugger
                   (funcall popup)
                   (transient-quit-all))))))
