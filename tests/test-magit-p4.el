;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'magit)
(require 'magit-p4)

(load "helpers")

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
