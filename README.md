# Git-p4 plugin for Magit #

[![MELPA](http://melpa.org/packages/magit-p4-badge.svg)](http://melpa.org/#/magit-p4)
[![Build Status](https://travis-ci.org/lexa/magit-p4.svg?branch=master)](https://travis-ci.org/lexa/magit-p4)

This package is in development, and some functionality might be missing.

## Requirements ##

Emacs 25.1 with `magit-3.3.0` or newer, `transient-0.4.3` or newer, and `p4` packages installed. You do not have to install these packages yourself if you use [MELPA](https://melpa.org) - all required dependencies will be downloaded together with `magit-p4`.

Additionally, the `git-p4` add-on is required to be installed on your system (outside of Emacs). If running `git p4` gives you a short help string rather than complaining that 'p4' is not a git command, you have it installed and are good to go. Ordinarily, git comes with `git-p4` included, so no extra steps should be necesssary, but Debian and Ubuntu notably do _not_ package it. If that is the case for you, you can download [`git-p4.py` from git's own source](https://github.com/git/git/blob/master/git-p4.py) and put it somewhere in your `$PATH`.

## Installation ##

The recommended way to install `magit-p4` is via MELPA and `use-package`:

```elisp
(use-package magit-p4)
```

If you plan to enable the package manually be sure to have `magit` and `p4` installed already and add the downloaded package directory to Emacs `load-path`:

    (add-to-list 'load-path "<path-to-magit-p4-directory>")

and then call:

    (require 'magit-p4)

Regardless of which method you choose, `magit-p4` will automatically hook into magit's menu system and will be available as its own set of commands.

## Usage ##

Inside the Magit status buffer, press <kbd>?</kbd> key to reveal main magit menu. The menu should be equipped with <kbd>4</kbd> key binded to `magit-p4` submenu. The submenu has four items which represent four activities implemented by `git-p4` add-on:

* Cloning - bound to key <kbd>`c`</kbd> and `magit-p4-clone` function;
* Syncing - bound to key <kbd>`f`</kbd> (similar to magit's fetch) and `magit-p4-sync` function;
* Rebasing - bound to key <kbd>`r`</kbd> and `magit-p4-rebase` function;
* Submitting - bound to key <kbd>`P`</kbd> (capital "P", similar to magit's push) and `magit-p4-submit` function.

As a shorthand, the <kbd>4</kbd> key is also bound directly in the magit status buffer, similar to the majority of git commands, meaning you don't need to press <kbd>?</kbd> first to access it.

After one of the activities is chosen another menu appears where one may specify additional options and arguments - like `magit` does.

One may use `magit-p4` function outside `magit` buffer but be sure to do it while the active buffer reflects a directory _inside_ Git repository. Some of these four functions make use of command prefix argument (<kbd>C-u</kbd>).

### Cloning ###

Press <kbd>4</kbd> key in `magit` main menu and then <kbd>`c`</kbd> key. The submenu presents some additional switches and arguments. The most important argument is `=d` (destination directory). Without specifying this argument the default directory will be used for cloning which may not be appropriate. Then one must press <kbd>`c`</kbd> key once more. The minibuffer will ask for P4 depot to clone. Pressing <kbd>Tab</kbd> key invokes completion (here `p4` package comes with help). Unfortunately, the completion might be longsome due to network communication. The cloning process is launched asynchronously so one must visit *magit process* buffer from time to time to know cloning current status.

You may use directly `magit-p4-clone` command prepending with <kbd>C-u</kbd> - the command will ask you for target directory after you choose P4 depot to clone.

### Syncing ###

Press <kbd>4</kbd> key in `magit` buffer followed with <kbd>`s`</kbd> key. The submenu presents some additional switches and arguments for `git-p4 sync` command. Press <kbd>`s`</kbd> once again and the synchronization starts. If command prefix argument (<kbd>C-u</kbd>) has been pressed before the final <kbd>`s`</kbd> you will be presented with minibuffer asking for P4 depot path to sync with (of course the completion will work thanks to `p4` package).

### Rebasing ###

While visiting *magit status* buffer press <kbd>4</kbd> and <kbd>`r`</kbd> key. Additional switches are provided in rebase submenu. Finally press <kbd>`r`</kbd> key.

### Submitting ###

While visiting *magit status* buffer press <kbd>4</kbd> and <kbd>`r`</kbd> key. Additional switches are provided in submit submenu. Press <kbd>`s`</kbd> key to submit. A buffer will appear where you may add some more info
to submitted change-set log. Press <kbd>C-x</kbd><kbd>#</kbd> to confirm (`emacsclient` is used).
