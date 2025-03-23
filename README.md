# Git-p4 plugin for Magit #

[![MELPA](http://melpa.org/packages/magit-p4-badge.svg)](http://melpa.org/#/magit-p4)
[![Build Status](https://github.com/emacsorphanage/magit-p4/actions/workflows/test.yml/badge.svg)](https://travis-ci.org/lexa/magit-p4)

This package adds integrated `git-p4` support to `Magit`.

> [!NOTE]
> The package is actively maintained again, even though the repository will remain in
> [@emacsorphanage](https://github.com/emacsorphanage). Please don't hesitate to report
> issues and feature requests, though keep in mind I'm normally only able to work on it
> every other weekend, so some delay before I respond is expected.

## Requirements ##

Emacs 27.1 with `magit-4.0.0` or newer, `transient-0.8.0` or newer, and `p4` packages
installed.

> [!NOTE]
> `p4.el` is not maintained anymore and hasn't been updated in many years. The [updated
> fork](https://github.com/JohnC32/perforce-emacs) by
> [@JohnC32](https://github.com/JohnC32) is recommended. Although both the last released
> version (12.0) of the original package and the fork are currently supported, future
> compatibility with the old package is not guaranteed.
>
> Note that the MELPA packages don't currently exist for the fork and it will need to be
> manually configured. This is particularly easy to do using `use-package` and
> [`straight.el`](https://github.com/radian-software/straight.el):
>
> ```
> (use-package p4 :straight (:fork (:repo "JohnC32/perforce-emacs")))
> ```

Additionally, the `git-p4` add-on is required to be installed on your system (outside of
Emacs). If running `git p4` gives you a short help string rather than complaining that
'p4' is not a git command, you have it installed and are good to go. Ordinarily, git comes
with `git-p4` included, so no extra steps should be necesssary, but Debian and Ubuntu
notably do _not_ package it. If that is the case for you, you can download [`git-p4.py`
from git's own source](https://github.com/git/git/blob/master/git-p4.py) and put it
somewhere in your `$PATH`.

## Installation ##

The recommended way to install `magit-p4` is via MELPA and `use-package`, using one of the
following forms (only choose one, as approprita for your configuration):

```elisp
;; If using straight.el with automatic use-package integration,
;; this will automatically install magit-p4 as necessary
(use-package magit-p4 :after magit)

;; This will auto-install using package.el;
;; make sure MELPA is configured as a package repo!
(use-package magit-p4 :after magit :ensure t)

;; Request installation with straight.el explicitly
(use-package magit-p4 :after magit :straight t)
```

If you plan to enable the package manually be sure to have `magit` and `p4` installed
already and add the downloaded package directory to Emacs `load-path`:

    (add-to-list 'load-path "<path-to-magit-p4-directory>")

and then call:

    (require 'magit-p4)

Regardless of which method you choose, `magit-p4` will automatically hook into magit's
menu system and will be available as its own set of commands.

## Usage ##

`magit-p4` is a wrapper for the `git-p4` command, just as `magit` is a
wrapper around `git`, so you should have at least some understanding
of how `git-p4` operates. Please refer to [its
documentation](https://git-scm.com/docs/git-p4) for an introduction.

To invoke `Magit-P4`, inside the Magit status buffer press
<kbd>?</kbd> key to reveal main magit menu. The menu should be have
the <kbd>4</kbd> listed as bound to the `Git P4` submenu
(a.k.a. "Magit-P4 dispatch"). The dispatch has four items which
represent four activities implemented by `git-p4` add-on:

* Cloning - bound to key <kbd>`c`</kbd> and `magit-p4-clone` function;
* Syncing - bound to key <kbd>`f`</kbd> (similar to magit's fetch) and `magit-p4-sync` function;
* Rebasing - bound to key <kbd>`r`</kbd> and `magit-p4-rebase` function;
* Submitting - bound to key <kbd>`P`</kbd> (capital "P", similar to magit's push) and `magit-p4-submit` function.

As a shorthand, the <kbd>4</kbd> key is also bound directly in the magit status buffer,
similar to the majority of git commands, meaning you don't need to press <kbd>?</kbd>
first to access it.

After one of the activities is chosen another menu appears where one may specify
additional options and arguments - like `magit` does.

One may use `magit-p4` function outside `magit` buffer but be sure to do it while the
active buffer reflects a directory _inside_ Git repository. Some of these four functions
make use of command prefix argument (<kbd>C-u</kbd>).

### Cloning ###

Press <kbd>`c`</kbd> inside Magit-P4 dispatch. The clone submenu
presents some additional switches and arguments. The most important
argument is `=d` (destination directory). Without specifying this
argument the default directory will be used for cloning which may not
be appropriate. Then one must press <kbd>`c`</kbd> key once more. The
minibuffer will ask for P4 depot to clone. Pressing <kbd>Tab</kbd> key
invokes completion (here `p4` package comes with help). Unfortunately,
the completion might be slow due to network communication. The
cloning process is launched asynchronously so one must visit *magit
process* buffer from time to time to know cloning current status.

You may use directly `magit-p4-clone` command prepending with <kbd>C-u</kbd> - the command
will ask you for target directory after you choose P4 depot to clone.

### Syncing ###

Press <kbd>`f`</kbd> inside Magit-P4 dispatch. The submenu presents
some additional switches and arguments for `git-p4 sync`
command. Press <kbd>`p`</kbd> to start synchronization. If command
prefix argument (<kbd>C-u</kbd>) has been pressed before the final
<kbd>`s`</kbd> you will be presented with minibuffer asking for P4
depot path to sync with (of course the completion will work thanks to
`p4` package).

### Rebasing ###

Press <kbd>`r`</kbd> inside Magit-P4 dispatch. Additional switches are provided in rebase
submenu. Finally press <kbd>`r`</kbd> key to rebase. This will first sync the git repo
with the P4 depot, then rebase the current branch on top of the `p4/master` remote.

### Submitting ###

Press <kbd>`P`</kbd> inside Magit-P4 dispatch. Additional switches are
provided in submit submenu. Press <kbd>`p`</kbd> to submit. A buffer
will appear where you may add some more info to submitted change-set
log. Press <kbd>C-x</kbd><kbd>#</kbd> to confirm (`emacsclient` is
used). By default, `git-p4` will both prepare a new CL and submit it
via the `p4` client, but by using the `-p` switch, it's possible to
prepare the CL only, for later manual submission.

## Credits ##

Magit-P4 was originally written by Damian T. Dobroczyński (<qoocku@gmail.com>), and later
maintained by Aleksey Fedotov (<lexa@cfotr.com>). It is currently maintained by Maciej
Katafiasz (<mathrick@gmail.com>).
