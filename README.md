# Git-p4 plugin for Magit #

Attention! This is in development. Some of the info below is nothing but a plain lie!

## Requirements ##

Emacs 24.x with `magit` and `p4` packages installed. You do not have to install these packages if you use Melpa service - all required dependencies will be dowloaded altogether with `magit-p4`.
`git-p4` add-on is obviously required.

## Installation ##

The package should be available via Melpa service using elpa packages mechanism.
If you plan to enable the package manually be sure to have `magit` and `p4` installed already
and add the downloaded package directory to Emacs `load-path`:

    (add-to-list load-path "<path-to-magit-p4-directory")

and then you should call:

    (require 'magit-p4)

Evalutation of the package code is sufficient to plug into `magit` menu some helpful key bindings.

## Usage ##

Being inside Magit status buffer one should press `?` key to reveal magit menu. The menu should be equipped with `4` key binded to `magit-p4` submenu, all right. The submenu has four items which represent four activities implemented by `git-p4` add-on:

* Cloning - binded to key `c` and `magit-p4-clone` function;
* Syncing - binded to key `s` and `magit-p4-sync` function;
* Rebasing - binded to key `r` and `magit-p4-rebase` function;
* Submitting - binded to key `S` (capital "s") and `magit-p4-submit` function.

After one of the activities is chosen another menu appears where one may specify additional options and arguments - like `magit` does.

One may use `magit-p4` function outside `magit` buffer but one may be sure to do it while the active buffer reflects a directory _inside_ Git repository. Some of these four functions make use of command prefix argument (`C-u`).

### Cloning ###

Press `4` key in `magit` main menu and then `c` key. The submenu presents some additional switches and arguments. The most important argument is `=d` (destination directory). Without specifying this argument the default directory will be used for cloning which may not be appropriate. Then one must press `c` key once more. The minibuffer will ask for P4 depot to clone. Pressing `Tab` key invokes completion (here `p4` package comes with help). Unfortunately, the completion might be longsome due to network communication. The cloning process is launched asynchronously so one must visit *magit process* buffer from time to time to know cloning current status.

You may use directly `magit-p4-clone` command prepending with `C-u` - the command will ask you for target directory after you choose P4 depot to clone.

### Syncing ###

Press `4` key in `magit` buffer followed with `s` key. The submenu presents some additional switches and arguments for `git-p4 sync` command. Press `s` once again and the synchronization starts. If command prefix argument (`C-u`) has been pressed before the final `s` you will be presented with minibuffer asking for P4 depot path to sync with (of course the completion will work thanks to `p4` package).

### Rebasing ###

While visiting *magit status* buffer press `4` and `r` key. Additional switches are provided in rebase submenu. Finally press `r` key.

### Submitting ###

While visiting *magit status* buffer press `4` and `r` key. Additional switches are provided in submit submenu. Press `s` key to submit. A buffer will appear where you may add some more info
to submitted change-set log. Press `C-c C-c` to confirm.

## Some consideration using p4 and git-p4 tools ##

* `p4` package usage - so far some of the package functions are used to help while choosing P4 depoth (for example while cloning or syncing from/with P4 depot). The best solution to make this process painless is to have a P4 client file somewhere within the working directory tree defining default port, user and password.

TO BE CONTINUED...
