# Installation

Download the repository and load `wi-project.el` or `wi-servers.el` in Emacs
config `~/.emacs`:

``` emacs-lisp
(load "PATH_TO_REPOSITORY/wi-project.el")
(load "PATH_TO_REPOSITORY/wi-servers.el")
```

# Configuration

First of all you need to define wi-groups-direcotory and
wi-projects-directories:

``` emacs-lisp
(setq wi-groups-direcotory "~/gitlab")
(setq wi-projects-directories '("~/src" "~/archive/src"))
```

Then you could use one commands bellow.

# Usage

## wi-project

  * `wi-project-ivy` opens Ivy window to find a project.
  * `wi-project-browse-at-remote` opens project with browser.
  * `wi-project-list-describe` opens Magit for a project.
  * `wi-project list` projects in tablist-mode.

## wi-servers

Open a list of servers in `.intr` domain with `wi-installed-servers` list
servers, mark them with <kbd>m</kbd> key and apply one of commands:

  * `wi-servers-list-open-ansible-console` <kbd>A</kbd> open Ansible console.
  * `wi-servers-list-tramp` <kbd>f</kbd> open TRAMP.
  * `wi-servers-list-tramp-sudo` <kbd>F</kbd> open TRAMP as root (via sudo).
  * `wi-servers-list-xpanes-open-terminal` <kbd>S</kbd> open [xpanes](https://github.com/greymd/tmux-xpanes "tmux-based terminal divider") with interactive SSH session.
  * `wi-servers-list-xpanes-open-top` <kbd>t</kbd> open xpanes with `top` command.
