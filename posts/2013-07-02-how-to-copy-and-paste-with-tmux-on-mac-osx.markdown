---
title: How to Copy and Paste with Tmux on Mac OSX
tags: tmux
---

**UPDATE (2013/07/23):** What follows is only necessary prior to tmux
version 1.8. If you're using tmux 1.8 or later, check out [Thoughtbot's
notes on using
`copy-pipe`](http://robots.thoughtbot.com/post/55885045171/tmux-copy-paste-on-os-x-a-better-future).

# Step 1: Install reattach-to-user-namespace

If you're using Homebrew:

``` bash
brew install reattach-to-user-namespace
```

If not, go compile [the
code](https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard) yourself.

# Step 2: Configure the Deafult Shell Command

Open your Tmux config file (usually `~/.tmux.conf`) and add
the\
following:

``` bash
set-option -g default-command "command -v reattach-to-user-namespace >/dev/null 2>&1 && reattach-to-user-namespace -l $SHELL || $SHELL -l"
```

Now, every time you open a new window or pane, Tmux will will attempt to
use\
`reattach-to-user-namespace` before starting your shell.

# Step 3: Configure Copy-Mode Bindings

Assuming you're using the vi keybindings, add the following to your
config:

``` bash
bind -t vi-copy v begin-selection
bind -t vi-copy c copy-selection
bind y send-keys c\;\
       run-shell "tmux save-buffer - | reattach-to-user-namespace pbcopy"
```

Here's what each line does:

1.  Just for convenience; pressing v will now begin a selection.
2.  Provides a key binding that we'll use in line 3.
3.  Provides a global keybinding (*prefix*-y) that invokes
    `copy-selection` and then pipes the buffer into
    `pbcopy`.\
    Unfortunately, we have to use a global keybinding here because tmux
    cannot bind multiple commands to any key under a specific key-table,
    only globally.

Et voila - you can now use *prefix*-y to copy text to the clipboard.

If you're using emacs bindings, you might want to use something like
this\
instead:

``` bash
# unbind M-w; we'll use prefix-M-w instead
unbind -t emacs-copy M-w
bind -t emacs-copy c copy-selection
bind M-w send-keys c\;\
       run-shell "tmux save-buffer - | reattach-to-user-namespace pbcopy"
```
