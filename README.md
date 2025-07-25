<!--- -*- gfm -*- -->

# dotfiles

My dotfiles and scripts, and configuration. The shell part is specific for Zsh,
using [zinit](https://github.com/zdharma-continuum/zinit) &
[powerlevel10k](https://github.com/romkatv/powerlevel10k).

## Modularity

The files are managed by [GNU Stow](https://www.gnu.org/software/stow/), divided
by tool or functional area: Git, emacs, etc—see the top level-directories.
Different systems use different subsets of configuration, specified by some
files, as described in the [installation](#installation) section.

## Emacs

The biggest part of configuration is for Emacs, currently version 29.1. The
focus is on seamless integration between all the different packages, avoiding
surprises, and adding polish here and there. Of course, that is very much a work
in progress.

I have started splitting out logical units into standalone Elisp packages, where
it makes sense, to increase maintainability:

- [org-autotask](https://github.com/laurynas-biveinis/org-autotask/): Org agenda
  views, clocking, automation.

The bulk of configuration is in
[my-setup.el](https://github.com/laurynas-biveinis/dotfiles/blob/master/emacs/.emacs.d/my/my-setup.el),
along with some parts split into its own files in the same directory,
there is also
[early-init.el](https://github.com/laurynas-biveinis/dotfiles/blob/master/emacs/.emacs.d/early-init.el)
and
[init.el](https://github.com/laurynas-biveinis/dotfiles/blob/master/emacs/.emacs.d/init.el).
There is also some system-specific setup over at
[darwin.el](https://github.com/laurynas-biveinis/dotfiles/blob/master/emacs/.emacs.d/darwin.el).

I am not using literate configuration, which seems to be very popular among Org
users at least. Maybe I should. Likewise for
[use-package](https://github.com/jwiegley/use-package).

### Configured packages

I always prefer a melpa-stable version, if available and not broken.
Unfortunately, that is not always possible.

#### The big ones

[org](https://orgmode.org) [magit and forge](https://magit.vc)
[lsp-mode](https://github.com/emacs-lsp/lsp-mode)
[flycheck](https://www.flycheck.org)
[company](https://github.com/company-mode/company-mode)
[projectile](https://github.com/bbatsov/projectile)
[m4e](https://djcbsoftware.nl/code/mu/)

#### Major

[GCMH](https://gitlab.com/koral/gcmh)
[org-roam](https://github.com/org-roam/org-roam)
[wgrep](https://github.com/mhayashi1120/Emacs-wgrep)
[org-gcal](https://github.com/kidd/org-gcal.el)
[vterm](https://github.com/akermu/emacs-libvterm)
[deadgrep](https://github.com/Wilfred/deadgrep)
[undo-tree](http://www.dr-qubit.org/undo-tree.html)

#### Nice to have, niche, & specific major modes

[rich-minority](https://github.com/Malabarba/rich-minority)
[dispwatch](https://github.com/mnp/dispwatch)
[lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs)
[calfw](https://github.com/kiwanami/emacs-calfw)
[which-key](https://github.com/justbur/emacs-which-key)
[keyfreq](https://github.com/dacap/keyfreq)
[iedit](https://github.com/victorhge/iedit)
[eldoc-cmake](https://github.com/ikirill/eldoc-cmake)
[aggressive-indent-mode](https://github.com/Malabarba/aggressive-indent-mode)
[yaml-mode](https://github.com/yoshiki/yaml-mode)
[markdown-mode](https://jblevins.org/projects/markdown-mode/)
[ssh-config-mode](https://github.com/jhgorrell/ssh-config-mode-el)
[bison-mode](https://github.com/Wilfred/bison-mode)
[cmake-mode](https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el)
[grab-mac-link](https://github.com/xuchunyang/grab-mac-link.el)
[cheat-sh](https://github.com/davep/cheat-sh.el)
[beginend](https://github.com/DamienCassou/beginend)
[git-modes](https://github.com/magit/git-modes)
[flycheck-google-cpplint](https://github.com/flycheck/flycheck-google-cpplint/)
[difftastic.el](https://github.com/pkryger/difftastic.el)
[pr-review](https://github.com/blahgeek/emacs-pr-review)

#### Appearance

[solarized-theme](https://github.com/bbatsov/solarized-emacs)
[git-gutter-fringe](https://github.com/emacsorphanage/git-gutter-fringe)
[company-box](https://github.com/sebastiencs/company-box)
[page-break-lines](https://github.com/purcell/page-break-lines)
[xterm-color](https://github.com/atomontage/xterm-color)
[all-the-icons-dired](https://github.com/jtbm37/all-the-icons-dired)
[cmake-font-lock](https://github.com/Lindydancer/cmake-font-lock)
[beacon](https://github.com/Malabarba/beacon)
[flycheck-status-emoji](https://github.com/liblit/flycheck-status-emoji)
[stripe-buffer](https://github.com/sabof/stripe-buffer)
[prism.el](https://github.com/alphapapa/prism.el)
[info-colors](https://github.com/ubolonton/info-colors)
[topsy.el](https://github.com/alphapapa/topsy.el)
[org-sticky-header](https://github.com/alphapapa/org-sticky-header)

#### Packages bundled with Emacs

tramp ts

### Improvements, fixed annoyances, and bug workarounds

- [Disable global-display-fill-column-indicator in read-only buffers](https://www.reddit.com/r/emacs/comments/ja97xs/weekly_tipstricketc_thread/g903xa3?utm_source=share&utm_medium=web2x&context=3)
- [macOS: add the missing man page paths for woman](https://www.reddit.com/r/emacs/comments/ig7zzo/weekly_tipstricketc_thread/g34s8dl?utm_source=share&utm_medium=web2x&context=3)
- [Re-enable Shellcheck if using lsp-mode with bash-language-server](https://www.reddit.com/r/emacs/comments/hqxm5v/weekly_tipstricketc_thread/fy4pvr8?utm_source=share&utm_medium=web2x&context=3)
- [Clicking on URLs should open them instead of spell checking them](https://www.reddit.com/r/emacs/comments/it4m2w/weekly_tipstricketc_thread/g5pff92?utm_source=share&utm_medium=web2x&context=3)
- Workaround the [projectile
  issue](https://github.com/bbatsov/projectile/issues/347) of remote projects
  not being added to Projectile project list.
- Add [Projectile reconfigure command for CMake
  presets](https://github.com/bbatsov/projectile/issues/1676).
- [Automatically update Git gutter on Magit
  actions](https://stackoverflow.com/questions/43236670/visual-studio-code-git-diff-over-git-gutter-indicator).
- [Disable color-identifiers-mode under LSP with semantic
  highlighting](https://github.com/laurynas-biveinis/dotfiles/blob/ce044dab576c525f418a5383180d06c888a33599/emacs/.emacs.d/setup.el#L1924)
- [Reduce LSP info in the modeline](https://github.com/laurynas-biveinis/dotfiles/commit/be71cb57292e3cda3759a373a0b7c38688780ab0)
- [Show Git ignored files in `dired-ignored` face](https://www.reddit.com/r/emacs/comments/u2lf9t/comment/i4n9aoa/?utm_source=share&utm_medium=web2x&context=3)

### Custom commands and functionality

- [Set frame geometry after docking/undocking laptop automatically](https://www.reddit.com/r/emacs/comments/ev2q9q/weekly_tipstricketc_thread/fftpfj0?utm_source=share&utm_medium=web2x&context=3)
- `my-recompile-packages`: force recompiling all the installed packages, after
  a Emacs version upgrade or a borked package upgrade.
- `kill-buffers-rm-worktree`: bound to `y` in the Projectile keymap, kills all
  buffers and executes `gitrmworktree.`

## Installation

See
[INSTALLATION.md](https://github.com/laurynas-biveinis/dotfiles/blob/master/INSTALLATION.md)

## New system setup

See
[setup-ubuntu.sh](https://github.com/laurynas-biveinis/dotfiles/blob/master/setup-ubuntu.sh),
[setup-centos6.sh](https://github.com/laurynas-biveinis/dotfiles/blob/master/setup-centos6.sh),
and
[setup-macos.sh](https://github.com/laurynas-biveinis/dotfiles/blob/master/setup-macos.sh).

## Dotfiles, Emacs distros, macOS defaults, etc. I have been stealing from

- [Doom Emacs](https://github.com/hlissner/doom-emacs)
- [EmacsWiki: Dot Emacs
  Challenge](https://www.emacswiki.org/emacs/DotEmacsChallenge)
- <https://sites.google.com/site/steveyegge2/my-dot-emacs-file>
- <https://emacs.nasy.moe/>
- <https://ebzzry.io/en/emacs-pairs/>
- <https://github.com/snackon/Witchmacs>
- <https://github.com/jhenahan/dots/tree/main/config/emacs>
- <https://github.com/link0ff/emacs-init>
- <https://www.lucacambiaghi.com/vanilla-emacs/readme.html>
- <https://emacs.stackexchange.com/a/17281/16376>
- <https://github.com/Bassmann/emacs-config/>
- <https://github.com/KaratasFurkan/.emacs.d/>
- <https://stackoverflow.com/questions/23344540/emacs-update-git-gutter-annotations-when-staging-or-unstaging-changes-in-magit>
- <https://www.reddit.com/r/emacs/comments/ja97xs/weekly_tipstricketc_thread/g985tg9>
- <https://irreal.org/blog/?p=6297>
- <https://www.reddit.com/r/emacs/comments/6iqtze/org_mreturn_annoyance/>
- <https://github.com/kaushalmodi/.emacs.d>
- <https://www.reddit.com/r/emacs/comments/74ruu0/how_to_copy_the_contents_of_a_single_cell_in_org/do0utjf/?context=3>
- <https://gitlab.com/gSwag/emacs-configuration>
- <https://ianyepan.github.io/posts/emacs-git-gutter/>
- <https://github.com/diamondBond/emacs>
- <https://github.com/aadi58002/stow-dotfiles/blob/main/.config/emacs/init.org>
- <https://github.com/thornjad/aero>
- <https://qua.name/mrb/an-org-babel-based-emacs-configuration>

- <https://github.com/michelegera/dotfiles>
- <https://pawelgrzybek.com/change-macos-user-preferences-via-command-line/>
- <https://github.com/mathiasbynens/dotfiles>
- <https://github.com/ryuta69/dotfiles/>
- <https://twitter.com/nibroc/status/963088893758259200>
- <https://github.com/geerlingguy/dotfiles>
- <https://github.com/caarlos0/dotfiles>
- <https://gitlab.com/bkhl/dotfiles>
- <https://macowners.club/>

- <https://metaredux.com/posts/2020/07/07/supercharge-your-bash-history.html>
- <https://www.topbug.net/blog/2017/07/31/inputrc-for-humans/>

- <https://twitter.com/trav_downs/status/1280004737455271936>
