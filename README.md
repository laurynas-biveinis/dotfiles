<!--- -*- gfm -*- -->

# dotfiles

My dotfiles and scripts, and configuration. The shell part is specific for zsh,
using [zinit](https://github.com/zdharma/zinit) &
[powerlevel10k](https://github.com/romkatv/powerlevel10k).

## Modularity

The files are managed by [GNU Stow](https://www.gnu.org/software/stow/), divided
by tool or functional area: git, emacs, etc—see the top level-directories.
Different systems use different subsets of configuration, specified by some
files, as described in the [installation](#installation) section.

## Emacs

The biggest part of configuration is for Emacs, currently version 27.1. The
focus is on seamless integration between all the different packages, avoiding
surprises, and adding polish here and there. Of course, that is very much a work
in progress.

The bulk of configuration is in
[setup.el](https://github.com/laurynas-biveinis/dotfiles/blob/master/emacs/.emacs.d/setup.el),
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

[org](https://orgmode.org) [magit](https://magit.vc)
[lsp-mode](https://github.com/emacs-lsp/lsp-mode)
[flycheck](https://www.flycheck.org)
[company](https://github.com/company-mode/company-mode)
[helm](https://emacs-helm.github.io/helm/)
[projectile](https://github.com/bbatsov/helm-projectile)
[vterm](https://github.com/akermu/emacs-libvterm)
[deadgrep](https://github.com/Wilfred/deadgrep)
[undo-tree](http://www.dr-qubit.org/undo-tree.html)

#### Major

[GCMH](https://gitlab.com/koral/gcmh)
[neuron-mode](https://github.com/felko/neuron-mode)
[wgrep](https://github.com/mhayashi1120/Emacs-wgrep)
[google-c-style](https://github.com/google/styleguide/blob/gh-pages/google-c-style.el)

#### Nice-to-have, niche, & specific major modes

[rich-minority](https://github.com/Malabarba/rich-minority)
[dispwatch](https://github.com/mnp/dispwatch)
[lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs)
[calfw](https://github.com/kiwanami/emacs-calfw)
[helm-make](https://github.com/abo-abo/helm-make)
[which-key](https://github.com/justbur/emacs-which-key)
[keyfreq](https://github.com/dacap/keyfreq)
[helm-dash](https://github.com/dash-docs-el/helm-dash)
[helm-org](https://github.com/emacs-helm/helm-org)
[helm-lsp](https://github.com/emacs-lsp/helm-lsp)
[helm-projectile](https://github.com/bbatsov/helm-projectile)
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

#### Appearance

[solarized-theme](https://github.com/bbatsov/solarized-emacs)
[git-gutter-fringe](https://github.com/emacsorphanage/git-gutter-fringe)
[helm-icons](https://github.com/yyoncho/helm-icons)
[company-box](https://github.com/sebastiencs/company-box)
[modern-cpp-font-lock](https://github.com/ludwigpacifici/modern-cpp-font-lock) [highlight-indent-guides](https://github.com/DarthFennec/highlight-indent-guides)
[page-break-lines](https://github.com/purcell/page-break-lines)
[xterm-color](https://github.com/atomontage/xterm-color)
[all-the-icons-dired](https://github.com/jtbm37/all-the-icons-dired)
[cmake-font-lock](https://github.com/Lindydancer/cmake-font-lock)
[beacon](https://github.com/Malabarba/beacon)
[flycheck-color-mode-line](https://github.com/flycheck/flycheck-color-mode-line)
[stripe-buffer](https://github.com/sabof/stripe-buffer)
[color-identifiers-mode](https://github.com/ankurdave/color-identifiers-mode)
[info-colors](https://github.com/ubolonton/info-colors)

#### Packages bundled with Emacs

tramp erc cc-mode

### Improvements, fixed annoyances, and bug workarounds

* [Disable global-display-fill-column-indicator in read-only buffers](https://www.reddit.com/r/emacs/comments/ja97xs/weekly_tipstricketc_thread/g903xa3?utm_source=share&utm_medium=web2x&context=3)
* [Replace some cc-mode formatting commands with lsp-mode ones](https://www.reddit.com/r/emacs/comments/ikgfxd/weekly_tipstricketc_thread/g3z9rcb?utm_source=share&utm_medium=web2x&context=3)
* [macOS: add the missing man page paths for woman](https://www.reddit.com/r/emacs/comments/ig7zzo/weekly_tipstricketc_thread/g34s8dl?utm_source=share&utm_medium=web2x&context=3)
* [27.1 do GC if no frame has focus](https://www.reddit.com/r/emacs/comments/ibwzcu/weekly_tipstricketc_thread/g1zlh2t?utm_source=share&utm_medium=web2x&context=3)
* [Re-enable Shellcheck if using lsp-mode with bash-language-server](https://www.reddit.com/r/emacs/comments/hqxm5v/weekly_tipstricketc_thread/fy4pvr8?utm_source=share&utm_medium=web2x&context=3)
* [Clicking on URLs should open them instead of spellchecking them](https://www.reddit.com/r/emacs/comments/it4m2w/weekly_tipstricketc_thread/g5pff92?utm_source=share&utm_medium=web2x&context=3)
* Workaround [helm-buffers-list error if all-the-icons is used and a dired
  buffer is present](https://github.com/yyoncho/helm-icons/issues/16).
* Workaround the [emacs-wgrep
  issue](https://github.com/mhayashi1120/Emacs-wgrep/issues/75) of edited
  helm-grep buffers over TRAMP not applying their changes.
* Workaround the [projectile
  issue](https://github.com/bbatsov/projectile/issues/347) of remote projects
  not being added to Projectile project list.
* Workaround [projectile not being integrated with
  project.el](https://github.com/bbatsov/projectile/issues/1282) at least for
  xref.
* Add [Projectile reconfigure command for CMake
  presets](https://github.com/bbatsov/projectile/issues/1676).
* Workaround [a bug in projectile integration with CMake
  presets](https://github.com/bbatsov/projectile/issues/1673) until the next
  Projectile release in melpa-stable.
* Integrate deadgrep with Projectile
* [Automatically update git gutter on Magit
  actions](https://stackoverflow.com/questions/43236670/visual-studio-code-git-diff-over-git-gutter-indicator).
* [Disable color-identifiers-mode under LSP with semantic
  highlighting](https://github.com/laurynas-biveinis/dotfiles/blob/ce044dab576c525f418a5383180d06c888a33599/emacs/.emacs.d/setup.el#L1924)
* lsp-mode 7.0.1 headerline [fix
  1](https://github.com/laurynas-biveinis/dotfiles/blob/8aa6e94c5d23b43706740ce02982001759087743/emacs/.emacs.d/setup.el#L1482),
  [fix
  2](https://github.com/laurynas-biveinis/dotfiles/blob/1dece8ec3230144a6bb7ed96de2155e37c7bf047/emacs/.emacs.d/setup.el#L1528)

### Custom commands and functionality

* [Set frame geometry after docking/undocking laptop automatically](https://www.reddit.com/r/emacs/comments/ev2q9q/weekly_tipstricketc_thread/fftpfj0?utm_source=share&utm_medium=web2x&context=3)
* `my-recompile-packages`: force recompiling all the installed  packages, after
  a Emacs version upgrade or a borked package upgrade.

# Installation

See
[INSTALLATION.md](https://github.com/laurynas-biveinis/dotfiles/blob/master/INSTALLATION.md)

# New system setup

See
[setup-ubuntu.sh](https://github.com/laurynas-biveinis/dotfiles/blob/master/setup-ubuntu.sh),
[setup-centos6.sh](https://github.com/laurynas-biveinis/dotfiles/blob/master/setup-centos6.sh),
and
[setup-macos.sh](https://github.com/laurynas-biveinis/dotfiles/blob/master/setup-macos.sh).

# Dotfiles, Emacs distros, macOS defaults, etc. I have been stealing from

Incomplete, will add to it.

* [Doom Emacs](https://github.com/hlissner/doom-emacs)
* [EmacsWiki: Dot Emacs Challenge](https://www.emacswiki.org/emacs/DotEmacsChallenge)
* <https://emacs.nasy.moe/>
* <https://pawelgrzybek.com/change-macos-user-preferences-via-command-line/>
* <https://github.com/mathiasbynens/dotfiles/blob/master/.macos>
* <https://github.com/ryuta69/dotfiles/>
* <https://ebzzry.io/en/emacs-pairs/>
* <https://www.topbug.net/blog/2017/07/31/inputrc-for-humans/>
* <https://github.com/snackon/Witchmacs>
* <https://github.com/jhenahan/dots/tree/main/config/emacs>
* <https://github.com/link0ff/emacs-init>
* <https://www.lucacambiaghi.com/vanilla-emacs/readme.html>
