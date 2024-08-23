# My MySQL Development Tools

## Tool configuration

- [clangd](https://github.com/laurynas-biveinis/dotfiles/blob/master/mysql-work/vilniusdb/.clangd):
  I am so glad C++ finally got universal IDE tooling and
  [clangd](https://clangd.llvm.org) is very useful for MySQL development.
- [Emacs](https://github.com/laurynas-biveinis/dotfiles/blob/master/mysql-work/vilniusdb/.dir-locals.el):
  my primary Emacs config is
  [elsewhere](https://github.com/laurynas-biveinis/dotfiles/tree/master/emacs/.emacs.d),
  while this is a directory-local config that takes effect in the source trees.
  It prevents creating spurious `.ispell.dict` files, sets the C++ style to
  Google, and makes [Magit](https://magit.vc)
  ([magit-todos](https://github.com/alphapapa/magit-todos) actually) exclude 3rd
  party TODO comments.
- [cpplint](https://github.com/laurynas-biveinis/dotfiles/blob/master/mysql-work/vilniusdb/CPPLINT.cfg):
  [cpplint](https://github.com/cpplint/cpplint) is still marginally useful with
  MySQL sources.

I symlink these into the parent directory of all the source trees.

## Could be useful to you

- [patch2testlist](https://github.com/laurynas-biveinis/dotfiles/blob/master/mysql-work/usr/bin/patch2testlist):
  for a given patch, print a list of MTR tests to run to cover the test changes
  in the patch. Introduced in its [own blog
  post](https://of-code.blogspot.com/2024/01/introducing-patch2testlist-for-mysql.html).
  Unlike the scripts below, `patch2testlist` is standalone and works without the
  zsh utility functions.

## Useful to me

- [mysql-work.sh](https://github.com/laurynas-biveinis/dotfiles/blob/master/mysql-work/.zsh.d/rc/mysql-work.sh):
  a shell initialization script that sets up environment variables and helper
  functions to configure, build, & test various MySQL versions on macOS and
  Linux. For example, `$MY8037D` will expand to CMake options for a debug build
  of MySQL 8.0.37. As for the functions, among others, `mysql_build` will
  configure and build a given tree, and `rmtr` will incrementally build and run
  MTR tests with libeatmydata. This script adds various warning disable flags so
  that older unpatched trees can be built with newer compilers within reason. It
  also absorbs bug workarounds and complications related to build environments,
  for example, different MySQL versions need both OpenSSL 1.1 and 3 installed,
  which then in turn results in build issues if both versions are present
  simultaneously. This script is not usable without its [zsh utility
  functions](https://github.com/laurynas-biveinis/dotfiles/tree/master/mysql-work/.zsh.d/functions).
- [fetchworksrc](https://github.com/laurynas-biveinis/dotfiles/blob/master/mysql-work/usr/bin/fetchworksrc):
  fetch all remotes for Git repos in `WORK_REPOS_TO_PULL` environment variable;
  pull all the worktrees in `WORK_TREES_TO_UPDATE` one and do incremental
  builds. This script runs at midnight on my work machine. There are some
  quality of life embellishments such as skipping the the build if the machine
  is a laptop on battery and managing Homebrew OpenSSL version incompatibilities
  in builds. Again, this script is not usable without its [zsh utility
  functions](https://github.com/laurynas-biveinis/dotfiles/tree/master/mysql-work/.zsh.d/functions).

## zsh function library

[This
directory](https://github.com/laurynas-biveinis/dotfiles/tree/master/mysql-work/.zsh.d/functions).
One file per function as zsh requires. Currently they fall into two sets of
responsibilities: querying the source tree for version information and managing
[OpenSSL 3 build workarounds](https://github.com/laurynas-biveinis/dotfiles/blob/master/mysql-work/.zsh.d/functions/mysql_need_openssl3_workaround).

## No longer maintained (patches welcome)

- [gca](https://github.com/laurynas-biveinis/dotfiles/blob/master/mysql-work/usr/bin/gca),
  the workhorse script from my time at Percona. Used when the same patch has to
  be developed for more than one major MySQL series (e.g. the patch is needed
  for 8.0, 8.4, & 9.0, using an example that is current at the time of the
  writing). There is a constraint that the lower series version of the patch has
  to be `git merge`d to the next higher series, but the latest commits of these
  series are not fully merged to each other due to how Oracle pushes their
  source trees. So, a lower series feature branch has to be created not from the
  tip but from some older commit which had been fully merged. This script
  manages finding such commits and creating the worktrees. Luckily I only deal
  with single series these days.
- [reupmerge](https://github.com/laurynas-biveinis/dotfiles/blob/master/mysql-work/usr/bin/reupmerge),
  used in that same multiple-series workflow. It handles the scenario where,
  i.e. a code review change needs to be applied to a lower branch commit in a
  feature branch. Changing that commit requires re-merging it to the higher
  series. For the case where the higher series commit needs not to change, this
  script automates saving its diff, and re-merging the lower series with that
  exact saved diff.
- [mysql-work.el](https://github.com/laurynas-biveinis/dotfiles/blob/master/mysql-work/.emacs.d/dotfiles/mysql-work.el):
  mostly outdated Emacs configuration bits, mostly copied from colleagues at
  Percona. They set up MySQL (and InnoDB) indentation styles that were used up
  to 5.7, and provide a bit of syntax support for MTR `.test` files.
