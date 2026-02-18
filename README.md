<div align="center">

# Doom Emacs (Guix Fork)

A fork of [Doom Emacs](https://github.com/doomemacs/doomemacs) that replaces
[straight.el](https://github.com/radian-software/straight.el) with
[GNU Guix](https://guix.gnu.org) for package management.

![Supports Emacs 27.1–30.2](https://img.shields.io/badge/Supports-Emacs_27.1–30.2-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)

</div>

---

### Table of Contents
- [What's different](#whats-different)
- [Prerequisites](#prerequisites)
- [Installing Guix](#installing-guix)
- [Install](#install)
- [Package Management](#package-management)
- [Testing](#testing)


# What's different

This fork replaces Doom's package manager backend (straight.el) with
[GNU Guix](https://guix.gnu.org). Everything else — modules, keybindings, UI,
the `doom` CLI — works the same as upstream Doom Emacs.

Guix gives you:
- **Reproducible builds** — packages are built from source with pinned inputs.
- **Atomic upgrades and rollbacks** — every `doom sync` creates a new Guix
  profile generation you can roll back to.
- **Per-profile isolation** — Doom's packages live in their own Guix profile,
  separate from your system Emacs packages.
- **No Elisp package manager at runtime** — straight.el is gone. Guix handles
  fetching, building, and dependency resolution externally.

This fork also adds a working `doom test` command using ERT (Emacs' built-in
test framework), with a test suite for the Guix integration.

For upstream Doom Emacs documentation (modules, configuration, keybindings),
see the [upstream README](https://github.com/doomemacs/doomemacs).


# Prerequisites

- GNU Emacs 27.1–30.2 (30.2 recommended)
  - Doom's core requires 27.1+, modules require 28.1+, tree-sitter requires 29.1+
- [GNU Guix](https://guix.gnu.org) (package manager)
- Git >= 2.23
- [ripgrep](https://github.com/BurntSushi/ripgrep) >= 11.0
- **Optional:** [fd](https://github.com/sharkdp/fd) 7.3.0+ (faster file indexing)


# Installing Guix

### Guix System (GuixSD)

Guix is already available. No extra steps needed.

### Debian / Ubuntu

```sh
sudo apt install guix
```

If the version in your distro's repos is too old, use the official installer
(see below).

### Arch Linux

```sh
pacman -S guix
```

Or from the AUR: `yay -S guix`

### Fedora / openSUSE / macOS / other

Use the official Guix installer script:

```sh
cd /tmp
wget https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh
chmod +x guix-install.sh
sudo ./guix-install.sh
```

> [!NOTE]
> On macOS, use `curl -O` instead of `wget`, and you may need to set up the
> Guix daemon and build users manually. See the
> [Guix installation manual](https://guix.gnu.org/manual/en/html_node/Installation.html)
> for full details.

### Post-install (all platforms)

```sh
# Start the daemon (if not started by your init system)
sudo systemctl enable --now guix-daemon  # systemd
# or: sudo guix-daemon --build-users-group=guixbuild &

# Source the profile in your shell
GUIX_PROFILE="$HOME/.guix-profile"
. "$GUIX_PROFILE/etc/profile"

# Verify
guix --version
```


# Install

```sh
git clone --depth 1 https://github.com/kovan/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
```

Add `~/.config/emacs/bin` to your `PATH`, then:

```sh
doom sync      # Install packages, regenerate caches
doom doctor    # Diagnose common issues
doom upgrade   # Update Doom + all packages
doom env       # Snapshot shell environment for Emacs
doom test      # Run the test suite
```


# Package Management

Packages are declared in `packages.el` files using the `package!` macro and
managed through a dedicated Guix profile.

## Declaring packages

Add `package!` declarations to `~/.doom.d/packages.el` (your private config)
or to a module's `packages.el`:

```emacs-lisp
;; Install from the guix-emacs channel (mirrors MELPA)
(package! evil-surround)

;; Pin to a specific commit
(package! magit
  :pin "abc1234567890")

;; Install from a custom Git repository
(package! my-package
  :recipe (:host github :repo "user/my-package"))

;; GitLab, Codeberg, Sourcehut, or Bitbucket
(package! another-package
  :recipe (:host gitlab :repo "user/another-package"))

;; Direct Git URL
(package! some-package
  :recipe (:url "https://example.com/repo.git"))

;; Only include specific files
(package! big-package
  :recipe (:host github :repo "user/big-package"
           :files ("big-package.el" "extensions/*.el")))

;; Disable a package installed by a module
(package! package-i-dont-want :disable t)

;; Ignore (don't install, but don't disable use-package! blocks)
(package! optional-dep :ignore t)

;; Use the built-in version
(package! org :built-in t)

;; Prefer built-in if available, otherwise install
(package! jsonrpc :built-in 'prefer)

;; Override the Guix package name (when it doesn't follow emacs-<name>)
(package! vterm :guix-name "emacs-vterm-next")
```

## `package!` properties reference

| Property     | Description                                                  |
|:-------------|:-------------------------------------------------------------|
| `:pin`       | Pin to a commit hash string. `nil` to unpin.                 |
| `:recipe`    | Plist specifying where to fetch the package (see below).     |
| `:disable`   | If non-nil, don't install and disable `use-package!` blocks. |
| `:ignore`    | If non-nil, don't install (but don't disable config blocks). |
| `:built-in`  | `t` to use built-in version. `'prefer` to prefer built-in.  |
| `:type`      | `core`, `local`, `built-in`, or `virtual`.                   |
| `:env`       | Alist of env vars to set during build. Triggers rebuild on change. |
| `:guix-name` | Override the Guix package name (default: `emacs-<name>`).    |

### Recipe keys

| Key           | Description                                                |
|:--------------|:-----------------------------------------------------------|
| `:host`       | `github`, `gitlab`, `codeberg`, `sourcehut`, or `bitbucket` |
| `:repo`       | Repository path, e.g. `"user/repo"`                        |
| `:url`        | Direct Git URL (alternative to `:host`/`:repo`)             |
| `:branch`     | Git branch to track                                         |
| `:files`      | List of files/globs to include in the build                 |
| `:build`      | Build instructions (`t`, `nil`, or list of build steps)     |
| `:pre-build`  | Elisp form to run before building                           |
| `:local-repo` | Local directory name or path for the repo                   |

## How it works

1. **Declaration**: `package!` calls in `packages.el` files populate
   `doom-packages` with package metadata.
2. **Channel generation**: `doom sync` translates these declarations into Guix
   package definitions (Guile Scheme) and writes them to a local Guix channel
   at `~/.emacs.d/.local/guix-channel/`.
3. **Profile build**: Guix installs all packages into a dedicated profile at
   `~/.emacs.d/.local/guix-profile/`, resolving dependencies automatically.
4. **Load path**: At startup, Doom adds the profile's `site-lisp` directories
   to Emacs' `load-path`.

Pinned packages (`:pin`) get their own package definitions with the exact
commit hash baked in. Unpinned packages inherit from the
[guix-emacs](https://github.com/garrgravarr/guix-emacs) channel, which mirrors
MELPA.

## Rollback

Because Guix profiles are atomic, you can roll back to a previous state:

```sh
# List profile generations
guix package --profile=~/.emacs.d/.local/guix-profile --list-generations

# Roll back to the previous generation
guix package --profile=~/.emacs.d/.local/guix-profile --roll-back
```


# Testing

This fork includes a working `doom test` command using ERT (Emacs' built-in
test framework).

```sh
# Run all tests
doom test

# Run only tests matching a pattern
doom test guix

# Use Buttercup instead of ERT
doom test --buttercup
```

Test files live in `lisp/test/test-*.el` (core tests) and
`modules/**/test/test-*.el` (module tests).
