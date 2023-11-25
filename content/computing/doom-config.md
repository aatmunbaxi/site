+++
title = "My DOOM Emacs Configuration"
author = ["Aatmun Baxi"]
tags = ["emacs", "org", "hobby"]
draft = false
weight = 2003
type = "post"
+++

<details class="toc-class">
<summary><b>Table of Contents</b></summary>
<div class="details">

<div class="ox-hugo-toc toc local">

- [Intro](#intro)
- [Housekeeping](#housekeeping)
- [Font](#font)
- [Theme, Appearance, General Behavior](#theme-appearance-general-behavior)
    - [Doom Dashboard](#doom-dashboard)
    - [Modeline](#modeline)
    - [Global Appearance](#global-appearance)
    - [Custom Faces](#custom-faces)
    - [Custom Functionality](#custom-functionality)
- [Global keybindings](#global-keybindings)
- [Major Modes and Languages](#major-modes-and-languages)
    - [`org-mode`](#org-mode)
    - [`haskell-mode`](#haskell-mode)
    - [`python-mode`](#python-mode)
    - [`plantuml`](#plantuml)
    - [`pdf-view-mode`](#pdf-view-mode)
    - [`evil`](#evil)
    - [`lsp`](#lsp)
- [Packages](#packages)
    - [`bibtex`](#bibtex)
    - [`embark`](#embark)
- [`org` packages](#org-packages)
    - [`org-ref`](#org-ref)
    - [`org-roam`](#org-roam)
    - [`org-noter`](#org-noter)
    - [`org-super-agenda`](#org-super-agenda)
    - [CV](#cv)
- [Misc Packages and Behavior](#misc-packages-and-behavior)
    - [`helm-bibtex`](#helm-bibtex)
    - [`xenops-mode`](#xenops-mode)
    - [`yasnippets`](#yasnippets)
    - [`ink`](#ink)
    - [quiver](#quiver)
    - [`company`](#company)

</div>
<!--endtoc-->
</div>
</details>

{{< figure src="/ox-hugo/doom-emacs-filled.png" >}}


## Intro {#intro}

This config file is organized by mode and/or package.
Each mode has their own subsection for common configuration patterns such as keybindings, hooks, setting variables, etc.


## Housekeeping {#housekeeping}

```emacs-lisp
(setq user-full-name "Aatmun Baxi")
```


## Font {#font}

The font distribution on arch linux (my laptop) is named differently from the one on the Ubuntu and distributions of Roboto Mono on the internet, so we conditionally set a `font-spec` accordingly.

```emacs-lisp
(setq roboto-mono-thin (if (equal (system-name) "pop-os")
                           (font-spec :family "Roboto Mono Light" :size 20)
                         (font-spec :family "Roboto Mono Nerd Font Mono Lt" :size 20)))
(setq roboto-mono-big (if (equal (system-name) "pop-os")
                          (font-spec :family "Roboto Mono" :style "Regular" :size 24)
                        (font-spec :family "Roboto Mono Nerd Font Mono"  :size 24)))

(setq doom-font roboto-mono-thin
      doom-variable-pitch-font roboto-mono-thin
      doom-serif-font (font-spec :family "IBM Plex Serif" :size 19)
      doom-big-font roboto-mono-big)
```


## Theme, Appearance, General Behavior {#theme-appearance-general-behavior}

```emacs-lisp
(setq catppuccin-flavor 'mocha)
(setq doom-theme 'doom-homage-white)
;; (after! doom-themes
;;   (load-theme 'doom-nano-light t))
```


### Doom Dashboard {#doom-dashboard}

```emacs-lisp
(setq fancy-splash-image (expand-file-name "splash/doom-emacs-filled.png" doom-user-dir))
```


### Modeline {#modeline}

```emacs-lisp
;; (use-package! doom-modeline
;;   :hook (after-init . doom-modeline-mode)
;;   :custom
;;   (doom-modeline-height 40)
;;   (doom-modeline-bar-width 10)
;;   (doom-modeline-icon t)
;;   (doom-modeline-major-mode-icon t)
;;   (doom-modeline-major-mode-color-icon t)
;;   (doom-modeline-buffer-file-name-style 'auto)
;;   (doom-modeline-buffer-state-icon nil)
;;   (doom-modeline-buffer-modification-icon nil)
;;   (doom-modeline-minor-modes nil)
;;   (doom-modeline-enable-word-count nil)
;;   (doom-modeline-buffer-encoding nil)
;;   (doom-modeline-indent-info nil)
;;   (doom-modeline-checker-simple-format nil)
;;   (doom-modeline-vcs-max-length 12)
;;   (doom-modeline-env-version nil)
;;   (doom-modeline-github-timer nil)
;;   (doom-modeline-hud nil)
;;   (doom-modeline-modal nil)
;;   (doom-modeline-time t)
;;   (doom-modeline-time-live-icon t)
;;   (doom-modeline-buffer-name t)
;;   (doom-modeline-battery t)
;;   (doom-themes-padded-modeline 5)
;;   (doom-homage-white-padded-modeline t))
;;   ;; (doom-modeline-always-visible-segments '(mu4e))

;; (add-hook! 'doom-modeline #'display-battery-mode)
;; (add-hook! 'doom-modeline #'display-time-mode)
(add-hook! 'modeline #'display-battery-mode)
(add-hook! 'modeline #'display-time-mode)
```


### Global Appearance {#global-appearance}

Margins are nice to look at, even if they are impractical ;)

```emacs-lisp
(setq visual-fill-column-width 100
      visual-fill-column-center-text t)
(setq spacious-padding-widths '(:internal-border-width 25 :right-divider-width 10 :scroll-bar-width 5))
(spacious-padding-mode)
```

Banish line numbers, because they aren&rsquo;t useful with all the utilities DOOM emacs provides for navigation.

```emacs-lisp
(setq display-line-numbers-type nil)
```


### Custom Faces {#custom-faces}

These custom faces will appear later on in the config.
We could define them immediately before they are used, but I ran into issues with them not being initialized properly and not being applied if they are defined too close to where they are used.
So they&rsquo;re define here so they get loaded in early.

This section appears very long, but none of it is very complicated.
The first block of `defface` statements creates new faces for my custom org TODO keywords.
The second block sets the faces for org agenda and how they&rsquo;ll appear in the agenda view.
Similarly, the third block sets the faces for the super-agenda.

```emacs-lisp
(defun set-my-faces ()
  (defface todo-event-face
    `((t  :inverse-video t :foreground ,(doom-color 'red)) ) "" :group 'org-modern-faces)
  (defface todo-research-face
    `((t :inverse-video t :foreground ,(doom-color 'cyan))) "" :group 'org-modern-faces)
  (defface todo-read-face
    `((t :inverse-video t :foreground ,(doom-color 'teal)) ) "" :group 'org-modern-faces)
  (defface todo-prog-face
    `((t :inverse-video t :foreground ,(doom-color 'green+3))) "" :group 'org-modern-faces)
  (defface todo-idea-face
    `((t :inverse-video t :foreground ,(doom-color 'violet))) "" :group 'org-modern-faces)
  (defface todo-wait-face
    `((t :inverse-video t :foreground ,(doom-color 'orange))) "" :group 'org-modern-faces)
  (defface todo-next-face
    `((t :inverse-video t :foreground ,(doom-color 'green '256))) "" :group 'org-modern-faces)

  (custom-set-faces!
    `(org-agenda-date
      :foreground ,(doom-color 'violet) :height 1.3 :weight bold :slant italic)

    `(org-agenda-date-weekend
      :foreground ,(doom-color 'violet) :height 1.3 :weight light :slant italic)

    `(org-agenda-date-today
      :foreground ,(doom-color 'violet) :height 1.3 :weight bold :slant italic)

    `(org-modern-tag
      :background ,(doom-color 'base1) :foreground ,(doom-color 'fw-base1)))

  (defface super-agenda-due-today-face
    `((t :background ,(doom-color 'red) :underline t :inverse-video t)) "" :group 'org-super-agenda)

  (defface super-agenda-today-face
    `((t :foreground ,(doom-color 'fg) :underline t :slant italic)) "" :group 'org-super-agenda )

  (defface super-agenda-todo-face
    `((t :foreground ,(doom-color 'green+4)))  "" :group 'org-super-agenda )

  (defface super-agenda-date-face
    `((t :foreground ,(doom-color 'fg-1)) :slant italic )  "" :group 'org-super-agenda )

  (defface super-agenda-wait-face
    `((t :foreground ,(doom-color 'orange)))  "" :group 'org-super-agenda )

  (defface super-agenda-active-task-face
    `((t :foreground ,(doom-color 'green '256)))  "" :group 'org-super-agenda )

  (defface super-agenda-prog-face
    `((t :background ,(doom-darken 'green+4 0.5)))  "" :group 'org-super-agenda )

  (defface super-agenda-next-face
    `((t :background ,(doom-darken (doom-color 'green '256) 0.3)))  "" :group 'org-super-agenda )

  (defface super-agenda-maybe-face
    `((t :foreground ,(doom-darken (doom-color 'grey '256) 0.3)))  "" :group 'org-super-agenda )
  )

(add-hook 'doom-load-theme-hook #'set-my-faces)
```


### Custom Functionality {#custom-functionality}

Here I write some custom functionality, like writing functions for convenience&rsquo;s sake.

```emacs-lisp
(defun find-book ()
  (interactive)
  (+vertico/find-file-in "~/Documents/books/"))
```

```emacs-lisp
(defun find-org-file ()
  "Interactively select and open a .org file from a directory."
  (interactive)
  (+vertico/find-file-in org-directory))
```

```emacs-lisp

(defun find-article ()
  "Interactively select and open an article in zotero pdfs directory."
  (interactive)
  (+vertico/find-file-in "~/Documents/bib/pdfs/"))
```

I open book fairly often, so I want to make this function accessible quickly.

| Key       | Function        | Desc.         |
|-----------|-----------------|---------------|
| `SPC f b` | `find-book`     | Find book     |
| `SPC f a` | `find-article`  | Find article  |
| `SPC f o` | `find-org-file` | Find org file |

```emacs-lisp
(map!
 :desc "Find book"
 :leader
 :nv "f b" #'find-book

 :desc "Find article"
 :leader
 :nv "f a" #'find-article

 :desc "Find org file"
 :leader
 :nv "f o" #'find-org-file)
```


## Global keybindings {#global-keybindings}

Global keybindings are defined here.
These are keybinds that should work in all modes.

```emacs-lisp
(evilem-default-keybindings "SPC")
```

```emacs-lisp
(setq default-line-spacing 0.075)
(setq line-spacing default-line-spacing)
```

```emacs-lisp
(defun my/toggle-line-spacing ()
  (interactive)
  (if (eq default-line-spacing line-spacing)
      (setq line-spacing 0.5) ; add 0.5 height between lines
    (setq line-spacing default-line-spacing)   ; no extra heigh between lines
    )
  (redraw-frame (selected-frame)))
```

| Key       | Function                 | Desc.                          |
|-----------|--------------------------|--------------------------------|
| `SPC t m` | `my/toggle-line-spacing` | Toggle increased line spacing  |
| `C-c a`   | `spell-fu-word-add`      | Add current word to dictionary |

```emacs-lisp
(map! :map org-mode-map
      :n "SPC t m" #'my/toggle-line-spacing)
(map! :ni "C-c a" #' spell-fu-word-add)
(map! "M-C-l" #'drag-stuff-right)
```

```emacs-lisp
(map! "M-C-h" #'drag-stuff-left)
(map! "M-C-k" #'drag-stuff-up)
(map! "M-C-j" #'drag-stuff-down)
```


## Major Modes and Languages {#major-modes-and-languages}


### `org-mode` {#org-mode}

This section ropes in a number of settings for `org-mode`.
My primary use case for `org-mode` is to typset documents with mathematical notation in  \LaTeX, but other use cases exist such as literate programming a la this config file.


#### Some variables {#some-variables}

This variable sets default apps that org uses to open certain filetypes.

```emacs-lisp
(setq org-file-apps
      (quote
       ((auto-mode . emacs)
        ("\\.m\\'" . default)
        ("\\.?html?\\'" . /usr/bin/firefox)
        ("\\.pdf\\'" . emacs))))
```

Set our variable for `org-journal`.

```emacs-lisp
(setq org-journal-dir "~/Documents/org/journal/")
```

By default, the `org-preview-latex-default-process` doesn&rsquo;t play well with tikz pictures.
To fix this, we use `imagemagick` instead.

```emacs-lisp
(setq org-preview-latex-default-process 'dvisvgm)
```

```emacs-lisp
(after! org
  (setq org-directory "~/Documents/org/")
  (setq org-agenda-files '( "~/Documents/org/inbox.org"
                            "~/Documents/org/gtd.org"
                            "~/Documents/org/tickler.org"
                            "~/Documents/org/readinglist.org"))
  (setq org-default-notes-file "~/Documents/org/notes.org")
  (setq +org-capture-notes-file "inbox.org"))
```

TODO keywords will be used in `org-agenda` and stylized by `org-modern` later on.

```emacs-lisp
(after! org
  (setq org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
        '((sequence
           "TODO(t)"           ; A task that is ready to be tackled
           "NEXT(n)"           ; This task is actionable
           "PROG(g)"           ; A task in progress
           "PROJ(p)"           ; A project
           "RSCH(r)"           ; A task that neeeds to be researched
           "IDEA(i)"           ; An idea
           "EVENT(e)"           ; An event
           "READ(R)"           ; Thing to read
           "WAIT(w)"           ; This task is waiting on someone/thing
           "MAYBE(m)"           ; This task is a maybe

           "DRAFT(D)"           ; Draft tag for ox-hugo
           "|"                 ; The pipe necessary to separate "active" states and "inactive" states
           "DONE(d)"           ; Task has been completed
           "CANCELLED(c)" ))  ; Task has been cancelled
        )
  )
```

```emacs-lisp
(after! org
  (setq org-structure-template-alist
        '
        (("a" . "export ascii")
         ("c" . "center")
         ("C" . "comment")
         ("e" . "equation")
         ("E" . "export")
         ("h" . "export html")
         ("l" . "export latex")
         ("q" . "quote")
         ("s" . "src")
         ("v" . "verse"))
        ))
```

```emacs-lisp
(setq org-attach-id-dir "~/Documents/org/.attach/")
```

```emacs-lisp
(setq org-latex-pdf-process (list "latexmk -cd -shell-escape -bibtex -f -pdfxe  %f"))
(setq org-latex-listings 'minted)
```


#### Hooks {#hooks}

These will be activated when `org-mode` is opened.

```emacs-lisp
(defun my/org-hooks ()
  (mixed-pitch-mode)
  (org-cdlatex-mode)
  (evil-tex-mode)
  (org-appear-mode)
  (evil-owl-mode))

(add-hook! 'org-mode-hook  #'my/org-hooks)
```


#### Appearance {#appearance}

Specify sizes of org headlines by level.

```emacs-lisp
(custom-set-faces!
  '(org-level-1 :inherit outline-1 :height 2.0)
  '(org-level-2 :inherit outline-2 :height 1.8)
  '(org-level-3 :inherit outline-3 :height 1.5)
  '(org-level-4 :inherit outline-4 :height 1.2)
  '(org-level-5 :inherit outline-5 :height 1.0))
```

Startup with all trees folded and format some org features

```emacs-lisp
(after! org
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-startup-folded t            ;; Start org with all headings folded
        org-startup-indented nil
        org-startup-with-inline-images t ;; Start with inline images showing
        org-fontify-whole-heading-line t ;; Fontify various lines
        org-fontify-done-headline t ;; ..=..
        org-fontify-quote-and-verse-blocks nil
        org-ellipsis "  "                 ;; Change folded ellipsis character
        org-image-actual-width 400     ;; Size of inline images
        org-hide-emphasis-markers t  ;; Hide ==,//,** emphasis markers
        org-indent-mode nil
        )
  )
```


#### Keybinds {#keybinds}

| Key     | Function                       | Desc.                       |
|---------|--------------------------------|-----------------------------|
| `C-c ]` | `org-ref-insert-link`          | Inserts `org-ref` link      |
| `C-c [` | `org-ref-insert-ref-link`      | Inserts `org-ref` crosslink |
| `z o`   | `+org/open-all-folds`          | Open all folds              |
| `z M`   | `+org/close-all-folds`         | Close all folds             |
| `M-m`   | `org-next-visible-heading`     | Move to next heading        |
| `M-p`   | `org-previous-visible-heading` | Move to prev. heading       |

```emacs-lisp
(map! :map org-mode-map
      :i "C-c ]" #'org-ref-insert-link
      :i "C-c [" #'org-ref-insert-ref-link
      :n "z o" #'+org/open-all-folds
      :n "z M" #'+org/close-all-folds
      :nvi "M-m" #'org-next-visible-heading
      :nvi "M-p" #'org-previous-visible-heading)
```


#### Export Settings {#export-settings}

<!--list-separator-->

-  Disable export of TODO keywords

    ```emacs-lisp
    (setq org-export-with-todo-keywords nil)
    ```

<!--list-separator-->

-  `noex`  Drawers

    Sometimes I have text in an org document that I want to see while editing, but don&rsquo;t want it exported to the final document such as PDF.
    This setting excludes all drawers carrying the name `noex`  from being exported to the final output.
    Word by word, this variable says &ldquo;export all drawers that _don&rsquo;t_ have the name `noex`&rdquo;

    ```emacs-lisp
    (setq org-export-with-drawers '(not "noex"))
    ```

    This setting applies to all org files, but can be overwritten on a per-file basis in the `:options` header.


#### `org-capture` {#org-capture}

Used mostly in tight integration with the `agenda`, capture templates let me capture snippets of org text and quickly refile them to worry about later.
They can of course be used to capture more arbitrary snippets of text and refiled to non-agenda files, but my workflow is still in a young state.
It is inspired mostly by &ldquo;Getting Things Done&rdquo;.
Maybe I will find use for them later.

These templates set up the outline. Here is a table of what they do:

| Entry    | Text Content                 | File        | Top Headline |
|----------|------------------------------|-------------|--------------|
| Todo     | &ldquo;\* TODO&rdquo;        | inbox.org   | Tasks        |
| research | &ldquo;\* RSCH&rdquo;        | inbox.org   | Research     |
| idea     | &ldquo;\* IDEA&rdquo;        | inbox.org   | Ideas        |
| event    | &ldquo;\* EVENT&rdquo;       | tickler.org | N/A          |
| read     | &ldquo;\* READ&rdquo;&ldquo; | inbox.org   | N/A          |

```emacs-lisp
(after! (org-modern)
  (use-package! org-capture
    :defer t
    :config
    (setq org-refile-targets '(("~/Documents/org/gtd.org" :maxlevel . 1)
                               ("~/Documents/org/inbox.org" :maxlevel . 1)
                               ("~/Documents/org/tickler.org" :maxlevel . 1)
                               ("~/Documents/org/maybe.org" :maxlevel . 1)
                               ("~/Documents/org/notes.org" :maxlevel . 3)
                               ))
    (setq org-capture-templates
          '( ("t" "Todo" entry (file "~/Documents/org/inbox.org")
              "* TODO %?%i\n%a\n#+created: %t\n")
             ("r" "research" entry (file "~/Documents/org/inbox.org")
              "* RSCH %?\n%i\n%a\n#+created: %t\n")
             ("i" "idea" entry (file "~/Documents/org/readinglist.org")
              "* IDEA %?\n%i\n%a\n#+created: %t\n")
             ("e" "event" entry (file "~/Documents/org/tickler.org")
              "* EVENT %?%i\n#+created: %t\n")
             ("R" "read" entry (file "~/Documents/org/readinglist.org" )
              "* READ %?%i\n#+created: %t\n")
             ("m" "Email workflow")
             ("mf" "Follow Up" entry (file "~/Documents/org/inbox.org" )
              "* TODO Follow up with %:fromname on %a\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i" :immediate-finish t)
             ("mt" "Action Required" entry (file "~/Documents/org/inbox.org" )
              "* TODO %? \n:PROPERTIES:\n:REFERENCE: %a\n:END:\n%i")
             ("mr" "Read Later" entry (file"~/Documents/org/readinglist.org")
              "* READ %:subject\nSCHEDULED: %t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i" :immediate-finish t)))
    )
  )
```


#### `org-agenda` {#org-agenda}

```emacs-lisp
(after! org-modern
  (setq org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-compact-blocks nil
        org-agenda-start-day nil ;; i.e. today
        org-agenda-span 4)
  (custom-set-faces!
    `(org-super-agenda-header
      :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'red) :weight ultra-bold :slant italic :height 1.2)
    `(org-agenda-date
      :height 1.5  :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'fw-base2) :slant normal :weight ultra-light)
    `(org-agenda-date-today
      :height 1.5 :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'fw-base1) :slant italic :weight ultrabold)
    `(org-agenda-date-weekend
      :height 1.5 :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'fg) :slant normal :weight ultra-light))
  )
```


#### `org-modern` {#org-modern}

We will set custom faces for each of the TODO keywords in the above list.
Some modifications were made to the following [reference.](https://discourse.doomemacs.org/t/change-style-of-idea-in-org-mode/3041/4)

```emacs-lisp
(use-package! org-modern
  :demand t
  :after org
  :hook (org-mode . org-modern-mode)
  :init
  (setq org-modern-block-fringe 0)
  (setq org-modern-todo-faces
        '(
          ("PROG" . 'todo-prog-face)
          ("NEXT" .  'todo-next-face)
          ("WAIT" . 'todo-wait-face)
          ("RSCH" . 'todo-research-face)
          ("PROJ" . (:inherit org-modern-todo))
          ("READ" . 'todo-read-face)
          ("IDEA" . 'todo-idea-face)
          ("EVENT" .  'todo-event-face)
          ("MAYBE" .  (:inhert org-modern-todo))
          ))
  (with-eval-after-load 'org (global-org-modern-mode))
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  :config
  (global-org-modern-mode))
```

```emacs-lisp
(after! org
  (require 'org-indent)
  (org-indent-mode -1))
```

<!--list-separator-->

-  Agenda

    ```emacs-lisp
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-start-day "-1d")
    (setq org-agenda-todo-ignore-scheduled t)
    ```


#### `org-journal` {#org-journal}

```emacs-lisp
(setq org-journal-file-type 'monthly)
```


### `haskell-mode` {#haskell-mode}

```emacs-lisp
(setq haskell-compile-command "ghc -Wall -ferror-spans -fforce-recomp -dynamic -c %s")
```


#### Hooks {#hooks}

```emacs-lisp
(add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook #'lsp-ui-mode)
```


### `python-mode` {#python-mode}

```emacs-lisp
(add-hook 'python-mode 'elpy-enable)
```


### `plantuml` {#plantuml}

Sometimes I insert `plantuml` diagrams, and emacs needs some configuration to make it work.

```emacs-lisp
(after! org
  (setq org-plantuml-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  )
```


### `pdf-view-mode` {#pdf-view-mode}


#### Keybindings {#keybindings}

| Key   | Function                         | Desc.                        |
|-------|----------------------------------|------------------------------|
| `M-m` | `pdf-view-auto-slice-minor-mode` | Slice off whitespace of PDFs |
| `M-f` | `pdf-view-themed-minor-mode`     | Theme PDFs                   |

```emacs-lisp
(map! :map pdf-view-mode-map
      "M-m" #'pdf-view-auto-slice-minor-mode
      "M-f" #'pdf-view-themed-minor-mode)
```


#### Hooks {#hooks}

```emacs-lisp
(add-hook 'pdf-tools-enabled-hook 'pdf-view-themed-minor-mode)
(add-hook 'pdf-tools-enabled-hook 'pdf-view-auto-slice-minor-mode)
```


### `evil` {#evil}


#### `evil-owl` {#evil-owl}

`evil-owl` lets you view your marks in a posframe in the buffer before you commit to jumping to them.

I don&rsquo;t find myself using marks as often as I should be, but this package has made it easier to do so.

```emacs-lisp
(use-package! evil-owl
  :defer t
  :config
  (require 'posframe)
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args '(:width 50 :height 20)
        evil-owl-max-string-length 50)

  (setq evil-owl-register-groups
        `(("Named"     . ,(cl-loop for c from ?a to ?z collect c))
          ("Numbered"  . ,(cl-loop for c from ?0 to ?9 collect c))
          ("Special"   . (?\" ?* ?+ ?-))
          ("Read-only" . (?% ?# ?/ ?: ?.))))

  (setq evil-owl-mark-groups
        `(("Named Local"  . ,(cl-loop for c from ?a to ?z collect c))
          ("Named Global" . ,(cl-loop for c from ?A to ?Z collect c))
          ("Numbered"     . ,(cl-loop for c from ?0 to ?9 collect c))
          ("Special"      . (?\[ ?\] ?< ?> ?^ ?\( ?\) ?{ ?}))))
  (setq evil-owl-local-mark-format " %m: [l: %-5l, c: %-5c]\n    %s")
  (setq evil-owl-global-mark-format " %m: [l: %-5l, c: %-5c] %b\n    %s")

  )
```


### `lsp` {#lsp}

```emacs-lisp
(require 'eldoc-box)
(add-hook! eglot-managed-mode #'eldoc-box-hover-mode t)
```


## Packages {#packages}


### `bibtex` {#bibtex}

Bibtex is a bibliography package used for \LaTeX.

```emacs-lisp
(setq LaTeX-always-use-Biber t)
(setq bibtex-dialect 'biblatex)
```


### `embark` {#embark}

```emacs-lisp
(defun my/vsplit-file-open (f)
  "Opens file in vertically split window"
  (let ((evil-vsplit-window-right t))
    (+evil/window-vsplit-and-follow)
    (find-file f)))

(defun my/hsplit-file-open (f)
  "Opens file in horizontally split window"
  (let ((evil-split-window-below t))
    (+evil/window-split-and-follow)
    (find-file f)))
```

Embark lets us act on the contents of minibuffers.

```emacs-lisp
(use-package! embark
  :defer t
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)))

(map! :after embark

      :map embark-file-map
      :desc "Open in vertical split"
      "v" #'my/vsplit-file-open

      :map embark-file-map
      :desc "Open in horizontal split"
      "s" #'my/hsplit-file-open
      )
```


## `org` packages {#org-packages}


### `org-ref` {#org-ref}

`org-ref` is a useful package to simplify and unify the addition of citations to documents.

```emacs-lisp
(use-package! org-ref
  :ensure t
  :after org
  ;; :defer t
  :init
  (require 'bibtex)
  (require 'org-ref-helm)
  ;; (require 'org-ref-ivy)

  (with-eval-after-load 'ox
    (defun my/org-ref-process-buffer--html (backend)
      "Preprocess `org-ref' citations to HTML format. Do this only if the export backend is `html' or a derivative of that."
      ;; `ox-hugo' is derived indirectly from `ox-html'.
      ;; ox-hugo <- ox-blackfriday <- ox-md <- ox-html
      (when (org-export-derived-backend-p backend 'html)
        (org-ref-process-buffer 'html))))

    (add-to-list 'org-export-before-parsing-hook #'my/org-ref-process-buffer--html)

  :config
  (setq   org-ref-default-bibliography "~/Documents/bib/zotero_refs.bib"
          org-ref-pdf-directory '("~/Documents/books" "~/Documents/bib/pdfs")
          org-ref-insert-link-function 'org-ref-insert-link-hydra/body
          org-ref-insert-cite-function 'org-ref-cite-insert-helm
          org-ref-insert-ref-function 'org-ref-insert-ref-link

          bibtex-autokey-year-length 4
          bibtex-autokey-name-year-separator "-"
          bibtex-autokey-year-title-separator "-"
          bibtex-autokey-titleword-separator "-"
          bibtex-autokey-titlewords 2
          bibtex-autokey-titlewords-stretch 1
          bibtex-autokey-titleword-length 5)
  )
```


### `org-roam` {#org-roam}


#### `org-roam` {#org-roam}

`org-roam` is a package to keep a digital zettelkasten, and is a method to take atomic notes and build a &rsquo;second brain&rsquo;.

```emacs-lisp
(use-package! org-roam
  ;; :defer  t
  :custom
  (org-roam-directory "~/Documents/org/roam")

  :config
  (defun my/org-roam-hooks ()
    (xenops-mode)
    (org-cdlatex-mode)
    (xenops-dwim)
    (org-roam-bibtex-mode))

  (add-hook! 'org-roam-capture-new-node-hook #'my/org-roam-hooks)
  (add-hook! 'org-roam-find-file-hook #'my/org-roam-hooks)
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:40}" 'face 'org-modern-tag)))

  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (require 'org-roam-protocol)


  (setq org-roam-capture-templates '(
                                     ("h" "default" plain
                                      "%?"
                                      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                         "#+title: ${title}\n#+created: %U\n#+setupfile: ~/Documents/org/latex_template.org\n\n")
                                      :unnarrowed t)
                                     ("d" "definition" plain
                                      "%?"
                                      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                         "#+title: ${title}\n#+filetags: :definition:\n#+created: %U\n#+setupfile: ~/Documents/org/latex_template.org\n\n")
                                      :unnarrowed t)

                                     ("f" "fleeting" plain
                                      "%?"
                                      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                         "#+title: ${title}\n#+filetags: :fleeting:\n#+created: %U\n#+setupfile: ~/Documents/org/latex_template.org\n\n")
                                      :unnarrowed t)
                                     ("r" "reference" plain
                                      "%?"
                                      :if-new (file+head "${citekey}.org"
                                                         "#+title: ${author}: ${title}\n#+filetags: :reference:\n#+created: %U\n#+setupfile: ~/Documents/org/latex_template.org\n\n")
                                      :unnarrowed t)
                                     ("n" "reference notes" plain
                                      "%?"
                                      :if-new (file+head "${citekey}.org" "#+title: ${author}: ${title}\n#+filetags: :reference:\n#+created: %U\n#+setupfile: ~/Documents/org/latex_template.org\n\n* Notes\n:PROPERTIES:\n:NOTER_DOCUMENT: ${file}\n:END:\n") :unnarrowed t)
                                     )

        )
  )
(use-package! websocket
  :defer t
  :after org-roam)

(use-package! org-roam-ui
  :defer t
  :after org  ;; or :after org
  ;; normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;; a hookable mode anymore, you're advised to pick something yourself
  ;; if you don't care about startup time, use
  ;; :hook (      after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
```

By default DOOM offers only keybindings for these functions that work in normal mode. These will work in insert mode as well.

<a id="table--roam-kt"></a>

| Key         | Function               |
|-------------|------------------------|
| `C-c n r i` | `org-roam-node-insert` |
| `C-c n r f` | `org-roam-node-find`   |
| `SPC n r t` | `org-roam-tag-add`     |
| `SPC n r u` | `org-roam-tag-remove`  |

```emacs-lisp
(map! :map org-mode-map
      :prefix "C-c"
      :desc "Insert node"
      :i "n r i" #'org-roam-node-insert
      :desc "Find node"
      :i "n r f" #'org-roam-node-find

      :map org-roam-mode-map
      :leader
      :desc "Add tag"
      :nv "n r t" #'org-roam-tag-add
      :desc "Remove tag"
      :nv "n r u" #'org-roam-tag-remove)
```


#### `org-roam-bibtex` {#org-roam-bibtex}

```emacs-lisp
(use-package! org-roam-bibtex
  :after org-roam
  :config
  (setq orb-file-field-extensions '("pdf" "epub"))
  (setq orb-preformat-keywords '("citekey" "file" "author" "date"))
  )
```


### `org-noter` {#org-noter}

```emacs-lisp
(use-package! org-noter
  :defer t
  :config
  (setq org-noter-always-create-frame nil))
```


### `org-super-agenda` {#org-super-agenda}

```emacs-lisp
(setq org-agenda-format-date (lambda (date) (concat "\n"
                                                    (make-string (window-width) 9472)
                                                    "\n"
                                                    (org-agenda-format-date-aligned date))))
```

```emacs-lisp
(use-package! org-super-agenda
  :config
  (setq org-agenda-custom-commands
        '(("s" "Super view"
           ((agenda "" ((org-super-agenda-groups
                         '((:name "Due Today❗"
                            :deadline today
                            :order 0)
                           (:name "Todo ✍"
                            :todo ("PROG" "WAIT" "NEXT")
                            :and (:todo "TODO" :scheduled today)
                            :and (:todo "TODO" :deadline today)
                            :habit t
                            :order )))))
            (alltodo ""
                     ((org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '(
                         (:name "Maybe / To Read 🤔"
                          :todo ("IDEA" "READ" "MAYBE")
                          :order 10)

                         (:name "Unscheduled"
                          :children ("TODO" "DONE")
                          :and (:scheduled nil :deadline nil)
                          :order 6)

                         (:discard (:children nil)))))))))))
(after! org-super-agenda
  (org-super-agenda-mode))
```


### CV {#cv}

```emacs-lisp
(use-package! ox-modern
  :load-path "~/working/org-cv/"
  :init (require 'ox-moderncv)
  :defer t)
```


## Misc Packages and Behavior {#misc-packages-and-behavior}


### `helm-bibtex` {#helm-bibtex}

`helm-bibtex` is a package that provides tools for bibliography management with the `helm` completion framework. `helm-bibtex` does not have this issue, hence I use it.

```emacs-lisp
(use-package! helm-bibtex
  :config
  (setq bibtex-completion-bibliography '("~/Documents/bib/zotero_refs.bib")
        bibtex-completion-library-path '("~/Documents/books/"  "~/Documents/bib/articles/")
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-field "file"
        bibtex-completion-display-formats '((article . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
                                            (inbook . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
                                            (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
                                            (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}") (t . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))))
```


### `xenops-mode` {#xenops-mode}

NOTE: This package will be unnecessary soon, as a new implementation of `org-latex-preview` will be available in vanilla Org (most likely 9.7).

`xenops-mode` is a \LaTeX editing environment that automatically and asynchronously compiles LaTeX dvi files.
It is a more feature-rich replacement for `org-latex-preview`.

<a id="table--xenops-kt"></a>

| Key       | Function      | Desc.                |
|-----------|---------------|----------------------|
| `SPC m z` | `xenops-mode` | Starts `xenops-mode` |

```emacs-lisp
(map! :map org-mode-map
      :n "SPC m z" #'xenops-mode
      :map LaTeX-mode-map
      :n "SPC m z" #'xenops-mode)
```

Increase the preview image sizes so that they&rsquo;re readable.

```emacs-lisp
(setq xenops-math-image-scale-factor 1.4
      xenops-reveal-on-entry nil)
```


### `yasnippets` {#yasnippets}

This section is taken from the ideas and code in this article: [LaTeX Input for the Impatient Scholar](https://karthinks.com/software/latex-input-for-impatient-scholars/).

This function allows for auto expanding of snippets.

```emacs-lisp
;; (require 'yasnippet)
(defun my/yas-try-expanding-auto-snippets ()
  (when (bound-and-true-p yas-minor-mode)
    (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
      (yas-expand))))
```

Then actually implement the auto expansion.

```emacs-lisp
(add-hook 'post-self-insert-hook #'my/yas-try-expanding-auto-snippets)
```

Suppress the complaining `yasnippet` tends to do when it directly edits the buffer.

```emacs-lisp
(with-eval-after-load 'warnings
  (cl-pushnew '(yasnippet backquote-change) warning-suppress-types
              :test 'equal))
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))
```


### `ink` {#ink}

[ink](https://github.com/foxfriday/ink) is a package that allows for fast insertion of inkspace figures into `latex-mode` and `org-mode` buffers.

By default, `ink` embeds `.png` files into `org-mode` files.
I wanted to emulate Castel&rsquo;s [inkscape workflow](https://castel.dev/post/lecture-notes-2/) for inserting Inkscape figures into LaTeX files quickly, and `ink` gets us close, but not quite there.
This configuration lets me get what I want, by allowing insertion of a `.pdf_tex` file into documents, which will allow for adjustment of fonts on the fly as we export to PDF.
The behavior should insert a LaTeX snippet with a new figure containing a `.pdf_tex` file, while also providing an `org-mode` link to a png to be viewed inline in the org buffer.

The `ink-insert-org-combined` function inserts a LaTeX figure with the `pdf_tex` exported version of the `svg` file so that it is sensitive to LaTeX formatting changes.
It also puts a second `png` version in a `:noex:` drawer.
This `:noex:` drawer is a general notation that I use to withhold some portions of an org document from being exported.
In this case, we use this drawer because I&rsquo;d still like to see the image of the figure inline, but don&rsquo;t want it exported twice.

```emacs-lisp
(use-package! ink
  :defer t
  :config
  ;; These flags export all the necessary file formats
  (setq ink-flags-custom (list "--export-area-drawing"
                               "--export-dpi 100"
                               "--export-type=png,pdf"
                               "--export-latex"
                               "--export-overwrite"))
  (setq ink-flags-options
        (list (cons 'latex-mode ink-flags-latex)
              (cons 'org-mode ink-flags-custom)
              (cons 'markdown-mode ink-flags-png)))
  ;; This variable inserts both the LaTeX fragment to insert
  ;; the .pdf_tex file and commented org-mode link to let us view
  ;; the image inline in the org buffer
  (setq ink-insert-org-combined "\n\\begin{figure}[H]
    \\centering
    \\def\\svgscale{0.5}
    \\subimport{%s}{%s.pdf_tex}
    \\caption{}
    \\label{fig:%s}\n\\end{figure}\n:noex:\n[[file:%1$s/%2$s.png]]\n:END:\n")
  (setq ink-insert-options
        (list (cons 'latex-mode ink-insert-latex)
              (cons 'org-mode ink-insert-org-combined)
              (cons 'markdown-mode ink-insert-md))))
```

| Key       | Function          | Desc.                |
|-----------|-------------------|----------------------|
| `SPC i i` | `ink-make-figure` | Make inkscape figure |

Set keybindings to insert inkscape figures

```emacs-lisp
(map! :map org-mode-map
      :n "SPC i i"
      #'ink-make-figure)
```


### quiver {#quiver}

Related to `ink` in its use case, but not exactly the same is [quiver](https://github.com/varkor/quiver) [Dependency].
This is a program to easily and interactively create commutative diagrams using the `tikz-cd` LaTeX package.
All we need is a function to interactively insert open the program.

```emacs-lisp
(defun open-quiver-local ()
  "Open quiver program locally"
  (interactive)
  (start-process "open-quiver" nil "firefox" "--new-window" "/home/aatmun/working/quiver/src/index.html"))

(defun open-quiver-web ()
  "Open quiver program on the web"
  (interactive)
  (start-process "open-quiver" nil "firefox" "--new-window" "https://q.uiver.app"))
```

| Key         | Function            | Desc.                           |
|-------------|---------------------|---------------------------------|
| `SPC i c l` | `open-quiver-local` | Open quiver instance locally    |
| `SPC i c w` | `open-quiver-web`   | Open quiver instance in browser |

```emacs-lisp
(map! :map org-mode-map
      :prefix "SPC i c"
      :nv "l"
      #'open-quiver-local
      :nv "w"
      #'open-quiver-web)
```


### `company` {#company}

Disable company in `org-mode`.

```emacs-lisp
(setq company-global-modes '(not org-mode))
```
