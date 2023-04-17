+++
title = "My DOOM Emacs Configuration"
author = ["Aatmun Baxi"]
tags = ["emacs", "org", "hobby"]
draft = false
weight = 2002
type = "post"
+++

{{< figure src="/ox-hugo/black-hole.png" >}}


## Intro {#intro}

This config file is organized by mode and/or package.
Each mode has their own subsection for common configuration patterns such as keybindings, hooks, setting variables, etc.


## Housekeeping {#housekeeping}

```emacs-lisp
(setq user-full-name "Aatmun Baxi"
      user-mail-address "baxiaatmun@gmail.com")
```


## Font {#font}

```emacs-lisp
(setq doom-font (font-spec :family "Iosevka Aile" :style "Light" :size 18)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :style "Light" :size 18)
      doom-serif-font (font-spec :family "Iosevka" :size 18)
      doom-big-font (font-spec :family "Iosevka Aile" :style "Regular" :size 24))
```


## Theme, Appearance, General Behavior {#theme-appearance-general-behavior}

Set our theme

```emacs-lisp
(setq doom-theme 'doom-zenburn)
```


### Doom Dashboard {#doom-dashboard}

```emacs-lisp
;; (setq fancy-splash-image (concat doom-user-dir "emacs-e-logo.png"))
(setq fancy-splash-image (expand-file-name "splash/black-hole.png" doom-user-dir))
```


### Fancy Splash Image <span class="tag"><span class="ARCHIVE">ARCHIVE</span></span> {#fancy-splash-image}


### Custom Faces {#custom-faces}

These custom faces will appear later on in the config.
We could define them immediately before they are used, but I ran into issues with them not being initialized properly and not being applied if they are defined too close to where they are used.
So they're define here so they get loaded in early.

Faces for [`org-super-agenda`](#org-super-agenda).

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
      :foreground ,(doom-color 'violet) :height 1.3 :box nil :weight bold)

    `(org-agenda-date-weekend
      :foreground ,(doom-color 'violet) :height 1.3 :box nil :weight bold)

    `(org-agenda-date-weekend-today
      :foreground ,(doom-color 'violet) :height 1.3 :box nil :weight bold))

  (defface super-agenda-due-today-face
    `((t :background ,(doom-darken 'red 0.5) :underline t)) "" :group 'org-super-agenda)

  (defface super-agenda-today-face
    `((t :foreground ,(doom-color 'fg) :underline t)) "" :group 'org-super-agenda)

  (defface super-agenda-todo-face
    `((t :foreground ,(doom-color 'green+4)))  "" :group 'org-super-agenda )

  (defface super-agenda-date-face
    `((t :foreground ,(doom-color 'fg-1)))  "" :group 'org-super-agenda )

  (defface super-agenda-wait-face
    `((t :foreground ,(doom-color 'orange)))  "" :group 'org-super-agenda )

  (defface super-agenda-active-task-face
    `((t :foreground ,(doom-color 'green '256)))  "" :group 'org-super-agenda )

  (defface super-agenda-prog-face
    `((t :background ,(doom-darken 'green+4 0.5)))  "" :group 'org-super-agenda )

  (defface super-agenda-next-face
    `((t :background ,(doom-darken (doom-color 'green '256) 0.3)))  "" :group 'org-super-agenda )
  )

(add-hook 'doom-load-theme-hook #'set-my-faces)
```


### Setting Colors {#setting-colors}

```emacs-lisp
(custom-set-faces! ;; Super cool macro by the way!
  `(org-block
    :background ,(doom-color 'bg-alt ) :extend t))
(custom-set-faces! ;; Super cool macro by the way!
  `(org-block-begin-line
    :background ,(doom-color 'bg-alt ) :extend t))
(custom-set-faces! ;; Super cool macro by the way!
  `(org-block-end-line
    :background ,(doom-color 'bg-alt ) :extend t))
```


### Hook {#hook}

Define some functions that will update some aspects of our theme dynamically.

```emacs-lisp
(custom-set-faces! ;; Super cool macro by the way!
  `(org-block
    :background ,(doom-color 'bg-alt ) :extend t))
(custom-set-faces!
  `(org-block-begin-line
    :background ,(doom-color 'bg-alt ) :extend t))
(custom-set-faces!
  `(org-block-end-line
    :background ,(doom-color 'bg-alt ) :extend t))
(defun update-org-faces ()
  (custom-set-faces! ;; Super cool macro by the way!
    `(org-block
      :background ,(doom-color 'bg-alt ) :extend t))
  (custom-set-faces!
    `(org-block-begin-line
      :background ,(doom-color 'bg-alt ) :extend t))
  (custom-set-faces!
    `(org-block-end-line
      :background ,(doom-color 'bg-alt ) :extend t))
  )

```

```emacs-lisp

(global-pretty-mode t)

(setq visual-fill-column-width 120
      visual-fill-column-center-text t
      line-spacing 0.27)
```

Hook them to be executed when theme is loaded

```emacs-lisp
;; (add-hook 'doom-load-theme-hook #'update-pdf-colors)
;; (add-hook 'doom-load-theme-hook #'update-org-faces)
```


## Global {#global}

Global keybindings

```emacs-lisp
(evilem-default-keybindings "SPC")
```


## `embark` {#embark}

```emacs-lisp
(use-package! embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)))
```


## `vertico` {#vertico}

[vertico](https://github.com/minad/vertico) is a vertical completion framework.
Packages that are more popular and accomplish similar goals are `ivy` and `helm`.

```emacs-lisp
(use-package! vertico
  :ensure t
  :init
  (vertico-mode))
```


## `org-mode` {#org-mode}

This section ropes in a number of settings for `org-mode`.
My primary use case for `org-mode` is to typset documents with mathematical notation in  \LaTeX, but other use cases exist such as literate programming a la this config file.


### Some variables {#some-variables}

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
          (setq org-journal-dir "~/Documents/org/journal")
```

By default, the `org-preview-latex-default-process` doesn't play well with tikz pictures.
To fix this, we use `imagemagick` instead.

```emacs-lisp
          (setq org-preview-latex-default-process 'imagemagick)
```

```emacs-lisp
          (after! org
          (setq org-directory "~/Documents/org")
            (setq org-agenda-files '( "~/Documents/org/inbox.org"
                                    "~/Documents/org/gtd.org"
                                    "~/Documents/org/tickler.org")))
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


### Hooks {#hooks}

These will be activated when `org-mode` is opened.

```emacs-lisp
          (add-hook 'org-mode-hook #'mixed-pitch-mode)
          (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
          (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))
          ;; (add-hook 'org-mode-hook #'org-modern-mode)
          (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
          (add-hook 'org-mode-hook #'visual-fill-column-mode)
          (add-hook 'org-mode-hook #'evil-tex-mode)
```

```emacs-lisp
          ;; (defun set-my-org-margins ()
          ;;   "Set margins for org"
          ;;   (setq left-margin-width 7)
          ;;   (setq right-margin-width 7))

          ;; (add-hook 'org-mode-hook 'set-my-org-margins)
          ;; (setq-hook! 'org-mode-hook left-margin-width 7)
          ;; (setq-hook! 'org-mode-hook right-margin-width 7)
```


### Appearance {#appearance}

Specify sizes of org headlines by level.

```emacs-lisp
(custom-set-faces!
  '(org-level-1 :inherit outline-1 :height 2.0)
  '(org-level-2 :inherit outline-2 :height 1.8)
  '(org-level-3 :inherit outline-3 :height 1.5)
  '(org-level-4 :inherit outline-4 :height 1.2)
  '(org-level-5 :inherit outline-5 :height 1.0))
```

This makes the background of inline code blocks the same as the normal background color, because apparently I value form over function.

```emacs-lisp
  ;; (custom-set-faces!
  ;;   `(org-block
  ;;     :background ,(doom-lighten 'bg-alt 0.1) :extend t))
  ;; (custom-set-faces!
  ;;   `(org-block-begin-line
  ;;     :background ,(doom-lighten 'bg-alt 0.1) :extend t))
  ;; (custom-set-faces!
  ;;   `(org-block-end-line
  ;;     :background ,(doom-lighten 'bg-alt 0.1) :extend t))
```

Startup with all trees folded and format some org features

```emacs-lisp
(use-package! org
  :config
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

I use some of my own block names in typsetting math, so we make sure that these blocks have the face we just defined applied properly.

```emacs-lisp
;; (setq org-protecting-blocks '("src" "example" "export" "theorem" "proposition" "proof" "lemma" "corollary"))
```


## `bibtex` {#bibtex}

Bibtex is a bibliography package used for \LaTeX.
We configure some of its settings here.

```emacs-lisp
              (setq LaTeX-always-use-Biber t)
              (setq bibtex-dialect 'biblatex)
              (setq bibtex-completion-bibliography "~/Documents/bib/reference-texts.bib")
```


## `org-super-agenda` {#org-super-agenda}

```emacs-lisp
(after! org
  (use-package! org-super-agenda
    :init
    (org-super-agenda-mode)
    :config
    (require 'doom-themes)

    (custom-set-faces!
      `(org-super-agenda-header
        :background "bg" :underline t :height 1.1 ))

    (setq org-super-agenda-groups
          '(
            (:name "Tasks"
             :todo ("NEXT" "TODO" )
             :face 'super-agenda-todo-face)
            (:habit t)

            (:name "Waiting Tasks"
             :face 'super-agenda-active-task-face
             :todo ("WAIT"))

            (:name "Events"
             :todo "EVENT")
            ))
    )
  )
```


## `org-modern` {#org-modern}

We will set custom faces for each of the TODO keywords in the above list.
Some modifications were made to the following [reference.](https://discourse.doomemacs.org/t/change-style-of-idea-in-org-mode/3041/4)

```emacs-lisp
(setq org-modern-block-fringe 5)
```

```emacs-lisp
(after! org
  (use-package! org-modern
    :demand t
    :hook (org-mode . org-modern-mode)
    :config
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
            ))
    (global-org-modern-mode)
    ;; (setq org-modern-block-name 'modern-block-name)
    )

  )
```

```emacs-lisp
(after! org
  (require 'org-indent)
  (org-indent-mode -1)
  )
```


### Agenda {#agenda}

```emacs-lisp
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-start-day "-1d")
(setq org-agenda-todo-ignore-scheduled t)
```


### PDF Export Process {#pdf-export-process}

This PDF export process ensures that bibliographies are properly exported, making use of `biblatex`.

```emacs-lisp
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
```


### Keybinds {#keybinds}

```emacs-lisp
(map! :map org-mode-map
      :i "C-c ]" #'org-ref-insert-link
      :i "C-c [" #'org-ref-insert-ref-link
      :n "z o" #'+org/open-all-folds
      :n "z M" #'+org/close-all-folds
      :nvi "M-m" #'org-next-visible-heading
      :nvi "M-p" #'org-previous-visible-heading
      :nvi "M-l" #'org-latex-preview )
```


### Export Settings {#export-settings}


#### Disable export of TODO keywords {#disable-export-of-todo-keywords}

```emacs-lisp
(setq org-export-with-todo-keywords nil)
```


#### Hide image links from export <span class="tag"><span class="ARCHIVE">ARCHIVE</span></span> {#hide-image-links-from-export}


#### `noex`  Drawers {#noex-drawers}

Sometimes I have text in an org document that I want to see while editing, but don't want it exported to the final document such as PDF.
This setting excludes all drawers carrying the name `noex`  from being exported to the final output.
Word by word, this variable says "export all drawers that _don't_ have the name `noex`"

```emacs-lisp
(setq org-export-with-drawers '(not "noex"))
```

This setting applies to all org files, but can be overwritten on a per-file basis in the `:options` header.


#### Cleanup `org-directory` of log files from export {#cleanup-org-directory-of-log-files-from-export}

\LaTeX generates a bunch of log files and extraneous files during its compilation process which can really clutter the compile directory quickly.
This code adds the relevant file extensions to org's log file extension list, and so it will clean them up after the export process is finish.

\LaTeX:

```emacs-lisp
;; (setq org-latex-logfiles-extensions
;;       (quote
;;        ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl"  "run.xml" "bcf")))
```


## `helm-bibtex` {#helm-bibtex}

`helm-bibtex` is a package that provides tools for bibliography management with the `helm` completion framework.

`helm-bibtex` does not have this issue, hence I use it.

```emacs-lisp
(use-package! helm-bibtex
  :config
  (setq bibtex-completion-bibliography "~/Documents/bib/reference-texts.bib"
    bibtex-completion-library-path '("~/Documents/books"  "~/Documents/articles")
    bibtex-completion-notes-path "~/Documents/org/general-notes.org"
    bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

    bibtex-completion-additional-search-fields '(keywords)
    bibtex-completion-display-formats
    '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
      (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
      (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
      (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
      (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
    ))
```


## `org-ref` {#org-ref}

`org-ref` is a useful package to simplify and unify the addition of citations to documents.

```emacs-lisp
(use-package! org-ref
  :after org
  :ensure t
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
  (setq   org-ref-default-bibliography "~/Documents/bib/reference-texts.bib"
          org-ref-pdf-directory '("~/Documents/books" "~/Documents/articles")
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


## `org-noter` {#org-noter}

```emacs-lisp
(use-package! org-noter
  :defer f
  :config
  (setq org-noter-always-create-frame t))
```


## `org-roam` <span class="tag"><span class="ARCHIVE">ARCHIVE</span></span> {#org-roam}


## `org-capture` {#org-capture}

Used mostly in tight integration with the `agenda`, capture templates let me capture snippets of org text and quickly refile them to worry about later.
They can of course be used to capture more arbitrary snippets of text and refiled to non-agenda files, but my workflow is still in a young state.
It is inspired mostly by "Getting Things Done".
Maybe I will find use for them later.

These templates set up the outline. Here is a table of what they do:

| Entry    | Text Content | File        | Top Headline |
|----------|--------------|-------------|--------------|
| Todo     | "\* TODO"    | inbox.org   | Tasks        |
| research | "\* RSCH"    | inbox.org   | Research     |
| idea     | "\* IDEA"    | inbox.org   | Ideas        |
| event    | "\* EVENT"   | tickler.org | N/A          |
| read     | "\* READ""   | inbox.org   | N/A          |

```emacs-lisp
(after! (org-modern)
  (use-package! org-capture
    :config
    (setq org-refile-targets '(("~/Documents/org/gtd.org" :maxlevel . 1)
                               ("~/Documents/org/inbox.org" :maxlevel . 1)
                               ("~/Documents/org/tickler.org" :maxlevel . 1)
                               ("~/Documents/org/maybe.org" :maxlevel . 1)
                               ("~/Documents/org/topology2.org" :maxlevel . 2)
                               ("~/Documents/org/alggeo2.org" :maxlevel . 2)
                               ("~/Documents/org/algtop2.org" :maxlevel . 2)
                               ))
    (setq org-capture-templates
          '( ("t" "Todo" entry (file "~/Documents/org/inbox.org")
              "* TODO  %?\n  %i\n  %a #+created: %t")
             ("r" "research" entry (file "~/Documents/org/inbox.org")
              "* RSCH  %?\n  %i\n  %a #+created: %t")
             ("i" "idea" entry (file "~/Documents/org/inbox.org")
              "* IDEA  %?\n  %i\n  %a #+created: %t")
             ("e" "event" entry (file "~/Documents/org/tickler.org")
              "* EVENT  %?\n  %i\n #+created: %t")
             ("R" "read" entry (file "~/Documents/org/inbox.org" )
              "* READ  %?\n  %i\n #+created: %t")
             ))
    )
  )
```


## `pdf-view-mode` {#pdf-view-mode}


### Keybinds {#keybinds}

```emacs-lisp
(map! :map pdf-view-mode-map
      "M-m" #'pdf-view-auto-slice-minor-mode
      "M-f" #'pdf-view-themed-minor-mode)
```


### Hooks {#hooks}

```emacs-lisp
(add-hook 'pdf-tools-enabled-hook 'pdf-view-themed-minor-mode)
(add-hook 'pdf-tools-enabled-hook 'pdf-view-auto-slice-minor-mode)
```


## `haskell-mode` {#haskell-mode}

```emacs-lisp
(setq haskell-compile-command "ghc -Wall -ferror-spans -fforce-recomp -dynamic -c %s")
```


## `python-mode` {#python-mode}


### Elpy {#elpy}

```emacs-lisp
(elpy-enable)
```


## `xenops-mode` {#xenops-mode}

`xenops-mode` is a \LaTeX editing environment that automatically and asynchronously compiles LaTeX dvi files.
It is a more feature-rich replacement for `org-latex-preview`.

Easy way to toggle

```emacs-lisp
(map! :map org-mode-map
      :n "SPC m z" #'xenops-mode
      :map LaTeX-mode-map
      :n "SPC m z" #'xenops-mode)
```

Increase the preview image sizes so that they're readable.

```emacs-lisp
(setq xenops-math-image-scale-factor 1.35
      xenops-reveal-on-entry nil
      )
```


## `yasnippets` {#yasnippets}

This section is taken from the ideas and code in this article: [LaTeX Input for the Impatient Scholar](https://karthinks.com/software/latex-input-for-impatient-scholars/).

This function allows for auto expanding of snippets.

```emacs-lisp
(require 'yasnippet)
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
```


## `ink` {#ink}

[ink](https://github.com/foxfriday/ink) is a package that allows for fast insertion of inkspace figures into `latex-mode` and `org-mode` buffers.

By default, `ink` embeds `.png` files into `org-mode` files.
I wanted to emulate Castel's [inkscape workflow](https://castel.dev/post/lecture-notes-2/) for inserting Inkscape figures into LaTeX files quickly, and `ink` gets us close, but not quite there.
This configuration lets me get what I want, by allowing insertion of a `.pdf_tex` file into documents, which will allow for adjustment of fonts on the fly as we export to PDF.
The behavior should insert a LaTeX snippet with a new figure containing a `.pdf_tex` file, while also providing an `org-mode` link to a png to be viewed inline in the org buffer.

The `ink-insert-org-combined` function inserts a LaTeX figure with the `pdf_tex` exported version of the `svg` file so that it is sensitive to LaTeX formatting changes.
It also puts a second `png` version in a `:noex:` drawer.
This `:noex:` drawer is a general notation that I use to withhold some portions of an org document from being exported.
In this case, we use this drawer because I'd still like to see the image of the figure inline, but don't want it exported twice.

```emacs-lisp
(use-package! ink
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

Set keybindings to insert inkscape figures

```emacs-lisp
(map! :map org-mode-map
      :n "SPC i i"
      #'ink-make-figure)
```


## quiver {#quiver}

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

```emacs-lisp
(map! :map org-mode-map
      :prefix "SPC i c"
      :nv "l"
      #'open-quiver-local
      :nv "w"
      #'open-quiver-web)
```


## `org-babel` {#org-babel}

```emacs-lisp
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (haskell . t)
   (jupyter . t)))
```


## `haskell-mode` {#haskell-mode}

```emacs-lisp
(add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook #'lsp-ui-mode)
```


## `writeroom-mode` {#writeroom-mode}

`writeroom-mode` is a distraction-free editing mode.

```emacs-lisp
(after! xenops
  (use-package! writeroom-mode
    :init
    (defun fix-writeroom-xenops-on ()
      (require 'xenops)
      (progn
        (xenops-increase-size)
        (xenops-increase-size)
        (xenops-increase-size))
      (setq xenops-math-image-scale-factor 2.0)
      ))

  (defun fix-writeroom-xenops-off ()
    (require 'xenops)
    (progn
      (xenops-decrease-size)
      (xenops-decrease-size)
      (xenops-decrease-size))
    (setq xenops-math-image-scale-factor 1.35)
    )
:config
(add-hook 'writeroom-mode-enable-hook #'fix-writeroom-xenops-on)
(add-hook 'writeroom-mode-disable-hook #'fix-writeroom-xenops-off)
)
```


## `company` {#company}

Disable company in `org-mode`.

```emacs-lisp
(setq company-global-modes '(not org-mode))
```


## `ox-hugo` {#ox-hugo}

`ox-hugo` is an `org` export tool to Hugo-compatible markdown files for easy publishing to static websites.
