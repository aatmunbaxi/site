+++
title = "A meow-state for speedy org-fu"
author = ["Aatmun Baxi"]
tags = ["emacs", "meow"]
draft = false
weight = 2003
type = "post"
+++

_TL;DR: We implement our own `meow-state` to navigate `org-mode` document structure to recover and extend the function of org speed keys in meow._

I&rsquo;ve been very pleased with my switch to the `meow` modal editor, but one of the downsides of it is no obvious way to use [speed keys](https://orgmode.org/manual/Speed-Keys.html) in `org-mode`.
Namely, the meow normal state maps would clash with the speed key definitions.
One could just redefine the speed keys to avoid the normal state maps, or redefine the normal state maps themselves, but that&rsquo;s no fun.

Org speed keys offer a kind of &ldquo;transient modality&rdquo; to default emacs keybindings in the words of Mickey Petersen (which he states are already a kind of transient modality).
But meow is _already_ a modal editing scheme, there&rsquo;s no benefit that I can see to making speed keys a transient behavior, especially with how much I use org-mode.

We take a different approach and define our own `meow-state` to navigate org structure.
Meow offers excellent support for defining your own states.
For those who use evil, this like the user being able to define a different mode under which we can edit and navigate, much like `evil-normal-state` and `evil-visual-state`.
Like evil, each `meow-state` has its own set of keybindings that call specific functions.
We will define a state `meow-org-motion-state` that will be used for navigating org documents.
Doing so is shockingly simple; most of the code is lifted directly from [the meow customization documentation.](https://github.com/meow-edit/meow/blob/master/CUSTOMIZATIONS.org)

The entire code in defining the state is this:

```emacs-lisp
(setq meow-org-motion-keymap (make-keymap))
(meow-define-state org-motion
  "Org-mode structural motion"
  :lighter "[O]"
  :keymap meow-org-motion-keymap
  :form (when (not (eq major-mode 'org-mode))
          (message "You have no power here!")))

(meow-define-keys 'org-motion
  '("<escape>" . meow-normal-mode)
  '("i" . meow-insert-mode)
  '("g" . meow-normal-mode)
  '("u" .  meow-undo)
  ;; Moving between headlines
  '("k" .  org-previous-visible-heading)
  '("j" .  org-next-visible-heading)
  ;; Moving between headings at the same level
  '("p" .  org-backward-heading-same-level)
  '("n" .  org-forward-heading-same-level)
  ;; Moving subtrees themselves
  '("K" .  org-subtree-up)
  '("J" .  org-subtree-down)
  ;; Subtree de/promotion
  '("L" .  org-demote-subtree)
  '("H" .  org-promote-subtree)
  ;; Completion-style search of headings
  '("v" .  consult-org-heading)
  ;; Setting subtree metadata
  '("l" .  org-set-property)
  '("t" .  org-todo)
  '("d" .  org-deadline)
  '("s" .  org-schedule)
  '("e" .  org-set-effort)
  ;; Block navigation
  '("b" .  org-previous-block)
  '("f" .  org-next-block)
  ;; Narrowing/widening
  '("N" .  org-narrow-to-subtree)
  '("W" .  widen))

(meow-define-keys 'normal
  '("O" . meow-org-motion-mode))
```

Whilst being easy to define, this configuration is also very easy to read.
Every keymap is self explanatory.
In particular, we implement meow-style up-down nagivation of subtrees with `jk`, and add level-restricted motion with `np`.
Subtree metadata can be edited from this state without exiting the state, and source block navigation is added--useful for literate programmers.

There&rsquo;s one major benefit to using a custom `meow-state` instead of attempting to get speed keys working: **this state is available anywhere in the buffer.**
With speed keys, the point **must** be at the beginning of an org heading.
With our custom state, a simple press of `O` while in `meow-normal-mode` puts us in navigation mode.
This makes navigation of org documents fast and accessible in a way that is superior to speed keys, something afforded to us by adopting a modal editing paradigm.

A demo of this mode is given below; you can see the keys pressed in the tab-line.

{{< figure src="/ox-hugo/meow-org-motion.gif" caption="<span class=\"figure-number\">Figure 1: </span>Demo of our org-motion state" >}}
