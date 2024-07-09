+++
title = "Consistent, Dynamic Access to Custom Emacs Theme Palettes"
author = ["Aatmun Baxi"]
date = 2024-07-05
tags = ["emacs"]
draft = false
weight = 2005
series = ["Hacking emacs"]
section = "Computing"
+++

<div class="tldr">

[technicolor](https://www.github.com/aatmunbaxi/technicolor) provides a uniform way to identify desired colors in the current emacs theme. This article lays out the problem that prompted its creation and motivation for the its design.

</div>

<!--more-->


## The Problem {#the-problem}

In thinking of new ways to ~~waste time with~~ _extend_ emacs, I retread my brief attempts to use the color values from DOOM themes to customize faces in way cohesive with the current theme.
Fortunately, accessing DOOM themes' color palettes from elisp is possible with `doom-color` and friends.
Unfortunately for me, I don't always have a DOOM theme active.

Consider the following elisp:

```emacs-lisp
(set-face-attribute 'org-verbatim nil :foreground (doom-color 'green))
```

All this is set the font color for org verbatim text to the associated green color in the current DOOM theme.
If you _only_ use DOOM themes, you can hook this to `load-theme` and call it a day.
Here's what that looks like, but watch what happens when I load a non-DOOM theme:

{{< figure src="/ox-hugo/technicolor-bad-switch.gif" >}}

Blegh!
The code still accesses the variable `doom-themes--colors`, which remains unchanged upon loading a non-DOOM theme.
The green from the previous DOOM theme palette sticks out as very _wrong_ here, and our customization is no good.

A fix is in sight; the modus theme pack also comes with a function you can use to access color values from the current modus theme: `modus-themes-get-color-value`.
Huzzah!
All we have to do is write a function `current-theme-color` that determines the type of the current theme and dispatches the correct color-getting function, so our theme customization is now:

```emacs-lisp
(set-face-attribute 'org-verbatim nil :foreground (current-theme-color 'green))
```

Err, this doesn't quite work either.
Most all themes define a color "green", so this solution should be sufficient for basic colors like red, green, blue, magenta, etc.
What happens if we wanted to use the theme's foreground color, like so as in `(doom-color 'fg)`?

```emacs-lisp
(set-face-attribute 'org-verbatim nil :foreground (current-theme-color 'fg))
```

It breaks, but why?
DOOM themes and modus themes use different symbols to identify the foreground color: `fg` and `fg-main`, respectively.

In conclusion, there's no reasonable way (that I know of) to uniformly identify arbitrary colors in two arbitrary themes, short of a universal standard™ ([obligatory](https://xkcd.com/927/)) for how to define custom emacs themes.


## A Solution {#a-solution}

A decent compromise is to let the user choose a subset of colors (which we call a "universal" or "standard" palette) they want to use in elisp, and choose colors from each of their themes' palettes to associate to those colors.
This is the abstraction implemented in [technicolor](https://github.com/aatmunbaxi/technicolor).

In our situation, we want to access the foreground color of DOOM themes, modus themes, and catppuccin themes.
For good measure, we'll throw in background color, red, green, blue, magenta, and cyan into that list.
Our standard palette can be loaded into a technicolor variable, and the data for the themes we want to use this standard palette with can be specified.

```emacs-lisp
(setq technicolor-colors '(foreground background red green blue magenta cyan)
      technicolor-themes '(("^doom-.*" doom-color
                            ((foreground . fg)
                             (background . bg)))

                           ("^modus-.*" modus-themes-get-color-value
                            ((foreground . fg-main)
                             (background . bg-main)
                             (green . green-warmer))

                            ("^catppuccin" technicolor--get-catppuccin-color
                             ((foreground . text)
                              (background . base)
                              (magenta . pink)
                              (cyan . sky))))))
```

The `technicolor-themes` variable contains a list of "theme data", which contain a regex that will match a theme or group of themes, a function for technicolor to use which will access the colors in our universal palette, and an alist of mappings from the symbols in our universal palette to symbols that theme uses to refer to those colors.

Once configured, the standard function used to refer to a color in the current theme's palette is `technicolor-get-color`.
The argument for this function will be a symbol in `technicolor-colors`.
Now, for example, when `technicolor-get-color` function is called with a `doom-*` theme active, technicolor will return the output of `(doom-theme (alist-get 'color ((foreground . fg) (background . bg))))` if `color` has an associated key, or `(doom-color 'color)` if it does not.
For safe use with customizing faces, the function used to get the color should return `nil` or the symbol `unspecified`.

Remember how we wanted to access the color `red` from our themes?
Notice that `red` isn't in any of the configuration alists, since all our themes use the symbol `red` to identify the color green.
Similarly for `blue`.

What does this buy us?
The face customization from the beginning is now

```emacs-lisp
(set-face-attribute 'org-verbatim t :foreground (technicolor-get-color 'green))
```

Hooking appropriately, this now works seamlessly across themes that are matched in `technicolor-themes`.

{{< figure src="/ox-hugo/technicolor-good-switch.gif" >}}
