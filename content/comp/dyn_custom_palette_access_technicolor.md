+++
title = "Consistent, Dynamic Access to Your Current Emacs Theme's Palette"
author = ["Aatmun Baxi"]
date = 2024-07-05
tags = ["emacs"]
draft = false
weight = 2005
series = ["Hacking emacs"]
section = "Computing"
+++

<div class="tldr">

[technicolor](https://www.github.com/aatmunbaxi/technicolor) provides a uniform way to identify desired colors in the current emacs theme. This article describes the problem it solves and the  motivation for the its design.

</div>

<!--more-->


## The Problem {#the-problem}

In thinking of new ways to ~~waste time with~~ _extend_ emacs, I retread some attempts to use the hex color values from DOOM themes to customize faces in a way that was cohesive with the current theme.
Fortunately, accessing DOOM themes' color palettes from elisp is possible with the function `doom-color`.
It takes in a symbol identifying a color and spits out its hexadecimal value.
This works really well for what it does, but unfortunately for me, I don't always have a DOOM theme active.

Consider the following elisp:

```emacs-lisp
(set-face-attribute 'org-verbatim nil :foreground (doom-color 'green))
```

This sets the font color for org verbatim text to the associated green color in the current DOOM theme.
If I _only_ used DOOM themes, I could hook this to `load-theme` and save a weekend's worth of time.
Here's what that looks like, but watch what happens when I load a non-DOOM theme:

{{< figure src="/ox-hugo/technicolor-bad-switch.gif" caption="<span class=\"figure-number\">Figure 1: </span>Works for DOOM themes, but yucky transition to `modus-operandi-tinted`..." >}}

Blegh!
Internally, `doom-color` refers to the variable `doom-themes--colors`, which remains unchanged upon loading a non-DOOM theme[^fn:1].
The green from the previous DOOM theme is downright illegible, and we've exposed our customization's fragility.

A fix is in sight; the modus theme pack also comes with the function `modus-themes-get-color-value`, which does exactly what `doom-color` does.
Huzzah!
All we have to do is write a function `current-theme-color` that determines the type of the current theme and dispatches the correct color-getting function, so our theme customization becomes:

```emacs-lisp
(set-face-attribute 'org-verbatim nil :foreground (current-theme-color 'green))
```

Err, this doesn't quite work either.
Most all themes define a color "green", so this solution is sufficient for basic colors like red, green, blue, magenta, etc.
What happens if we wanted to use the theme's foreground color, like with `(doom-color 'fg)`?

```emacs-lisp
(set-face-attribute 'org-verbatim nil :foreground (current-theme-color 'fg))
```

It _still_ breaks on modus themes[^fn:2].
The reason: DOOM themes and modus themes use different symbols for the foreground color: `fg` and `fg-main`, respectively.

In conclusion, there's no reasonable way (that I know of) to uniformly pick out arbitrary colors (or even the same type of color) in two arbitrary themes, short of a universal standard™ ([obligatory](https://xkcd.com/927/)) for how to define custom emacs themes.
Obvious issues with that option left unsaid, I propose a solution.


## A Solution? {#a-solution}

A compromise is to choose a subset of colors (called a "universal" or "standard" palette) to use in elisp, and choose colors from each of the themes' palettes to associate to those colors.
This is the abstraction implemented in [technicolor](https://www.github.com/aatmunbaxi/technicolor).

Let's say we want to use the foreground color of the DOOM themes, modus themes, and catppuccin themes.
For good measure, we also want the background color, red, green, blue, magenta, and cyan.
Our standard palette can be loaded into `technicolor-colors`, and the data for the themes we want to use can be specified:

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

The `technicolor-themes` variable contains a list of lists, each of which contain a regex that will match a theme or group of themes, a function to access the colors in our universal palette (like `doom-color`), and an alist mapping the symbols in our universal palette to symbols in the desired theme[^fn:3].

Now, the function used to get the hex value of a color in the current theme's palette is `technicolor-get-color`.
When it is called with a `doom-*` theme active, technicolor will return the output of

```emacs-lisp
(doom-theme (alist-get 'color ((foreground . fg) (background . bg))))
```

if `color` has an associated value, or `(doom-color 'color)` if it does not.
Similarly, it does what we wanted our defunct `current-theme-color` function to do for modus themes and catppuccin themes.

For safety, if `color` does not have a value in the mapping alist _and_ there is no `color` value in the current theme's palette, `technicolor-get-color` will return `unspecified`.
We may always use `unspecified` in face customization.

Notice that `red` isn't in any of the configuration alists, since all our themes use the symbol `red` to identify the color red.
Similarly for `blue`, so we don't need to write it out explicitly.
This saves us from a bit of visual clutter, but there's no harm in including them anyway.

What does this buy us?
The face customization from the beginning of the article becomes

```emacs-lisp
(set-face-attribute 'org-verbatim t :foreground (technicolor-get-color 'green))
```

This now works seamlessly across themes that are matched in `technicolor-themes`.

{{< figure src="/ox-hugo/technicolor-good-switch.gif" caption="<span class=\"figure-number\">Figure 2: </span>Nice, cohesive greens." >}}

One can imagine adding more face attribute changes and fine-tuning the faces on a per-theme basis.


## A Light/Dark-Aware Customization and More Goodies {#a-light-dark-aware-customization-and-more-goodies}

Technicolor also provides manipulations wrapping the built-in `color` library with the same ethos of accessing via our universal palette.
As an example, we will use `technicolor-blend` to create light/dark-aware colors.

Let's say we want to make the background of `org-modern-tag` green.
To prevent the tags from being too distracting, we will make sure the green isn't too light on dark themes (respectively, too dark on light themes).

```emacs-lisp
(set-face-attribute 'org-modern-tag nil :background (technicolor-blend 'background 'green 80))
```

This sets the background color of the tag to be a blend of 80% background color into the theme's `green` value.
The before and after of this customization is presented with the `doom-dracula` theme.

{{< figure src="/ox-hugo/tech_before.png" caption="<span class=\"figure-number\">Figure 3: </span>Before applying face customization" >}}

{{< figure src="/ox-hugo/tech_after.png" caption="<span class=\"figure-number\">Figure 4: </span>After applying face customization" >}}

If we didn't blend the green with the background color, it would be distractingly bright.
Otherwise, we could use `technicolor-darken`, but then it would not work as intended for light themes.

Other functions for color manipulation include adjusting saturation and brightness, getting a color's complementary color, and creating gradients between two colors of defined fineness.


## Conclusion {#conclusion}

I believe `technicolor` to be a natural abstraction and fair compromise of functionality and usability.
Hopefully this article is convincing for those that wish to ~~waste time with emacs~~ _manipulate the graphical aspects of their lisp environment_.
More information regarding `technicolor` can be found on [its github page](https://www.github.com/aatmunbaxi/technicolor) and the docstrings of function and variables therein.

[^fn:1]: Or even reloading a DOOM theme with `consult-theme`, see [here](https://discourse.doomemacs.org/t/consult-theme-does-not-reload-doom-themes-color-when-setting-already-loaded-theme/4669).
[^fn:2]: Not as catastrophically, the value in the face customization would become `unspecified`.
[^fn:3]: The intention is that the mappings will make sense, like `(magenta . magenta-cooler)` for modus themes, but no limits are placed; one could even use `(foreground . background)`
