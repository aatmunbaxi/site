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

In thinking of new ways to ~~waste time with~~ _extend_ emacs, I retread my brief attempts to use the hexadecimal color values from DOOM themes to customize faces in a way that is cohesive with the current theme.
Fortunately, accessing DOOM themes' color palettes from elisp is possible with the function `doom-color`.
It takes in a symbol identifying a color and spits out its hexadecimal value.
This works really well for what it does; unfortunately for me, I don't always have a DOOM theme active.

Consider the following elisp:

```emacs-lisp
(set-face-attribute 'org-verbatim nil :foreground (doom-color 'green))
```

This sets the font color for org verbatim text to the associated green color in the current DOOM theme.
If you _only_ use DOOM themes, you can hook this to `load-theme` and stop reading now.
Here's what it looks like, but watch what happens when I load a non-DOOM theme:

{{< figure src="/ox-hugo/technicolor-bad-switch.gif" caption="<span class=\"figure-number\">Figure 1: </span>Works for DOOM themes, but yucky transition to `modus-operandi-tinted`!" >}}

Blegh!
The code still refers to the variable `doom-themes--colors`, which remains unchanged upon loading a non-DOOM theme[^fn:1].
The green from the previous DOOM theme palette sticks out as _wrong_ here, and we've exposed our customization's fragility.

A fix is in sight; the modus theme pack also comes with the function `modus-themes-get-color-value`, which does exactly what `doom-color` does.
Huzzah!
All we have to do is write a function `current-theme-color` that determines the type of the current theme and dispatches the correct color-getting function, so our theme customization is now:

```emacs-lisp
(set-face-attribute 'org-verbatim nil :foreground (current-theme-color 'green))
```

Err, this doesn't quite work either.
Most all themes define a color "green", so this solution is sufficient for basic colors like red, green, blue, magenta, etc.
What happens if we wanted to use the theme's foreground color, like with `(doom-color 'fg)`?

```emacs-lisp
(set-face-attribute 'org-verbatim nil :foreground (current-theme-color 'fg))
```

It still breaks on modus themes!
DOOM themes and modus themes use different symbols for the foreground color: `fg` and `fg-main`, respectively.

In conclusion, there's no reasonable way (that I know of) to uniformly identify arbitrary colors in two arbitrary themes, short of a universal standard™ ([obligatory](https://xkcd.com/927/)) for how to define custom emacs themes.
With the obvious issues with that solution unsaid, I propose a solution.


## A Solution? {#a-solution}

A compromise is to choose a subset of colors (called a "universal" or "standard" palette) to use in elisp, and choose colors from each of their themes' palettes to associate to those colors.
This is the abstraction implemented in [technicolor](https://www.github.com/aatmunbaxi/technicolor).

Let's say we want to use the foreground color of the DOOM themes, modus themes, and catppuccin themes.
For good measure, we also want the background color, red, green, blue, magenta, and cyan.
Our standard palette can be loaded into a `technicolor-colors`, and the data for the themes we want to use this standard palette with can be specified:

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

The `technicolor-themes` variable contains a list of lists, each of which contain a regex that will match a theme or group of themes, a function for technicolor to use which will access the colors in our universal palette (like `doom-color`), and an alist of mappings from the symbols in our universal palette to symbols the desired theme uses to refer to those colors.

Now, the function used to get the hex value of a color in the current theme's palette is `technicolor-get-color`.
The argument for this function is a symbol in `technicolor-colors`.
Now, for example, when `technicolor-get-color` function is called with a `doom-*` theme active, technicolor will return the output of

```emacs-lisp
(doom-theme (alist-get 'color ((foreground . fg) (background . bg))))
```

if `color` has an associated key, or `(doom-color 'color)` if it does not.
For safety in customizing faces, if `color` does not have a value in the mapping alist _and_ there is no `color` in the current theme's palette, `technicolor-get-color` will return `unspecified`.

Remember how we wanted to access the color `red` from our themes?
Notice that `red` isn't in any of the configuration alists, since all our themes use the symbol `red` to identify the color green.
Similarly for `blue`, so we don't need to write it out explicitly.

What does this buy us?
The face customization from the beginning is now

```emacs-lisp
(set-face-attribute 'org-verbatim t :foreground (technicolor-get-color 'green))
```

This now works seamlessly across themes that are matched in `technicolor-themes`.

{{< figure src="/ox-hugo/technicolor-good-switch.gif" caption="<span class=\"figure-number\">Figure 2: </span>Nice, cohesive greens." >}}

As a proof of concept, this example demonstrates the utility of `technicolor`.
One can imagine adding more face attribute changes and fine-tuning the faces on a per-theme (or per-theme pack) basis.


## More Goodies {#more-goodies}

Technicolor also provides manipulations wrapping the `color` library with the same ethos of accessing via `technicolor-colors`.
We can use `technicolor-blend` to create light/dark theme aware colors.
For example, let's say we want to make the background of `org-modern-tag` green.
Since the foreground color of the `org-modern-tag` face is just the foreground color of the theme, we need to make sure the green isn't too light on dark themes (respectively, too dark on light themes).
The following code does just that:

```emacs-lisp
(set-face-attribute 'org-modern-tag nil :background (technicolor-blend 'background 'green 80))
```

This sets the background color of the tag to be an 80% blend of the background color into the corresponding green.
The before and after of this customization is visualized with the `doom-dracula` theme

{{< figure src="/ox-hugo/tech_before.png" caption="<span class=\"figure-number\">Figure 3: </span>Before applying face customization" >}}

{{< figure src="/ox-hugo/tech_after.png" caption="<span class=\"figure-number\">Figure 4: </span>After applying face customization" >}}

Notice that I make no claims about how _good_ these customization choices are!

Other functions for color manipulation include adjusting saturation and brightness, getting a color's complementary color, and creating gradients between two colors.


## Conclusion {#conclusion}

I believe `technicolor` to be a natural abstraction and fair compromise of functionality and usability.
Hopefully this article is convincing for those that wish to ~~waste time with emacs~~ _manipulate the UX of their lisp environment_.

[^fn:1]: Or even reloading a DOOM theme with `consult-theme`, see [here](https://discourse.doomemacs.org/t/consult-theme-does-not-reload-doom-themes-color-when-setting-already-loaded-theme/4669).
