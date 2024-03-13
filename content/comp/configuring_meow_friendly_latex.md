+++
title = "Configuring meow for Friendly LaTeX Editing"
author = ["Aatmun Baxi"]
tags = ["emacs", "meow"]
draft = false
weight = 2002
type = "post"
+++

_TL;DR: We implement some configuration to recover functionality possible in evil-tex in the meow modal editing scheme._

Having ditched `evil-collection` to get familiar with the vanilla emacs keybindings, I surprisingly found myself satisfied with many of the facilities emacs provides out-of-the-box for editing text.
Alas, I feel modal editing is something I prefer, so I&rsquo;ve adopted to `meow`.
I chose meow for a few reasons:

-   Trivial to extend and hack on
-   The &ldquo;selection-first&rdquo; ethos is intriguing
-   No dependencies
-   Prefers built-in methods for various tasks
-   Keypad mode offers a great, lighter weight alternative to the `SPC` DOOM leader with evil mode

So far, it&rsquo;s been an enjoyable experience, but one evil-mode package--`evil-tex`--gave me pause.


## `evil-tex` {#evil-tex}

[evil-tex](https://github.com/iyefrat/evil-tex) is an extension to evil that adds support for &ldquo;vimmable&rdquo; text objects in LaTeX syntax.
As an example, let `|` denote the location of the point when using evil mode.
Suppose we&rsquo;re given the situation:

```text
The qu|ick brown fox jumps over the lazy dog.
```

In `evil-normal-mode` you can type the keys `ciw` to &ldquo;Change Inner Word&rdquo;, deleting the work `quick` and placing you in `evil-insert-mode`.

If `w` denotes the &ldquo;word&rdquo; evil object, evil-tex offers the math-mode text object, allowing this same loop to be performed but inside LaTeX syntax.
Consider the following inline math expression with point `|`:

```text
\( X\cong | Y \)
```

With `evil-tex-mode` enabled, and inside `evil-normal-mode`, we can press `cim` to &ldquo;Change Inner Math&rdquo;, deleting all the text within the `\( \)` delimiters and placing us in insert mode.
This is just one example of what evil-tex offers; a more comprehensive picture is in the documentation.


## Doing Our Own Thing {#doing-our-own-thing}

Meow uses `things` (lit.) to demarcate sections of text that you can navigate around and select.
For example, some things that come preloaded with meow are sentences, defuns, paragraphs, buffers, windows, and lines.
When a thing is defined, you can press `, <THING_KEY>` to select the inner part of the thing, where `<THING_KEY>` is the key associated with that thing (e.g. `l` for line, `d` for defun).
Similarly, you can press `. <THING_KEY>` to select to the bounds of the thing.
Here&rsquo;s a demo on how that works with the `symbol` thing, mapped to `e`:

{{< figure src="/ox-hugo/meow-symbol-inner.gif" caption="<span class=\"figure-number\">Figure 1: </span>Demo of symbol thing in meow" >}}

There&rsquo;s a parallel between this behavior and the &ldquo;inner &lt;object&gt;&rdquo; and &ldquo;all &lt;object&gt;&rdquo; behavior in evil.
For example, suppose we have a text object in evil that picks out the line the point is on, mapped to `l`.
Then the key sequence `c i l` in evil mode (to &ldquo;Change Inner Line&rdquo;) could be replicated in meow with `, l c`.

In meow, it&rsquo;s easy to define a `thing` with the function `(meow-thing-register)`.
Let&rsquo;s register a `thing` that picks out the LaTeX inline math environment `\( \)`.
The simplest way to do this is using the pair matching:

```emacs-lisp
(meow-thing-register 'inline-math
                     '(pair ("\\(") ("\\)"))
                     '(pair ("\\(") ("\\)") ) )
```

Now we can map this thing to a key:

```emacs-lisp
(add-to-list 'meow-char-thing-table '(?m . inline-math))
```

Now, when we&rsquo;re inside an inline math environment, we can press `, m` to select all the text within the math environment, and `. m` to select all of the math environment.

{{< figure src="/ox-hugo/meow-math-demo.gif" caption="<span class=\"figure-number\">Figure 2: </span>Demo of our user-defined math thing" >}}

The bindings `, m` and `. m` replicate the evil-tex object identification `i m` and `a m`, respectively.

We can also use this same concept to extend functionality to full-scale LaTeX environments.
That configuration is

```emacs-lisp
(meow-thing-register 'latex-env
                     '(regexp "\\\\begin{.*?}\n\\(?:\\\\label{.*?}\n\\)?" "\n\\\\end{.*?}" )
                     '(regexp "\\\\begin{.*?}" "\\\\end{.*?}" ) )

(add-to-list 'meow-char-thing-table '(?E . latex-env))
```

This adds the same support for latex environments that look this like:

```text
\begin{ENV}
...
\end{ENV}
```

The `\label{...}` directive sometimes appears in these environments, so the regex used to match these things are included as optional match groups.

{{< figure src="/ox-hugo/meow-latex-env.gif" caption="<span class=\"figure-number\">Figure 3: </span>Demo of our user-defined latex-env thing" >}}

In fact, in my own configuration, I&rsquo;ve combined the `latex-math` and `latex-env` things into one thing, so I use just the key `m` to pick out either inline math environments or `\begin{...} \end{...}` environments.

These are all well and good, but one of my favorite evil-tex objects were the LaTeX delimiters: including `\left( \right)` and friends.
Let&rsquo;s do those too.

```emacs-lisp
(setq latex-left-delim (rx "\\left"
                           (or "\\langle"
                               "("
                               "\["
                               "\\{"
                               "\\lbrace")))
(setq latex-right-delim (rx "\\right"
                            (or "\\rangle"
                                ")"
                                "\]"
                                "\\}"
                                "\\rbrace")))

(meow-thing-register 'latex-delim
                     `(regexp ,latex-left-delim ,latex-right-delim)
                     `(regexp ,latex-left-delim ,latex-right-delim))

(add-to-list 'meow-char-thing-table '(?j . latex-delim))
```

Here&rsquo;s what that looks like:

{{< figure src="/ox-hugo/meow-delim-demo.gif" caption="<span class=\"figure-number\">Figure 4: </span>Demo of our user-defined delimiter thing" >}}

Note that the way we have defined the delimiters makes it trivial to add/subtract delimiters from the list of things we want to match.


## Edge Cases {#edge-cases}

Because of the way meow searches for the beginnings and ends of things, this implementation has obvious edge cases which I think are acceptable compromises.
Notably, meow searches behind and in front of the point for the inner/outer/bounds of the thing.
This implies that--for example--if the point was on a line containing `\label{...}` and you press `, E` in normal mode, it would select the line containing the label directive as well as the content before the `\end{..}`:

{{< figure src="/ox-hugo/meow-env-edge-case.gif" caption="<span class=\"figure-number\">Figure 5: </span>Edge case in latex-env implementation" >}}

I strongly suspect (nay, I _know_) that more clever implementations are possible using emacs lisp.
I don&rsquo;t mind this behavior; most of the times when I want to use this feature are when I&rsquo;m in the midst of typing on the line below the label directive.


## Closing Thoughts {#closing-thoughts}

What I&rsquo;ve shown here is a very small, quickly-put-together look at the hackability of meow.
The documentation
