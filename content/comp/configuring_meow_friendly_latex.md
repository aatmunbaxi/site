+++
title = "Configuring meow for Friendlier LaTeX Editing"
author = ["Aatmun Baxi"]
tags = ["emacs", "meow"]
draft = false
weight = 2002
type = "post"
+++

_TL;DR: We implement some configuration to recover functionality possible in evil-tex in the meow modal editing scheme._
_This article assumes some familiarity with the evil and meow modes._

<div class="ox-hugo-toc toc local">

- [`evil-tex`](#evil-tex)
- [Doing Our Own Thing](#doing-our-own-thing)
- [Inline Math](#inline-math)
- [Environments](#environments)
- [LaTeX Parentheses Delimiters](#latex-parentheses-delimiters)
- [Closing Thoughts](#closing-thoughts)

</div>
<!--endtoc-->

Having ditched `evil-collection` to get familiar with the vanilla emacs keybindings, I surprisingly found myself satisfied with many of the facilities emacs provides out-of-the-box for editing text.
Alas, I feel modal editing is something I prefer, so I&rsquo;ve adopted [meow](https://github.com/meow-edit/meow).
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
We can take this idea and help make meow friendlier for LaTeX editing.

In meow, it&rsquo;s easy to define a `thing` with the function `(meow-thing-register)`.


## Inline Math {#inline-math}

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


## Environments {#environments}

At their core, LaTeX environments look like

```text
\begin{ENV}
...
\end{ENV}
```

Happily, the code in `evil-tex` uses to grab the LaTeX environment objects is essentially some vanilla elisp, without dependency on evil internals.
In particular, we focus on this code:

```emacs-lisp
(require 'latex)
(setq evil-tex-select-newlines-with-envs nil)

(defun evil-tex--select-env ()
  "Return (outer-beg outer-end inner-beg inner-end) for enviornment object.

If `evil-tex-select-newlines-in-envs' is non-nil, the inner
variant would NOT include newlines proceeding the \\begin and
preceding the \\end.

\\begin{foobar}{bar}[baz]
^outer-beg              ^inner-beg
qux
\\end{foobar}
^inner-end  ^outer-end"
  (let (outer-beg outer-end inner-beg inner-end)
    (save-excursion
      (cond
       ;; `LaTeX-find-matching-begin' doesn't like being exactly on the \\begin
       ((looking-at (regexp-quote "\\begin{"))
        t)
       ;; `LaTeX-find-matching-begin' doesn't like being near the } of \\end{}
       ((or (= (char-before) ?})
            (= (char-after) ?}))
        (backward-char 2)
        (LaTeX-find-matching-begin))
       (t
        (LaTeX-find-matching-begin)))
      ;; We are at backslash of \\begin
      (setq outer-beg (point))
      (forward-sexp)
      (while (or
              (= (char-after) ?{)
              (= (char-after) ?\[))
        (forward-sexp))
      (when (and evil-tex-select-newlines-with-envs
                 (looking-at "\n[ \t]*"))
        (goto-char (match-end 0)))
      (setq inner-beg (point))
      (goto-char (1+ outer-beg))
      (LaTeX-find-matching-end)        ; we are at closing brace
      (setq outer-end (point))
      (search-backward "\\end")        ; goto backslash
      (when (and evil-tex-select-newlines-with-envs
                 (looking-back "\n[ \t]*" (- (point) 10)))
        (goto-char (match-beginning 0)))
      (setq inner-end (point))
      (list outer-beg outer-end inner-beg inner-end))))
```

The only evil-specific thing in this function is `evil-tex-select-newlines-with-envs`, which is a self-explanatory user-defined variable.
We can write functions that slice off the output of this function to get a cons cell of the inner bounds and outer bounds respectively, and use those functions to define our things.
The functions to get the inner and outer parts are

```emacs-lisp
;; Select inner and outer environment pairs
(defun my/meow-inner-env ()
  (let ((result (evil-tex--select-env)))
    (cons (nth 2 result) (nth 3 result))))

(defun my/meow-outer-env ()
  (let ((result (evil-tex--select-env)))
    (cons (nth 0 result) (nth 1 result))))
```

Now the configuration for our environment thing is just

```emacs-lisp
(meow-thing-register 'latex-env
                     #'my/meow-inner-env #'my/meow-outer-env)

(add-to-list 'meow-char-thing-table '(?E . latex-env))
```

{{< figure src="/ox-hugo/meow-latex-env.gif" caption="<span class=\"figure-number\">Figure 3: </span>Demo of our user-defined latex-env thing" >}}


## LaTeX Parentheses Delimiters {#latex-parentheses-delimiters}

Parentheses delimitiers in math mode are a bit of a tricky case.
We&rsquo;d like to include all possible delimiters in math mode, including the ones modified by `\left \right`, `\bigl \bigr`, etc.
In addition to that, we&rsquo;d hope to also capture basic delimiters like `(  )` and `[ ]` and `\{ \}`.
To do this, we will do the following:

-   create a master list of all possible LaTeX parentheses delimiters, including unmodified ones like `( )`
-   use meow&rsquo;s internal `meow--thing-pair-function` to find all pairs we can see around the point
-   find the match closest to the point

This will cover all cases of where the point could be, even in deeply nested parentheses.

First we declare our master list of delimiters:

```emacs-lisp
(setq meow--latex-mod-delim-pairs
      (cl-loop for (l r)
               in '(( "(" ")" )
                    ( "\\[" "\\]" )
                    ( "\\\\{" "\\\\}" )
                    ( "\\\\lvert" "\\\\rvert" )
                    ( "\\\\lVert" "\\\\rVert" )
                    ( "\\\\langle" "\\\\rangle" ))
               nconc
               (cl-loop for (pre-l pre-r)
                        in '( ("" "")   ;; after concatting corresponds to unmodified delim
                              ( "\\\\left"  "\\\\right")
                              ( "\\\\bigl"  "\\\\bigr")  ("\\\\big"  "\\\\big")
                              ( "\\\\biggl" "\\\\biggr") ("\\\\bigg" "\\\\bigg")
                              ( "\\\\Bigl"  "\\\\Bigr")  ("\\\\Big"  "\\\\Big")
                              ( "\\\\Biggl" "\\\\Biggr") ("\\\\Bigg" "\\\\Bigg"))
                        collect (cons (concat pre-l l) (concat pre-r r)))))
```

The next two functions are helpers for the main method.

```emacs-lisp
(defun append-bounds-distance (pair)
  "Appends the minimum distance of match pair to point"
  (if pair
      (cons pair (min (-  (point) (car pair))  (- (cdr pair) (point))))
    'nil))

(defun find-min-distance-match (matches-with-distances)
  "Finds the match with minimal distance to point"
  (let ((nearest-match (cons (point) (point)))
        (min-distance (float 1.0e+INF)))
    (dolist (match matches-with-distances)
      (when (cdr match)
        (when  (> min-distance (cdr match) )
          (setq nearest-match (car match))
          (setq min-distance (cdr match)))))
    nearest-match))
```

Now here&rsquo;s our main function:

```emacs-lisp
(defun my/meow-latex-paren-search (near)
  "Find nearest LaTeX parenthesis bounds.
NEAR denotes if match should be inner or bounds"
  (interactive)
  (let ((found-pairs (list )))
    (dolist (leftright meow--latex-delim-pairs)
      (push  (meow--thing-pair-function
              (car leftright) (cdr leftright) near) found-pairs))
    (let ((bounds-with-distances (mapcar #'append-bounds-distance found-pairs)))
      (find-min-distance-match bounds-with-distances))))
```

Don&rsquo;t let this elisp scare you; it does exactly what I said when I laid out the approach to this problem.
That is, it generates a list of delimiters we find aroud the point, and finds the closest such match, returning it.

The `near` argument specifies if we want to match the inner or bounds of the match.
It will be `t` for inner and `nil` for bounds.
We can hook this into two new functions for the inner and bounds matching, respectively.

```emacs-lisp
(defun my/meow-latex-paren-bounds ()
  (my/meow-latex-paren-search nil))

(defun my/meow-latex-paren-inner ()
  (my/meow-latex-paren-search t))

(meow-thing-register 'latex-delim
                     #'my/meow-latex-paren-inner
                     #'my/meow-latex-paren-bounds)

(add-to-list 'meow-char-thing-table '(?D . latex-delim))
```

Here&rsquo;s what the result looks like:

{{< figure src="/ox-hugo/meow-delim-demo.gif" caption="<span class=\"figure-number\">Figure 4: </span>Demo of our user-defined delimiter thing" >}}

Note that the way we have defined the delimiters makes it trivial to add/subtract delimiters from the list of things we want to match.


## Closing Thoughts {#closing-thoughts}

What I&rsquo;ve shown here is a very small, quickly-put-together look at the hackability of meow.
The documentation for meow is very comprehensive, and users should customize meow to their heart&rsquo;s content.
After all, one of the selling points of meow is how easy it is to &ldquo;roll your own&rdquo; modal editor.
Continued refinements of your workflow attuned to your particular idiosyncracies is a rewarding endeavour.
I hope I&rsquo;ve brought some inspirational ideas here.
