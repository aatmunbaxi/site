+++
title = "Emacs org-mode for LaTeX"
author = ["Aatmun Baxi"]
tags = ["emacs", "org", "hobby", "productivity"]
draft = false
weight = 2004
type = "post"
+++

About four years ago, my brother introduced me to Linux and free and open source software.
Around the same time I learned to use LaTeX to typeset documents with lots of mathematical notation.
I then stumbled across Gilles Castel&rsquo;s [article on typing LaTeX with vim](https://castel.dev/post/lecture-notes-1/), and I was amazed.

For those who don&rsquo;t know, typing LaTeX by default is a terrible experience.
_Terrible._
Using a program like vim with plugins made typing LaTeX so frictionless that I immediately dove into the world of extensible text editors.
I got more than I bargained for, but the payoff of effortless typesetting of math drove me to stick with it.
I finally replicated most of Castel&rsquo;s setup, and thought I had seen it all.

Then I found emacs org-mode.

<style>
aside {
    width: 40%;
    padding-left: 0.5rem;
    margin-left: 0.5rem;
    float: right;
    box-shadow: inset 5px 0 5px -5px #29627e;
    font-style: italic;
    color: #7a7c7d;
}

aside > p {
    margin: 0.5rem;
}
</style>

<aside>

> GNU Emacs [is] a sort of hybrid between Windows Notepad, a monolithic-kernel operating system, and the International Space Station.
>
> ---Steve Yegge

</aside>

Vim, the program I had been using for easy LaTeX input, has had a longstanding &ldquo;rivalry&rdquo; with another editor called GNU Emacs.
Since spending time with both programs, I&rsquo;ve found this &ldquo;rivalry&rdquo; a bit silly.
Vim and emacs are different programs designed with different use cases in mind with different philosophies.
They just _happen_ to both be text editors.
Comparing vim and emacs is a bit like comparing a bicycle to a tank simply because they can move people around quicker than by foot.

In any case, what brought me to emacs was org-mode.
Org-mode is a major mode of emacs, encompassing everything from marking up basic text files and exporting to PDF, keeping an agenda, and literally everything else you could imagine needing when it comes to typing documents of any sort.
Many think of it as souped-up markdown, but this is a disservice to its utility.
It&rsquo;s so powerful, I could probably find a way to make it do my laundry[^fn:1].

What initially drove me to org-mode was the function `org-latex-preview`.
In contrast to vim, emacs is a graphical program, which allows the program to _display images of compiled LaTeX over the text buffer you&rsquo;re editing._
Let&rsquo;s see what that looks like.

Org-mode is plaintext, so the raw text file you&rsquo;re editing might look something like this.

```text
**Hermitian Metrics
We'd like an analogue of Riemannian manifolds but "complexified".
Recall that a metric \( g \) is a \( 2\times 2 \) symmetric matrix at a point.
The complex analogue of "symmetric" is Hermitian, which motivates the following definition:

*Definition:* A /Hermitian metric/ on a complex manifold with local coordinates \( (z^1,\ldots , z^m) \) is a tensor field
\begin{equation*}
h_{j \overline{k}} dz^j\otimes dz^{\overline{k}}
\end{equation*}
where \( h_{j \overline{k}} \) varies smoothly, and is positive definite and Hermitian at each point \( z \).
The form
\begin{equation*}
\omega = \frac{i}{2} h_{j \overline{k}} dz^j\wedge dz^{\overline{k}}
\end{equation*}
is called the /Kahler form/ of the metric.
A complex manifold with a Hermitian metric is called a /Hermitian manifold/.
```

Here is this same snippet of text, but viewed in emacs, with all the org goodies activated.
![](/ox-hugo/org-latex-goodies-ex.png)

Notice how the compiled LaTeX appears inline, and the font size is variable for the heading.
These inline LaTeX previews are what pulled me to org-mode, and with the help of some third party programs like `xenops-mode`[^fn:2], these previews render asynchronously as I continue typing.

There are many moving parts to this puzzle, but videos speak louder than words, so here is a quick demo.

{{< figure src="/ox-hugo/emacs-math-demo.gif" caption="<span class=\"figure-number\">Figure 1: </span>Sped up 30%" >}}

I could write more about how I achieved this setup, but others that are much smarter than me already have.

[^fn:1]: My [DOOM Emacs Config]({{< relref "doom-config" >}}) is typed in org-mode thanks to its literate programming abilities. In fact, this entire website is just one big org-mode file.
[^fn:2]: With org version 9.7, `xenops-mode` will be wholly unnecessary thanks to a new default `org-latex-preview` function. See a demo [here](https://www.youtube.com/watch?v=n-AfvuV-bYo&t=376s).
