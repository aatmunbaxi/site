+++
title = "consult-second-brain"
author = ["Aatmun Baxi"]
date = 2024-07-17
tags = ["emacs", "shortread"]
draft = false
weight = 2006
series = ["Hacking emacs"]
section = "Computing"
+++

<div class="tldr">

consult's ability to provide multiple sources in a completing-read interface gives me a way to unify access and creation of org-roam nodes.
I've found this setup to minimize mental cache misses while benefiting from the easy-to-reach function consult-buffer.

</div>

<!--more-->

Okay, it's cliche to call it a "second brain", but this label evokes an expectation that your `org-roam` nodes should be as easy to access and modify as the thoughts you have in your head.
On a freshly set-up instance of `org-roam`, access of nodes probably looks like this:

1.  Call `org-roam-node-find`
2.  Type in name of node (possibly with completing-read)
3.  Select node

Unfortunately, this isn't good enough for me.
Don't ask me why, I actually can't really put my finger on it.
I've tried binding it to a short keybinding, but it still feels wrong to me.
For notes that are intended to be atomic and easily-reachable, having to call specific functions to create and refer to them doesn't cut it.
If it were up to me, I would list my `org-roam` notes under a command I use that answers my internal monologue when it asks _wait, what was this again?_.

Fortunately, the emacs cake may be had and eaten too.
I'd bet one of my most used commands is `consult-buffer`, which is incidentally what I use to satisfy the aforementioned _what is this?_ question.
After all, if everything is a buffer to emacs, why shouldn't my bite-sized notes be buffers?

This makes `consult-buffer`  a natural place to put all my `org-roam` notes under.
This can be accomplished thanks to `consult`'s ability to read from multiple sources and provide the results of all sources as completion candidates.
In fact, this is the default behavior of `consult-buffer`.
The variable `consult-buffer-sources` includes a list of all sources it pulls from for completion candidates.
All we have to do is add our own.


## The Code {#the-code}

Following the [plethora of examples on the consult wiki](https://github.com/minad/consult/wiki#consult-buffer-sources), the following code does just that.

```emacs-lisp
(defvar org-roam-nodes-source
         (list :name     "org-roam node"
               :category 'org-heading
               :face 'org-roam-title
               :narrow   ?n
               :require-match nil
               :action (lambda (cand)
                         ;; strip off nerd icon
                         (let ((node-name (substring-no-properties cand 2)))
                           (progn
                             (org-roam-node-open (org-roam-node-from-title-or-alias
                                                  node-name t))
                             (when (org-at-heading-p)
                               (org-fold-show-entry t)
                               (recenter-top-bottom 0))))

               :new (lambda (name)
                      (let ((info nil))
                        (setq info (plist-put info 'title name))
                        (org-roam-capture-  :goto nil
                                            :keys "h"
                                            :node (org-roam-node-create :title name)
                                            :templates org-roam-capture-templates
                                            :info info
                                            :props info)))
               :items (lambda ()
                        (mapcar
                         (lambda (str)
                           ;; requires nerd icons
                           (concat (nerd-icons-faicon "nf-fae-brain") " " str))
                         (org-roam--get-titles)))))


(add-to-list 'consult-buffer-sources 'org-roam-nodes-source 'append)
```

Now, access to my buffers _and_ `org-roam` nodes becomes a cinch.
I've also specified the ability to create new nodes if `consult` finds no match.
Note that I use top level headlines for my nodes, unlike the more popular one-file-per-node.

<div class="org">

<style>.org-center { margin-left: auto; margin-right: auto; text-align: center; }</style>

<div class="org-center">

<style>video {border: solid var(--border);}</style>

<video controls width="480"><source src="/videos/consult-second-brain.webm" type="video/webm"></video>

</div>

</div>

<style>details summary { font-size: 14px;justify-content:left;  }</style>

<style>details .details { font-size: 14px; justify-content: left;line-height:1.15rem }</style>

<details>
<summary><i>What just happened?</i></summary>
<div class="details">

**First demo**:

-   Call `consult-buffer`
-   Narrow to `org-roam` nodes by typing `n SPC`
-   Select completion candidate

**Second demo**:

-   Call `consult-buffer`
-   Select candidate with no match
-   `org-roam` capture as usual
</div>
</details>
