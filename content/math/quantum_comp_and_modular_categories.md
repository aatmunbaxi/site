+++
title = "Quantum Computing, Braids, and Modular Categories"
author = ["Aatmun Baxi"]
tags = ["math", "categories"]
draft = true
weight = 2003
+++

## Introduction {#introduction}

Quantum computing (QC) is an exciting new paradigm of computation, promising exponential speedup from state-of-the-art classical algorithms in certain computing tasks.
Despite vigorous research from all sides of the task, obstacles to a quantum computer architecture that is scalable and tolerant to errors inherent to the idiosyncrasies of the quantum world remain hard to overcome.

Qubits in existing models of quantum computation (such as trapped-ion models) are sensitive to their environment.
Such sensitivities of the environment make qubits liable to introducing noise that dominates the data we&rsquo;d like to actually measure.
To ensure we can recover usable data, we&rsquo;d (approximately) need on the order of thousands of physical qubits per logical qubit to near-guarantee our computation is correct.
This would be no issue if current QC implementations were highly scalable, but they are not.
Topological quantum computing is a theoretical basis to address both these issues simultaneously.


## Anyons and their statistics {#anyons-and-their-statistics}

While traditional QC implementations use bosons and fermions for qubits, topological quantum computing (TQC) uses _anyons_.
Recall that two bosons exchange in a wave function causing a global phase shift \\( +1 \\) (i.e. \\( | \varphi\_1 \varphi\_2 \rangle = | \varphi\_2 \varphi\_1 \rangle\\)).
Conversely, fermions exchange with a global phase shift \\( -1 \\), so \\( | \varphi\_2 \varphi\_2 \rangle = - | \varphi\_2 \varphi\_1 \rangle \\).
Anyons are type of quasiparticle that allow for more general phase shifts \\( e^{i\theta} \\) for some real value of \\( \theta \\).
We recover boson and fermions at \\( \theta= 0 \\) and \\( \theta=\pi \\), respectively.
Anyons can take any statistics between Bose-Einstein statistics and Fermi-Dirac statistics and beyond, corresponding to values \\( 0 \leqslant \theta \leqslant 2\pi \\).

I&rsquo;ve somewhat lied by omission in the previous paragraph.
The global phase shifts corresponding to \\( e^{i\theta} \\) are actually relevant to the so-called _abelian anyons_.
One can generalize this phase shift even further to an even larger class of unitary operators satisfying the [Yang-Baxter equation](https://en.wikipedia.org/wiki/Yang%E2%80%93Baxter_equation); such quasiparticles are _non-abelian anyons_.
These non-abelian anyons are the building blocks to encoding information in TQC.
The question remains--_what does this buy us?_
I&rsquo;ve already alluded to how TQC may be less sensitive to environmental noise than classical qubits, so we should understand how.

<a id="figure--anyonexchange"></a>

{{< figure src="/ox-hugo/anyon_exchange.svg" caption="<span class=\"figure-number\">Figure 1: </span>Inequivalent exchanges for identical anyons." width="150%" >}}

When thinking about how anyons interact with each other, we must think topologically.
That is, we can do everything up to a &ldquo;smooth/continuous&rdquo; deformation.
In one dimension of space, nothing interesting can happen: we cannot exchange anyons at all.
In two dimensions of space, things get more interesting.
In a flatland with two particles, there are two topologically distinct paths a particle can take in the plane.
They are illustrated in Figure [1](#figure--anyonexchange).

<a id="figure--braids"></a>

{{< figure src="/ox-hugo/braids.svg" caption="<span class=\"figure-number\">Figure 2: </span>Braids on \\( n=3 \\) strands traced out over time progression." width="150%" >}}

At each slice of time, the position of anyons in flatland correspond to points in a plane.
Therefore, as we progress time, the trajectories of the anyons trace out paths akin to strands of string.
The strings cannot cross through each other, as this would mean the particles &ldquo;collide&rdquo; at some point in time.
Figure [2](#figure--braids) illustrates this phenomenon, with the arrow indicating the passage of time.

Mathematically, the collection of all these trajectories for some fixed \\( n \\) particles--so long as we disallow the strands reversing direction--form a group under composition: it is the **braid group on \\( n \\) strands**.
Here, the braid group is an instance of a more general kind of group called a **motion group**.
The mathematical formulation of this is to say that the braid group on \\( n \\) strands is the motion group of a disjoint union of points embedded in a compact box.
More general motion groups corresponding to excitation states of anyons exist, such as motions of circles or trefoils in a compact box; these are quite a bit more complicated, and we know little about them compared to the braid groups.

We can say that mathematically, the statistics of \\( n \\) anyons correspond to certain representations of the braid group on \\( n \\) strands.
From a practical perspective, the act of applying a quantum gate to a system of anyons would correspond to braiding them.
This basis for computing is intuitively less sensitive to environmental noise in the sense that the topological properties of braids are more stable under small perturbation.
One can imagine that it is easier for a ball to bump into a wall from a slight gust of wind than to rearrange two crossed strings so that the lower string lies on top without undoing the cross.
This comparison is not unlike the difference between the sensitivity of qubits in the classical QC world and the TQC world.
Study of braid groups, motion groups, and the unitary representations of these groups shed light on the behavior of anyons, making it a subject of physical interest, and not just one of intrinsic beauty.


## Modular Categories {#modular-categories}

We switch gears a bit to discuss the other data of anyon systems, of which there are many.
The study of braids and their unitary representations will capture how anyons behave with respect to each other, but doesn&rsquo;t capture properties of the particles themselves.

<style>.csl-entry{text-indent: -1.5em; margin-left: 1.5em;}</style><div class="csl-bib-body">
</div>
