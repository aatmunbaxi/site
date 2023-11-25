+++
title = "Quantum Computing, Braids, and Modular Categories"
author = ["Aatmun Baxi"]
tags = ["math", "categories"]
draft = false
weight = 2003
+++

## Introduction {#introduction}

Quantum computing (QC) is an exciting new paradigm of computation, promising exponential speedup from state-of-the-art classical algorithms in certain computing tasks.
Despite vigorous research from all sides of the task, obstacles to a quantum computer architecture that is scalable and tolerant to errors remain hard to overcome.

Qubits in existing models of quantum computation (such as trapped-ion models) are sensitive to their environment.
Such sensitivities of the environment make qubits liable to introducing noise that dominates the computation we&rsquo;d like to measure.
To ensure we can recover usable data, we&rsquo;d (approximately) need on the order of thousands of physical qubits per logical qubit to near-guarantee our computation is correct.
This would be no issue if current QC implementations were highly scalable, but they are not.
Topological quantum computing is a theoretical basis to address both these issues simultaneously.


## Anyons and their statistics {#anyons-and-their-statistics}

While traditional QC implementations use bosons and fermions for qubits, topological quantum computing (TQC) uses _anyons_.
Recall that two bosons exchange in a wave function causing a global phase shift \\( +1 \\) (i.e. \\( | \varphi\_1 \varphi\_2 \rangle = | \varphi\_2 \varphi\_1 \rangle\\)).
Conversely, fermions exchange with a global phase shift \\( -1 \\), so \\( | \varphi\_1 \varphi\_2 \rangle = - | \varphi\_2 \varphi\_1 \rangle \\).
Anyons are type of quasiparticle that allow for more general phase shifts \\( e^{i\theta} \\) for some value of \\( \theta \\).
We recover bosons and fermions at \\( \theta= 0 \\) and \\( \theta=\pi \\), respectively.
Anyons can take any statistics between Bose-Einstein statistics and Fermi-Dirac statistics and beyond, corresponding to values \\( 0 \leqslant \theta \leqslant 2\pi \\).

I&rsquo;ve somewhat lied by omission in the previous paragraph.
The global phase shifts corresponding to \\( e^{i\theta} \\) are actually relevant to the so-called _abelian anyons_.
One can generalize this phase shift further to an even larger class of unitary operators satisfying the [Yang-Baxter equation](https://en.wikipedia.org/wiki/Yang%E2%80%93Baxter_equation); such quasiparticles are _non-abelian anyons_.
These non-abelian anyons are the building blocks to encoding information in TQC.
The question remains--_what does this buy us?_
I&rsquo;ve already alluded to how TQC may be less sensitive to environmental noise than classical qubits, so we should understand how.

<a id="figure--anyonexchange"></a>

{{< figure src="/ox-hugo/anyon_exchange.svg" caption="<span class=\"figure-number\">Figure 1: </span>Inequivalent exchanges for identical anyons." width="150%" >}}

When thinking about how anyons interact with each other, we must think topologically.
That is, we can do everything up to a &ldquo;smooth/continuous&rdquo; deformation.
In one dimension of space, nothing interesting can happen: we cannot exchange anyons at all.
In three dimensions of space, everything is trivial.
If an anyon moves around another anyon back to its starting position, one can &ldquo;drag&rdquo; the path and shrink it simultaneously, so that topologically the path is indistinguishable from the particle not moving at all.

In two dimensions of space, things get more interesting.
In a flatland with two particles, there are two topologically distinct paths a particle can take in the plane.
They are illustrated in Figure [1](#figure--anyonexchange).
In fact, there are infinitely many: one can wind a particle around the other as many times as needed to produce a path distinct from the others.

<a id="figure--braids"></a>

{{< figure src="/ox-hugo/braids.png" caption="<span class=\"figure-number\">Figure 2: </span>Braids on n=3 strands traced out over time progression." width="150%" >}}

At each slice of time, the position of anyons in flatland correspond to points in a plane.
As we progress time, the trajectories of the anyons trace out paths akin to strands of string.
The strings cannot cross through each other, as this would mean the particles &ldquo;collide&rdquo; at some point in time.
Figure [2](#figure--braids) illustrates this phenomenon, with the arrow indicating the passage of time.

Mathematically, the collection of all these trajectories for some fixed \\( n \\) particles--disallowing the strands doubling back--form a group under composition: it is the **braid group on \\( n \\) strands**, denoted \\( \mathcal{B}\_n \\).
Here, the braid group is an instance of a more general kind of group called a **motion group**.
The mathematical formulation of this is to say that the braid group on \\( n \\) strands is the motion group of a disjoint union of points embedded in a compact box.
More general motion groups corresponding exist, such as motions of circles or trefoils in a compact box.
These are quite a bit more complicated, and we know little about them compared to the braid groups.

To each braiding we want to associate a unitary operator (read: a quantum gate), so we can say that mathematically, the statistics of \\( n \\) anyons correspond to certain unitary representations \\( \mathcal{B}\_n \to U(V) \\) of the braid group on \\( n \\) strands.
From a practical perspective, the act of applying a quantum gate to a system of anyons would correspond to braiding them.
This basis for computing is intuitively less sensitive to environmental noise in the sense that the topological properties of braids are more stable under small perturbation.
One can imagine that it is easier for a ball to bump into a wall from a slight gust of wind than to rearrange two crossed strings so that the lower string lies on top by cutting and rearranging.
This comparison is not unlike the difference between the sensitivity of qubits in the classical QC world and the TQC world.
Study of braid groups, motion groups, and the unitary representations of these groups shed light on the behavior of anyons, making it a subject of physical interest, and not just one of intrinsic beauty.


## Modular Categories {#modular-categories}

We switch gears a bit to discuss the other data of anyon systems, of which there is many.
The study of braids and their unitary representations will capture how the interactions of anyons in space alter the data of the global quantum system, but says nothing of the data internal to the particles, and how that data changes.

Anyons carry lots of intrinsic data.
One can formalize the physics of anyons along with certain symmetries and stabilities and collect them into a set of equations to solve for.
Kitaev (<a href="#citeproc_bib_item_3">Kitaev 2006</a> Appendix E) suggested that the algebraic combinatorics of the solutions to these equations can be formalized in the language of **unitary modular categories**.

The language of categories came about much earlier as a language of formalize analogous phenomena that occur in seemingly unrelated areas of mathematics.
Recently, categories enriched with extra structure have been studied due to their flexibility in encoding certain data.
Technically speaking, modular categories arose from the study of conformal field theories, which are a kind of quantum field theory that exhibit certain topological invariances.

The definition of a modular category is a bit complex; it is a ribbon fusion category with nondegenerate \\( S \\)-matrix.
The interested reader can refer to Figure [3](#org66df504) for a tree of what each of these terms imply.
Each property/structure is interesting in its own right.

{{< figure src="/ox-hugo/modular_tree.png" caption="<span class=\"figure-number\">Figure 3: </span>Definition of monoidal category and what each term means. Moving to the right means &ldquo;has this property&rdquo;." >}}

The reader might ask if the &ldquo;braided&rdquo; in Figure [3](#org66df504) is related to the braiding discussed for anyons.
Indeed it is, the braiding in a unitary modular category corresponds to braiding of anyons.
There is an comprehensive dictionary of the correspondence between unitary modular categories and the physical interpretation of what the data represent.
(A more detailed dictionary is found in <a href="#citeproc_bib_item_4">Rowell and Wang 2018</a> Table 1).

<style>
sane-table#table {
    margin-left: auto;
    margin-right: auto;

}
sane-table table {
  border-collapse: collapse;
  width: 100%;
  margin-left: auto;
  margin-right: auto;
}
.sane-table th,
.sane-table td {
  padding: 0.25rem;
  text-align: left;
  border: 1px solid #ccc;
  margin-left: auto;
  margin-right: auto;
}
</style>

<div class="ox-hugo-table sane-table">
<a id="table--anyon-dictionary"></a>
<div class="table-caption">
  <span class="table-number"><a href="#table--anyon-dictionary">Table 1</a>:</span>
  Synonyms from the worlds of unitary modular categories and anyon systems
</div>

| Unitary Modular Category | Anyon System     |
|--------------------------|------------------|
| Simple object            | Anyon            |
| Tensor product           | Fusion           |
| Dual object              | Antiparticle     |
| Evaluation               | Annihilation     |
| Coevaluation             | Creation         |
| Unit object              | Vacuum Sector    |
| Twist                    | Topological Spin |

</div>

Classifying unitary modular categories up to some notion of equivalence is therefore equivalent to classifying systems of anyons.
This is a difficult problem, but is thought to be possible, and positive signs have appeared.
It was shown in (<a href="#citeproc_bib_item_1">Bruillard et al. 2016</a>) that there are only finitely many modular categories for a given rank.
Physically, this would mean that a system \\( n \\) anyons can only interact nontrivially in finitely many distinct ways for each amount \\( n \\).

The ergonomics of modular categories can be challenging given the sheer number of equations to solve, but down-to-earth examples exist.
Perhaps the most well known is \\( \mathsf{Vec} \\), the category of finite dimensional vector spaces.
This is a rank \\( 1 \\) modular category with \\( S \\)-matrix \\( (1) \\) and \\( T \\)-matrix \\( (1) \\).
Ok, not very interesting...
This is indeed the most trivial example of a modular category, though more examples exist that are only slightly more difficult to understand.
For example, the category of metric groups \\( (G,q) \\) is equivalent to the category of pointed modular categories.
(See <a href="#citeproc_bib_item_2">Etingof et al. 2016, chap. 8</a>).

Despite the difficulty of classifying modular categories, this task enjoys deep connections to representation theory, quantum field theory, and condensed matter physics.


## Conclusion {#conclusion}

TQC is an exciting field gaining more and more traction as the years go by.
Experimental evidence for topological matter with gapped phases earned Thouless, Kosterlitz, and Haldane the Nobel Prize in Physics in 2016.
Microsoft has staked its entire quantum computing research division on the premise of TQC.
The study of modular categories is advancing at rapid pace, and we are learning more and more about representation theory and quantum algebra along the way.


## Bibliography {#bibliography}

<style>.csl-entry{text-indent: -1.5em; margin-left: 1.5em;}</style><div class="csl-bib-body">
  <div class="csl-entry"><a id="citeproc_bib_item_1"></a>Bruillard, Paul, Siu-Hung Ng, Eric C. Rowell, and Zhenghan Wang. 2016. “Rank-Finiteness for Modular Categories.” <i>Journal of the American Mathematical Society</i> 29 (3). American Mathematical Society: 857–81. <a href="https://www.jstor.org/stable/jamermathsoci.29.3.857">https://www.jstor.org/stable/jamermathsoci.29.3.857</a>.</div>
  <div class="csl-entry"><a id="citeproc_bib_item_2"></a>Etingof, Pavel, Shlomo Gelaki, Dmitri Nikshych, and Victor Ostrik. 2016. <i>Tensor Categories</i>. American Mathematical Soc. <a href="https://books.google.com?id=Z6XLDAAAQBAJ">https://books.google.com?id=Z6XLDAAAQBAJ</a>.</div>
  <div class="csl-entry"><a id="citeproc_bib_item_3"></a>Kitaev, Alexei. 2006. “Anyons in an Exactly Solved Model and beyond.” <i>Annals of Physics</i>, January Special Issue, 321 (1): 2–111. doi:<a href="https://doi.org/10.1016/j.aop.2005.10.005">10.1016/j.aop.2005.10.005</a>.</div>
  <div class="csl-entry"><a id="citeproc_bib_item_4"></a>Rowell, Eric, and Zhenghan Wang. 2018. “Mathematics of Topological Quantum Computing.” <i>Bulletin of the American Mathematical Society</i> 55 (2): 183–238. doi:<a href="https://doi.org/10.1090/bull/1605">10.1090/bull/1605</a>.</div>
</div>
