+++
title = "Some Theorems"
author = ["Aatmun Baxi"]
draft = false
weight = 2003
type = "post"
+++

<details>
<summary>Table of Contents</summary>
<div class="details">

<div class="ox-hugo-toc toc">

<div class="heading">Table of Contents</div>

- [Modular categories are not determined by their modular data](#modular-categories-are-not-determined-by-their-modular-data)
- [Classification of commutative Frobenius algebras by TQFTs](#classification-of-commutative-frobenius-algebras-by-tqfts)
- [The Yoneda Lemma](#the-yoneda-lemma)

</div>
<!--endtoc-->
</div>
</details>


## Modular categories are not determined by their modular data {#modular-categories-are-not-determined-by-their-modular-data}

<details>
<summary>A modular category \( \mathcal{C} \) is not determined by its \( S \) and \( T \) matrices.</summary>
<div class="details">

**Comments:** If something seems too good to be true, it probably is.
</div>
</details>


## Classification of commutative Frobenius algebras by TQFTs {#classification-of-commutative-frobenius-algebras-by-tqfts}

<details>
<summary>For a field \( k \), there is an equivalence of categories \( \mathsf{2TQFT}_k \simeq \mathsf{cFrob}_k \) of 2-dimensional topological quantum field theories and commutative Frobenius algebras.</summary>
<div class="details">

**Comments:** This was the first result I learned that expressed how some classical tensor algebras arise as categorical constructions. Essential to this equivalence is the classification of closed 1-dimensional manifolds and how well behaved the category \\( \mathsf{2Cob} \\) is. A significant amount of work is needed to even hypothesize a higher dimensional analogue. This is the cobordism hypothesis, proposed by Baez and Dolan.
</div>
</details>


## The Yoneda Lemma {#the-yoneda-lemma}

<details>
<summary>Let \( \mathsf{C} \) be a locally small category and \( F : \mathsf{C} \to \mathsf{Set} \) be a functor. Then
\[
\operatorname*{Hom}(\operatorname*{Hom}(X,-),F) \cong FX
\]
and this isomorphism is natural in both \(X\) and \(F\).</summary>
<div class="details">

**Comments:** This theorem is remarkable. The object on the left, as a collection of natural transformations, is seemingly incalculably large. Not only does this theorem tell us that this collection is a set, but it also gives an **explicit description** of these transformations, parameterized by \\( FX \\). When applied to \\( F = \operatorname\*{Hom}(Y,-) \\) (or more generally representable functors), this theorem gives meaning to the intuitively-known idea that an object is uniquely determined by the maps into (our out of) it.
</div>
</details>
