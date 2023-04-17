+++
title = "Favorite Theorems and Equations"
author = ["Aatmun Baxi"]
draft = false
weight = 2003
type = "post"
+++

<div class="ox-hugo-toc toc">

<div class="heading">Table of Contents</div>

- [The Yoneda Lemma](#the-yoneda-lemma)
- [The Tensor-Hom Adjunction](#the-tensor-hom-adjunction)
- [Classification of commutative Frobenius algbebras](#classification-of-commutative-frobenius-algbebras)

</div>
<!--endtoc-->

Here are some of my favorite theorems and equations.


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


## The Tensor-Hom Adjunction {#the-tensor-hom-adjunction}

Given an \\( (R,S) \\)-bimodule \\( X \\), a right \\( R \\)-module \\( Y \\), and left \\( S \\)-module \\( Z \\),
\\[
\operatorname{Hom}\_S( Y \otimes \_R X , Z ) \cong \operatorname{Hom}\_{R}( Y , \operatorname{Hom}\_{S}(X,Z))
\\]
as abelian groups.


## Classification of commutative Frobenius algbebras {#classification-of-commutative-frobenius-algbebras}

<details>
<summary>For a field \( k \), there is an equivalence of categories \( \mathsf{2TQFT}_k \simeq \mathsf{cFrob}_k \) of 2-dimensional topological quantum field theories and commutative Frobenius algebras.</summary>
<div class="details">

**Comments:** This was the first result I learned that expressed how some classical tensor algebras arise as categorical constructions. Essential to this equivalence is the classification of closed 1-dimensional manifolds and how well behaved the category \\( \mathsf{2Cob} \\) is. A significant amount of work is needed to even hypothesize a higher dimensional analogue. This is the cobordism hypothesis, proposed by Baez and Dolan.
</div>
</details>
