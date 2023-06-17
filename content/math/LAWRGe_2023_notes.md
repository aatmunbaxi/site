+++
title = "LAWRGe 2023 Notes"
author = ["Aatmun Baxi"]
tags = ["math", "conference", "notes"]
draft = false
weight = 2002
+++

<div class="ox-hugo-toc toc local">

- [Info](#info)
- [Lecture 1: Mon Jun 12 11:05:22 2023](#lecture-1-mon-jun-12-11-05-22-2023)
    - [Preliminaries](#preliminaries)
    - [Extending up/down](#extending-up-down)
    - [Boundary Conditions](#boundary-conditions)
- [Lecture 2: Mon Jun 12 13:04:01 2023](#lecture-2-mon-jun-12-13-04-01-2023)
    - [Summary of previous lecture](#summary-of-previous-lecture)
    - [2d Mirror Symmetry](#2d-mirror-symmetry)
    - [Gerstenhaber algebra](#gerstenhaber-algebra)
- [Lecture 3: Mon Jun 12 16:07:19 2023](#lecture-3-mon-jun-12-16-07-19-2023)
    - [Goals](#goals)
    - [Field Theories](#field-theories)
    - [Supersymmetric Field Theories](#supersymmetric-field-theories)
    - [Twisting](#twisting)
- [Lecture 4: Mon Jun 12 18:02:13 2023](#lecture-4-mon-jun-12-18-02-13-2023)
    - [Gauge Theories](#gauge-theories)
    - [Hyper-Kahler manifolds](#hyper-kahler-manifolds)
- [Lecture 5: Tue Jun 13 10:34:03 2023](#lecture-5-tue-jun-13-10-34-03-2023)
    - [The Gauged Gromov-Witten Invariant](#the-gauged-gromov-witten-invariant)
- [Lecture 6: Tue Jun 13 12:44:10 2023](#lecture-6-tue-jun-13-12-44-10-2023)
    - [Topological Interpretation](#topological-interpretation)
    - [Connection to SW Invariants](#connection-to-sw-invariants)
- [Lecture 7: Tue Jun 13 15:44:00 2023](#lecture-7-tue-jun-13-15-44-00-2023)
    - [Review](#review)
    - [Moment Maps](#moment-maps)
    - [Supersymmetry](#supersymmetry)
    - [Fields](#fields)
- [Lecture 8: Tue Jun 13 18:01:16 2023](#lecture-8-tue-jun-13-18-01-16-2023)
    - [Schematic SUSY](#schematic-susy)
    - [Twists](#twists)
    - [What happens when you twist](#what-happens-when-you-twist)
- [Lecture 9: Wed Jun 14 10:46:24 2023](#lecture-9-wed-jun-14-10-46-24-2023)
    - [Recall](#recall)
    - [2d \\( A \\)-model](#2d-a-model)
    - [2d \\( B \\)-model](#2d-b-model)
    - [3d \\( A \\)-model](#3d-a-model)
    - [Example of 3d Mirror Symmetry](#example-of-3d-mirror-symmetry)
- [Lecture 10: Wed Jun 14 12:48:31 2023](#lecture-10-wed-jun-14-12-48-31-2023)
    - [The Affine Grassmannian](#the-affine-grassmannian)
    - [BFN Coulomb Branch](#bfn-coulomb-branch)
- [Lecture 11: Wed Jun 14 15:55:18 2023](#lecture-11-wed-jun-14-15-55-18-2023)
    - [Moduli Space of vacua](#moduli-space-of-vacua)
    - [Quantum vacua](#quantum-vacua)
    - [Examples of Higgs and Coulomb Branches](#examples-of-higgs-and-coulomb-branches)
- [Lecture 12: Wed Jun 14 18:05:37 2023](#lecture-12-wed-jun-14-18-05-37-2023)
    - [\\( \Omega\\) -background and quantization](#omega-background-and-quantization)
    - [The \\( A \\)-model](#the-a-model)
- [Lecture 13: Thu Jun 15 10:46:45 2023](#lecture-13-thu-jun-15-10-46-45-2023)
    - [Review](#review)
    - [Computations in Dualities](#computations-in-dualities)
    - [Mirror Symmetry](#mirror-symmetry)
- [Lecture 14: Thu Jun 15 13:05:02 2023](#lecture-14-thu-jun-15-13-05-02-2023)
    - [Quantizing the \\( B \\)-twisted Higgs branch](#quantizing-the-b-twisted-higgs-branch)
- [Lecture 15: Fri Jun 16 10:44:46 2023](#lecture-15-fri-jun-16-10-44-46-2023)

</div>
<!--endtoc-->


## Info {#info}

Mirror symmetry and 3d topological quantum field theories (TQFTs)

[Workshop website](https://sites.google.com/view/lawrge2023/)


## Lecture 1: Mon Jun 12 11:05:22 2023 {#lecture-1-mon-jun-12-11-05-22-2023}


### Preliminaries {#preliminaries}

**TQFT:** One starts with a cobordism, a manifold with boundary with disjoint union decomposition \\( \partial W = M\sqcup N \\).
Usual notions of framed, oriented, unoriented, etc apply to the boundary decomposition.
We will mostly focus on _oriented_ TQFTs via oriented cobordisms.
This will be implicitly assumed for the rest of the talk.

You can glue cobordisms together along a common boundary component.
Taking this gluing as morphism composition, the collection of cobordisms (with equal dimension) form a category \\( \mathsf{Cob}\_{d,d-1}^{\mathrm{or}} \\), whose objects are closed oriented \\( d-1 \\)-manifolds, and morphisms are diffeomorphism classes of compact cobordisms between them.
Here, &ldquo;closed&rdquo; means compact without boundary.
We can write morphisms like \\( M \xrightarrow{W}N  \\).
_Note:_ Taking the reverse orientation of \\( W \\) gives the opposite arrow \\( N \to M \\).

Taking the disjoint union of two cobordisms acts as the tensor product in this category.
Placing \\( W,V \\) side-by-side in a diagram is the morphism \\( W \otimes  V \\).
This makes \\( \mathsf{Cob}\_{d,d-1}^{\mathrm{or}} \\) a symmetric monoidal category.

**Definition:** A \\( d \\)-dimensional TQFT is a symmetric monoidal functor \\( Z : \mathsf{Cob}\_{d,d-1}^{\mathrm{or}} \to \mathsf{Vect} \\).

The definition expresses a lot of things, such as \\( Z( M\_1\sqcup M\_2) \cong Z(M\_1) \otimes Z(M\_2) \\).

**Remarks**

-   Given \\( M \\) a closed, oriented \\( d-1 \\)-manifold, the value \\( Z(M) \\) is a vector space called the _vector space of states of the field theory_.
    Elements of the vector space are the states of the field theory.
    -   Given \\( M \\) a closed \\( d \\)-manifold, can think of \\( M \in \operatorname\*{Hom}(\emptyset , \emptyset) \\) as a cobordism of the empty manifolds.
        Under \\( Z \\), \\( Z(M) \in \operatorname\*{Hom}( \mathbf{C} , \mathbf{C}) \\) a linear map, which is just a number called the _partition function_.
-   Given \\( M \xrightarrow{W}  N \\), \\( Z(W) : Z(M) \to Z(N) \\) corresponds to a _transition amplitude_, or \\( S \\)-matrix.


### Extending up/down {#extending-up-down}

Suppose we start with some closed \\( d \\)-manifold \\( M \\) which is a union of two manifolds with boundary \\( \Sigma \\).
The partition function \\( Z(M) \\) is a composite of morphisms \\( \mathbf{C} \to Z( \Sigma) \to \mathbf{C} \\).
This is the _locality principle_ of (T)QFTs.
The question is, _can we cut \\( \Sigma \\) in a certain way to determine \\( Z(\Sigma) \\)?_

**Definition:** Let \\(\mathsf{Cob}\_{d,d-1,d-2}^{\mathrm{or}}\\) be a symmetric monoidal 2-category whose objects are \\( d-2 \\)-manifolds, and \\( 1 \\)-morphisms are \\( d-1 \\)-dimensional cobordisms, and whose \\( 2 \\)-morphisms are diffeomorphism classes of cobordisms between the cobordisms.
The 2-morphisms are manifolds with corners.

-   TODO pair of pants drawing

You can repeat this extension process all the way down to \\( 0 \\)-manifolds at the bottom: a \\( d \\)-category \\( \mathsf{Cob}\_d^{\mathrm{or}} \\) whose \\( k \\)-morphisms are diffeomorphisms classes of cobordisms between the \\( k-1 \\)-morphisms.

The above is the process of **extending down**.

Extending up is the process of removing the reliance on &ldquo;diffeomorphism classes of&rdquo; from the previous definition.

Let \\( \mathsf{Bord}\_d^{\mathrm{or}} \\) be an \\( (\infty,d) \\)-category.
This is a category with \\( k < d \\)-morphisms the same as in \\( \mathsf{Cob}\_d^{\mathrm{or}} \\), and \\( d \\)-morphisms are done by \\( d \\)-dimensional iterated cobordisms

**Definition:** A _fully extended \\(d  \\)-dimensional TQFT_ is a symmetric monoidal functor \\( \mathsf{Bord}\_d^{\mathrm{or}} \to \mathcal{C} \\) where \\( \mathcal{C} \\) is a symmetric monoidal \\( (\infty,d) \\)-category.

In the case \\( Z : \mathsf{Cob}\_d^{\mathrm{or}} \to \mathcal{C} \\), if we take some \\( M \\) a \\( d-1 \\)-manifold, there is a natural cobordism from \\( M \to M \\) by diffeomorphisms, so there is now an action of \\( \operatorname{MCG}(M) \\) the mapping class group of \\( M \\) on the vector space \\( Z(M) \\).
In physics, the vector space \\( Z(M) \\) is often not a vector space, but a chain complex in \\( \mathsf{Ch} \\).
Since chain complexes have a notion of homotopy, we can view \\( \mathsf{Ch} \\) as an \\( \infty \\)-category.
The mapping class group action \\( \operatorname{MCG}(M) \\) is an action on \\( H^{\bullet}(Z(M)) \\).
Then \\( C\_{\bullet}(\operatorname{Diff}(M)) \\) as an algebra under the Pontjagin product acts in \\( Z(M) \\).
There will be an exercise about what happens on \\( M = S^1 \\), where \\( \operatorname{Diff}^{\mathrm{or}}(S^1) \sim S^1 \\), and \\( \operatorname{MCG}(S^1) = \star \\) a point.


### Boundary Conditions {#boundary-conditions}

Given a TQFT \\( Z : \mathsf{Cob}\_{d,d-1}^{\mathrm{or}} \to \mathsf{Vec} \\) and \\( M \\) a \\( d \\)-manifold with boundary.
A map \\( Z(M) : Z( \partial M) \to \mathbf{C} \\) induces a boundary condition \\( Z^{\partial} \\) to \\( Z \\).
In particular, what this gives you is a vector \\( Z^{\partial}( \partial M) \in Z( \partial M) \\).

Given such a boundary condition, we can construct \\( Z(M , Z^{\partial} \text{ at } \partial M)\in \mathbf{C} \\) as a composite \\( \mathbf{C} \xrightarrow{Z^{\partial}(\partial M)} Z(\partial M) \xrightarrow{ Z(\partial M)} \mathbf{C}  \\).

As an example, consider \\( Z : \mathsf{Cob}\_{2}^{\mathrm{or}} \to \mathcal{C} \\), for every closed \\( 1 \\)-manifold \\( N \\), \\( Z^{\partial}(N) \in Z(N) \\).
We can consider \\( Z^{\partial}(\text{point}) : \mathbf{1} \to Z(\text{point}) \\), where we can take \\( \operatorname\*{Hom}(\mathbf{1}, Z(\text{point})) \\) as a category since \\( \mathcal{C} \\) is a \\( 2 \\)-category.
This category contains all boundary conditions, as we are just picking out a vector.


## Lecture 2: Mon Jun 12 13:04:01 2023 {#lecture-2-mon-jun-12-13-04-01-2023}


### Summary of previous lecture {#summary-of-previous-lecture}

As a reminder, a TQFT is some kind of functor \\( Z : \mathsf{Bord}\_d^{\mathrm{or}} \to \mathcal{C} \\) which may or may not have some more enriched structure.

_Mirror symmetry_ is an equivalence of two kinds of TQFTs.


### 2d Mirror Symmetry {#2d-mirror-symmetry}

Given a symplectic manifold \\( M \\), there is a 2d TQFT \\( Z\_{2dA,M} : \mathsf{Bord}^{\mathrm{or}}\_2 \to \mathsf{Cat} \\), called the **2-dimensional \\( A \\)-model**.
Given a complex algebraic manifold \\( M \\), there is another 2d TQFT \\( Z\_{2dB, M} : \mathsf{Bord}\_2^{\mathrm{or}} \to \mathsf{Cat} \\) called the **2-dimensional \\( B \\)-model**.
When evaluating the \\( A \\)-model at a point, you get a Fukaya category of \\( M \\), and evaluating it at a circle, you get quantum/symplectic cohomology of \\( M \\)

In the complex algebraic case, the evaluation at a point is some derived category \\( D^b \mathsf{Coh}(M)  \\) of coherent sheaves on \\( M \\), and for the circle, some version \\( \bigoplus\_{p,q}^{} H^p(M , \Omega ^q) \\) of Hodge cohomology.
Mirror symmetry is an equivalence \\( Z\_{2dA,M} \cong Z\_{2dB,M} \\).
3d mirror symmetry says something analogous.


### Gerstenhaber algebra {#gerstenhaber-algebra}


#### Local operators {#local-operators}

Recall we wanted to make sense of the partition function with some boundary condition, or a _defect_.
Currently, let the defect on the boundary be just a point.
We want to say something akin to &ldquo;everything away from the defect is the original TQFT, but at the defect it could be different&rdquo;.

**Definition:** A _local operator_ in a TQFT \\( Z \\) is a vector \\( \mathcal{O} \in Z(S^{d-1}) \\).

Let \\( M \\) be a \\( d \\)-manifold, and consider a knot \\( K \\) inside \\( M \\).
The boundary of a neighborhood of the knot is \\( S^{d-2} \times S^1 \\), with neighborhood \\( S^{d-2} \times D \\).
\\( Z(M\_{\mathcal{O}}) \\) is a composite

\begin{equation\*}
 \mathbf{C} \xrightarrow{Z(\ldots\_{K})}  Z(S^{d-2} \times S^1) \xrightarrow{Z(\text{complement})} \mathbf{C}  .
\end{equation\*}

**Definition:** The vector space of line operators is \\( Z(S^{d-2} \times  S^1) \\).
Imagine now we cut \\( M \\) along some submanifold \\( N \\) so it cuts the knot \\( K \\).
The &ldquo;local line operators&rdquo; give objects of \\( Z(S^{d-2}) \\).
To a \\( d-1 \\)-manifold \\( N \\) with defect points \\( \mathcal{O} \in N \\), \\( Z(N) \\) is a composite of

\begin{equation\*}
\mathbf{C} \xrightarrow{Z(\ldots\_{\mathcal{O}})}  Z(S^{d-2}) \xrightarrow{Z(N \setminus pt)}  \mathbf{C}.
\end{equation\*}

The upshot is that line operators give objects of \\( Z(S^{d-2}) \\) as a category of line operators.

We can introduce algebraic structures on \\( Z(S^{d-1}), Z(S^{d-2}) \\).

Say \\( D \\) is a \\( d \\)-dimensional ball, and consider an embedding of \\( D^{\sqcup k} \hookrightarrow D \\).
To this embedding we can create a cobordism \\( D: (S^{d-1})^{\sqcup k} \to S^{d-1} \\).
Thus, if we have a TQFT, from this induced cobordism, we get an operation \\( Z(S^{d-2})^{\otimes  k} \to Z(S^{d-1}) \\) taking \\( k \\) local operators and giving a local operator
Now consider the space of embeddings of \\( k \\)-balls \\( \mathbb{E}^{\mathrm{f r}}\_d (k) \\) given the Whitney topology.
If \\( Z(S^{d-1}) \\) is a chain complex, then we get \\( C\_{\bullet}( \mathbb{E}^{\mathrm{ f r}}\_d(k))  \otimes  Z(S^{d-1})^{\otimes  k} \to Z(S^{d-1}) \\)
Then we get a co

\begin{equation}
\label{eq:1}
 \mathbb{E}^{\mathrm{f r}}\_d (k) \to \operatorname\*{Hom}((S^{d-1})^{\otimes k} , S^{d-1}) \xrightarrow{Z} \operatorname{Hom}\_{\mathsf{Ch}}(Z(S^d-1)^{\otimes k} , Z(S^{d-1}))
\end{equation}

Now suppose we nest balls inside the embedded balls.
We can combine all nested operations into \\(  \mathbb{E}^{\mathrm{f r}}\_d \\) as an _operad_.
The upshot is that \\( Z(S^{d-1}) \\) is an \\(  \mathbb{E}^{\mathrm{f r}}\_d \\)-algebra.

Let&rsquo;s introduce \\( \mathbb{E}\_d(k) = \operatorname{Emb}^{\mathrm{f r}}(D^{\sqcup k} , D) \\) the collection of framed embeddings.

**Proposition:** \\( \mathbb{E}\_d(k) \simeq \operatorname{Conf}\_k( \mathbf{R}^d) \\) the space of configurations of \\( k \\) points in \\( \mathbf{R}^d \\).
For the framed version \\(  \mathbb{E}^{\mathrm{f r}}\_d (k)  \simeq \operatorname{SO}(d) ^k \times \mathbb{E}\_d(k) \\).

What now is \\( H\_{\bullet}( \mathbb{E}\_d(k) ^{\mathrm{f r}} , \mathbf{C}) \\)?

**Definition:** a \\( \mathbb{P}\_d \\)-algebra is a chain complex \\( A \\) with graded commutative multiplication \\( m : A \otimes  A \to A \\) and a Lie bracket \\( \left\\{ -,- \right\\}: A \otimes  A \to A[1-d] \\) satisfying the super-Leibniz rule.

Such algebras have an operad.
Let \\( \mathbb{P}\_d(k) \\) be the graded vector space of \\( k \\)-ary operations on a \\( \mathbb{P}\_d \\)-algebra.
For example, \\( \mathbb{P}\_d(1) = \mathbf{C} \\), and \\( \mathbb{P}\_d(2) = \mathbf{C}m \oplus \mathbf{C}\left\\{ -,- \right\\}[d-1] \\).
A \\( \mathbb{P}\_2 \\)-algebra is called a _dg Poisson algebra_.

**Proposition:** \\( H\_{\bullet}(  \mathbb{E}^{\mathrm{f r}}\_d (k) ) \cong \mathbb{P}\_d(k) \\) for \\( d \geqslant 2 \\).

The upshot is that if we start with some \\( d \\)-TQFT valued in chain complexes, and look at \\( H^{\bullet}(Z(S^{d-1}) )\\), then this is a \\( \mathbb{P}\_d \\)-algebra.

The example we will be interested in is \\( d=3 \\).
We have \\( H^{\bullet}(Z(S^2))  \\) is a \\( \mathbb{P}\_3 \\)-algebra with Poisson bracket of degree \\( -2 \\).
Recall that \\( Z(S^{d-2}) \\) is the category of line operators.
This carries an \\(  \mathbb{E}^{\mathrm{f r}}\_{d-1}   \\)-algebra structure.

-   \\( \mathbb{E}\_2 \\)-category is a braided monoidal category
-   \\( \mathbb{E}\_1 \\)-category is a monoidal category
-   \\( \mathbb{E}\_2^{\mathrm{f r}} \\) is called a balanced monoidal category

In the case \\( d=3 \\), \\( Z(S^1) \\) the category of line operators is a braided, balanced monoidal category.


## Lecture 3: Mon Jun 12 16:07:19 2023 {#lecture-3-mon-jun-12-16-07-19-2023}


### Goals {#goals}

-   What is supersymmetry?
-   What is a topological twist?


### Field Theories {#field-theories}

Regardless of how we model a (quantum) field theory, it will always involve dg (differential graded) linear algebra.
i.e. some \\( \mathcal{E} \\) a graded vector space over \\( \mathbf{C} \\) and differential \\( d \\) (satisfying \\( d ^2=0\\) of degree \\( 1 \\))
\\( \mathcal{E} \\) could model:

-   quantum states
-   classical/quantum observables
-   classical fields

If \\( \mathfrak{g} \\) is a Lie algebra, we can equip \\( \mathcal{E} \\) with a \\( \mathfrak{g} \\)-action.

**Example**

-   Field theory on \\( \mathbf{R}^4 \\), where \\(  \mathfrak{g}  = \mathfrak{so}(1,3) \\) (the Lorentz algebra)
-   Poincare algebra \\( \mathfrak{so}(1,3) \rtimes \mathbf{R}^4 \\)
-   Complexified \\(  \mathfrak{g} \_{\mathbf{C}} = \mathfrak{so}(k, \mathbf{C}) \rtimes \mathbf{C}^4 \\), in case we don&rsquo;t want to worry about choice of signature


### Supersymmetric Field Theories {#supersymmetric-field-theories}

Let \\( n \\) be a dimension.
**Defintion:** A field theory \\(( \mathcal{E}, d )\\) is an \\( n \\)-dimensional _supersymmetric_ theory with supersymmetry \\( \Sigma \\) if \\( \mathcal{E} \\) carries an additional \\( \mathbf{Z}/2 \\)-grading (so in total \\( \mathbf{Z} \times \mathbf{Z}/2 \\)-graded) and \\( d \\) has bidegree \\( (1,0) \\) wrt this grading and \\( (\mathcal{E},d) \\) has an \\( \mathfrak{is o}(n , \mathbf{C})  = \mathfrak{so}(n, \mathbf{C}) \rtimes \mathbf{C}^n \\) action that lifts to an action of the **super Poincare algebra** associated to \\( \Sigma \\).
Some things to unpack here.

**Definition:** A _super-Lie algebra_ \\(  \mathfrak{g}  \\) is a \\( \mathbf{Z}/2 \\)-graded vector space with Lie bracket \\( [-,-] \\) such that it is graded-antisymmetric: \\( [x,y] = (-1)^{|x||y| +1}[y,x] \\), and satisfies the graded Jacobi identity.

\\( \Sigma \\) is a **spinorial representation** of \\( \mathfrak{so}(n, \mathbf{C}) \\), which means it is a finite sum of spin or semi-spin representations.

A **super-Poincare algebra** is a super-Lie algebra with underlying graded vector space \\( \mathfrak{iso}(n , \mathbf{C}) \\) as its even part, and \\( \Sigma \\) as its odd part.
i.e. \\( \mathfrak{iso}(n , \mathbf{C}) \oplus \Pi \Sigma \\).
This algebra is equipped with the brackets

-   usual bracket on the even part
-   action of \\(\mathfrak{so}(n)\\) on \\( \Sigma \\) in between the direct summands
-   \\( \mathfrak{so}(n , \mathbf{C}) \\)-equivariant map \\( \Gamma : \operatorname{Sym}^2 \Sigma \to \mathbf{C}^n \\) which is nondegenerate

| even                                                  | odd               |
|-------------------------------------------------------|-------------------|
| \\( \mathfrak{iso}(n, \mathbf{C}) \\)                 | \\( \Sigma \\)    |
| \\( \mathfrak{so}(n, \mathbf{C})\oplus \mathbf{C} \\) | \\( \Sigma \\)    |
| rotations + translations                              | supertranslations |

For example in \\( n=3 \\), \\( \mathfrak{so}(3, \mathbf{C}) \cong \mathfrak{sl}(2, \mathbf{C}) \\), and \\( \operatorname{Spin}(3, \mathbf{C})\cong \operatorname{Sl}(2, \mathbf{C}) \\).
The spinorial representations look like \\( S \otimes W = S^{\mathcal{N}} \\), where \\( W \\) is isomorphic to \\( \mathbf{C}^{ \mathcal{N}} \\).
Then \\( \Gamma : \operatorname{Sym}^2(S \otimes W) \to \mathbf{C}^3 \cong \operatorname{Sym}^2 S \\).
But then the domain is \\( \cong \operatorname{Sym}^2( \mathbf{C}) \otimes  \operatorname{Sym}^2(W) \\).
Thus \\( \Gamma \\) is given by specifying a linear map \\( g: \operatorname{Sym}^2(W) \to \mathbf{C} \\) that is nondegenerate, i.e. an inner product on \\( W \\).
The super Poincare algebra given by \\( (W,g) \\) is called the \\( \mathcal{N} = \dim W  \\) 3d complex super Poincare algebra.

The group \\( O(W) \\) (the orthogonal group) acts on the super Poincare algebra.
These are called \\( R \\)-symmetries.


### Twisting {#twisting}

Let \\( ( \mathcal{E},d) \\) be acted on by \\( \mathfrak{siso}(n  | \Sigma) \\), the super Poincare algebra with odd part \\( \Sigma \\).
Let \\( Q \in \Sigma \\), and suppose \\( \left[ Q,Q \right]=0 \\).
Let \\( \alpha(Q)\in \operatorname{End}( \mathcal{E}) \\) be the action of \\( Q \\).

**Definition:** The _twist_ of \\( ( \mathcal{E},d) \\) of \\( Q \\), \\(( \mathcal{E},d + \alpha(Q) ) = (\mathcal{E}\_Q , d\_Q) \\).
Recall the degree of \\( d \\) was originally \\( (1,0) \\), while the degree of \\( \alpha(Q) \\) is \\( (0,1) \\).
So the new differential \\( d\_Q \\) is only naturally \\( \mathbf{Z}/2 \\)-graded, and not _a priori_ \\( \mathbf{Z} \\)-graded.

Suppose that \\( \mathcal{E} \\) itself carries a Lie bracket and the action of \\( \mathfrak{siso}(n | \Sigma) \\) as an inner action, i.e. we have a homomorphism \\( H:\mathfrak{siso}(n | \Sigma) \to \mathcal{E} \\), with the action of \\( X\in \mathfrak{siso}(n | \Sigma) \\) by \\( [H(X) , -] \\).
Also suppose that \\( X \\) is \\( Q \\)-exact in \\(  \mathfrak{siso}(n | \Sigma)  \\), i.e. \\( X = [Q,Q'] \\) for some \\( Q' \\).
Then \\( H(X) = H([Q,Q']) = [H(Q), H(Q')]  \\) is exact wrt \\( d\_Q = d+[H(Q),-] \\).
Thus it vanishes in \\( d\_Q \\)-cohomology.

**Definition:** If the map \\( [Q, -] : \Sigma \to \mathbf{C}^{n}  \\) is surjective, we say that \\( Q \\) is _topological_.
The twisted theory \\( ( \mathcal{E}\_Q , d\_Q) \\) is called a _topological twist_.

**Defintion:** \\( \operatorname{Nilp} = \left\\{ Q \in \Sigma\mid [Q,Q]=0 \right\\} \\).
and \\( \mathbb{P} \operatorname{Nilp} = (\operatorname{Nilp} \setminus 0 )/ \mathbf{C}^{\times } \\).


## Lecture 4: Mon Jun 12 18:02:13 2023 {#lecture-4-mon-jun-12-18-02-13-2023}


### Gauge Theories {#gauge-theories}

3d \\( \mathcal{N}=4 \\) theories became a subject of interest in the mid 90s as a dimension reduction of 4d \\( \mathcal{N}=2 \\) theories.
Something that came up from this study was the discovery of 3d mirror symmetry.

A large family of quantum field theories are the non-linear gauged sigma models labeled by \\( (G, \mathcal{X}) \\) where \\( G \\) is a compact Lie group and \\( \mathcal{X} \\) is a smooth hyper-Kahler manifold with \\( G \\) action.
(Sigma model here is just a general term involving a theory of maps into manifolds).

Some specializations of this concept:

1.  A non-gauged sigma model has \\( G =1 \\) and \\( \mathcal{X} \\) hyper-Kahler.
    The \\( B \\) twist is called Rozansky-Witten theory
2.  Let \\( \mathcal{X} \\) be a point and any \\( G \\). This is pure 3d \\( \mathcal{N}=4 \\) gauge theory
3.  Linear gauge theory, or a linear gauged sigma model (GLSNs)
    We take here \\( \mathcal{X} \\) to be a hyper-Kahler (quaternionic) vector space.
    The action of \\( G \\) then means \\( \mathcal{X} \\) is a quaternionic representation of \\( G \\).
    This is what people mean when they say 3d \\( \mathcal{N}=4 \\) gauge theories
    1.  Cotangent matter, so \\( W = T^{\*}V = V \oplus V^{\*} \\) where \\( V \\) is a complex representation of \\( G \\)


### Hyper-Kahler manifolds {#hyper-kahler-manifolds}

**Definition:** A _hyper-Kahler manifold_ \\( \mathcal{X} \\) is a Riemannian manifold \\(( \mathcal{X}, \gamma )\\) with a \\( \mathbb{P}^1 \\) of Kahler structures compatible with \\( \gamma \\).
Typically, \\( \mathbb{P}^1 \\) is viewed as a the sphere in \\( \mathbf{R}^3 \\) with basis \\( I,J,K \\).
The associated complex structures associated to \\( I,J,K \\) satisfy \\( I^2 = J^2 = K^2 = -1 = IJK \\), which also implies \\( IJ = K \\) and \\( JK = I \\), and \\( KI = J \\).
All of this implies that a generic \\( \alpha I + \beta J + \gamma K \\) where \\( \sqrt{\alpha^{2} + \beta ^2 + \gamma ^2}=1 \\) is also \\( -1 \\).
There are also associated Kahler forms \\( \omega\_I , \omega\_J, \omega\_K \\), which are real nondegenerate two forms of Hodge type \\( (1,1) \\) in its complex structure such that the metric \\( \gamma(v,w) = \omega\_I(v,Iw)  \\), same for \\( J,K \\).

ALso, \\( \Omega\_I = \omega\_J + i\omega\_K \\) has type \\( (2,0) \\) in \\( I \\) and a nondegenerate holomorphic \\( 2 \\)-form.
i.e. a holomorphic symplectic form.
Similarly, \\( \Omega\_J, \Omega\_K \\) are holomorphic symplectic forms cyclically permuting \\( I,J,K \\).
Oftentimes we fix one such complex structure \\( I \\) with data \\( \gamma , I , \omega\_I , \Omega\_I \\).
Algebraic geometers choose a complex structure and throw everything else out.
In particular, there is no information about the metric.

**Definition:** A hyper-Kahler vector space is a vector space with real dimension divisible by \\( 4 \\) (equiv. complex dim divisible by \\( 2 \\)).
The main example is \\( W = \mathbf{C}^{2n} \\).
In a fixed complex structure \\( I \\), there are holomorphic coordinates \\( (X^{1}, \ldots , X^{n}, Y\_{1} , \ldots , Y\_{n}) \\).
In these coordinate the metric is

\begin{equation}
\label{eq:2}
\gamma = \sum\_{i=1}^n(|dX^i|^2 + |dY\_i|^2) = |d \vec{X}|^2 + |d \vec{Y}|^2.
\end{equation}

The Kahler metric is

\begin{equation}
\label{eq:3}
\omega\_I = \frac{i}{2}( d \vec{X} \wedge d \overline{\vec{X}} + d \vec{Y} \wedge d \overline{\vec{Y}}).
\end{equation}

We note we can view the metric as a Hermitian inner product on \\( \mathbf{C}^{2n} \\) and \\( \Omega \\) as a symplectic form on \\( \mathbf{C}^{2n} \\).
Then

\begin{multline\*}
\operatorname{Aut}(W; I , \gamma, \Omega) = \left\\{ g\in \operatorname{GL}(2n, \mathbf{C})\mid g^{\dagger} \gamma g = \gamma, g^T\Omega g = \Omega \right\\} = U(2n)\cap \operatorname{Sp}(2n , \mathbf{C})\\\ = U \operatorname{Sp}(n).
\end{multline\*}

Then a linear 3d \\( \mathcal{N}=4 \\) gauge theory is \\( G , W \cong \mathbf{C}^{2n} \\), and \\( \rho : G \to U \operatorname{Sp}(n) \\).


## Lecture 5: Tue Jun 13 10:34:03 2023 {#lecture-5-tue-jun-13-10-34-03-2023}

Idea: if you have a compact Lie group \\( G \\) and a quaternionic representation \\( W \\) of \\( G \\), you can associate 3d TQFTs \\( Z\_{3dA}, Z\_{3dB} \\).
If \\( (G,W) \\) is a 3d mirror to another pair \\( (G', W') \\), then the \\( A \\) model \\(  Z\_{3dA} \\) and \\( B \\) model \\( Z\_{3dB'} \\) will be equivalent

<style>.org-center { margin-left: auto; margin-right: auto; text-align: center; }</style>

<div class="org-center">

**Question**: If \\( N \\) is a closed oriented \\( 3 \\)-manifold, what is \\(  Z\_{3dA, W // G}(N)\in \mathbf{C} \\)?

</div>


### The Gauged Gromov-Witten Invariant {#the-gauged-gromov-witten-invariant}


#### 2d Case {#2d-case}

To start with an easier case, consider a 2d \\( A \\)-model:
Let \\( G \\) be a compact Lie group with an identification \\( \mathfrak{g} \cong  g^{\*}\\), and \\( W  \\) a unitary \\( G \\)-representation (or more generally, \\( W \\) is some hyper-Kahler manifold with a \\( G \\)-action).
Recall a unitary \\( G \\)-representation is a Hermitian vector space \\( W \\) and a homomorphism \\( G \to U(W) \\).
Then \\( Z\_{3dA, W/ /G} \\) is a 2d TQFT, with \\(  Z\_{3dA, W //G} ( \Sigma) \\) the _gauged Gromov-Witten invariant of \\( W \\)_, which counts the solutions of some PDE on \\( \Sigma \\), where \\( \Sigma \\) is some Riemann surface.
(The vanilla GW invariant is when \\( G \\) is trivial.)
The PDE in question is called the symplectic vortex equation.
Before we write it, let&rsquo;s introduce a function

\begin{align\*}
\label{eq:4}
\mu : W &\to \mathfrak{g}^{\*} \\\\
v &\mapsto \frac{1}{2}(xv,v)
\end{align\*}

where \\( x\in \mathfrak{g} \\).
\\( \mu \\) is a **moment map** for the \\( G \\)-action on \\( W \\).

Choose a principal \\( G \\)-bundle \\( P \to \Sigma \\) and a connection \\( \nabla \\) a connection on \\( P \\), and a smooth section \\( \varphi \in \Gamma(\Sigma , P^G \times W) \\).
The symplectic vortex equations are

\begin{align\*}
\overline{\partial} \varphi &= 0\in \Omega^{0,1}(\Sigma , P^G \times  W) \\\\
\star F + \mu(\varphi) &= 0 \in \Omega^0(\Sigma, \operatorname{ad} P).
\end{align\*}

We recall \\( \operatorname{ad}P = P^G \times \mathfrak{g}\to \Sigma \\) is the adjoint bundle.
The gauged GW invariant counts solutions of these equations modulo gauge transformations.
The dimension of the moduli space of solutions is

\begin{equation}
\label{eq:5}
d = (2-2g)(\dim\_{\mathbf{C}} W - \dim  G) + 2\deg (P^G \times W).
\end{equation}

**Remarks**

-   If \\( G \\) is trivial, no principal bundle is here, the second equation drops out, and the first equation just says that \\( \varphi \\) is a holomorphic map.
-   If \\( W=0 \\), then the first equation drops out, and the second equation just says that the curvature of the connection is zero.
    In general there are few flat connections compared to non-flat connections, but it is a theorem in this case that the moduli space of flat \\( G \\)-bundles is identified with the space of holomorphic \\( G\_{\mathbf{C}} \\)-bundles, the complexified \\( G \\)-bundles


#### 3d case {#3d-case}

Again in the 3d case there will be a pair of equations.
Fix again a compact Lie group \\( G \\), \\( W \\) a quaternionic representation of \\( G \\).
Recall that \\( \operatorname{Spin}(3) = \operatorname{SU}(2) \to \operatorname{SO}(3) \\), which can be thought of as unit quaternions.
If we have \\( u\in \mathbb{H} \\) a unit quaternion, it acts on \\( \operatorname{Im} \mathbb{H}  = \mathbf{R}^3 \\) by conjugation.
Let \\( \mathcal{S} = \mathbb{H} \\) the spin representation by action \\( u\cdot x = ux \\).
There is a natural map \\( c : V \otimes \mathcal{S}\to \mathcal{S} \\) where \\( V = \im \mathbb{H} \\).
This is a map of \\( \operatorname{Spin}(3) \\)-representations called _Clifford multiplication_.

A _spin structure_ on a Riemannian \\( 3 \\)-manifold \\( N \\) is a \\( \operatorname{Spin}(3) \\)-bundle \\( P \to N \\) with an identification \\( P^{\operatorname{Spin}(3)} \times V\cong TN \\) compatible with metrics.
The spinor bundle associated to the spinor representation is \\( \mathcal{S}\_N = P^{\operatorname{Spin}(3)} \times \mathcal{S} \\).
Spin structures are obstructed by \\( w\_2(N) \in H^2(N, \mathbf{Z} /2) \\) the second Stiefel-Whitney class.
The set of spin structures has a free transitive action by \\( H^1(N , \mathbf{Z}/2) \\).

<!--list-separator-->

-  Dirac operator

    Let \\( \varphi\in \Gamma(N , \mathcal{S}\_n \otimes \_{\mathbb{H}} W) \\).
    Given the Riemannian structure, we have the LV connection on \\( P \\), so we can take the covariant derivative \\( \nabla \varphi \in \Omega^1(W, \mathcal{S}\_N \otimes \_{\mathbb{H}} W ) \\).
    We can rewrite the latter collection as \\( \Gamma(N, P^{\operatorname{Spin}(3)} \times  (V \otimes \_{\mathbb{H}} \mathcal{S} \otimes \_{\mathbb{H}} W)) \\) which has the Clifford multiplication mapping to

    \begin{equation}
    \label{eq:6}
    \Gamma(N , P^{\operatorname{Spin}(3)} \times  (\mathcal{S} \otimes \_{\mathbb{H}} W)) = \Gamma(N , \mathcal{S}\_n \otimes  \_{\mathbb{H}} W).
    \end{equation}

    This composite is called the **Dirac operator** \\( \slashed \nabla \varphi \\).
    The equation \\( \slashed \nabla \varphi =0 \\) is the **Dirac equation**.

    Now let \\( W =0 \\).
    Let \\( P \\) be a principal \\( G \\)-bundle, \\( \nabla \\) a connection on \\( P \\), and \\( \sigma \in \Gamma(N, \operatorname{ad} P) \\).
    The **Bogomolny equation** is

    \begin{equation}
    \label{eq:7}
    \star F + \nabla \sigma =0 \in \Omega^1(N , \operatorname{ad} P).
    \end{equation}

    It is a fact that if \\( N \\) is closed, \\( \nabla \sigma =0 \\).

<!--list-separator-->

-  Sieberg-Witten Equations

    Now lets introduce the Seiberg-Witten equation.
    The Seiberg-Witten equation happens usually when \\( G = U(1), W = \mathbb{H} \\).
    Let&rsquo;s introduce \\( \operatorname{Spin}^G(3) = \left( \operatorname{Spin}(3) \times  G \right) / \pm 1 \\), i.e. \\( -1\in G \\) acts by \\( -1 \\) on \\( W \\).

    A relevant notion is the structure \\( \operatorname{Spin}^{\mathbf{C}}(4) = \operatorname{Spin}^{U(1)}(t) = \left( \operatorname{SU}(2) \times U(1) \right)/ \pm 1 = U(2) \\).
    Set \\( \overline{G} = G / \pm 1 \\), \\( \overline{P} = P^G \times \overline{G} \\).
    The data that go into the Seiberg-Witten equations consists of

    -   a principal \\( \operatorname{Spin}^G(3) \\)-bundle \\( P \to N \\) with identification \\( P^{\operatorname{Spin}^G(3)} \times  V \cong TN \\)
    -   a connection \\( \nabla  \\) on \\( \overline{P} \\) along with the LC connection on \\( P \\)
    -   \\( \sigma \in (N , \operatorname{ad} P) \\)
    -   \\( \varphi\in \Gamma(N, P^{\operatorname{Spin}^G(3)} \times W) \\).

    The actual equations are then

    \begin{align\*}
    \slashed \nabla \varphi &= 0\\\\
    \star F + \nabla \sigma + \mu(\varphi) &= 0
    \end{align\*}

    \\( \mu \\) is the correct analogue of the moment map as in the 2d case.

    One fact is that the linearization is elliptic, and the index of the elliptic operator is \\( 0 \\).

<!--list-separator-->

-  The \\( U(1) \\) Case

    Consider the case where \\( W = \mathbb{H} \\) and \\( G = U(1) \\).
    In this case the spin structure is \\( \operatorname{Spin} ^{ \mathbf{C}} \\) on \\( N \\), which has a free transitive action by \\( h\in H^2(N , \mathbf{Z}) \\).
    Then for \\( c\in \operatorname{Spin}^{ \mathbf{C}} \\) we get a \\( c + h\in \operatorname{Spin}^{ \mathbf{C}} \\), and \\( \operatorname{SW}\_N( c) \\) counts the solutions.


## Lecture 6: Tue Jun 13 12:44:10 2023 {#lecture-6-tue-jun-13-12-44-10-2023}

Recall we constructed the Seiberg-Witten invariant of a manifold, which counts the solutions to a system of PDEs.
Fix some \\( c\_0 \in \operatorname{Sim}^{ \mathbf{C}} \\) on \\( N \\), let \\( H = H\_1( N, \mathbf{Z} ) \cong H^2(N , \mathbf{Z}) \\).
We can build a &ldquo;total&rdquo; invariant \\( SW\_{N,c\_0} = \sum\_{h\in H}^{} \widetilde{SW}\_N(c\_{0 } + h)h \in \mathbf{Z} (H) \\).
(cf taking the sum of Stiefel-Whitney numbers)
If we mod out by \\( H \\), we can get a number independent of the choice of spin structure.

This all holds for the \\( A \\)-model


### Topological Interpretation {#topological-interpretation}

Now what is \\( Z\_{3dB, W / / / G} (N)\\)?


#### Reidemeister Torsion {#reidemeister-torsion}

**Historical note:**
There is a class of \\( 3 \\)-manifolds called _lens spaces_ \\( L(p,q) \\), which ones are homotopy equivalent?
Which ones are homeomorphic?
There exist examples of homotopy equivalent lens spaces that are not homeomorphic, distinguished by their R torsion.

Let \\( N \\) be a finite connected CW complex with basepoint \\(x\\), \\( R \\) a commutative ring, \\( k \\) a field, and \\( R \to k \\) a homomorphism.
Say \\( \mathcal{L}  \\) is a free, finite rank \\( R \\)-module which is a representation \\( \rho: \pi\_1(N) \to \operatorname{GL}\_R(\mathcal{L}) \\)
Say \\( \tilde{N}\to N \\) is a universal cover, and lift the CW structure to \\( \tilde{N} \\).
e.g. we can lift the 1-cell structure of \\( S^1 \\) to \\( \mathbf{R} \\) by taking the points to the \\( \mathbf{Z} \\)-lattice, and the 1-cell lifts to the intervals between the \\( \mathbf{Z} \\)-points.

Consider \\( C\_{\bullet}( \widetilde{N} , \mathbf{Z}) \\) the chain complex of cells of the universal cover endowed with the action of \\( \pi\_1( N) \\).
Thus this is a chain complex of \\( \mathbf{Z}[ \pi\_1(N) ] \\)-modules.
Define \\(  C\_{\bullet} (N , \mathcal{L}) =  C\_{\bullet} ( \widetilde{N}, \mathbf{Z}) \otimes \_{\mathbf{Z}[ \pi\_1(N) ]} \mathcal{L} \\) twisting the coefficients by the representation.
Assume \\(  C\_{\bullet} (N , \mathcal{L})\otimes \_R k \\) is acyclic, i.e. the homology is trivial

<style>.org-center { margin-left: auto; margin-right: auto; text-align: center; }</style>

<div class="org-center">

**Goal:** To define a torsion \\( \tau(N, \mathcal{L})\in k^{\times } /\sim \\) with \\( \sim \\) to be defined.

</div>

You could think of this as a &ldquo;determinant&rdquo; of \\( d \\) in the chain complex.

Fact: there exists a homotopy \\( h:  C\_{\bullet} (N , \mathcal{L}) \otimes \_R k  \to  C\_{\bullet + 1}(N , \mathcal{L})\otimes \_R k  \\) such that \\( dh + hd = \operatorname{id} \\) called the _contracting homotopy_.

Consider \\( d+h : C\_{\mathrm{even}}(N , \mathcal{L}) \otimes  \_{R }k  \to C\_{\mathrm{odd}}(N , \mathcal{L})\otimes  \_{R }k \\).
Fact: It is an isomorphism.
We want to compute its &ldquo;determinant&rdquo;.
First we should give bases for \\(  C\_{\bullet} (N , \mathcal{L}) \otimes \_R k \\).
Choose

-   \\( R \\)-basis of \\( \mathcal{L} \\)
-   ordering of \\( d \\)-cells of \\( N \\) for all \\( d \\)
-   a Turaev spider

**Definition:** A Turaev spider on \\( N \\) is a path from \\( x \\) to every cell in \\( N \\).

Given these choices,

\begin{equation}
\label{eq:8}
 C\_{\bullet} ( \widetilde{N} , \mathbf{Z}) = \bigoplus\_{d\text{-cells}}^{} \mathbf{Z}(\pi\_1(N)).
\end{equation}

Adding in the basis of \\( \mathcal{L} \\) gives a basis of the \\( R \\)-module \\(  C\_{\bullet} (N , \mathcal{L}) =  C\_{\bullet} ( \widetilde{N} , \mathbf{Z}) \otimes \_{\mathbf{Z} [ \pi\_1(N)]} \mathcal{L} \\).
This implies the \\( \det (d+h) \in k^{\times } /  (R^{\times } \det(H\_1(N , \mathbf{Z}))) \\).
Varying the first two choices pushes us around by a factor of \\( R^{\times } \\), while changing the choice of Turaev spider pushes us around by the determinant of a map induced from the homology group.
This \\( \det (d+h) \\) is called the Reidemeister torsion, and is well defined up to these choices.


#### Milnor Torsion {#milnor-torsion}

Assume \\(H = H\_1(N, \mathbf{Z}) \\) has no torsion of rank \\( r \\).
Take \\( R = \mathbf{Z}[H] = \mathbf{Z}[ t\_1^{\pm 1} , \ldots , t\_r^{\pm 1}] \\), and \\( k = \mathbf{Q}(H) = \mathbf{Q}(t\_{1} , \ldots , t\_{r}) \\).
Let \\( \mathcal{L} = R \\) and \\( \rho: \pi\_1(N)\to H\_1(N , \mathbf{Z}) \to \mathcal{L} \\).
Let \\( Z(H)^{\times } = \pm H \\).
We get that \\( \tau(N)\in \mathbf{Q}(H) / \pm H  \\) called the _Milnor torsion_.

Example:

-   For \\( S^1  \\), \\(H\_1(S^1) = \mathbf{Z}\\), so \\( \tau(S^1) = \frac{1}{1-t} \\).
-   \\( \tau(S^1 \times  S^2) = \tau(S^1) = \frac{1}{1-t} \\).


### Connection to SW Invariants {#connection-to-sw-invariants}

Two Turaev spiders \\( S\_1,S\_2 \\) are _equivalent_ if \\( S\_1 - S\_2 =0  \\) in \\( H\_1(N , \mathbf{Z}) \\).
A theorem of Turaev says that if \\( N \\) is a closed oriented \\( 3 \\)-manifold, there is a natural isomorphism between \\( \left\\{ \text{spiders} \right\\} \\) and \\( \left\\{ \operatorname{Spin}^{ \mathbf{C}}\text{ structures} \right\\} \\).
On the former there is a natural action of \\(H\_1( N , \mathbf{Z})\\) and the latter has an action by \\( H^2(N , \mathbf{Z}) \\).

Let \\( N \\) be a closed oriented \\( 3 \\)-manifold and choose a \\( \operatorname{Spin}^{ \mathbf{C}} \\) structure \\( c \\), also giving a spider on \\( N \\).
In this case, there exists a refined torsion (due to Turaev) \\( \tau\_{c}(N) \in \mathbf{Q}(H) / \pm 1 \\).

**Theorem:** Assume \\( b\_1(N) > 1 \\) where \\( N \\) is a closed oriented \\( 3 \\)-manifold.
Choose a \\( \operatorname{Spin}^{ \mathbf{C}} \\) structure \\( c \\).
Then \\( SW\_{N, c} = \tau\_c(N)\in \mathbf{Z}(H)/\pm 1 \\).

Recall that \\( SW\_N = Z\_{3dA, \mathbb{H} /// U(1)}(N) \\).
In fact, \\( \tau(N) = Z\_{3dB , \mathbb{H}}(N) \\).
We can generalize this relationship to more general targets, where \\( V \\) is a unitary \\( G \\)-representation, and \\( W = V \otimes \_{\mathbf{C}} \mathbb{H} \\) &ldquo;cotangent matter&rdquo;.
Then \\( Z\_{3dB, W / / / G}(N) \\) is also related to torsion.
The theorem then provides an equivalence between the 3d TQFTs.
This is an example of mirror symmetry of \\( (U(1), \mathbb{H})\leftrightarrow (\bullet,  \mathbb{H}) \\).


## Lecture 7: Tue Jun 13 15:44:00 2023 {#lecture-7-tue-jun-13-15-44-00-2023}


### Review {#review}

The goal of this lecture is write down fields and susy actions.
As a reminder of yesterday, hyper-Kahler manifolds are Riemannian manifolds who have a \\( \mathbb{P}^1 \\) worth of Kahler structures parameterized by the sphere \\( S^2 \\) in coordinates \\( I,J,K \\).
The group \\( U \operatorname{Sp}(n) = U(n)\cap \operatorname{Sp}(n) \\) acts as isometries of \\( W = \mathbf{C}^{2n} \\).
Gauge groups in these field theories will act as subgroups of this group.
In order to write down equations of motion for fields, we need a notion of a moment map.


### Moment Maps {#moment-maps}

**Definition:** A continuous HK \\( G \\)-action on a HK \\( W \\) comes with a \\( \mathbb{P}^1 \\)-worth of moment maps: functions \\( W \to \mathfrak{g}^{\*} \\) such that \\( V = \omega\_I d\mu\_J = \omega\_J ^{-1} d\mu\_K =\cdots \\) where \\( V \\) is the vector field generating the \\( G \\)-action.
Like with coordinates, there are moment maps for each complex structure \\( \mu\_I, \mu\_J, \mu\_K \\).
In the case of a vector space \\( W \\), these moment maps are always quadratic.

In a fixed complex structure \\( I \\) with \\( \omega\_I , \Omega\_I \\), we can split the moment map into real part \\( \mu\_{ \mathbf{R}} = \mu\_I \\) and \\( \mu\_{\mathbf{C}} = \mu\_J + i \mu\_K \\).
It will turn out that \\( \Omega ^{-1} d \mu\_{\mathbf{C}} \\) is a holomorphic vector field that complexifies the action we started with: a complexified holomorphic \\( G\_{\mathbf{C}} \\)-action on \\( W \\).
In general this does <span class="underline">not</span> preserve the metric, and it acts on \\( W \\) _only_ as a complex symplectic vector space.

-   In the case of cotangent matter, \\( W = T^{\*}V \\), \\( \rho : G \to U(n) \to U \operatorname{Sp}(n) \\) by \\( g \mapsto \operatorname{diag}(g,g^{\*}) \\).

Let \\( V\_n \\) denote the \\( n \\)-dimensional complex irreducible representation of \\( \operatorname{SU}(2) \\).
Denote \\( \overline{V}\_n \\) its dual/conjugate, meaning \\( \operatorname{SU}(2) \\) acts by \\( g \\) as \\( \overline{g} \\).

Fact: \\( \overline{V}\_n  \cong V\_{n} \\) because for all \\( g\in \operatorname{SU}(2) \\), \\( \overline{g} = \epsilon g \epsilon ^{-1}\\) where \\( \epsilon =\begin{pmatrix}0 & -1 \\\1 & 0 \\\\end{pmatrix}\\).
Let \\( e\_a \\) \\( a\in  \left\\{ \pm \right\\} \\) denote a weight basis for \\( V\_2 \in \mathbf{C}^2 \\).
Here \\( e\_+ = \operatorname{diag}(1,0) \\) and \\( e\_- = \operatorname{diag}(0,1) \\).
The matrix \\( \operatorname{diag}(e^{i\theta},e^{-i\theta}) \\) acts on \\( e\_{\pm} \\) as \\( e^{\pm i \theta} \\).
Let \\( \epsilon : V\_2 \xrightarrow{\sim} \overline{V}\_{2} \\).
Then \\( e^a  = \epsilon^{ab}e\_b\\) is a weight basis for \\( \overline{V}\_2 \\).

Note \\( V\_2 \otimes  V\_2 \cong V\_3 \oplus V\_1 \\), The map that goes from the former to \\( V\_1 \\) can be though of as \\( \epsilon \\).
The other intertwiner map that goes from the former to \\( V\_3 \\) is called \\( \sigma \\), the matrix elements of which are the Pauli matrices.


### Supersymmetry {#supersymmetry}

Recall the 3d \\( \mathcal{N}=4 \\) susy algebra is \\( 3+8 \\)-dimensional, even and odd dimension respectively, and is a representation of \\( \operatorname{SU}(2)\_E = \operatorname{Spin}(3)\_E \\).
It is a representation of \\( \operatorname{SU}(2)\_E \times  \operatorname{SU}(2)\_H \times  \operatorname{SU}(2)\_C \\) and has bracket

\begin{equation}
\label{eq:9}
\left\\{ Q\_{\alpha}^{a, a'} , Q\_{\beta}^{b b'} \right\\} = \epsilon^{ab}\epsilon^{a'b'} \sigma\_{\alpha\beta}^{\mu}\partial\_{\mu}
\end{equation}

with all other brackets vanishing.
The \\( H \\) and \\( C \\) stand for &ldquo;Higgs&rdquo; and &ldquo;Coulomb&rdquo;.
The \\( E \\) stands for Euclidean.
The susy algebra lives inside an extension of itself by \\( \operatorname{SU}(2)\_E \\) by semidirect product which itself sits inside the extension by \\(  \operatorname{SU}(2)\_E \times  \operatorname{SU}(2)\_H \times  \operatorname{SU}(2)\_C  \\) via semidirect product.

3d mirror symmetry is nothing but the swapping of the last two copies of \\( \operatorname{SU}(2) \\) in the formulation.


### Fields {#fields}

Fix a gauge theory labelled by \\( G \\) and a hyper-Kahler vector space \\( W \\), and \\( \rho : G \to U \operatorname{Sp}(W) \\).
We want to describe gauge theory on \\( \mathbf{R}^3 \\).
Classically, gauge theory is a space of sections of various bundles on \\( \mathbf{R}^3 \\) together with differential _equations of motion_ such that the solutions to the equations of motion have an action of the susy algebra extended _at least_ by \\( \operatorname{SU}(2)\_E \\) via semidirect product.

A quantum field theory with 3d \\( \mathcal{N}=4 \\) susy is the same data except the equations of motion are replaced with some function of distribution to be used in a path integral.

Fix a trivial principal \\( G \\)-bundle \\( \mathcal{E} \\) on \\( \mathbf{R}^3 \\).
There is a connection \\( A = A\_{\mu}dx^{\mu} \\) on \\( \mathcal{E} \\) living in sections of \\( \mathfrak{g} \otimes V^E\_{3, \mathbf{R}} \\).


## Lecture 8: Tue Jun 13 18:01:16 2023 {#lecture-8-tue-jun-13-18-01-16-2023}

Fix a gauge theory with data \\( G, W , \rho : G \to U \operatorname{Sp}(W) \\).

Let \\( A = A\_{\mu} dx^{\mu} \\) be a connection on a principal \\( G \\)-bundle as a section of \\( \mathfrak{g} \otimes  \Omega^1( \mathbf{R}^3) \\).
So that \\( A\_{\mu} \in \mathfrak{g} \otimes \_{\mathbf{R}} V\_E^{3, \mathbf{R}} \\).
We also have \\( d\_A = d + A ( = \nabla) \\), \\( F = d^2\_A \\), \\( \Phi^{m'} \in \mathfrak{g}\_{\mathbf{R}} \otimes \_{\mathbf{R}} V\_{3, \mathbf{R}}^C  \\), \\( \lambda\_{\alpha}^{a a'}\in \Pi (\mathfrak{g}  \otimes  V\_2^E \otimes  V\_2^C \otimes  V\_2^H) \\), with the \\( V\_2^E \\) being the spin part.

Fermions are called gaugeinos (fermion related to the gauge boson)

We also have \\( Z^{ai} \in V\_2^H \otimes \_\mathbf{C} W \\) such that \\( \overline{Z^{ai}} \in \epsilon\_{ab} \Omega\_{ij}Z^{bj} \\).
Also
\\( \Psi\_{\alpha}^{a'i'} \in \Pi ( V\_2^E \otimes  V\_2^C \otimes \_{\mathbf{C}} W) \\).
Here, \\( \Pi  \\) means parity shift in the sense of a supervector space.


### Schematic SUSY {#schematic-susy}

The supercharges act on matter by
\\( QA \sim \lambda \\), \\( Q \Phi \sim \lambda \\), \\( Q \lambda \sim F + d\_A \Phi + [\Phi, \Phi] + \mu(Z)  \\), \\( QZ \sim \Psi \\), \\( Q\Psi \sim \partial\_A Z + \Phi Z \\).

\\( \partial\_{\alpha\beta} = \sigma\_{\alpha\beta}^{\mu} \partial\_{\mu} \\) as a map \\( V\_2^E\to \overline{V\_2}^{E} \\).
Also \\( \partial^{\alpha}\_{\beta} := (\sigma^{\mu})\_{\beta}^{\alpha} \partial\_{\mu} \\).
It is convenient to rewrite

\begin{align\*}
 A\_{\alpha\beta} &:= \sigma\_{\alpha\beta}^{\mu} A\_{\mu}  \\\\
 \Phi^{a'b'} &:=  \sigma\_{m'}^{a'b'} \Phi ^{m'}  \\\\
 \mu(Z)^{ab} &= \sigma\_m^{ab}\mu^m
\end{align\*}

in \\( \operatorname{SU}(2)\_E, \operatorname{SU}(2)\_C, \operatorname{SU}(2)\_H \\), respectively.

The differential operator on \\( V\_2^E \otimes \_{\mathbf{C}} V\_2^C \\) as

\begin{equation}
\label{eq:10}
\mathbb{D}\_{\beta, b'}^{\alpha ,a'} = (\partial + A)^{\alpha}\_{\beta}\delta^{a'}\_{b'} + \delta^{\alpha}\_{\beta}\Phi^{a'}\_{b'}
\end{equation}

where \\( \Phi \\) is taken as a bispinor.

This differential has modified curvature \\( \mathbb{F} = \mathbb{D}^2 \\).
With this the schematic SUSY equations take the form

\begin{align\*}
\label{eq:11}
Q\_{\alpha}^{a\dot{a}} A\_{\beta\gamma} &= \lambda\_{\beta}^{a \dot{a}}e\_{\gamma\alpha} + d\_{\gamma}^{a \dot{a}}\epsilon\_{\beta\alpha} \\\\
Q\_{\alpha}^{a\dot{a}}\Phi^{\dot{b} \dot{c}} &= i \left( \lambda\_{\alpha}^{a \dot{b}} \epsilon^{\dot{c} \dot{a}} + \lambda\_{\alpha}^{a \dot{c}}\delta^{ \dot{b} \dot{a}} \right)\\\\
Q\_{\alpha}^{a\dot{a}}\lambda\_{\beta}^{b \dot{b}} &= \frac{1}{2} \epsilon^{ab} \mathbb{F}\_{\alpha\beta}^{\dot{a} \dot{b}} - i\epsilon\_{\alpha\beta}\epsilon^{\dot{a} \dot{b}}\mu^{ab} \\\\
Q\_{\alpha}^{a\dot{a}}Z^{b i} &= \epsilon^{ab} \Psi\_{\alpha}^{\dot{a} \dot{i} } \\\\
Q\_{\alpha}^{a\dot{a}}\Psi\_{\beta}^{\dot{b} i} &= \mathbb{D}\_{\alpha\beta}^{\dot{a} \dot{b}}Z^{ai}
\end{align\*}


### Twists {#twists}


#### Step 1 {#step-1}

The first step in twisting is to choose a \\( Q \in SUSY \\) such that \\( Q ^2=0\\) and \\( \im \left\\{ Q,- \right\\} \\) is surjective onto the even part.

There are two \\( \mathbb{P}^1 \\) families of such twists.
One \\( \mathbf{C}\mathbb{P}^1 \\) of \\( A \\)-twists:

\begin{equation}
\label{eq:12}
Q\_A^{\dot{a}} = \delta\_{\alpha}^a Q\_{\alpha}^{a \dot{a}}= Q\_+^{+ \dot{a}} + Q\_-^{- \dot{a}}
\end{equation}

These are linear combos of \\( \dot{a} = +,- \\) modulo scaling with an \\( \operatorname{SU}(2)\_C \\) action.
Choose \\( Q\_A = Q\_A^{\dot{+}} \\).
Then

\begin{equation}
\label{eq:13}
Q\_B^a = \delta\_{\dot{a}}^{\alpha}Q\_{\alpha}^{a \dot{a}} = Q\_+^{a +} + Q\_-^{a \dot{-}}
\end{equation}

choosing \\( Q\_B = Q\_B^+ \\).
The act of choosing is the statement that topological twists do not see the full hyper-Kahler structure.


#### Step 2 {#step-2}

The next step is to globalize from \\( \mathbf{R}^3 \\) to a curved 3-manifold \\( M \\).
The field theory is still a representation of \\( Q\_{A,B} \\) on curved space if we identify \\( A : V\_2^H \cong V\_{2,\mathrm{spin}}^E \\) for the \\( A \\)-twist, and \\( B: V\_2^C \cong V\_{2, \mathrm{spin}}^E \\) for the \\( B \\)-model, respectively.

The new local symmetry group in the twist is for the \\( A \\)-twist: \\( \operatorname{SU}(2)'\_E\xhookrightarrow{\Delta} \operatorname{SU}(2)\_E \times \operatorname{SU}(2)\_H \times \operatorname{U}(1)\_{C} \\).
In the \\( B \\)-twist: \\( \operatorname{SU}(2)'\_E \xhookrightarrow{\Delta} \operatorname{SU}(2)\_E \times  \operatorname{SU}(2)\_{C} \times \operatorname{U}(1)\_{H} \\)


#### Final Step {#final-step}

The final step is currently being worked on: to populate the functorial TQFT view an populate it with objects extracted from the data presented in this lecture.

In dimensions \\(0, 1,2,3 \\) oriented manifolds \\( \Sigma \\) (or really to a thickened version \\( \Sigma \times D^{3-k} \\)) want a quantization \\( \sim Z(\Sigma^k) \\) of \\( Q\_{A,B} \\) fixed points of fields on \\( \Sigma^k \times D^{3-k} \\).


### What happens when you twist {#what-happens-when-you-twist}


#### Fixed Points {#fixed-points}

<!--list-separator-->

-  \\( A \\)-twist

    A choice of \\( Q\_A \lambda =0 \\) and \\( Q\_A\Psi =0 \\) implies for the even fields

    -   \\( \mu(Z)\_m dx^m\\) in \\(\mathfrak{g} \otimes  V\_{3, \mathbf{R}}^{E'} \\)
    -   \\( Z^{i a}\in W \otimes \_{\mathbf{C}} V\_2^{E'} \\)
    -   \\( \Phi^{ \dot{m}} \sim \sigma = \Phi^{\dot{3}} \\), \\( \varphi = \Phi^i + i \Phi ^{\dot{2}} \\) in \\( \mathfrak{g}, \mathfrak{g}\_{\mathbf{C}} \\), respectively
    -   \\( \star F + d\_A \sigma + \mu(Z) = 0 \\)
    -   \\(\partial\_{ab} Z^{ia} = 0\\)
    -   \\( \left[ \varphi, \varphi^+ \right]=0 \\)
    -   \\( \rho(\varphi) Z = 0 \\)

<!--list-separator-->

-  \\( B \\)-twist

    A choice of \\( Q\_B \lambda =0 \\) and \\( Q\_B\Psi =0 \\) implies

    -   \\( \Phi\_{\dot{m}} d x^{\dot{m}} \in V\_{3, \mathbf{R}}^{E'} \otimes  \_{\mathbf{R}} \mathfrak{g} \\)
    -   \\( A + i\Phi := \mathcal{A}\in \mathfrak{g}\_{\mathbf{C}} \otimes  V\_{3, \mathbf{R}}^{E'} \\) is a covariant derivative
        -   \\( \mathcal{F}\_{\mathcal{A}} = 0 \\) complexified curvature
        -   The new covariant derivative \\( \mathcal{D}\_{\mathcal{A}} \\) obeys \\( \left[ \mathcal{D}\_{\mathcal{A}}, \mathcal{D}\_{\mathcal{A}}^+ \right] = \mu\_{\mathbf{R}} \\)
            -   \\( \mu\_{ \mathbf{C}}=0 \\)
            -   \\( \mathcal{D}\_{\mathcal{A}}Z^{i +} =0 \\).

    The \\( B \\)-twist often gets treated as algebraic geometry


## Lecture 9: Wed Jun 14 10:46:24 2023 {#lecture-9-wed-jun-14-10-46-24-2023}


### Recall {#recall}

We had \\( X \\) a hyperKahler manifold (typically a quaternionic vector space) with a \\( G \\)-action by a Lie group \\( G \\).
This situation reduced to 3d TQFTs \\( Z\_{3dA, X // / G} \\), and \\(  Z\_{3dB, X // / G}  \\).
An example of mirror symmetry happens with \\( (U(1) , \mathbb{H}) \leftrightarrow (\bullet, \mathbb{H}) \\) for \\(  Z\_{3dA, X // / G}  \cong    Z\_{3dB, X // / G}   \\), respectively.


### 2d \\( A \\)-model {#2d-a-model}

Let \\( X \\) be a Kahler manifold.
The Cauchy-Riemann equation says that \\( \varphi : \Sigma \to X \\) satisfies \\( \overline{\partial} \varphi =0 \\).
Consider the case \\( \Sigma = S^{1 }\times  \mathbf{R} \\).
You can rewrite the CR equation in the following way: consider \\( \varphi : S^1 \times  \mathbf{R} \to X \\) as a map \\( \mathbf{R} \to \operatorname{Map}(S^1, X)  \\).
The CR equation then has the form

\begin{equation}
\label{eq:14}
\frac{\partial \varphi}{\partial t } + g^{\\#}(\alpha (\varphi))=0.
\end{equation}

The collection \\( \operatorname{Map}(S^1, X) \\) has a metric, and \\( g^{\\#} : \Omega^1 \to T \\).
The space \\( \operatorname{Map}(S^1, X) \\) has a closed \\( 1 \\)-form \\( \alpha \\) (in some cases \\( \alpha  = dS \\) is exact with \\( S \\) a _symplectic action functional_).
In this case the CR eqn is the gradient flow equations for \\( S \\).

We encounter gradient flow equations in Morse theory.
The state space \\(  Z\_{2dA, X}(S^1)   \\) is the Morse homology of \\( S:  \operatorname{Map}(S^1, X) \to \mathbf{R} \\).
(Though \\( S \\) might not be Morse).

Consider the specific case where \\( X = T^{\*}Y \\) where \\( Y \\) is a Riemannian manifold.
In this case, \\( S = \int\_{S'} p(s) dq(s) \\) for \\( s\in S' \\).
Here \\( q : S^1 \to Y \\) and \\( p\in \Gamma(S', q^{\*} T^{\*}Y) \\).
We can perturb \\( S \\) by adding \\( \frac{1}{2} \int(p(s),p(s)) ds \\).
Then

\begin{equation\*}
\widetilde{S} = S + \frac{1}{2}\int |p(s)|^2 ds = \frac{1}{2}\int |p(s) + q'(s)|^2ds - \frac{1}{2} \int|q'(s)|^2ds
\end{equation\*}

If one sets \\( p = -q' \\) identifying by the metric, the Morse homology of \\( \widetilde{S} \\) will reduce to Morse homology of the last term, an energy functional \\( LY \to \mathbf{R} \\).
The claim is that Morse homology of the functional is the homology of the space \\( Y \\).
The upshot: \\(  Z\_{2dA, T^{\*}Y }(S^1) = H\_{\bullet}(Y)  \\).


### 2d \\( B \\)-model {#2d-b-model}

We will consider the case when \\( X =V \\) is a complex vector space, and \\( \Sigma \\) is a surface.
Classical solutions of the QFT on \\( \Sigma \\) are locally constant maps \\( \Sigma \to V \oplus V^{\*}[1] \\).
Here by locally constant maps we mean the chain complex \\( C^{\bullet} (\Sigma, V\oplus V^{\*}[1]) \\).

Consider again \\( \Sigma = S^1 \times  \mathbf{R}  \\).
Then get \\( C^{\bullet}(S^1 , V \oplus V^{\*}[1]) \cong T^{\*}C^{\bullet}(S^1,V) \\), which means the cotangent bundle of the latter vector space, which is just the space plus its dual.
The latter is called _phase space_ of the theory.
To get the state space, should apply a quantization procedure from the lore of quantum mechanics.
The reason we take algebraic (polynomial) functions is because the \\( B \\)-model is &ldquo;algebraic&rdquo; in nature.
One could take other kinds of functions in a different flavor of QFT.
Then, \\(   Z\_{2dB, V }(S^1) = \operatorname{Sym}(C\_{\bullet}(S^1, V^{\*}))   \\).

On the first day we saw that the state space was a Hodge cohomology.

\begin{align\*}
\label{eq:15}
 Z\_{2dB, V }(S^1) &= \operatorname{Sym}(C\_{\bullet}(S^1, V^{\*}))   \\\\
                  &= \bigoplus\_{p,q}^{}H^p (V, \Omega^q)
\end{align\*}

in which \\( p=0 \\) is the only nonzero case.


### 3d \\( A \\)-model {#3d-a-model}

Let \\( W \\) be a quaternionic vector space.
Say \\( N \\) is a Riemannian \\( 3 \\)-manifold with spin structure.
The classical solutions are solutions of the Dirac equation \\( \slashed\nabla \varphi =0 \\) for some spinor field \\( \varphi \in \Gamma(N, \mathcal{S}\_{N} \otimes \_{\mathbb{H}} W) \\).

Take \\( N = \Sigma \times  \mathbf{R} \\).
We want a spin structure on \\( N \\), which by the exercises we can construct from a spin structure on \\( \Sigma \\).
The Dirac equation on \\( N \\) becomes an equation on \\( \varphi : \mathbf{R} \to \Gamma(\Sigma, \mathcal{S}\_{\Sigma} \otimes \_{ \mathbf{C}} W) \\).
This equation becomes a gradient flow equation.
The space
\\(\Gamma( \Sigma , \mathcal{S}\_{N} \otimes  \mathbf{C} W)\\) has a metric and closed 1-form \\( \alpha \\), and the Dirac equation becomes a gradient flow equation.
We can repeat the computation in the 2d case.

Consider the special case \\( W = V \otimes\_{\mathbf{C}} \mathbb{H} \\) the cotangent matter case.
In this instance, the state space is

\begin{equation}
\label{eq:17}
 Z\_{3dA, W}(\Sigma) = H\_{\bullet}( H^0(\Sigma, V))
\end{equation}

the homology of the space of global sections.

Now say \\( G \\) is a Lie group acting on \\( V \\) as a unitary representation, and \\( G\_{\mathbf{C}} \\) is the complexification.
Let \\( \mathcal{M}\_{G,V}( \Sigma) \\) be the moduli space of pairs of \\( G\_{\mathbf{C}} \\)-holomorphic bundles \\( P \\) on \\( \Sigma \\), and holomorphic sections \\( \varphi \\) of \\( P^G \times V \\).

**Remark**: \\( \mathcal{M}\_{G,V}(\Sigma) \\) is the moduli space of solutions of symplectic vortex equations from yesterday.

Then

\begin{equation}
\label{eq:18}
  Z\_{3dA, V \otimes  \_{\mathbf{C}} \mathbb{H} / / / G} = H\_{\bullet}( \mathcal{M}\_{G,V}( \Sigma)).
\end{equation}

If you take \\( \Sigma = S^2 \\), then

\begin{equation\*}
Z\_{3dA, V \otimes  \_{\mathbf{C}} \mathbb{H} / / / G}
\end{equation\*}

is the functions on BFN definition of Coulomb branch.


### Example of 3d Mirror Symmetry {#example-of-3d-mirror-symmetry}


#### \\( A \\)-model {#a-model}

Consider \\( (U(1), \mathbb{H})\leftrightarrow (\*, \mathbb{H}) \\).
There is an isomorphism of state spaces

\begin{equation}
\label{eq:19}
 Z\_{3dA, \mathbb{H} // / U(1)} (\Sigma) \cong   Z\_{3dB, \mathbb{H} }  (\Sigma).
\end{equation}

Let \\( V = \mathbf{C}, G = U(1), G\_{\mathbf{C}} = \operatorname{GL}(1, \mathbf{C}) \\).
Then \\( \mathcal{M}\_{G,V} ( \Sigma)  \\) consists of holomorphic line bundles \\( \mathcal{L} \to \Sigma \\) and a nonzero holomorphic section of these bundles.
The requirement to be nonzero is a technical necessity to make this a scheme.
You can consider

\begin{equation\*}
 \mathcal{M}\_{G,V}(\Sigma) = \operatorname{Sym}(\Sigma)  = \bigsqcup\_{d \geqslant 0}^{} \operatorname{Sym}^d(\Sigma).
\end{equation\*}

**Proposition:** \\( H\_{\bullet}( \operatorname{Sym}^d(\Sigma)) = \operatorname{Sym}^d( H\_{\bullet}(\Sigma)) \\).

Thus,

\begin{equation}
\label{eq:20}
 Z\_{3dA, \mathbb{H} // / U(1)} (\Sigma)  = \bigoplus\_{d \geqslant 0}^{} \operatorname{Sym}^d(H\_{\bullet}(\Sigma)) = \operatorname{Sym}(H\_{\bullet}(\Sigma)).
\end{equation}


#### 3d \\( B \\)-model {#3d-b-model}

We can repeat the setup from the last section.
If we do the same stuff,

\begin{equation}
\label{eq:21}
Z\_{3dB, W}(\Sigma) = \operatorname{Sym}(C\_{\bullet}(\Sigma , V^{\*})).
\end{equation}

As promised, they match:

\begin{equation\*}
\label{eq:22}
 Z\_{3dA, \mathbb{H} // / U(1)} (\Sigma) = \bigoplus\_{d \geqslant 0}^{} H\_{\bullet}(\operatorname{Sym}^d(\Sigma )) \cong \operatorname{Sym}(H\_{\bullet}(\Sigma)) = Z\_{3dB, W}(\Sigma)
\end{equation\*}

This is an example of 3d mirrory symmetry.


## Lecture 10: Wed Jun 14 12:48:31 2023 {#lecture-10-wed-jun-14-12-48-31-2023}

This lecture will discuss the affine Grassmannian and the BFN Coulomb branch.


### The Affine Grassmannian {#the-affine-grassmannian}

Our groups will always be \\( G = \operatorname{GL}, \operatorname{SL}, \operatorname{PGL}, \prod\_{k}^{}\operatorname{GL}\_{k} \\).
For \\( A \\) a ring, we can look at \\( G(A) \\) the \\( A \\) points of \\( G \\).
Let \\( O \subset K \\) be \\( \mathbf{C} \\)-algebras, \\( G(K) / G(O) \\) with \\( K \\) usually \\( \operatorname{Frac} O \\) and \\( O = \mathbf{C}[[t]] \\).
The quotient space is often noted \\( \operatorname{Gr}\_G \\) the affine Grassmannian.

**Example:**

-   Fix \\( T \subset G \\) a maximal torus with action on the coset space \\( \operatorname{Gr}\_G \\).
    The fixed points can be identified with coweights \\( \operatorname{Gr}^T = P^{\vee} \\)  where \\( P^{\vee} \\) is the coweight lattice of \\( G \\): \\( \operatorname\*{Hom}(\mathbf{C}^{\dagger}, T) \\).
    Every coweight \\( \lambda\in P^{\vee} \\) has \\( \lambda(t) \equiv t^{\lambda} \in \operatorname{Gr}\_G  \\).
    The \\( G(O) \\)-orbits through \\( t^{\lambda} \\) stratify \\( \operatorname{Gr}\_G = \bigsqcup\_{(P^{\vee})^+}^{} \operatorname{Gr}\_G^{\lambda} \\), where \\( \operatorname{Gr}\_G^{\lambda} = G(O)t^{\lambda} \\), indexed over the dominant coweights.

    As a moduli space, we can describe \\( \operatorname{Gr}\_{G} \\) as

\begin{equation}
\label{eq:16}
\left\\{ ( P, \varphi) : P \to \mathbb{D} = \operatorname{Spec}O \text{ principal }G \substack{- bundle\\\ trivialized away \\\from 0} \right\\}.
\end{equation}

The condition says that \\( \varphi :P|\_{ \mathbb{D}^{\times }} \to P|\_0|\_{\mathbb{D}^{\times }} \\) where \\( \mathbb{D}^{\times } = \operatorname{Spec} K \\).

When \\( G = \operatorname{GL}\_n \\), \\( \operatorname{Gr}\_g = \left\\{ (P,\varphi)\mid P  \text{ rank }n \text{ vector bundle trivable on }\mathbb{D}^{\times } \right\\} \\).
There is another lattice description:

\begin{equation}
\label{eq:23}
 \operatorname{Gr}\_{G}  = \left\\{ O \text{- lattices of rank n in }K^n \right\\}.
\end{equation}

An \\( O \\)-lattice is a free \\( O \\)-module space \\( L \\) such that \\( L \otimes  \_O K \cong K^{n} \\).
For example, \\( g\in G(K) \\) has associated \\( GO^n \\) (called the &ldquo;standard lattice&rdquo;).
On a pair \\( (P, \varphi)  \\), we associate \\( H^0(\mathbb{D}, P)\subset H^{0}( \mathbb{D}^{\times }, P\_0) = K^n \\).

For a rank \\( 0 \\) example, take \\( G = \operatorname{GL}\_1 \\).
Then

\begin{equation\*}
  \operatorname{Gr}\_{G}  = \mathbf{C}^{\times }(K)  / \mathbf{C}^{\times } = K^{\times } /\left\\{ \underbrace{u}\_{\neq 0} + t \underbrace{p(t)}\_{=0} \right\\}
\end{equation\*}

We can write any element of \\( K^{\times } \\) as \\( t^nq(t) \\) for some integer \\( n \\), where \\( q(t)=0 \\).


### BFN Coulomb Branch {#bfn-coulomb-branch}

The BFN Coulomb branch is built out of a \\( G \\) gauge group and its action on \\( N \\).
\\( N \\) is the &ldquo;matter&rdquo;.

Step 1 is to construct an algebra

\begin{equation}
\label{eq:25}
A(G,N) = H\_{\bullet} (\underbrace{\operatorname{Map}(\mathbb{B}, \left[ \frac{N}{G} \right]}\_{M})).
\end{equation}

\\( \mathbb{B} \\) is the &ldquo;bubble&rdquo; or &ldquo;raviolo&rdquo;.
The Coulomb branch associated to this gauge and matter is

\begin{equation}
\label{eq:26}
\mathcal{M}\_C(G,N) = \operatorname{Spec}(A (G,N))
\end{equation}

\\( \mathbb{B} \\) is the bubble \\( \mathbb{D} \cup\_{ D^{\times }} \mathbb{D} \\) analogous to the construction of the affine line with double origin.
We get the algebraic structure corresponding to stacking bubbles \\( \mathbb{B}\mathbb{B} = \mathbb{D}^1\cup\_{\mathbb{D}^{\times }} \mathbb{D}^2 \cup\_{\mathbb{D}^{\times }} \mathbb{D}^2\\).
This double bubble admits three different maps into each disk, this gives a way to multiply.

To understand the maps, we can describe it as

\begin{equation\*}
\left\\{ ( \mathcal{P}, S) \mid \mathcal{P} \xrightarrow{p}  \mathbb{B}\xrightarrow{s}  N\_P := N^G \times P \right\\} = \left\\{ (\mathcal{P}\_1, \mathcal{P}\_2, \varphi , s)\mid \mathcal{P}\_i \to \mathbb{D}, \varphi : \mathcal{P}\_1|\_{\mathbb{D}^{\times }} \xrightarrow{\sim } \mathcal{P}\_2|\_{\mathbb{D}^{\times }} \right\\}
\end{equation\*}

where we can extend \\( \overline{\varphi } : S \to \mathbf{R} \\)

We can trivialize \\( \mathcal{P}\_2 \\) to get the space

\begin{equation}
\label{eq:28}
\mathcal{R}\_{G,N} = \left\\{ ( \mathcal{P}, \varphi,s)\mid \mathcal{P}\to \mathbb{D}, \varphi \text{ a triv and }s \text{ a section} \right\\}
\end{equation}

and we impose that \\( \varphi s \\) extends to \\( \mathbb{D} \to N\_{\mathcal{P}\_0} \cong N \otimes  O \\).

**Fact:** \\( M \cong \mathcal{R}\_{G,N}/ G(O) \\).
Then

\begin{equation}
\label{eq:29}
A(G,N) = H\_{\bullet}( \mathcal{R}\_{G,N} / G(O)) = H^{G(O)}\_{\bullet}( \mathcal{R}\_{G,N}).
\end{equation}

We now want to understand the product structure on \\( A(G,N) \\).
Instead of \\( \mathcal{R}\_{G,N} \\), we will look at the slightly bigger space

\begin{equation\*}
 T\_{G,N}  = \left\\{ (\mathcal{P}, \varphi,s)\mid \mathcal{P}\to \mathbb{D}, \varphi \text{ a triv}, s \text{ a section} \right\\}.
\end{equation\*}

Note there is no requirement for \\( \varphi s \\) to lift.
The space \\( T\_{G,N} \to  \operatorname{Gr}\_{G}  \\)  is a vector bundle and it is a resolution of \\( N \otimes  O \\).
This is in analogy with the Springer resolution \\( T^{\*}  G/B \to \mathcal{N} \\), but we don&rsquo;t really care much about it.

Set \\( T\_{G,N} \otimes \_{N \otimes K} T\_{G,N} = Z\_{G,N} \\).
Then \\( Z\_{G,N} / G(K) \cong \mathcal{R}\_{G,N} / G(O) \\) has again a description

\begin{equation}
\label{eq:30}
\left\\{ (g\_1 , [g\_2], s)\mid s\in N \otimes  O , g\_2 \in N \otimes  O, s\in N \otimes  O, g\_1 s\in N \otimes  O \right\\}.
\end{equation}

Again this space admits maps \\( g\_1 s \to ([g\_1g\_2, g\_1s]) \\), \\( N \otimes O \to ([g\_1, g\_1s]) \\), and \\( g\_2 \to ([g\_2 ,s]) \\).

For example, take \\(N = \mathbf{C}^m\\), \\( G = \mathbf{C}^{\times } \\) with weight \\( -1 \\).
Then the coweight space \\( P^{\vee} = \mathbf{Z}\\) giving stratification \\(  \operatorname{Gr}\_{G}  = \bigsqcup\_{n\in \mathbf{Z}}^{} t^n \\).
We can map \\( \mathcal{R} \_{G,N}\to  \operatorname{Gr}\_{G}  \\) and lookin on \\( \mathcal{R}\_{G,N} = \bigsqcup\_{n\in \mathbf{Z}}^{} \mathcal{R}^n\_{G,N} \\) with each

\begin{equation}
\label{eq:31}
\mathcal{R}^{n}\_{G,N} = \left\\{ t^n \right\\} \times  \left\\{ (t ^{-1} O \cap O)^m \right\\}
\end{equation}

Working out

\begin{align\*}
 r\_n &= [ \mathcal{R}\_{G,N}^n]\in H^{G(O)}\_{\bullet}( \mathcal{R}\_{G,N}) \\\\
 &= \bigoplus H^{G(O)} ( \mathcal{R}\_{G,N}^n) \\\\
 &= \bigoplus H^{G(O)} (pt) \otimes  r\_n \\\\
 &= \bigoplus \mathbf{C}[w]r\_n.
\end{align\*}

So to compute the product \\( r\_1 r\_{-1} \\), it becomes

\begin{align\*}
\label{eq:32}
r\_1r\_{-1} &= \left[ t \times t ^{-1}O^m \cap O^m \right]\left[ \frac{t ^{-1} \times  t O^m \cap O^{m}}{t O^m} \right]\\\\
          &= \left[ \frac{O^m}{t O^m} \right]r\_0 \\\\
          &= \left[ N \right] r\_0\\\\
          &= w^m r \_{0}.
\end{align\*}

This relation gives everything we want to know about \\( A\_{G,N} \\).
That is

\begin{equation}
\label{eq:33}
A\_{G,N} = \mathbf{C}[w] [r\_1 , r\_{-1}]/ (r\_1r\_{-1} - w^m r\_0).
\end{equation}

Thus

\begin{equation}
\label{eq:34}
\operatorname{Spec} A\_{G,N} = A\_{m-1}
\end{equation}

a Kleinian singularity.


## Lecture 11: Wed Jun 14 15:55:18 2023 {#lecture-11-wed-jun-14-15-55-18-2023}

We last left off with

\begin{align\*}
A &\in V\_{3 \mathbf{R}}^E \otimes \_{ \mathbf{R}} \mathfrak{g} \quad\quad \text{connection}\\\\
\Phi &\in V\_{3, \mathbf{R}} \otimes  \_{\mathbf{R}} \mathfrak{g} \quad\quad \text{gaugeinos} \\\\
\lambda &\in \Pi  \left( V\_2^C \otimes  V\_2^H \otimes  V\_2^E \otimes \mathfrak{g} \right)\\\\
Z &\in W \otimes V\_2^H\quad\quad \text{matter} \\\\
\Psi &\in \Pi \left( W \otimes  V\_2 ^C \otimes V\_2^{E} \right)\quad\quad \text{fermionic fields}.
\end{align\*}

The plan for today is to discuss the moduli space of SUSY vacua.


### Moduli Space of vacua {#moduli-space-of-vacua}

The classical moduli space of SUSY vacua in any SUSY QFT on some \\( \mathbf{R}^d \\).
Schematically this is the set of boundary conditions near \\( \infty \\) that

1.  Allow finite energy solutions to the equations of motion in the interior
2.  Are fixed points of <span class="underline">all</span> \\( Q \\)&rsquo;s

These imply
\\( \left\\{ Q,Q \right\\}=\partial \\) which implies \\( \partial (-)=0 \\) implying the field is constant.
We know \\( QA \sim \lambda \\) and \\( QZ \sim \Psi \\), so that \\( \lambda = \Psi = 0 \\), which is why no one speaks of fermions in the moduli space of vacua.

To the bosons, \\( Q\lambda \sim F  + d\_A \Phi +\left[ \Phi, \Phi \right] + \mu \\), and \\( Q \Psi \sim d\_A Z + \Phi Z \\).
Classical vacuum equations are

\begin{align\*}
\label{eq:24}
\mu^m(Z) &=0 \\\\
\rho( \Phi ^{\dot{m}})Z &= 0 \\\\
\left[ \Phi^{\dot{m}} , \Phi^{\dot{n}} \right]&= 0.
\end{align\*}

There are at least two types of solutions: a classical Higgs branch and classical Coulomb branch

\begin{align\*}
\mathcal{M}\_H^{\mathrm{class}} & = \left\\{ \Phi =0, \mu=0 \right\\} / G = W / / /G \\\\
\mathcal{M}\_C^{\mathrm{class}} & = \left\\{ Z =0, [\Phi , \Phi]=0 \right\\} / G  \\\\
                               & \cong \mathfrak{t}^3 / \mathrm{Weyl}.
\end{align\*}

The Higgs branch is hyperKahler.
The former has an \\( \operatorname{SU}(2)\_H \\) action, while the latter has a \\( \operatorname{SU}(2)\_C \\) action rotating the 3 copies of Cartan algebra \\( \mathfrak{t} \\).
This is not yet hyperKahler though, but we expect(?) it to be.


### Quantum vacua {#quantum-vacua}

In QFT, \\( \mu^m \\) is replaced with expectation value.
So a vacuum is a boundary condition for fields at \\( \infty \\) on \\( \mathbf{R} \\) such that

\begin{align\*}
\left\langle \mathcal{O} \mathcal{O}' \mathcal{O}'' \right\rangle\_{\mathrm{vac}} &= ??
\end{align\*}

In QFT on a compact manifold, one should sum over topological types of gauge bundles.
Boundary conditions at \\(\infty\\) on \\( \mathbf{R}^3 \\) should include a choice of topological type of \\( G \\) bundle on \\( S^2\_{\infty} (= \partial \mathbf{R}^3) \\).

Better yet, we can sum over all topological types with some specified weight.
The upshot of this is, at a generic point on the Coulomb branch \\( \mathcal{M}\_C \\) (where the other boundary conditions have been fixed \\( Z=0 \\), \\( \Phi \\) is generic), the weight for summing bundles is a point \\( \gamma \\) of \\( T^{\vee} \\) the dual torus.

If \\( \Phi^{\dot{m}} \\) are diagonal with generic eigenvalues, their stabilizer is \\( T \subset G \\).
So at \\( \infty \\) we get a gauge bundle on \\( S^2\_{\infty} \\) with structure reduced to \\( T \\).
Such bundles are classified by \\( \pi\_1(T) \\) (the _cocharacter lattice_ of the group).

\begin{equation}
\label{eq:35}
\sum\_{n\in \pi\_1(T)}^{} \left\langle \cdots \right\rangle \gamma(n)
\end{equation}

So we need to choose \\( \gamma \in \operatorname\*{Hom}(\operatorname{cochar}(G) , U(1))\simeq T^{\vee} \simeq (S^r)^{\operatorname{rk}G} \\).

For example, if \\( G = U(1) \\), the bundles are classified by integers \\( n \in \mathbf{Z} \\) and \\( \gamma\in \mathbf{R} / 2\pi \mathbf{Z} \simeq T^{\vee} \\)
and the expression would be

\begin{equation}
\label{eq:36}
\sum\_{n\in \mathbf{Z}}^{} \left\langle \cdots \right\rangle\_{n} e^{i \gamma n}.
\end{equation}

The effect of this tiny extra bit of information is that the quantum Coulomb branch has a map \\( T^{\vee} \dashrightarrow \mathcal{M}\_C \to \mathfrak{t}^3 / \mathrm{Weyl} \\).


### Examples of Higgs and Coulomb Branches {#examples-of-higgs-and-coulomb-branches}

The Higgs and Coulomb branches intersect at least at a point.
\\( \mathcal{M}\_C, \mathcal{M}\_H \\) are in general singular hyperKahler manifolds with \\( \operatorname{SU}(2)\_C, \operatorname{SU}(2)\_H \\) actions, respectively.
In mirror 3d gauge theories:

\begin{align\*}
\mathcal{M}\_C^{!} &\simeq \mathcal{M}\_H \\\\
\mathcal{M}\_H^{!} &\simeq \mathcal{M}\_C.
\end{align\*}


## Lecture 12: Wed Jun 14 18:05:37 2023 {#lecture-12-wed-jun-14-18-05-37-2023}


### \\( \Omega\\) -background and quantization {#omega-background-and-quantization}

Recall from Pavel&rsquo;s lectures that in a 3d field theory \\( Z(S^2)  \\) is an \\( E\_3 \\)-algebra.
This means for every \\( n \\), there&rsquo;s a set of maps \\( C\_{\bullet}( \operatorname{Conf}\_n( \mathbf{R}^3) ) \otimes  Z(S^2)^{\otimes  n} \to Z(S^2) \\), or \\( C\_{\bullet}( \operatorname{Conf}\_n( \mathbf{R}^3))\to \operatorname\*{Hom}(Z(S)^{\otimes  n}, Z(S^2)) \\).
In particular, when \\( n=2 \\), there&rsquo;s a &ldquo;multiplication map&rdquo;.
This product is homotopy-commutative.
For example \\( \operatorname{Conf}\_2( \mathbf{R}^2) \sim S^2 \\), by taking the vector between two points at unit norm, producing a homotopy equivalence.
Additionally, the product at the north and south poles are homotopty equivalent via a path, hence homologically the same.

You can also take the fundamental class and apply the second south pole multiplication \\( \alpha\_2 \\): \\( \alpha\_2( [S^2]) : Z(S^2)^{\otimes  2} \to Z(S^2) \\) as a degree \\( 2 \\) map.
This we will denote \\( \left\\{ -,- \right\\} \\) as a Poisson bracket.
The exercises show that the product give \\(H\_{\bullet}( Z(S^2) )\\) the structure of a \\( P\_3 \\)-algebra.

Choose a copy of \\( S^1 \subseteq \operatorname{SO}(3) \\).
There is a natural map \\( C\_{\bullet}(S^1) \to C\_{\bullet}(\operatorname{SO}(3)) \to \operatorname{End}(Z(S^2)) \\).
The way we can choose \\( S^1 \\) is to pick an axis and rotate \\( \operatorname{SO}(3) \\) about the axis.
Then one can consider the space of \\( S^1 \\) invariants

\begin{equation}
\label{eq:37}
Z(S^2)^{S^1} = \operatorname{Hom}\_{C\_{\bullet}(S^1)}( \mathbf{C} , Z(S^2)).
\end{equation}

We can ask: _what sort of object is this?_

Review of \\( S^1 \\)-invariants.

Some facts

1.  For any vector space with \\( S^1 \\)-action \\( V \\), \\( V^{S^1} \\) is a module \\( \mathbf{C}[ h] \\) where \\( h \\) is degree \\( 2 \\).
2.  \\( S^1 \\) acting on \\( X \\) means \\( C\_{\bullet}(S^1)  \\) acts on \\( C\_{\bullet}(X) \\).
    Then \\( C\_{\bullet}(X)^{S^1} =: C\_{\bullet}^{S^1}(X) \\).

Now we can take

\begin{equation}
\label{eq:38}
C\_{\bullet}^{S^1}( \operatorname{Conf}\_n(\mathbf{R}^3)) \otimes (Z(S^2)^{S^1})^{\otimes  n} \to Z(S^2)^{S^1}.
\end{equation}

We would now intuitively expect the north and south pole multiplications to no longer be the same.
This is because we are now forced to stay on the axis we chose.
So for a computation, what is \\( C\_{\bullet}^{S^1}(S^2) \\)?

**Facts**:

1.  If \\( S^1 \\) acts freely on \\( X \\), and \\( T < S^1 \\), then the equivariant action \\( C\_{\bullet}^{T}(X) = C\_{\bullet}^{T / S^1}(X / S^1) \\).
2.  We can also view \\( S^2 \\) as \\( \mathbb{P^{1}} \\).
    Then we have \\(\mathbf{C}^2\hookleftarrow  S^3 \to \mathbb{P}^1  \\).
    It has been shown that given \\( S^1 \subset S^1 \times  S^1 \\): \\( H\_{0}^{S^1 \times  S^1 \times S^1}(\mathbf{C}^2)\xrightarrow{i^{\*}} H\_{0}^{S^1 \times S^1}( S^3) \to H\_{0}^{S^1}( \mathbb{P}^1) \\) is surjective where the first map is induced by Poincare duality.
    But the first group is \\( \mathbf{C}[x\_1, x\_2] \\).
    So there is a map \\( \mathbf{C}[h] \to \mathbf{C}[x\_1, x\_2] \\) by \\( h \mapsto x\_1 - x\_2 \\).

3.  Suppose we have a torus \\( T  \\) acting on \\( V \\) a vector space.
    Also suppose there is a fixed \\( W \subset V \\) subspace.
    Then \\( [W]\in H\_0^{S^1}(V) \\) is computed as \\( \operatorname{eu}^T(V/W)\cdot [V] \\) (?).

Recall we have

\begin{equation}
\label{eq:39}
\mathbf{C}^2 \hookleftarrow S^3 \to S^2 = S^3 / \Delta S^3
\end{equation}

with an action of \\( S^1 \times  S^1 \\) action on \\( S^3 \\).
Let&rsquo;s find where \\( [ \mathbf{C} \times  0 ] , [ 0 \times  \mathbf{C}] \\) go.
We claim they go to the north and south pole, respectively.
In homogeneous coordinates of \\( \mathbb{P}^1 \\), they&rsquo;d go to \\( [1,0] \\) and \\( [0,1] \\).

We know that \\( [ \mathbf{C} \times  0 ] \cdot [ 0 \times  \mathbf{C}]  \mapsto \emptyset\\).
Recall we had \\( \mathbf{C}[x\_1, x\_2] = H\_0^{S^1 \times S^1}(pt) \\) from the passage to homology.
When we quotient by each term and take weights, we&rsquo;re left with \\( x\_2,x\_1 \\) respectively.
So the image of the product of the two is \\( 0 \\), so \\( x\_1x\_2 \mapsto \emptyset \\).
So \\( \mathbf{C}[x\_1, x\_2] / \left\langle x\_1x\_2 \right\rangle \\) is the equivariant cohomology \\(H^{S^1}(S^2)\\).
If you work everything out you figure out \\( h \mapsto x\_1 - x\_2 \\).

Back to our example, we find that \\( \star\_N - \star\_S =h \left\\{- ,- \right\\} \\) where \\( \star\_i \\) is the multiplication at the point \\( i\in S^2 \\).
So when \\( h \mapsto 0 \\), we get something commutative.
In summary, we find that the \\( S^1 \\)-invariant ring map \\( \mathbf{C}[h]\to Z(S^1)^{S^1} \\) quantizes the Poisson algebra.


### The \\( A \\)-model {#the-a-model}

Recall that \\( Z\_A(S^2)= C\_{\bullet}( \operatorname{Maps}( \mathbb{B} , N /G)) \\) from the BFN construction.
What we learned today is that

\begin{equation}
\label{eq:40}
Z\_A(S^2)^{S^1} = C\_{\bullet}^{S^1}(\operatorname{Maps}(\mathbb{B}, N/G))
\end{equation}

where this \\( S^1 \\) acts be rotating the bubble.

Example when \\( N=0 \\) assuming \\( G = U(1) \\):
In this case

\begin{equation\*}
(\operatorname{Bun}\_G( \mathbb{B}) =)\operatorname{Maps}( \mathbb{B} , BG)=  G[[t]] \backslash G((t)) / G[[t]]
\end{equation\*}

The easiest way to incorporate ---- is to look at

\begin{equation}
\label{eq:41}
\mathbf{C}^{\times } \rtimes G[ [ t ] ]\backslash\mathbf{C}^{\times } \rtimes G((t)) / \mathbf{C}^{\times } \rtimes G[[t]] = \mathbf{C}^{\times }\backslash G[ [t]] \backslash G((t)) / G[ [t]]
\end{equation}

Fact: The preceding space has two different maps to \\( pt / \mathbf{C}\rtimes G[ [t]] \\), sending either the left side to a point, or the right side to a point.
These give two separate maps

\begin{equation}
\label{eq:42}
H\_{S^1 \times G}^{\bullet}(pt) \xrightarrow{p\_{L}^{\*}, p\_R^{\*}} H\_0 ^{S^1}( \operatorname{Bun}\_G(\mathbb{B})).
\end{equation}

Fact: \\( (s,1) \cdot (1,t^n) = (1, t^ns^n) (s,1) \\).
This implies that on the \\( t^n \\) component of \\( \operatorname{Bun}\_G(\mathbb{B}) \\), \\( p\_l^{\*}(x) = p\_R^{\*}(x) + p\_R^{\*}(nh) \\).

Fact: \\( x\cdot a = p\_L^{\*}(a) \\) and \\( a \cdot x = p\_R^{\*}a \\).

We also have \\( H\_3^{S^1}( \operatorname{Bun}\_G(\mathbb{B})) = \bigoplus\_{n}^{}H\_n^{S^1 \times G}(pt)\cdot t^n \\).

Moreover,

\begin{equation}
\label{eq:43}
x t^n - t^n x = p\_L^{\*}(x) t^n - p\_R^{\*}(x) t^n = n h t^n.
\end{equation}

So when \\( h=0 \\), \\( x,t^n \\) commute.

Then

\begin{equation}
\label{eq:44}
Z\_A(S^2)^{S^1}=C\_{\bullet}^{S^1}( \operatorname{Bun}\_G(\mathbb{B})) = \mathbf{C}\left\langle h, xt^{\pm 1} \right\rangle
\end{equation}

where \\( [h,x] = [t,h]=0 \\) and \\( [t,x]=ht \\).


#### Trick {#trick}

This space \\( \operatorname{Maps}( \mathbb{B} , N /G) / \mathbf{C}^{\times }\rtimes G = R\_{G,N} / \mathbf{C}^{\times } \rtimes G \\).

Then

\begin{equation}
\label{eq:45}
R\_{G,N} \xrightarrow{L}  T\_{G,N} \leftrightarrow  \operatorname{Gr}\_G
\end{equation}

where the right side maps are bundle maps and section.
It is a cool fact that

\begin{equation}
\label{eq:46}
C\_{\bullet}^{S^1}(R\_{G,N} / \operatorname{Gr}) \to C\_{\bullet}^{S^1}(\operatorname{BUn}\_G(\mathbb{B}))
\end{equation}

is an algebra homomorphism.

For example when \\( N = \mathbf{C}\\), \\( G \subset \mathbf{C}^{\times } \\),  we have

\begin{equation}
\label{eq:48}
t^n \times t^n \mathbf{C}[ [t]] \hookrightarrow \bigsqcup t^n \times  t^n \mathbf{C}[ [t]] \hookleftarrow \bigsqcup t^n \times  \left\\{ 0 \right\\}
\end{equation}

So

\begin{equation}
\label{eq:49}
t^n \times  \mathbf{C}[[t] ] \mapsto \operatorname{eu}^T \left( t^n \mathbf{C}[ [t]] / t^n \mathbf{C}( (t))\cap \mathbf{C} ( (t)) \right).
\end{equation}


## Lecture 13: Thu Jun 15 10:46:45 2023 {#lecture-13-thu-jun-15-10-46-45-2023}


### Review {#review}

So far we&rsquo;ve considered 3d \\( \mathcal{N}=4 \\) gauge theories labeled by a compact Lie group \\( G \\) and \\( W = T^{\*} V \\) (sometimes) a hyperKahler representation of \\( G \\).
Physical QFTs are representations \\( \mathcal{T}\_{G,V} \\) with action of \\( \operatorname{SU}(2) \_E \otimes \operatorname{SU}(2)\_H \otimes  \operatorname{SU}(2)\_C\\).
We&rsquo;ve hinted towards gauge theories that are related to each other by 3d mirror symmetry, which means that the QFTs are equivalent at infinitely low energy up to a swapping of \\( \operatorname{SU}(2)\_H , \operatorname{SU}(2)\_C \\).

Compare this situation with 2d \\( \mathcal{N}=(2,2) \\) \\( \sigma \\)-models (\\( \equiv 2\mod 4\\) means it comes in a pair).
This model is labeled by Calabi-Yau manifolds, so one gets a 2d QFT \\( \mathcal{T}\_{\mathcal{X}} \\) with an action by the SUSY algebra \\( \rtimes U(1)\_E \times  U(1)\_V \times  U(1)\_A \\).
There exist CY manifolds \\( \mathcal{X}^{\vee} \\) such that their QFTs are equivalent up to swapping of \\( U(1)\_V, U(1)\_A \\).
This is the basis of 2d mirror symmetry.


### Computations in Dualities {#computations-in-dualities}

In any physical duality (equivalence of QFTs) you can compute physical quantities (observables) on both sides of the duality and try to match them.
One might be interested in (at a low level) the moduli space of vacua, partition functions on closed manifolds, state spaces, expectation values (AKA correlation functions) of operators.
In practice these things are impossibly difficult to compute, so historically very simple examples are worked out in great detail to try and get a handle on even the smallest toy case.
We can characterize these computations mathematically and recover or predict new math or new physics.

There is some hope though, because many of the computations can be captured by twists.
In other words, they live in the cohomology of various nilpotent elements of the SUSY algebra.
In our case, SUSY acted on \\( \mathcal{T}\_{G,N} \\) and had two supercharges \\( \mathcal{O}\_A , \mathcal{O}\_{B} \\).
One expects for each 3d \\( \mathcal{N}=4 \\) theory QFT functors \\( Z\_{G,V}^A, Z^B\_{G,V} \\) such that for a mirror pair \\(  Z\_{G',V'}^A, Z^B\_{G',V'}  \\) that \\( Z\_{G,V}^A\simeq Z\_{G' ,V'}^B \\) and likewise for the original \\( B \\)-model.

On Tuesday Pavel said that

\begin{equation}
\label{eq:50}
Z\_{U(1) , \mathbf{C}}^A(M ^3) = \text{SW invariants of 3-manifold}
\end{equation}

Meng-Taubes theorem says that \\( \operatorname{Tor}(M^3) \cong Z\_{1, \mathbf{C}}^B(M^3) \\).
Yesterday we saw state space

\begin{equation}
\label{eq:51}
Z\_{U(1) , \mathbf{C}}^A(M ^3) (\Sigma\_g) = H^{\bullet}( \text{symp vrtx eqns on } \Sigma\_g) = \operatorname{Sym}^{\bullet}(H^{\bullet}(\Sigma\_g))
\end{equation}

By mirror symmetry, this is equivalent to

\begin{equation\*}
 Z\_{1, \mathbf{C}}^B ( \Sigma\_g)\_{\mathrm{Roz-Witten}}= H\_{ \overline{\partial}}^{0,0}\left(T^{\*} \mathbf{C} , \bigwedge^{\bullet} (T^{(1,0)\*})^{\oplus g}  T^{\*} \mathbf{C}\right).
\end{equation\*}

And

\begin{equation}
\label{eq:52}
Z\_{G,V}^A( S^2) \simeq \mathbf{C}[ \mathcal{M}\_C],\quad Z\_{G,V}^B(S ^2) \simeq  \mathbf{C}[ \mathcal{M}\_{H}]
\end{equation}

One can get the first equivalence by expressing \\( S^2 \\) as a raviolo.

How does one compute \\( Z\_{G,V}^B(S ^2) \\)?
Like

\begin{align\*}
Z\_{G,V}^B(S ^2) &= H\_{ \overline{\partial}}^{0,0}( \text{solns to }Q\_B \text{ eqns of motion on }S^2) / G\_{ \mathbf{C}}
\end{align\*}

The \\( Q\_B \\) equations of motion are flatness equations.
We have a flat \\( G\_{ \mathbf{C}} \\) connection, and flat sections of an associated \\( T^{\*}V \\)-bundle such that \\( \mu\_{ \mathbf{C}}  \\) vanishes (all assuming a fixed complex structure).
On \\( S^2 \\), flat connections are trivial, so solutions are constant.
So this entirely comes down to \\( \mathbf{C} [ \mu\_{\mathbf{C}}(0)] / G\_{ \mathbf{C}} = T^{\*} V / / G\_{ \mathbf{C}} \\).


#### Example {#example}

Consider \\( G = U(1) \\) acting on \\( \mathbf{C}^n \\) with weights \\( 1 \\) in each summand.
So \\( W = T^{\*} \mathbf{C}^{n} \\).
The Higgs branch \\( \mathcal{M}\_H \\) ends up being the minimal nilpotent branch on \\( \operatorname{SO}(n) \\), and has resolution that looks like \\( O\_{\mathrm{sln}}^{\mathrm{min}}\leftarrow T^{\*} \mathbb{P}^{n-1} \\).
The Coulomb branch \\( \mathcal{M}\_C \\) looks like \\( T^{\*} \mathbf{C}  / \mathbf{Z}/n \\).


### Mirror Symmetry {#mirror-symmetry}

Suppose \\( G \\) acting on \\( V \\) is an abelian theory (e.g. \\(  G = U(1)^r \\), \\( V = \mathbf{C}^n \\)) with weights encoded in an integral matrix.
If \\( G \\) acts faithfully, there is a \\( (n-r) \times n \\) matrix inducing a SES

\begin{equation}
\label{eq:53}
0 \to \mathbf{Z}^r \to \mathbf{Z}^n \to \mathbf{Z}^{n-r} \to 0
\end{equation}

The mirror theory is the action of \\( G^{!} = U(1)^{n-r} \\) acting on \\( V^{!} = \mathbf{C}^n \\) with weights \\( \tau^T \\).
This theory has a SES dual to the one of the original.


## Lecture 14: Thu Jun 15 13:05:02 2023 {#lecture-14-thu-jun-15-13-05-02-2023}

Last time we say \\( \mathbf{C} [h]\to Z(S ^2)^{S^1} \\) as a module where \\( |h| =2 \\).
When \\( h =0 \\), this recovers \\( Z(S ^2) \\), which is a \\( P\_3 \\)-algebra, which means it has a Poisson bracket of degree \\( 2 \\) and a commutative product, and it satisfies the Leibniz rule.
Generally \\( Z( S ^2)^{S^1} \\) is an \\( E\_1 \\)-algebra, which is an ordinary homotopy associative algebra which is not usually commutative.
We learned that it had the property that when we take the Lie bracket of two elements \\( [a,b] \\), we get \\( h \left\\{ -,- \right\\} \\) the old Poisson bracket.

In the \\( A \\)-twist, \\( Z^{A}(S^1)^{S^1} = H^{S^1}( \operatorname{Maps}( \mathbb{B} , N /G)) \\), which had some other possibly nicer ways to write down.


### Quantizing the \\( B \\)-twisted Higgs branch {#quantizing-the-b-twisted-higgs-branch}

We want to ask _what is the analogue in the \\( B \\)-twist?_
We saw last lecture that \\( Z(S^2) = \mathbf{C}[ \mathcal{M}\_H] / G\_{ \mathbf{C}}  \\) the space of maps on the Higgs branch modulo gauge transformation.
Where \\( \mathcal{M}\_H = \mathcal{M}\_{ \mathbf{C}} ^{-1}(0) \\).
What is the noncommutative version \\( Z^B(S^2)^{S^1} \\)?

In order to study function on \\( W = T^{\*} N \\), we can use the natural quantization \\( D\_h(N) \to  \mathbf{C} [ T^{\*} N] \\).
We have \\( D\_h( \mathbf{C}^n) = \mathbf{C} [h] \left\langle x\_{1} , \ldots , x\_{n} , \partial\_{1} , \ldots , \partial\_{n} \right\rangle\\) satisfying \\( [\partial\_i , x\_j] = \delta\_{ij}h \\).
We can view this as a subset of \\( \operatorname{End}\_{ \mathbf{C}} ( \mathcal{O}\_N) \\) (the structure sheaf \\( \mathcal{O}\_N \\)) generated by the function on \\( N \\) acting by multiplication and by vector fields.

Suppose we have an action of \\( G \\) on a smooth variety \\( X \\).
By differentiating, we get a map \\( \mathfrak{g} \to T^{\*}X \\).
If we take the universal enveloping algebra \\( U(\mathfrak{g}) \\), get a map \\( U( \mathfrak{g}) \xrightarrow{\mu\_{h}^{\*}}  D\_h(X) \\).
The bracket in the enveloping algebra satisfies \\( [v\_1,  v\_2] = \left[ v\_1,v\_2 \right]\_{\mathrm{Lie}} h \\).
It is a fact that \\( \mu\_0^{\*} = \mu^{\*} \\).

Now \\( Z^B(S^2)^{S^1} \cong \left( D\_h(N) / D\_h(N)\mu^{\*}\_h(\mathfrak{g}) \right)^G \\).
The second term in \\( \mathbf{C}\_{h}[ \mathcal{M}\_H] \\).


## Lecture 15: Fri Jun 16 10:44:46 2023 {#lecture-15-fri-jun-16-10-44-46-2023}

We will talk about \\( Z(S^1) \\).
Justin will talk about \\( Z(\*) \\) of a point, then for the last lecture we&rsquo;ll wrap up.
In 3d TQFTs in physics \\( Z(S^1) \\) is called the _category of line operators_.
It is a linear category, the objects of this category &ldquo;compactify holes on surfaces&rdquo;.
To every surface with firmed up holes, it assigns a vector space.
In particular, \\( Z(S^2 \setminus \left\\{ N,S \right\\}) \\) labeled by \\( L\_1, L\_2 \\), defines \\( \operatorname\*{Hom}(L\_1, L\_2) \\).
In other words, if you think of this as a cobordism between circles, this is the \\( \operatorname\*{Hom}(-,-) \\) functor.

Moreover \\( Z(S^1)  \\) is an \\( E\_2 \\)-category, AKA a braided tensor category.
\\( Z(D \setminus D \sqcup D) : Z(S^1) \times  Z(S^1) \to Z(S^1) \\) defines the tensor product.
The braiding comes from the 3d cobordism which is a big ball with cylinders drilled out crossing over inside the ball.
Finally, \\( Z(S^1) \\) has a distinguished identity object \\( \mathbb{1} \\) for the tensor product defined as \\( Z( disk) \\), which is called the &ldquo;identity line&rdquo; or &ldquo;trivial line&rdquo;.
You can view the disk as a cobordism between the empty \\( 1 \\)-manifold and the circle (the cup construction) which defines a map \\( \mathbf{C} \to \mathbb{1} \\).

We&rsquo;ve talked about circle actions on local operators which we took equivariants of.
Similarly we can &ldquo;quantize&rdquo; the category and deform it from an \\( E\_2 \\)-category to just a category.

We can recover \\( Z( \mathrm{all surfaces, 3-mans}) \\) from \\( Z(S^1) \\).

\begin{align\*}
Z(S^2) &= Z(D^2\cup D^2) \\\\
&= \operatorname\*{Hom}\_{Z(S^1)}(Z(D^2), Z(D^2)) \\\\
&= \operatorname{End}( \mathbb{1}) \\\\
&= \mathbf{C} [ \mathcal{M}\_{\mathrm{vac}}].
\end{align\*}

Applying Yoneda, we can use \\( \operatorname\*{Hom}\_{Z(S^1)}( -, \mathbb{1}) : Z(S^1) \to \mathbf{C}[ \mathcal{M}\_{\mathrm{Vac}}]-mod \\) is  \\( Z(S^1) \\) maps to \\( D^b \mathsf{Coh}(\mathcal{M}\_{\mathrm{vac}}^{\mathrm{aff}}) \\).
If \\( \mathcal{M}\_{\mathrm{vac}} \\) is smooth, this should be an equivalence.

\begin{align\*}
Z(T^2)&= Z(\text{ cylinder w ends identified})\\\\
&= \text{"trace of Hom"} \\\\
&= HH\_0 Z(S^1).
\end{align\*}

The Hochschild homology of the category.

From \\( G,W \\) gauge theories

1.  Roughly solve \\( A,B \\)-twisted eqns of motions (look for \\( Q\_A \\) or \\( Q\_B \\) fixed points) on the circle and quantize
    1.  \\( A: \\) take a Fukaya category (roughly speaking)
    2.  \\( B \\): take coherent sheaves (roughly speaking)


#### B Twist {#b-twist}

Suppose we have a Lie group \\( G \\) acting on \\( T^{\*}V = V \oplus  V^{\*} \\).
\\( V \\) will have column vectors \\( x^i \\) and \\( V^{\*} \\) will have row vectors \\( y\_i \\).
\\( \phi : G \to U(V) \\).
Remember that

\begin{align\*}
Q\_B(pt) &=0 \implies \mathcal{F}\_{\mathcal{A}}=0 \text{ for complexified }G\_{\mathbf{C}}- \text{connection}\\\\
d\_{\mathcal{A}} \vec{X} &= d\_{ \mathcal{A} }\vec{Y} = 0 \\\\
\mu\_{\mathcal{M}}(X,Y) &= \rho^{\*}(X,Y)=0.
\end{align\*}

This is all replaced with complexified \\( G\_{\mathbf{C}} \\) gauge transformations to avoid the additional reality condition.

In a neighborhood of \\( S^1 \\), the only info in \\( \mathcal{A} \\) is the holonomy.
Fix a basepoint \\( p\in S^1 \\), let \\( g = \operatorname{Hol}\_{S^1\_p}( \mathcal{A} )\in G\_{\mathbf{C}} \\).

Let \\( X\_p , Y\_p \\) be the values at \\( p \\).
Being flat implies \\( g X\_p = X\_p, Y\_p g ^{-1} = Y\_p \\).
Then we still have the moment map constraint \\( \mu\_{\mathbf{C}}=0 \\).
These remaining equations are equivalent to \\( dW =0 \\) taking the critical locus of a function \\( W : G\_{ \mathbf{C}} \times V \times V^{\*} \to \mathbf{C} \\) taking \\( (g,X,Y) \mapsto Y\cdot (\rho(g)-1) X \\).

So solutions to equations of motion on a circle are equivalent to function \\( G\_{ \mathbf{C}} \times  V \times V^{\*} \to \mathbf{C} \\) such that \\( dW=0 \\) modulo certain gauge transformations.
The next step is to take coherent sheaves on this locus.

**Claim:** \\( Z\_{G,V}^B \simeq \operatorname{MF}^G (G\_{\mathbf{C}} \times V \times V^{\*}, W) \\) the category of matrix factorizations.

When \\( G=1, V= \mathbf{C} \\),

\begin{equation}
\label{eq:54}
Z\_{G,V}^B(S^1) =  D^b \mathsf{Coh}(T^{\*}  \mathbf{C}).
\end{equation}

The unit object \\( \mathbb{1} = \mathcal{O}\_{T^{\*} \mathbf{C}} \\) the structure sheaf, with \\( \operatorname{End}( \mathbb{1} ) = \mathbf{C}[T^{\*} \mathbf{C}] \\).
And
\begin{equation\*}
Z(T^2) = HH<sub>&bull;</sub>(D^b \mathsf{Coh}(T<sup>\*</sup> \mathbf{C})) = \mathbf{C}\left[X,Y, \frac{\partial}{\partial  X} , \frac{\partial}{\partial Y}\right]
\end{equation\*}.


#### A-twist {#a-twist}

Let \\( G,V \\) be a gauge theory again.
Recall the equations of motion in the \\( A \\)-twist are Dirac equations.
In a neighborhood of \\( S^1 \\), \\( D^{\*} = \operatorname{Spec} \mathbf{C}((t)) \\).
Solutions to \\( Q\_A \\) EOM on \\( D^{\*} \\) are \\( \simeq T^{\*} \left( V((z)) / G\_{\mathbf{C}}((z)) \right) \\).
So

\begin{equation}
\label{eq:55}
Z^A\_{G,V} (S^1) := D-mod^{ G\_{\mathbf{C}}((z)) }(V((z)))
\end{equation}

the category of \\( D \\)-modules on the loop space.

This category has basic objects labeled by \\( L,H \\) where \\( L \subset V((z)) \\) a subspace of the algebraic loop space and \\( H \subset G((z)) \\) is a subgroup that stabilizes \\( L \\).
The category has

\begin{multline\*}
\operatorname\*{Hom}((L,H), (L', H'))= H\_{\bullet}(L'/H' \times \_{V((z))/G((z)) L/H}) \\\ = H'\backslash H\_{\bullet} \left\\{ (X,X',g)\in L \times  L' \times G\_{\mathbf{C}}((z))\mid X' = gX \right\\} / H
\end{multline\*}

where \\( H\_{\bullet} \\) is Borel-Moore homology.

The unit object

\begin{equation}
\label{eq:56}
\mathbb{1} = (V[[z]] , G\_{\mathbf{C}}[[z]])
\end{equation}

And \\( \operatorname{End}(\mathbb{1}) = \mathbf{C} [ \mathcal{M}\_C] \\).

**Theorem:** \\( Z\_{1, \mathbf{C}}^A (S^1) \simeq  \\) a de Rham version of the \\( B \\)-twisted category \\( Z\_{U(1), \mathbf{C}}^B(S^1) \\).

**Theorem:** Abelian \\( G \\) acting on \\( V \\) faithfully has

\begin{align\*}
\label{eq:57}
Z\_{G,V}^B (S^1)^{\text{fin supp on }G\_{\mathbf{C}}} &\simeq D^B( \text{VOA modules}) \\\\
&\simeq \text{other A category fo VOA modules} \subset Z^A\_{G',V'}(S^1).
\end{align\*}

At present no analogous mirror symmetry statements about nonabelian gauge theories are known.
