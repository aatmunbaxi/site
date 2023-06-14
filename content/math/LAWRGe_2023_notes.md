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
