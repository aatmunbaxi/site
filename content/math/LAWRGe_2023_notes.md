+++
title = "LAWRGe 2023 Notes"
author = ["Aatmun Baxi"]
lastmod = 2023-06-12T19:57:21-05:00
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
    - [Gerstenhabev algebra](#gerstenhabev-algebra)
- [Lecture 3: Mon Jun 12 16:07:19 2023](#lecture-3-mon-jun-12-16-07-19-2023)
    - [Goals](#goals)
    - [Field Theories](#field-theories)
    - [Supersymmetric Field Theories](#supersymmetric-field-theories)
    - [Twisting](#twisting)
- [Lecture 4: Mon Jun 12 18:02:13 2023](#lecture-4-mon-jun-12-18-02-13-2023)
    - [Gauge Theories](#gauge-theories)
    - [Hyper-Kahler manifolds](#hyper-kahler-manifolds)

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


### Gerstenhabev algebra {#gerstenhabev-algebra}


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
