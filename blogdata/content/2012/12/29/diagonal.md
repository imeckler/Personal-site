| title: A Beautiful Function
| author: Izaak Meckler <izaakmeckler@me.com>
| published: 2012-12-29T02:55:24-0500
| updated: 2012-12-29T02:55:24-0500

At some point in the near future I'll come back to the enumeration of the integer lattice, and how to compute what a move in the plane corresponds to as a change in indices in the enumeration. For now, I just want to share a beautiful function I wrote in the process of writing a theorem prover (the logical calculus in question being natural deduction, realized as the simply typed lambda calculus). I wanted to generate all terms of a certain type, say \\(T\\). The top level of a term of this type might be an application of some term \\(f : A \\to T\\) to a term \\(x : A\\), where \\(A\\) ranges over all possible types. To generate all such terms requires in some way iterating over all types \\(A\\), all terms \\(x : A\\), and all terms \\(f : A \\to T\\). In Haskell, something along the lines of

<pre class="prettyprint sh_haskell">
appTermsOfTypeN t :: Type -> [[[Term]]]
appTermsOfTypeN t = [ 
	[
		[App f x | f &lt;- termsOfType (AbsTy a t)] 
		| x &lt;- termsOfType a
	]
	| a &lt;- allTypes 
]
</pre>

Hopefully the indentation helps make the structure clearer. `appTermsOfTypeN t` is a list, indexed by a type `a`, of lists, each indexed by a term `x` of type `a`, of lists of every possible function `f` of type `a -> t` applied to `x`. What we want to do is turn this nested structure into a single flat list of all the terms in each sub-sublist. If we do something like `concat . concat $ appTermsOfType t`, there will be infinitely many terms in some sub-sublist that the new flat list will not contain, since there are infinitely many types, and we will get stuck enumerating terms of the first infinite type we encounter in `allTypes`, never enumerating terms of the rest of the types. A correct thing to do here is to iterate over the sublists along the "diagonals", just as in one of the standard proofs of the equinumerosity of \\(\\mathbb{N}\\) and \\(\\mathbb{Q}^2\\).


<div class="image-box">
	<img src="http://www.askamathematician.com/wp-content/uploads/2011/03/countingrationals.jpg">
</div>

The above table can be represented as an infinite list of infinite lists, and the displayed enumeration can be easily (and beautifully) encoded as follows.

<pre class="prettyprint sh_haskell">
diagonalize :: [[a]] -> [a]
diagonalize = go []
    where go active (y:yys) = map head active ++ go (y : map tail active) yys

rationalTable :: [[(Integer, Integer)]]
rationalTable = [[(n, d) | n &lt;- [1..]] | d &lt;- [1..]]

rationalList :: [(Integer, Integer)]
rationalList = diagonalize rationalTable
</pre>

We keep a list of "active" sublists. That is, lists that have an element on the diagonal currently under consideration. In the case of the rational numbers, on the first (non-trivial) run of `go`, we are considering the first diagonal and so the `active` list contains the only list that has an element on this diagonal. On the nth iteration, we consider the nth diagonal, which has elements from the first n lists. The key to how the function works is in the observation that the nth diagonal can correspond to `map head active`, if we have been "shifting" all the currently active lists to the left before the next iteration.

The table below demonstrates the behavior of `go [] rationalTable` for the first few iterations.

<div class="image-box">
	<img src="/static/images/blog/diagonal.png" style="width: 600px;">
</div>

With `diagonalize`, a flat list of terms of a given type, where every possible term appears at some finite position in the list, can be computed as follows.

<pre class="prettyprint sh_haskell">
appTermsOfType :: Type -> [Term]
appTermsOfType = diagonalize . diagonalize . appTermsOfTypeN
</pre>
