| title: Haskell for mathematical exploration
| author: Izaak Meckler <izaakmeckler@me.com>
| published: 2012-10-28T13:31:46-0500
| updated: 2012-10-28T13:31:46-0500

For a recent assignment, I had to show the equivalence of Turing machines with a tape infinite in one direction and Turing machines with a two dimensional tape, infinite in all directions, that are able to move up, down, left, or right. The first thing that came to mind was to associate each point in \\(\\mathbb{Z}^2\\) with a natural number as in this picture:

<div class="image-box">
	<img src="/static/images/blog/integerlattice.png">
</div>

Given some point, I wanted to find out how to compute the natural number associated with the point one to the left of it so that I could construct a TM to do so. The first step was to enumerate the points of \\(\\mathbb{Z}^2\\) in the proper order. The first step was to notice that in following the orange path from \\((0, 0)\\), it moves up, then right, then down, then left and then begins the cycle again. Also, the lengths of these runs follow the sequence \\([1, 1, 2, 2, 3, 3,...]\\). Here's how I defined this sequence in Haskell: 

<pre class="prettyprint sh_haskell">
runs = [1..] >>= replicate 2
</pre>

To define a move up, down, left, or right, I used the functions `first` and `second` from `Control.Arrow`. Both these functions take an arbitrary arrow as their argument, but for functions, we essentially have 

<pre class="prettyprint sh_haskell">
first :: (a -> c) -> ((a, b) -> (c, b))
first f = \(x, y) -> (f x, y)

first :: (b -> d) -> ((a, b) -> (a, d))
second f = \(x, y) -> (x, f y)
</pre>

`first` applies a function to the first element of a tuple, `second` to the second. Here's the definition of the movement functions:

<pre class="prettyprint sh_haskell">
up    = second (+ 1)
down  = second (subtract 1)
right = first (+ 1)
left  = first (subtract 1)
</pre>

So for example, `up (x, y) = (x, y + 1)`. Now we need to describe the sequence of movements of the orange line, i.e., `[up, right, down, down, left, left, ...]`. We do so with

<pre class="prettyprint sh_haskell">
concat $ zipWith replicate runs (cycle [up, right, down, left])
</pre>

We create an infinite list of the movement functions, cycling them in the desired order, replicate each of them the corresponding number of times as defined in `runs`, and concatenate the resultant lists.

Having described the infinite sequence of movements we want to make to touch each point of the integer lattice, all that's left to do is feed the list our starting point, \\((0,0)\\).

<pre class="prettyprint sh_haskell">
up    = second (+ 1)
down  = second (subtract 1)
right = first (+ 1)
left  = first (subtract 1)
</pre>
