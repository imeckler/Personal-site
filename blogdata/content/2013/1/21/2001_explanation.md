| title: 2001 Frames: A Markov model movie
| author: Izaak Meckler <izaakmeckler@me.com>
| published: 2013-01-21T11:20:46-0500
| updated: 2013-01-21T11:20:46-0500


If I say the word "swimming" to you, you've got a fair bit of information about what word I'm going to say next. Most likely I'll say "pool", but I might say "is" or any other verb, or something else. Conversely I'm probably not going to say "banana" or "golf" after "swimming". By examining a large chunk of text, for any word w in the text, we can produce a probability distribution on what a random word following w will be. With all these distributions in hand, we can build a new text as follows: Randomly choose some word. From the distribution on words that follow this word, randomly select the next word. Now with the distribution on words that follow this new word, randomly select another word, and continue in this manner for as long as you like. The object underlying this technique is called a [Markov chain](http://en.wikipedia.org/wiki/Markov_chain), here's a good intuitive explanation:

 > A Markov Chain describes things that change in a way that has no memory.  What happens in the future doesn’t depend on what happened in the past.  Picture a drunk, staggering home after a night out.  Each step he takes is in a random direction.  He might recognise the local shop, and walk towards it – but if he gets lost, and finds the shop again, there is nothing to stop him making the same mistake twice and walking in circles – because he can’t remember where he’s been, but only where he is.  
 > [Source](http://thinkingdan.wordpress.com/2008/07/)


People sometimes use Markov Chains to generate [poetry](http://juliesayseverythingisinteresting.blogspot.com/2008/10/markov-chain-generated-poetry.html), [absurdist jokes](http://computationalhumor.tumblr.com/), or [nonsensical text](http://en.wikipedia.org/wiki/Atlanta_Nights#Preparation). I used the same basic idea to generate movies by looking at the colors of frames from *2001: A Space Odyssey*. I.e., given that the color of the current frame is c, what is the probability distribution on the color of the following frame. Specifically, I discretized the HSV space into 144 by dividing hue and saturation each into 12 parts, and tagged images with the bucket which a plurality of their pixels fell into. Then I computed \\(freq(Frame_{i + 1} = x \\mid Frame_i = C)\\) for each color bucket \\(C\\) and used those frequencies to train the Markov chain. I also tried using average color, but I found the results weren't as good. This movie, *2001 Frames* (actually more like 1998 to fit with the music), is a single run of a markov chain trained in this manner on frames from *2001: A Space Odyssey*. I hope you enjoy it.

<iframe src="http://player.vimeo.com/video/57761586" width="500" height="225" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe> <p><a href="http://vimeo.com/57761586">2001 Frames</a> from <a href="http://vimeo.com/user11410194">Izaak Meckler</a> on <a href="http://vimeo.com">Vimeo</a>.</p>

The code is [on Github](https://github.com/imeckler/hiddenmarkovmovie).