# historical-streams

# A study of algebraic specification.

## Define a simplified model.

We want to define an indexing data structure that has the capacity of maintaining a current state (viewed as a fold over a stream of events) and which can rewind to a previous value in case a rollback happens on the stream. We only store K (we call it depth) versions of the previous accumulator (since we know that we will only rollback at most K blocks).

The model is split between constructors and observations. You can identify the constructors with a grammar that generates a program in your algebra and observations define the semantics of this defined language.

Example of a program in this algebra: `rewind 1 $ insert 1 $ new (+) 5`. We can think as the value of the accumulator at the end of running this program as an observation which has value `5`.

In our case we have three constructors:
* new => creates a new index, and takes as arguments a function used to fold events into an accumulator and an initial value for the accumulator.
* insert => adds a new event into the index
* rewind => returns the data structure to a previous version.

.. and 3 observations:
* view:
 * depth - how many versions do we remember
 * size - how many versions do we currently store.
 * accumulator value - what is the current value of the accumulator.
* history (the historical accumulator values)
* function (looks up the function used for folding)

One of the mistakes I made initially was to include the possiblity of failing when building syntax (new and review could return `Nothing`). In retrospect this was a bad decision because it would not allow me to test the negative of the property for other data types. I removed all possibilities of errors from the constructors and moved them to the observations.

On this model we identify some properties by specifying how our observations are influenced by the chaining of constructors.

1) *observeNew* talks about what we can observe from viewing or getting the history after a call to `new`. If the depth is negative then all observations should return `Nothing`.

2) *rewindDepth* checks that observations of rewind are only valid when the number of events we want to undo is lower than what we have stored (and by proxy is lower than the maximum depth of the data structure).

3) *rewindInverse* captures that intuition that (under very specific conditions) insert and rewind behave a bit like inverses.

4) *observeInsert* captures the way insert changes observations. This is one of the gripes I've had with testing for a while when you write tests that duplicate the code used to implement the functions you are testing. However, in this case it makes sense, since this property is not used to check our model as much as check other, more complex data structures that we check for conformity with the model.

5) *sizeLEDepth* this is an invariant for the data structure. Size should always be less than the maximum size. I am not sure I want to keep this property. It does not seem very useful, but it's an example of an invariant.

## Define a more complex data type

I define a more efficient data type based on the observation that part of the chain is immutable and the part that is immutable could be stored fully on disk. Then we would only use RAM to store the changes that happened in the mutable part of the blockchain.

The structure is a bit more complex in order to support batching events to be persisted more efficiently.

The way I implemented the verification that this data structure is compliant with the properties identified for the model previously defined is by generalising the properties themselves by providing functions that convert between model programs and observations of the complex data structures.

To test the properties for the initial model we use the `Identity` monad and the view conversion functions are the ones defined by the `Index` module.

For the complex data structure the flow is as follow: given an Index program that has one pure observation derived from the model, it must match the complex observation of the data structure.

## Lessons learned

* The first implementation of the simplified model was not algebraic and it was awkward to think of a more complex data structure as a interpretation of a language. So, when defining a model, thinking in terms of constructors and observations seems very important. Then what we get is something equivalent to a free algebra (hence algebraic specification).
* The process of developing things in this manner is fairly slow and tenuous, but it does force one to contemplate the design choices that were made and think deeply about the meaning of the used data structures (do we need to expose a size or a depth in our views? why? what happens if we do not do that?).
* The process *is* an interative one, where the model changes informed by requirements from the software, and the software changes to satisfy the properties required by the model. Both the model and the implementation have changed several times, and they will probably keep on changing (test don't pass yet for the complex data structure).
* Thinking about these problems has given me a lot more insight into the problem that we are trying to solve and I could identify easily shortcomings, inefficiencies or unwaranted complexity of other approaches.
* The amount of bugs that only 5 properties can detect is impresive. Not all of those were actual reasoning failings, but some were (a couple of bugs were off-by-one errors).
* The amount of bugs that can fit into a more complex than the model, but not really that complex data structure is also quite impressive.
* Finding the cause of property violations is not very fun, but not incredibly annoying either. I think there are improvements to be made in this area.
