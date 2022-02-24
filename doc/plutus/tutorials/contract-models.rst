.. highlight:: haskell
.. _contract_models_tutorial:

Testing Plutus Contracts with Contract Models
=============================================

Introduction
------------

In this tutorial we will see how to test Plutus contracts with
*contract models*, using the framework provided by
:hsobj:`Plutus.Contract.Test.ContractModel`. This framework generates
and runs tests on the Plutus emulator, where each test may involve a
number of emulated wallets, each running a collection of Plutus
contracts, all submitting transactions to an emulated blockchain.
Once the user has defined a suitable model, then QuickCheck can
generate and run many thousands of scenarios, taking the application
through a wide variety of states, and checking that it behaves
correctly in each one. Once the underlying contract model is in place,
then the framework can check user-defined properties specific to the
application, generic properties such as that no funds remain locked in
contracts for ever, and indeed both positive and negative
tests---where positive tests check that the contracts allow the
intended usages, and negative tests check that they do *not* allow
unintended ones.

The `ContractModel` framework is quite rich in features, but we will
introduce them gradually and explain how they can best be used.

Basic Contract Models
---------------------

Example: A Simple Escrow Contract
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We begin by showing how to construct a model for a simplified escrow
contract, which can be found in
:hsobj:`Plutus.Contracts.Tutorial.Escrow`. This contract enables a
group of wallets to make a predetermined exchange of tokens, for
example selling an NFT for Ada. There are two endpoints, a ``pay``
endpoint, and a ``redeem`` endpoint. Each wallet pays in its
contribution to the contract using the ``pay`` endpoint, and once all
the wallets have done so, then any wallet can trigger the
predetermined payout using the ``redeem`` endpoint.

For simplicity, we will begin by testing the contract for a fixed set
of predetermined payouts. These are defined by the ``EscrowParams``, a
type exported by the escrow contract, and which is actually passed to
the on-chain validators. :hsobj:`Plutus.Contract.Test` provides ten
emulated wallets for use in tests, ``w1`` to ``w10``; in this case we
will use five of them:

.. literalinclude:: Escrow.hs
   :start-after: START testWallets
   :end-before: END testWallets

Let us decide arbitrarily that ``w1`` will receive a payout of 10 Ada,
and ``w2`` will receive a payout of 20, and define an ``EscrowParams``
value to represent that:

.. literalinclude:: Escrow.hs
   :start-after: START escrowParams
   :end-before: END escrowParams

The Contract Model Type
^^^^^^^^^^^^^^^^^^^^^^^

In order to generate sensible tests, and to decide how they should
behave, we need to track the expected state of the system. The first
step in defining a contract model is to define a type to represent
this expected state. We usually need to refine it as the model
evolves, but for now we keep things simple.

In this case, as wallets make payments into the escrow, we will need
to keep track of how much each wallet has paid in. So let us define an
``EscrowModel`` type that records these contributions. Once the
contributions reach the targets, then the escrow may be redeemed, so
let us keep track of these targets in the model too. We define

.. literalinclude:: Escrow.hs
   :start-after: START EscrowModel
   :end-before: END EscrowModel
                  
Note that we use `lenses <http://hackage.haskell.org/package/lens>`_
to access the fields of the model. This is why the field names begin
with an underscore; the ``makeLenses`` call creates lenses called just
``contributions`` and ``targets`` for these fields, which we will use
to access and modify the fields below.

We turn this type into a contract model by making it an instance of
the ``ContractModel`` class:

.. literalinclude:: Escrow.hs
   :start-after: START ContractModelInstance
   :end-before: END ContractModelInstance

The rest of the contract model is provided by defining the methods and
associated data types of this class.

What contracts shall we test?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In general, a contract model can be used to test any number of
contracts, of differing types, running in any of the emulated
wallets. But we need to tell the framework *which* contracts we are
going to test, and we need a way for the model to refer to each
*contract instance*, so that we can invoke the right endpoints. We do
so using a ``ContractInstanceKey``, but since different models will be
testing different collections of contracts, then this type is not
*fixed*, it is defined as part of each model, as an associated type of
the ``ContractModel`` class.

In this case only one kind of contract is involved in the tests, the
escrow contract, but there will be many instances of it, one running
in each wallet. To identify a contract instance, a
``ContractInstanceKey`` just has to record the wallet it is running
in, we only need one constructor in the type. In general there will be
one constructor for each type of contract instance in the test.

.. literalinclude:: Escrow.hs
   :start-after: START ContractInstanceKeyType
   :end-before: END ContractInstanceKeyType

Note that the ``ContractInstanceKey`` type is a GADT, so it tracks not only the
model type it belongs to, but also the type of the contract instance
it refers to.

The framework also needs to be able to show and compare
``ContractInstanceKey``, so you might expect that we would add a
deriving clause to this type definition. But a deriving clause is
actually not supported here, because the type is a GADT, so instead we
have to give separate 'standalone deriving' declarations outside the
``ContractModel`` instance:

.. literalinclude:: Escrow.hs
   :start-after: START ContractInstanceKeyDeriving
   :end-before: END ContractInstanceKeyDeriving


Defining ``ContractInstanceKey`` is only part of the story: we also
have to tell the framework how to *interpret* the contract instance
keys, in particular


#.  which contract instances to start
#.  which emulated wallets to run them in
#.  which actual contract each contract instance should run.


We do so by defining three methods in the ``ContractModel`` class:


.. literalinclude:: Escrow.hs
   :start-after: START ContractKeySemantics
   :end-before: END ContractKeySemantics

The first line above tells the test framework to start a contract
instance in each of the test wallets (with contract parameter ``()``),
the second line tells the framework which wallet each contract key
should run in, and the third line tells the framework which contract
to run for each key--in this case, the same ``testContract`` in each
wallet. ``Spec.Tutorial.Escrow`` does not actually export a complete
concrete, only contract endpoints, so for the purposes of the test we
just define a contract that invokes those endpoints repeatedly:

.. literalinclude:: Escrow.hs
   :start-after: START testContract
   :end-before: END testContract


What actions should tests perform?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The type of Actions
'''''''''''''''''''

The final *type* we need to define as part of a contract model tells
the framework what *actions* to include in generated tests. This is
defined as another associated datatype of the ``ContractModel`` class,
and in this case, we will just need actions to invoke the two contract
endpoints:

.. literalinclude:: Escrow.hs
   :start-after: START ActionType
   :end-before: END ActionType

The framework needs to be able to show and compare
``Action`` too, but in this case we *can* just add a ``deriving``
clause to the definition.

Performing Actions
''''''''''''''''''

QuickCheck will generate sequences of ``Action`` as tests, but in
order to *run* the tests, we need to specify how each action should be
performed. This is done by defining the ``perform`` method of the
``ContractModel`` class, which maps ``Action`` to a computation in the
emulator. ``perform`` takes several parameters besides the ``Action`` to
perform, but for now we ignore all but the first, whose purpose is to
translate a ``ContractInstanceKey``, used in the model, into a
``ContractHandle``, used to refer to a contract instance in the
emulator. The ``perform`` method is free to use any ``EmulatorTrace``
operations, but in practice we usually keep it simple, interpreting
each ``Action`` as a single call to a contract endpoint. This gives
QuickCheck maximal control over the interaction between the tests and
the contracts. In this case, we just call either the ``pay`` or the
``redeem`` endpoint.

.. literalinclude:: Escrow.hs
   :start-after: START perform
   :end-before: END perform

Notice that we *do* need to allow each ``Action`` time to complete, so
we include a ``delay`` to tell the emulator to move on to the next
slot after each endpoint call. Of course we are free *not* to do this,
but then tests will submit many endpoint calls per slot, and fail
because the endpoints are not ready to perform them. This is not the
most interesting kind of test failure, and so we avoid it by delaying
an appropriate number of slots after each endpoint call. The number of
slots we need to wait varies from contract to contract, so we usually
determine these numbers experimentally. Exactly the same problem
arises in writing unit tests, of course.

Modelling Actions
'''''''''''''''''

Remember that we need to track the real state of the system using the
contract model state? We defined a type for this purpose:

.. literalinclude:: Escrow.hs
   :start-after: START EscrowModel
   :end-before: END EscrowModel

We need to tell the framework what the *effect* of each ``Action`` is
expected to be, both on wallet contents, and in terms of the model state. We do
this by defining the ``nextState`` method of the ``ContractModel``
class, which just takes an ``Action`` as a parameter, and interprets
it in the ``Spec`` monad, provided by the ``ContractModel`` framework.

.. literalinclude:: Escrow.hs
   :start-after: START 0nextState
   :end-before: END 0nextState

You can see that the ``Spec`` monad allows us to withdraw and deposit
values in wallets, so that the framework can predict their expected
contents, and also to read and update the model state using the lenses
generated from its type definition. For a ``Pay`` action, we withdraw
the payment from the wallet, and record the contribution in the model
state, using ``(%=)`` to update the ``contributions`` field. For a
``Redeem`` action, we read the targets from the model state (using
``viewContractState`` and the lens generated from the type
definition), and then make the corresponding payments to the wallets
concerned. In both cases we tell the model to ``wait`` one slot,
corresponding to the ``delay`` call in ``perform``; this is necessary
to avoid the model and the emulator getting out of sync.

We also have to specify the *initial* model state at the beginning of
each test: we just record that no contributions have been made yet,
along with the targets we chose for testing with.

.. literalinclude:: Escrow.hs
   :start-after: START initialState
   :end-before: END initialState

Given these definitions, the framework can predict the expected model
state after any sequence of ``Action``.

Generating Actions
^^^^^^^^^^^^^^^^^^

The last step, before we can actually run tests, is to tell the
framework how to *generate* random actions. We do this by defining the
``arbitraryAction`` method, which is just a QuickCheck generator for
the ``Action`` type. It gets the current model state as a parameter,
so we can if need be adapt the generator depending on the state, but
for now that is not important: we just choose between making a payment
from a random wallet, and invoking ``redeem`` from a random
wallet. Since we expect to need several payments to fund a redemption,
we generate ``Pay`` actions a bit more often than ``Redeem`` ones.

.. literalinclude:: Escrow.hs
   :start-after: START arbitraryAction1
   :end-before: END arbitraryAction1

Strictly speaking the framework now has enough information to generate
and run tests, but it is good practice to define *shrinking* every
time we define *generation*; we just defined a generator for actions,
so we should define a shrinker too. We do so by defining the
``shrinkAction`` method, which, like the QuickCheck ``shrink``
function, just returns a list of smaller ``Action`` to try replacing
an action by when a test fails. It is always worth defining a
shrinker: the small amount of effort required is repaid *very*
quickly, since failed tests become much easier to understand.

In this case, as in most others, we can just reuse the existing
shrinking for those parts of an ``Action`` that make sense to
shrink. There is no sensible way to shrink a wallet, really, so we
just shrink the amount in a payment. 

.. literalinclude:: Escrow.hs
   :start-after: START shrinkAction
   :end-before: END shrinkAction

With this definition, failing test cases will be reported with the
*minimum* payment value that causes a failure.


Running tests and debugging the model
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We are finally ready to run some tests! We do still need to define a
*property* that we can call QuickCheck with, but the ``ContractModel``
framework provides a standard one that we can just reuse. So we define

.. literalinclude:: Escrow.hs
   :start-after: START prop_Escrow
   :end-before: END prop_Escrow

The important information here is in the type signature, which tells
QuickCheck *which* contract model we want to generate and run tests
for.

A failing test
''''''''''''''

Once the property is defined, we are ready to test--and a test fails
immediately! This is not unexpected--it is quite rare that a model and
implementation match on the first try, so we should expect a little
debugging--*of the model*--before we start to find interesting bugs in
contracts. When models are written *after* the implementation, as in
this case, then the new code--the model code--is likely to be where
bugs appear first.

Looking at the test output, the first thing QuickCheck reports is the
failed test case:

.. code-block:: text

   Prelude Spec.Tutorial.Escrow Test.QuickCheck Main> quickCheck prop_Escrow
   *** Failed! Assertion failed (after 7 tests and 2 shrinks):
   Actions 
    [Redeem (Wallet 5)]

Here we see what generated tests looks like: they are essentially
lists of ``Action``, performed in sequence. In this case there is only
one ``Action``: wallet 5 just attempted to redeem the funds in the
contract.

The next lines of output tell us why the test failed:
    
.. code-block:: text

   Expected funds of W[2] to change by
     Value (Map [(,Map [("",20000000)])])
   but they did not change
   Expected funds of W[1] to change by
     Value (Map [(,Map [("",10000000)])])
   but they did not change

Remember we defined the expected payout to be 10 Ada to ``w1``, and 20
Ada to ``w2``. Our model says (in ``nextState``) that when we perform
a ``Redeem`` then the payout should be made (in Lovelace, not Ada,
which is why the numbers are a million times larger than those in the
model). But the wallets did not get the money--which is hardly
surprising since no payments at all have been made *to* the contract,
so there is no money to disburse.

The remaining output displays a log from the failing contract
instance, and the emulator log, both containing the line
   
.. code-block:: text

    Contract instance stopped with error: RedeemFailed NotEnoughFundsAtAddress

This is an error thrown by the off-chain ``redeem`` endpoint code,
which (quite correctly) checks the funds available, and fails since there
are not enough.

Positive testing with preconditions
'''''''''''''''''''''''''''''''''''

We now have a failing test, that highlights a discrepancy between the
model and the implementation--and it is the model that is wrong. The
question is how to fix it, and there is a choice to be made. Either we
could decide that the ``nextState`` function in the model should
*check* whether sufficient funds are available, and if they are not,
predict that no payments are made. Or perhaps, we should *restrict our
tests so they do not attempt to use ``Redeem`` when it should not
succeed*.

Both choices are reasonable. The first alternative is usually called
*negative testing*--we deliberately test error situations, and make
sure that the implementation correctly detects and handles those
errors. The second alternative is *positive testing* (or "happy path"
testing), when we make sure that the implementation provides the
functionality that it should, when the user makes *correct* use of its
API.

It is usually a good idea to focus on positive testing first--indeed,
good positive testing is a prerequisite for good negative testing,
because it enables us to get the system into a wide variety of
interesting states (in which to perform negative tests). So we shall
return to negative testing later, and focus--in this section--on
making positive testing work well.

To do so, we have to *restrict* test cases, so that they do not
include ``Redeem`` actions, when there are insufficient funds in the
escrow. We restrict actions by defining the ``precondition`` method of
the ``ContractModel`` class: any ``Action`` for which ``precondition``
returns ``False`` will *not* be included in any generated test. The
``precondition`` method is also given the current ``ModelState`` as a
parameter, so that it can decide to accept or reject an ``Action``
based on where it appears in a test.

In this case, we want to *allow* a ``Redeem`` action only if there are
sufficient funds in the escrow, so we just need to compare the
contributions made so far to the targets:

.. literalinclude:: Escrow.hs
   :start-after: START precondition1
   :end-before: END precondition1

In this code, ``s`` is the entire model state maintained by the
framework (including wallet contents, slot number etc), but it
contains the "contract state", which is the state we have defined
ourselves, the ``EscrowModel``. The *lens* ``contractState
. contributions . to fold`` extracts the ``EscrowModel``, extracts the
``contributions`` field from it, and then combines all the ``Value``
using ``fold``. When we apply it to ``s`` using ``(^.)``, then we get
the total value of all contributions. Likewise, the second lens
application computes the combined value of all the targets. If the
contributions exceed the targets, then the ``Redeem`` is allowed;
otherwise, it will not be included in the test. Once
we define ``precondition``, then it has to be defined for every form
of ``Action``, so we just add a default branch that returns ``True``.

.. note::
   
   We can't use ``(>=)`` to compare ``Value``; there is no
   ``Ord`` instance. That is because some ``Value`` are incomparable,
   such as one Ada and one NFT, which would break our expectations about
   ``Ord``. That is why we have to compare them using ``geq`` instead.

With this precondition, the failing test we have seen can no longer be
generated, and will not appear again in our ``quickCheck`` runs.

A second infelicity in the model
''''''''''''''''''''''''''''''''

Adding a precondition for ``Redeem`` prevents the previous failing
test from being generated, but it does not make the tests pass: it
just allows QuickCheck to reveal the next problem in the
model. Running tests again, we see:

.. code-block:: text

   Prelude Spec.Tutorial.Escrow Test.QuickCheck Main> quickCheck prop_Escrow
   *** Failed! Assertion failed (after 4 tests and 5 shrinks):
   Actions 
    [Pay (Wallet 2) 0]

This time the test just consists of a single ``Pay`` action, making a
payment of zero (!) Ada to the the contract.

.. note::

   It may seem surprising that the test tries to make a zero payment,
   given that the *generator* we saw above only generates payments in
   the range 1 to 30 Ada. But remember that the failing test cases we
   see are not necessarily freshly generated, they may also have been
   *shrunk*. In this case, the zero is a result of shrinking: the
   shrinker we saw can certainly shrink payments to zero, and the
   *precondition* for ``Pay`` allows that... it's always ``True``. And
   so, a zero payment can appear in tests. If we wanted to prevent
   this, the correct way would be to tighten the precondition of
   ``Pay``.

The next part of the output explains why the test failed:
    
.. code-block:: text
                
   Expected funds of W[2] to change by
     Value (Map [])
   but they changed by
     Value (Map [(,Map [("",-2000000)])])
   a discrepancy of
     Value (Map [(,Map [("",-2000000)])])
  
In other words, the model expected that a payment of zero would not
affect the funds held by the calling wallet, but in fact, the wallet
lost 2 Ada.

Why did this happen? In this case, the emulator log that follows
provides an explanation:

.. code-block:: text
     
   .
   .
   .
   [INFO] Slot 1: W[2]: Balancing an unbalanced transaction:
          Tx:
            Tx 2dc052b47a1faeacc0f50b99359990302885a34104df0109576597cc490b8a98:
              {inputs:
              collateral inputs:
              outputs:
                - Value (Map [(,Map [("",2000000)])]) addressed to
                  ScriptCredential: bcf453ff769866e23d14d5104c36ce4da0ff5bcbed23c622f46b94f1 (no staking credential)
              mint: Value (Map [])
              fee: Value (Map [])
              mps:
              signatures:
              validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
              data:
                "\128\164\244[V\184\141\DC19\218#\188L<u\236m2\148<\b\DEL%\v\134\EM<\167"}
          Requires signatures:
          Utxo index:
          Validity range:
            (-? , +?)
   .
   .
   .

We see the transaction submitted by the contract, and we can see from
its outputs that it *is* paying 2 Ada to the script, even though we
specified a payment of zero. The reason for this is that the Cardano
blockchain requires a *minimum Ada amount* in every transaction
output, currently 2 Ada. It is therefore *impossible* to make a
payment of zero Ada to the script--and the Plutus libraries avoid this
by adding Ada to each output, if necessary, to meet the minimum
requirement. It is these 2 Ada that the wallet has lost.

This is not really a bug in the escrow contract: it's a fundamental
limitation enforced by the blockchain itself. Therefore we must adapt
our model to allow for it. Once again we have a choice: we *could*
specify that every ``Pay`` action costs at least the minimum Ada, even
if the ``Action`` contains a lower payment, *or* we can restrict
``Pay`` actions to amounts greater than or equal to the minimum. We
choose the latter, because it is simpler to express--we just tighten
the precondition for ``Pay``:

.. literalinclude:: Escrow.hs
   :start-after: START precondition2
   :end-before: END precondition2

Now this failure can no longer appear.

.. note::

   It's debatable whether the contract's behaviour is actually buggy
   or not. We decided to accept it, and exclude payments smaller than
   2 Ada from our tests. But of course, a *user* of the contract might
   attempt to make a payment of, say, 1 Ada--nothing prevents
   that. Such a user will see their wallet debited with 2 Ada, and may
   be surprised by that behaviour. Arguably the contract should
   explicitly *reject* payments below the minimum, rather than
   silently increase them. So this failing test does expose this
   issue.

A third infelicity in the model, and a design issue
'''''''''''''''''''''''''''''''''''''''''''''''''''

Now that we have reasonable preconditions for each ``Action`` in a
test, we can expect to see more interesting failures. And indeed, the
tests still fail, but now with a test case that combines payment and
redemption:

.. code-block:: text
                
   Prelude Spec.Tutorial.Escrow Test.QuickCheck Main> quickCheck prop_Escrow
   *** Failed! Assertion failed (after 8 tests and 5 shrinks):
   Actions 
    [Pay (Wallet 4) 11,
     Pay (Wallet 5) 20,
     Redeem (Wallet 5)]
   Expected funds of W[5] to change by
     Value (Map [(,Map [("",-20000000)])])
   but they changed by
     Value (Map [(,Map [("",-19000000)])])
   a discrepancy of
     Value (Map [(,Map [("",1000000)])])

Here we made two payments, totalling 31 Ada, *which is exactly one Ada
more than the combined targets* (recall our targets are 10 Ada to
``w1`` and 20 Ada to ``w2``). Then ``w5`` redeemed the escrow, *and
ended up with 1 Ada too much* (last line). That extra Ada is, of
course, the extra unnecessary Ada that was paid to the script in the
previous action.

This raises the question: what *should* happen if an escrow holds more
funds than are needed to make the target payments? The designers of
this contract decided that any surplus should be paid to the wallet
submitting the ``redeem`` transaction. Since this is part of the
intended behaviour of the contract, then our model has to reflect
it. We can do so with a small extension to the ``nextState`` function
in the model:

.. literalinclude:: Escrow.hs
   :start-after: START nextState1
   :end-before: END nextState1

The extra code just computes the total contributions and the surplus,
and deposits the surplus in the calling wallet.

Now, at last, the tests pass!

.. code-block:: text

   Prelude Spec.Tutorial.Escrow Test.QuickCheck Main> quickCheck prop_Escrow
   +++ OK, passed 100 tests.

By default, quickCheck runs 100 tests, which is enough to reveal
easily-caught bugs such as those we have seen, but far too few to
catch really subtle issues. So at this point, it's wise to run many
more tests--the number is limited only by your patience and the speed
of the emulator:

.. code-block:: text

   Prelude Spec.Tutorial.Escrow Test.QuickCheck Main> quickCheck . withMaxSuccess 1000 $ prop_Escrow
   +++ OK, passed 1000 tests.
   
Analysing the distribution of tests
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Once tests are passing, then the framework displays statistics
collected from the running tests. These statistics give us important
information about the *effectiveness* of our tests; a risk with any
automated test case generation is that, since we do not usually
inspect the running tests, we may not notice if almost all of them
are trivial.

The contract model framework gathers some basic statistics by default,
and can be configured to gather more, but for now we just consider the
built-in ones. After each successful test run, we see a number of
tables, starting with a distribution of the actions performed by
tests:

.. code-block:: text

   Prelude Spec.Tutorial.Escrow Test.QuickCheck Main> quickCheck . withMaxSuccess 1000 $ prop_Escrow
   +++ OK, passed 1000 tests.
   
   Actions (25363 in total):
   75.894% Pay
   12.771% Redeem
   11.335% WaitUntil

Here we ran 1,000 tests, and as we see from the table, around 25,000
actions were generated. So, on average, each test case consisted of
around 25 actions.

Of those actions, three quarters were ``Pay`` actions, and 10-15% were
``Redeem``. This is not unreasonable--we decided when we wrote the
``Action`` generator to generate more payments than redemptions. The
remaining actions are ``WaitUntil`` actions, inserted by the
framework, which simply wait a number of slots to test for timing
dependence; we shall return to them later, but can ignore them for
now. Thus this distribution looks quite reasonable.

The second table that appears tells us how often a *generated*
``Action`` could not be included in a test, because its *precondition*
failed.

.. code-block:: text

   Actions rejected by precondition (360 in total):
   87.8% Redeem
   12.2% Pay
                
We can see that 360 actions--in addition to the 25,000 that were
included in tests--were generated, but *discarded* because their
preconditions were not true. This does represent wasted generation
effort, although rejecting 360 out of over 25,000 actions is not
really a serious problem--especially given that test case generation
is so very much faster than the emulator.

Nevertheless, we can see that the vast majority of rejected actions
were ``Redeem`` actions, and this is because a ``Redeem`` is not
allowed until sufficient payments have been made--but our generator
produces them anyway.

We can, of course, change this, to generate ``Redeem`` actions only
when redemption is actually possible:

.. literalinclude:: Escrow.hs
   :start-after: START arbitraryAction2
   :end-before: END arbitraryAction2

Measuring the distribution again after this change, we see that only
valid ``Redeem`` actions are now generated; the only discarded actions
are ``Pay`` actions.

.. code-block:: text
                
   Prelude Spec.Tutorial.Escrow Test.QuickCheck Main> quickCheck . withMaxSuccess 1000 $ prop_Escrow
   +++ OK, passed 1000 tests.
   
   Actions (25693 in total):
   76.717% Pay
   13.035% Redeem
   10.248% WaitUntil
   
   Actions rejected by precondition (650 in total):
   100.0% Pay

The main *disadvantage* of making this change is that it limits the
tests that *can* be generated, if the precondition of ``Redeem``
should be changed in the future. In particular, when we move on to
negative testing, then we will want to test invalid attempts to redeem
the escrow also. Once the generator is changed like this, then
relaxing the precondition is no longer enough to introduce invalid
calls. For this reason it could be preferable to *keep* the
possibility of generation invalid calls alongside the generator for
valid calls, but to assign the potentially-invalid generator a much
lower weight.

We will discuss the remaining tables in a later section.

Exercises
^^^^^^^^^

You can find the final version of the contract model discussed in this
section in ``Spec.Tutorial.Escrow1``, in the ``plutus-apps`` repo.

#. Try running the code in ``ghci``. You can do so by starting a
   ``nix`` shell, and starting ``ghci`` using

   .. code-block:: text
                   
      cabal repl plutus-use-cases-test

   Then import QuickCheck and the contract model:

   .. code-block:: text
                   
      import Test.QuickCheck
      import Spec.Tutorial.Escrow1

   and run tests using

   .. code-block:: text
                   
      quickCheck prop_Escrow

   The tests should pass, and you should see tables showing the
   distribution of tested actions, and so on.

#. Try removing the preconditions for ``Pay`` and ``Redeem``, and
   reinserting them one by one. Run ``quickCheck`` after each change,
   and inspect the failing tests that are generated.

#. Try removing the line

   .. code-block:: text
                   
      deposit w leftoverValue

   from the ``nextState`` function, and verify that tests fail as
   expected.

#. Try removing one of the lines

   .. code-block:: text

      wait 1

   from the ``nextState`` function (so that the model and the
   implementation get out of sync). What happens when you run tests?

#. This model does generate ``Pay`` actions that are discarded by the
   precondition. Adjust the generator so that invalid ``Pay`` actions
   are no longer generated, and run ``quickCheck`` to verify that this
   is no longer the case.


Parameterising Models and Dynamic Contract Instances
----------------------------------------------------

One of the unsatisfactory aspects of the tests developed in the
previous section is that they *always* pay 10 Ada to wallet 1, and 20
Ada to wallet 2. What if the contract only works for certain amounts,
or what if it only works with exactly two beneficiary wallets? Of
course, we would like to *generate* a random set of payment targets
for each test. Such a generator is easy to write:

.. literalinclude:: Escrow2.hs
   :start-after: START arbitraryTargets
   :end-before: END arbitraryTargets

but it is a little more intricate to make the model *use* these
generated targets.

There are two problems to overcome:

#. The generated targets are an important part of a test case, *so they
   must be included in the test case* somehow. But a test case is just
   a list of actions. So where do we put the targets?

#. The running contracts need to know what the targets are--but our
   model just contains a static list of contract instances in the test
   (``initialInstances``). How can we pass the generated targets to
   each running contract instance?


Solve these two problems, and we can test escrows with arbitrary payout targets. The techniques we learn will be applicable in many other situations.

Adding an initial action, and test case phases
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The first problem is quite easy to solve, in principle. The generated
payment targets are an important part of a test case, and a test case
is just a list of actions, therefore the generated payment targets
must be included in one or more of the actions. Quite simply, *any
generated data in a contract model test must be part of an action*. In
this case, we just decide that every test should begin with an
``Init`` action, that specifies the targets to be used in this test
case. So we must extend the ``Action`` type to include ``Init``:

.. literalinclude:: Escrow2.hs
   :start-after: START Action
   :end-before: END Action

We must also ensure that ``Init`` actions *only* appear as the first
action of a test case, and that every test case starts with an
``Init`` action. We restrict the form of test cases using
preconditions, so this means that we must refine the ``precondition``
function so that the ``Init`` precondition only holds at the beginning
of a test case, and the other operations' preconditions only hold
*after* an ``Init`` has taken place.

However, the ``precondition`` method is only given the action and the
contract state as parameters, which means in turn that we must be able
to tell whether or not we are at the beginning of the test case, just
from the model state. So we have to add a field to the model, to keep
track of where in a test case we are. In this simple case we could
just add a boolean ``initialised``, but we will be a little more
general and say that a test case is made up of a number of *phases*,
in this case just two, ``Initial`` and ``Running``:

.. literalinclude:: Escrow2.hs
   :start-after: START ModelState
   :end-before: END ModelState

Now we can specify that at the beginning of a test case we are in the
``Initial`` phase, and there are no targets:

.. literalinclude:: Escrow2.hs
   :start-after: START initialState
   :end-before: END initialState

and that when we model the ``Init`` action, we update both the phase
and the targets accordingly:

.. literalinclude:: Escrow2.hs
   :start-after: START nextState
   :end-before: END nextState

We have to specify how to perform an ``Init`` action also, but in this
case it exists only to initialise the model state with generated
targets, so performing it need not do anything:
                
.. literalinclude:: Escrow2.hs
   :start-after: START perform
   :end-before: END perform

Now we can add a precondition for ``Init``, and restrict the other
actions to the ``Running`` phase only:

.. literalinclude:: Escrow2.hs
   :start-after: START precondition
   :end-before: END precondition
             
It only remains to *generate* ``Init`` actions, using the
generator for targets that we saw above. We can take the phase into
account, and generate an ``Init`` action only at the start of the test
case, and other actions only in the ``Running`` phase.

.. literalinclude:: Escrow2.hs
   :start-after: START arbitraryAction
   :end-before: END arbitraryAction

.. note::

   Here we ensure that we always *generate* test cases that begin with
   ``Init``, but this is *not* enough to ensure that every test case
   we *run* begins with ``Init``. Remember that failed tests are
   always shrunk, and the first thing the shrinker will try is to
   discard the leading ``Init`` action (if that still results in a
   failing test, which it probably will). The only way to prevent
   shrinking from discarding the leading ``Init`` is for the
   *preconditions* to require it to be there. This is why we focussed
   on writing the preconditions first: they are more important.

As a matter of principle, when we write a generator, we also write a shrinker, which just requires a one-line addition to the ``shrinkAction`` function:

.. literalinclude:: Escrow2.hs
   :start-after: START shrinkAction
   :end-before: END shrinkAction

We cannot shrink wallets, which is why we can't simply apply
``shrink`` to the list of targets, but using the ``shrinkList``
function from ``Test.QuickCheck`` we can easily write a shrinker that
will discard list elements and shrink the target values.

Dynamic contract instances
^^^^^^^^^^^^^^^^^^^^^^^^^^

At this point we can generate tests that begin by initialising the
escrow targets randomly, but we cannot yet run them successfully. If we try, we see failures like this:

.. code-block:: text

   *** Failed! Assertion failed (after 11 tests and 5 shrinks):
   Actions 
    [Init [],
     Redeem (Wallet 1)]
   Contract instance log failed to validate:
   ...
   Slot 1: 00000000-0000-4000-8000-000000000000 {Wallet W[1]}:
             Contract instance stopped with error: RedeemFailed NotEnoughFundsAtAddress
   ...             

Here we started a test with an empty list of targets, and tried to
redeem the escrow, but failed because there were 'not enough
funds'. Why not? Because *the contracts we are running still expect
the fixed targets* that we started with; we have not yet passed our generated
targets to the contract instances under test.

Recall the contract we are testing:

.. literalinclude:: Escrow.hs
   :start-after: START testContract
   :end-before: END testContract
                

It invokes the contract endpoints with the fixed set of ``EscrowParams``
we defined earlier. Clearly we need to parameterise the contract on
these ``EscrowParams`` instead:

.. literalinclude:: Escrow2.hs
   :start-after: START testContract
   :end-before: END testContract

Now the question is: how do we pass this parameter to each ``testContract`` as we start them?

Recall the way we started contracts in the previous section. We
defined the contracts to start at the beginning of a test in the
``initialInstances`` method:
                
.. literalinclude:: Escrow.hs
   :start-after: START initialInstances
   :end-before: END initialInstances

                

Each contract is specified by a ``StartContract``, containing not only
a contract instance key, but also a *parameter*--in this case ``()``,
since we did not need to pass any generated values to
``testContract``. Now we do need to, so we must replace that ``()``
with escrow parameters generated from our payment targets. Moreover,
we can no longer start the contracts at the beginning of the test--we
must see the ``Init`` action first, so that we know what the generated
targets are. To do so, we redefine

.. literalinclude:: Escrow2.hs
   :start-after: START initialInstances
   :end-before: END initialInstances

and instead add a definition of the ``startInstances`` method:

.. literalinclude:: Escrow2.hs
   :start-after: START startInstances
   :end-before: END startInstances

where the escrow parameters are now constructed from our generated targets:

.. literalinclude:: Escrow2.hs
   :start-after: START escrowParams
   :end-before: END escrowParams

The effect of this is to start the contracts *just before* the
``Init`` action; in fact, using this mechanism, we can start contracts
dynamically at any point in a test case.                

.. note::
                
   We should be careful to avoid reusing the same contract instance
   key more than once, though, since this may lead to confusing
   results.

   You may wonder why we don't simply start new contract instances in the
   ``perform`` method instead. The answer is the framework needs to track
   the running contract instances, and using ``startInstances`` makes
   this explicit.
                
The ``StartContract`` just specifies the ``ContractInstanceKey`` to be
started; we define the actual contract to start in the
``instanceContract`` method, *which receives the contract parameter*
from ``StartContract`` as its last argument. So we can just define

.. literalinclude:: Escrow2.hs
   :start-after: START instanceContract
   :end-before: END instanceContract

and our work is (almost) done. The last step is just to update the
*type* of ``WalletKey``, since it includes the type of the parameter
that ``StartContract`` accepts.

.. literalinclude:: Escrow2.hs
   :start-after: START ContractInstanceKey
   :end-before: END ContractInstanceKey

Now, at last, our extended model is complete.
                
Running our extended tests; another design issue
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We can now run our new tests, and, as so often happens when the scope
of QuickCheck tests is extended, they do not pass. Here is an example
of a failure:

.. code-block:: text

   Actions 
    [Init [(Wallet 5,0)],
     Redeem (Wallet 4)]
   Expected funds of W[4] to change by
     Value (Map [])
   but they changed by
     Value (Map [(,Map [("",-2000000)])])
   a discrepancy of
     Value (Map [(,Map [("",-2000000)])])
   Expected funds of W[5] to change by
     Value (Map [])
   but they changed by
     Value (Map [(,Map [("",2000000)])])
   a discrepancy of
     Value (Map [(,Map [("",2000000)])])
   Test failed.

In this case the generated target just specifies that wallet 5 should
receive 0 Ada--a slightly odd target, perhaps, but not obviously
invalid. Since the total of all targets is 0 Ada, then the target is
already met, and wallet 4 attempts to redeem the escrow. We might
expect the effect to be a no-op--and this is what our model
predicts--but it is not what happens. Instead, *wallet 4 pays two Ada
to wallet 5*!

The reason this happens is the blockchain rule that every transaction
output must contain at least 2 Ada. When wallet 4 attempts to redeem
the escrow, then the off-chain code attempts to create a transaction
with an output paying 0 Ada to wallet 5, but that is increased to 2
Ada to make the transaction valid. Then when the transaction is
balanced, the 2 Ada is taken from the submitting wallet.

Is this a bug in the contract? It is certainly an inconsistency with
the ``nextState`` function in the model, and we could modify that
function to reflect the *actual* transfers of Ada that the contract
performs. But these transfers were surely not intentional: a more
reasonable approach is to say that target payments that are too small
to be accepted by the blockchain are invalid; such targets should not
be chosen.

We can make our tests pass by tightening the precondition of ``Init``
so that targets below the minimum are not accepted:

.. literalinclude:: Escrow2.hs
   :start-after: START tightprecondition
   :end-before: END tightprecondition

This demonstrates that the contract works as expected, provided we
*don't* specify targets less than the minimum, but nothing prevents a
*user* of the contract from specifying such targets--and we know that
the contract code will accept them, and deliver surprising results in
those cases. Arguably *all* the contract endpoints should check that
the specified targets are valid, and raise an error if they are
not. This would prevent the *creation* of invalid escrows, rather than
generating unexpected behaviour when they are redeemed.

Thus these failing tests *do* suggest a way in which the contract
implementation can be improved, even if the failing cases are fairly
unlikely in practice.

.. note::

   QuickCheck was able to find this bug because our *generator* for
   target payments includes invalid values; we chose values in the
   range 1 to 31, where 1 is invalid (and shrinking reduced the 1 to a
   0 in the failing case that was reported). It is a good thing we did
   not ensure, from the start, that only valid target values could be
   generated--had we done so, we would not have discovered this
   anomalous behaviour.

   In general, it is a good idea for generators to produce, at least
   occasionally, every kind of input that a user can actually supply,
   even if some of them are invalid (and may be filtered out by
   preconditions). Doing so enables this kind of strange behaviour to
   be discovered.

Exercises
^^^^^^^^^

#. You will find the code presented here in ``Spec.Tutorial.Escrow2``\,
   with the exception of the last precondition we discussed for
   ``Init``\. Run the tests using

   .. code-block:: text

                   quickCheck prop_Escrow

   and make sure you understand how they fail.

#. Make your own copy of the code, and add the tighter precondition
   for ``Init``\. Verify that the tests then pass.

#. An alternative explanation for the problem might have been that a
   target of *zero* should not be allowed (and perhaps the contract
   implementation should interpret a target of zero by not creating a
   transaction output at all). *Change the precondition* of ``Init``
   so that it only excludes targets of zero, rather than any target
   below the minimum. Verify that the tests still fail, and make sure
   you understand the (slightly more complex) failure.

#. There are quite a few steps involved in introducing these
   dynamically chosen targets. You can practice these steps by taking
   the code from ``Spec.Tutorial.Escrow1``\, which uses fixed targets,
   and following the steps outlined in this tutorial to turn it into a
   copy of ``Spec.Tutorial.Escrow2``.


No Locked Funds
---------------

Take the Escrow example with no Refund. Show NoLockedFunds proof: just
complete the escrow and withdraw. But requires cooperation.

Wallet strategy. Can't work.

Add a Refund action to the contract. Now passes.

EscrowStrict with bad payments: no locked funds fails.

Taking Time into Account
------------------------

Add a deadline to the Escrow contract, and a refund
action. Show that many actions fail (monitoring).

Preconditions. Still fail.
Adapting the generator.
Adding a phase

Crash Tolerance
---------------

Auction contract: pays out automatically.
Strategy: wait until deadline.

Testing crash tolerance. Strategy no longer works.


Measuring coverage of on-chain code
-----------------------------------

Do coverage of Escrow, explain meanings. Motivate negative testing.
Look at the real Escrow; note that payments to scripts are not tested.

   
Negative testing
----------------

Part 1: Bad Actions
^^^^^^^^^^^^^^^^^^^

Idea: include Bad actions that violate preconditions. Precondition is
inverse. nextState is identity.

Generation: apply Bad constructor if precondition fails.  Must ensure
generators are not targetting preconditions precisely; remove
conditions.

Expect to get bad payment? Introduce wellFormed, weaker precondition
for Bad actions.

What if we deal with surplus differently, by forbidding.

Escrow, with preconditions that restrict Redeem and Pay. Negative
testing shows Escrow contract permits wider behaviour.

Part 2: Bad Endpoints
^^^^^^^^^^^^^^^^^^^^^

Use the bad endpoints for Escrow as example.

Dynamically created tokens
--------------------------

Show how to test Escrow with generated tokens.

A larger example: the Uniswap contract
--------------------------------------

Needs symbolic tokens, and wiggle room in "No Locked Funds"

Notes
=====

   

  
  Precondition that there are no extra funds? Very small number of Redeems.
  Model the actual behaviour. Much better distribution.
  eeeeeeeeeeee
