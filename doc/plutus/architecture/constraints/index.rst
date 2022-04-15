.. highlight:: haskell
.. _plutus-ledger-constraints:

Design of `plutus-ledger-constraints`
=====================================

This document describes the design of the `plutus-ledger-constraints` library.
More specifically, it describes

* how the constraint-based API is used to generate Plutus scripts and transactions.
* the rational behind the selected approach
* possible alternative approaches

Specification
-------------

We define the constraints with the following data types.

.. code-block:: haskell

  data TxConstraints i o =
      TxConstraints
          { txConstraints :: [TxConstraint]
          , txOwnInputs   :: [ScriptInputConstraint i]
          , txOwnOutputs  :: [ScriptOutputConstraint o]
          }

  data TxConstraint =
        MustHashDatum DatumHash Datum
      -- ^ The transaction's datum witnesses must contain the given 'DatumHash'
      -- and 'Datum'. Useful when you already have a 'DatumHash' and
      -- want to make sure that it is the actual hash of the 'Datum'.
      | MustIncludeDatum Datum
      -- ^ Like 'MustHashDatum', but the hash of the 'Datum' is computed automatically.
      | MustValidateIn POSIXTimeRange
      -- ^ The transaction's validity range must be set with the given 'POSIXTimeRange'.
      | MustBeSignedBy PaymentPubKeyHash
      -- ^ The transaction must add the given 'PaymentPubKeyHash' in its signatories.
      | MustSpendAtLeast Value
      -- ^ The sum of the transaction's input 'Value's must be at least as much as
      -- the given 'Value'.
      | MustProduceAtLeast Value
      -- ^ The sum of the transaction's output 'Value's must be at least as much as
      -- the given 'Value'.
      | MustSpendPubKeyOutput TxOutRef
      -- ^ The transaction must spend the given unspent transaction public key output.
      | MustSpendScriptOutput TxOutRef Redeemer
      -- ^ The transaction must spend the given unspent transaction script output.
      | MustMintValue MintingPolicyHash Redeemer TokenName Integer
      -- ^ The transaction must mint the given token and amount.
      | MustPayToPubKeyAddress PaymentPubKeyHash (Maybe StakePubKeyHash) (Maybe Datum) Value
      -- ^ The transaction must create a transaction output with a public key address.
      | MustPayToOtherScript ValidatorHash Datum Value
      -- ^ The transaction must create a transaction output with a script address.
      | MustSatisfyAnyOf [[TxConstraint]]

  data ScriptInputConstraint a =
      ScriptInputConstraint
          { icRedeemer :: a -- ^ The typed 'Redeemer' to be used with the target script
          , icTxOutRef :: TxOutRef -- ^ The UTXO to be spent by the target script
          }

  data ScriptOutputConstraint a =
      ScriptOutputConstraint
          { ocDatum :: a -- ^ Typed datum to be used with the target script
          , ocValue :: Value
          }

The `TxConstraints` can be combined with its `Semigroup` instance.
Values of `TxConstraints` are constructed using smart constructors like `mustBeSignedBy`, `mustPayToTheScript`, `mustSpendAtLeast`, etc. which are available in Ledger.Constraints.TxConstraints_.

.. code-block:: haskell

  checkScriptContext
      :: forall i o. ToData o
      => TxConstraints i o
      -> ScriptContext
      -> Bool

  data ScriptLookups a =
      ScriptLookups
          { slMPS                  :: Map MintingPolicyHash MintingPolicy
          -- ^ Minting policies that the script interacts with
          , slTxOutputs            :: Map TxOutRef ChainIndexTxOut
          -- ^ Unspent outputs that the script may want to spend
          , slOtherScripts         :: Map ValidatorHash Validator
          -- ^ Validators of scripts other than "our script"
          , slOtherData            :: Map DatumHash Datum
          -- ^ Datums that we might need
          , slPaymentPubKeyHashes  :: Map PaymentPubKeyHash PaymentPubKey
          -- ^ Public keys that we might need
          , slTypedValidator       :: Maybe (TypedValidator a)
          -- ^ The script instance with the typed validator hash & actual compiled program
          , slOwnPaymentPubKeyHash :: Maybe PaymentPubKeyHash
          -- ^ The contract's payment public key hash, used for depositing tokens etc.
          , slOwnStakePubKeyHash   :: Maybe StakePubKeyHash
          -- ^ The contract's stake public key hash (optional)
          } deriving stock (Show, Generic)
            deriving anyclass (ToJSON, FromJSON)

  mkTx
      :: ( FromData (DatumType a)
         , ToData (DatumType a)
         , ToData (RedeemerType a)
         )
      => ScriptLookups a
      -> TxConstraints (RedeemerType a) (DatumType a)
      -> Either MkTxError UnbalancedTx

Example
-------

Here's a complete example using constraints.
We want to create a Plutus app that locks some Ada in a script output and splits them evenly between two recipients.

Let's start by declaring the required imports.

.. literalinclude:: BasicAppConstraints.hs
   :start-after: BLOCK0
   :end-before: BLOCK1

We will then declare the ``SplitData`` datatype which describes the two recipients of the funds, and the total amount of the funds denoted in Ada.

.. literalinclude:: BasicAppConstraints.hs
   :start-after: BLOCK1
   :end-before: BLOCK2

Based on this datatype, we will start by creating a function which will generate constraints for:

1- the validator script which will validate the spending of it's script output based on ``SplitData``

2- the transaction we will create to spend these script outputs

.. literalinclude:: BasicAppConstraints.hs
   :start-after: BLOCK2
   :end-before: BLOCK3

We can then define the validator script.

.. literalinclude:: BasicAppConstraints.hs
   :start-after: BLOCK3
   :end-before: BLOCK4

With this validator script, we can create two types of transactions:

1- transactions which lock Ada to the script address

2- transactions which unlock Ada that has been locked to the script address

The function below handles case 1.
Note that we only show the transaction creation step, i.e. not the balance, sign and submit steps.

.. literalinclude:: BasicAppConstraints.hs
   :start-after: BLOCK4
   :end-before: BLOCK5

Then with case 2, to unlock the funds, we can use the same constraints as the validator script.

.. literalinclude:: BasicAppConstraints.hs
   :start-after: BLOCK5
   :end-before: BLOCK6

Rationale
---------

In order to write and interact with Plutus script on the Cardano blockchain, one needs to:

* write a Plutus script which validates certain actions (like spending a transaction script output or minting a currency)
* create and submit transactions which create or spend a transaction output with the script's address.

Plutus scripts are written using Haskell, but there are multiple options for creating transactions.
The most notable option is to use the `cardano-cli` component of `cardano-node` to create, balance, sign and submit transactions.
`cardano-cli` provides a low-level way to create transactions in any era (Byron, Shelley, Alonzo, etc.).

However, `cardano-cli` is a very low-level way of creating transactions, especially when using the `cardano-api` Haskell library (`cardano-cli` is a wrapper on `cardano-api`).
On top of that, the user that created the transaction needs to make sure that it will be successfully validated with the related Plutus script.

Thus, we propose a high-level library to create transactions using a constraint-based API, which generates both a Plutus script and a transaction given a set of constraints.

Requirements
^^^^^^^^^^^^

We have a number of requirements that we need to fulfil.

* users should be able to describe transactions using predefined constraints.
* transaction creation should be limited to the latest Cardano era.
* constraints should be able to generate a Plutus script as well as an unbalanced transaction

Argument
^^^^^^^^

For the `TxConstraints` defined above, it's data constructors should only contain the information needed by the Plutus script, i.e. to validate the transaction.
The arguments behind this design choice are that:

* less information is needed to validate the spending of a transaction output, than to build the transaction itself
* not all datatypes are permitted to be used in Plutus scripts (only builtin functions and data types from `plutus-tx` can be used)
* the more data we include in Plutus scripts, the higher the execution cost. TODO: Is this true? Or is this only true if we *actually* use this additionnal data in the validation code?

For example, let's take one the data constructors of `TxConstraint`: `MustSpendAtLeast Value`.
When generating validation code, this constraint returns `True` when the sum of the transaction's input `Value` are greater than the provided `Value`.
However, when using this constraint to generate a transaction, it will:

1- sum up the `Value` of the transaction outputs.
2- add the missing `Value` through a new transaction output if the sum is less than the provided `Value`

Then, it will need to know who pays (`PaymentPubKeyHash`) for that transaction output.
Thus, this `PaymentPubKeyHash` needs to be provided through some other mean.
In our case, it will be provided through a lookups value.

Limitations
^^^^^^^^^^^

There is one major drawback to this approach: the lookups value in transaction creation step.
To create a transaction, you need to provide the required lookups that are needed by the constraints.
The problem is that there is nothing linking a specific constraint to a specific lookup.
More spefically, there is nothing in the API (apart from reading the documentation) which prevents the users from providing an unrequired lookup, or forgetting to provide a required lookup.
The result is a runtime error, and not a compile error.

To illustrate the point, there is nothing preventing the user from doing the following:

.. code-block:: haskell

  mkMyTx1 :: PaymentPubKeyHash -> TxOutRef -> Either MkTxError UnbalancedTx
  mkMyTx1 pkh ref =
    let lookups = mempty -- Missing lookup for the transaction output reference
     in mkTx lookups (mustSpendPubKeyOutput ref)

  mkMyTx2 :: PaymentPubKeyHash -> TxOutRef -> Either MkTxError UnbalancedTx
  mkMyTx2 pkh ref =
    let lookups = ownPaymentPubKeyHash pkh -- Unnecessary lookup
     in mkTx lookups (mustSpendPubKeyOutput ref)

Both of these functions will return a `Left` value.

Alternatives
^^^^^^^^^^^^

Integrating lookups in the constraints
""""""""""""""""""""""""""""""""""""""

The data constructors of `TxConstraint` could contain all the needed information for both the Plutus script and the transaction creation.

For example, we could replace `TxConstraint` with:

.. code-block:: haskell

  data TxConstraint =
      MustBeSignedBy PaymentPubKeyHash (Maybe PaymentPubKey)
      MustSpentPubKeyOutput TxOutRef ChainIndexTxOut
      MustSpendAtLeast PaymentPubKeyHash Value
      -- ... (OTHER CONSTRAINTS)

On the upside:

* the lookups are integrated in the constraints itself for the transaction creation.

On the downside:

* some of these additionnal types cannot be used in Plutus scripts such as `ChainIndexTxOut`.
* some of these additionnal types might increase cost of Plutus scripts. TODO: to validate
* the users has the burden of handling runtime errors such as not finding the `ChainIndexTxOut` of a `TxOutRef` from an external source.

Distinct isomorphic constraints
"""""""""""""""""""""""""""""""

Instead of a single `TxConstraint` which is used for both Plutus scripts and creating transactions, we can create two distinct and isomorphic datatypes. TODO: Not sure if isomorphic is the right word here.

For example, we could replace `TxConstraint` with:

.. code-block:: haskell

  data ValidationTxConstraint =
      MustBeSignedBy PaymentPubKeyHash
      MustSpentPubKeyOutput TxOutRef
      MustSpendAtLeast Value
      -- ... (OTHER CONSTRAINTS)

  data MkTxConstraint =
      MustBeSignedBy PaymentPubKeyHash (Maybe PaymentPubKey)
      MustSpentPubKeyOutput TxOutRef ChainIndexTxOut
      MustSpendAtLeast PaymentPubKeyHash Value
      -- ... (OTHER CONSTRAINTS)

On the upside:

* the lookups are integrated in the constraints itself for the transaction creation.
* we only include the minimum amount of required information in the Plutus script

On the downside:

* the user has the burden of maintining a correspondance between Plutus scripts constraints and transaction creation constraints.
* the users has the burden of handling runtime errors such as not finding the `ChainIndexTxOut` of a `TxOutRef` from an external source.

.. _Ledger.Constraints.TxConstraints: ../../../../../plutus-ledger-constraints/src/Ledger/Constraints/TxConstraints.hs
