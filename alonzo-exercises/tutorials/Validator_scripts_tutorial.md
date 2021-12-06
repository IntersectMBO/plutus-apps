## Validator scripts tutorial

A validator script is used to lock transaction outputs on the chain. It is attached to a script output in the extended UTXO model and determines the address of the output. It must return positively in order for the output to be spent.

The purpose of the validator script is to signal failure. If it fails, it signals that the attempt to spend the output in invalid and therefore the transaction should fail. It indicates failure using the PlutusTx.Builtins.error builtin.

Validator scripts are written as Haskell functions, which are compiled with Plutus Tx into Plutus Core. The type of a validator function is Data -> Data -> Data -> ()- a function which takes three arguments of type Data, and returns a value of type () (“unit” or “the empty tuple”).

### An intuitive understanding of a validator function.

Let’s start with a couple of examples in pseudo code. These are examples of things that need to satisfy a “rule” before performing an “action”.

#### Example 1: Ferris Wheel

For example, a kid wants to go on a ferris wheel, but before getting on, they must be taller than the safety sign.

We could express that idea in pseudo code, like:


```python

if isTallEnough(attraction=ferrisWheel,passenger=michael):
    getOnFerrisWheel()

def isTallEnough(attraction,kid):
    return kid["height"] >= attraction["minimumHeight"]

def getOnFerrisWheel():
    print ("get On the Ferris Wheel")

ferrisWheel = {"minimumHeight":120}
michael = {"height":135}
```


#### Example 2: Drinking age
Another example could be a young person trying to buy a beer in a bar.
But before she can do that, she must first comply with the rule that she must be the legal drinking age in the country where the bar is located.

```python
if mayLegalyDrink(country=USA,person=sarah):
    payBeer()

def mayLegalyDrink(country,person):
    return person["age"] >= country["drinkingAge"]

def payBeer()
    print ("enjoy your beer")

usa = {"drinkingAge":21}
sarah = {"age":18}

```

Given these two examples we can already identify a pattern.

#### Validator function and script as a pattern

An action: `getOnFerrisWheel()`, `payBeer()` needs to pass a rule: `isTallEnough()` , `mayLegalyDrink()`, before being performed. The rule needs two bits of information: some context `attraction`, `country` and some data particular to this instance of the rule `michael`, `sarah`.

Now we can translate those concepts into the names they are given in Plutus. The action to be performed is the `redeemer`, the rule is called the `validator function`, the context is the `Script Context` and the data is called the `datum`.

With that information we could create another program that actually abstracts the previous two. We could call it `performActionGivenARule` but let’s use it’s Plutus name: `makeValidator`.


```python

def makeValidator(validator,context,datum,redeemer):
    return lambda: if validator(context,datum): redeemer()

```

With this function we can now define our previous examples simply as “contracts”.

```python
example1 = makeValidator(validator=isTallEnough
                        ,context=ferrisWheel
                        ,datum=michael
                        ,redeemer=getOnFerrisWheel)
example2 = makeValidator(validator=mayLegalyDrink
                        ,context=usa
                        ,datum=sarah
                        ,redeemer=payBeer)
```


So with all these examples we can create an intuitive definition of a validator.


> A validator is a function that encodes a rule. Its parameters are a Context, state of the world, and a datum, a specific instance of data. It succeeds as long as it doesn’t throw an exception.

Now that we know what a validator function is, let’s define a validator /script/.

> A validator function that has been compiled using `PlutusTX.compile` and template Haskell is known as a validator script. The code hash will be used as the address for the script.

### Validators in Plutus Contracts

Now let’s look at actual Plutus contracts, no more pseudo code, and see how they implement validator functions and validator scripts.

#### Plutus example: Vesting
Imagine we want to give an inheritance to our child. However she may only receive the inheritance when she turns 18. Analyzing the whole example is beyond the scope of this article, but it is presented with detail in the 3rd Lecture of the Plutus Pioneer program, with the complete code, and the lecture notes by the community.


##### Define the validator function

So we need to create a validator function that takes into account the time when the request is being made.

```haskell
{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                         traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
```
Vesting [validator function (click here for complete code)](https://github.com/input-output-hk/plutus-pioneer-program/blob/3a7d675f7b53dcd846a0c286c1f56170d079e3ef/code/week01/src/Week01/EnglishAuction.hs#L102-L123)

##### Transform the validator function to the validator script.
Now we create the validator Script of the validator function.

```haskell
...

typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()
...
```

##### Get the address for the contract
And you can finally do some boiler plate for deployment:

```haskell
...

validator :: Validator
validator = Scripts.validatorScript typedValidator


valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator


scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
...

```

#### Plutus example: English auction

The goal of an English auction is to sell something to the highest bidder. Analyzing the whole example is beyond the scope of this article, but it is presented with detail in the 1st lecture of the Plutus Pioneer program, with the complete code, and the lecture notes by the community.

For a complete review of the program refer to the video lecture, or the excellent notes in .
However we want to focus on two sections:

##### Defining the validator function
So let’s start by defining the validator function that will decide if the action `redeem` is possible given the current context and the datum.


```haskell
{-# INLINABLE mkAuctionValidator #-}
mkAuctionValidator :: AuctionDatum -> AuctionAction -> ScriptContext -> Bool
mkAuctionValidator ad redeemer ctx =
    traceIfFalse "wrong input value" correctInputValue &&
    case redeemer of
        MkBid b@Bid{..} ->
            traceIfFalse "bid too low" (sufficientBid bBid)                &&
            traceIfFalse "wrong output datum" (correctBidOutputDatum b)    &&
            traceIfFalse "wrong output value" (correctBidOutputValue bBid) &&
            traceIfFalse "wrong refund"       correctBidRefund             &&
            traceIfFalse "too late"           correctBidSlotRange
        Close           ->
            traceIfFalse "too early" correctCloseSlotRange &&
            case adHighestBid ad of
                Nothing      ->
                    traceIfFalse "expected seller to get token" (getsValue (aSeller auction) tokenValue)
                Just Bid{..} ->
                    traceIfFalse "expected highest bidder to get token" (getsValue bBidder tokenValue) &&
                    traceIfFalse "expected seller to get highest bid" (getsValue (aSeller auction) $ Ada.lovelaceValueOf bBid)


  where
    info :: TxInfo

```
English Aauction [validator function (click here for complete code)](https://github.com/input-output-hk/plutus-pioneer-program/blob/3a7d675f7b53dcd846a0c286c1f56170d079e3ef/code/week01/src/Week01/EnglishAuction.hs#L102-L123)

Observe that the `makeAuctionValidator` is a function that takes as parameters a datum, redeemer and a context… just like our pseudo code version!

##### Transform the validator function into the validator script.

Now once we have the function we can create the validator *script*. Which as we established before is the validator function that has been compiled through template haskell and inlined in the program.

```haskell

auctionTypedValidator :: Scripts.TypedValidator Auctioning
auctionTypedValidator = Scripts.mkTypedValidator @Auctioning
    $$(PlutusTx.compile [|| mkAuctionValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator
```
English Auction Validator Script

Now as we can see the `mkAuctionValidator` function is compiled with template haskell to plutus core. `PlutusTx.compile [|| … ||]` which in turn gets “spliced” i.e. made available to the current program with `$$(...)`

##### Getting the contract address boilerplate

Once the validator script is created we can calculate what it’s address is going to be:

```haskell

...
auctionValidator :: Validator
auctionValidator = Scripts.validatorScript auctionTypedValidator


auctionAddress :: Ledger.ValidatorHash
auctionAddress = Scripts.validatorHash auctionValidator
...

```

#### Abstracting the validator script as a pattern

With the two previous examples is easy to abstract the pattern needed to create validator scripts:

1. Define a validator function that if doesn’t throw an error means it succeeded.
    * The validator function will receive as parameters the datum, the context and the redeemer (i.e. the action)
1. Transform the validator function into the validator script.
    * This requires the use of template haskell `[|| … |]]` and `PlutusTX.compile`. However it’s always the same pattern.
1. Calculate the contract address in the blockchain
    * It’s always the same boilerplate: calculate the script with: `Scripts.validatorScript typedValidator`
    * Calculate the hash of the typed validator `Script.validatorHash typedValidator`
    * Calculate the address `scriptAddress validator`

### Takeaways

* A validator function:
  * Succeeds as long as it doesn’t throw an error.
  * Requires a Datum (i.e. info about the action), a Redeemer (the action that wants to be performed), and a Context
  * Is compiled to a validator script with PlutusTx.compile and template haskell

* A validator script

  * It’s generated from a validator function
  * The code is not stored on the blockchain, *until* the reedemer transaction actually executes.
  * You need to calculate it’s address with `scriptAddress`
