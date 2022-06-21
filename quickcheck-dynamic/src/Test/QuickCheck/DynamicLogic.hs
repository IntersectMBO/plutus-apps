{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Test.QuickCheck.DynamicLogic
    ( module Test.QuickCheck.DynamicLogic.Quantify
    , DynLogic, DynPred, DynFormula
    , DynLogicModel(..), DynLogicTest(..), TestStep(..)
    , ignore, passTest, afterAny, after, (|||), forAllQ, weight, withSize, toStop
    , done, errorDL, monitorDL, always
    , forAllScripts, forAllScripts_, withDLScript, withDLScriptPrefix, forAllMappedScripts, forAllMappedScripts_
    , forAllUniqueScripts, propPruningGeneratedScriptIsNoop
    ) where

import Data.List
import Data.Typeable

import Control.Applicative

import Test.QuickCheck hiding (generate)

import Test.QuickCheck.DynamicLogic.CanGenerate
import Test.QuickCheck.DynamicLogic.Quantify
import Test.QuickCheck.DynamicLogic.SmartShrinking
import Test.QuickCheck.DynamicLogic.Utils qualified as QC
import Test.QuickCheck.StateModel

-- | Dynamic logic formulae.
data DynLogic s = EmptySpec             -- ^ False
                | Stop                  -- ^ True
                | AfterAny (DynPred s)  -- ^ After any action the predicate should hold
                | Alt ChoiceType (DynLogic s) (DynLogic s)
                  -- ^ Choice (angelic or demonic)
                | Stopping (DynLogic s)
                  -- ^ Prefer this branch if trying to stop.
                | After (Any (Action s)) (DynPred s)
                  -- ^ After a specific action the predicate should hold
                | Weight Double (DynLogic s)
                  -- ^ Adjust the probability of picking a branch
                | forall a. (Eq a, Show a, Typeable a) =>
                    ForAll (Quantification a) (a -> DynLogic s)
                  -- ^ Generating a random value
                | Monitor (Property -> Property) (DynLogic s)
                  -- ^ Apply a QuickCheck property modifier (like `tabulate` or `collect`)

data ChoiceType = Angelic | Demonic
  deriving (Eq, Show)

type DynPred s = s -> DynLogic s

newtype DynFormula s = DynFormula {unDynFormula :: Int -> DynLogic s}
  -- a DynFormula may depend on the QuickCheck size parameter

-- API for building formulae

ignore    :: DynFormula s
passTest  :: DynFormula s
afterAny  :: (s -> DynFormula s) -> DynFormula s
after     :: (Show a, Typeable a, Eq (Action s a)) =>
               Action s a -> (s -> DynFormula s) -> DynFormula s
(|||)     :: DynFormula s -> DynFormula s -> DynFormula s
forAllQ   :: Quantifiable q =>
               q -> (Quantifies q -> DynFormula s) -> DynFormula s
weight    :: Double -> DynFormula s -> DynFormula s
withSize  :: (Int -> DynFormula s) -> DynFormula s
toStop    :: DynFormula s -> DynFormula s

done      :: s -> DynFormula s
errorDL   :: String -> DynFormula s

monitorDL :: (Property -> Property) -> DynFormula s -> DynFormula s

always    :: (s -> DynFormula s) -> (s -> DynFormula s)

ignore       = DynFormula . const $ EmptySpec
passTest     = DynFormula . const $ Stop
afterAny f   = DynFormula $ \n -> AfterAny $ \s -> unDynFormula  (f s) n
after act f  = DynFormula $ \n -> After (Some act) $ \s -> unDynFormula (f s) n
DynFormula f ||| DynFormula g  = DynFormula $ \n -> Alt Angelic (f n) (g n)
                           -- In formulae, we use only angelic
                           -- choice. But it becomes demonic after one
                           -- step (that is, the choice has been made).
forAllQ q f
    | isEmptyQ q' = ignore
    | otherwise   = DynFormula $ \n -> ForAll q' $ ($n) . unDynFormula . f
    where q' = quantify q

weight w f   = DynFormula $ Weight w . unDynFormula f
withSize f   = DynFormula $ \n -> unDynFormula (f n) n
toStop (DynFormula f) = DynFormula $ Stopping . f

done _       = passTest
errorDL s    = DynFormula . const $ After (Error s) (const EmptySpec)

monitorDL m (DynFormula f) = DynFormula $ Monitor m . f

always p s   = withSize $ \n -> toStop (p s) ||| p s ||| weight (fromIntegral n) (afterAny (always p))

data DynLogicTest s = BadPrecondition [TestStep s] [Any (Action s)] s
                    | Looping [TestStep s]
                    | Stuck   [TestStep s] s
                    | DLScript [TestStep s]

data TestStep s = Do (Step s)
                | forall a. (Eq a, Show a, Typeable a) => Witness a

instance Eq (TestStep s) where
    Do s == Do s' = s == s'
    Witness (a :: a) == Witness (a' :: a') =
        case eqT @a @a' of
            Just Refl -> a == a'
            Nothing   -> False
    _ == _ = False

instance StateModel s => Show (TestStep s) where
  show (Do step)   = "Do $ "++show step
  show (Witness a) = "Witness ("++show a++" :: "++show (typeOf a)++")"

instance StateModel s => Show (DynLogicTest s) where
    show (BadPrecondition as bads s) =
        unlines $ ["BadPrecondition"] ++
                  bracket (map show as) ++
                  ["  " ++ show (nub bads)] ++
                  ["  " ++ showsPrec 11 s ""]
    show (Looping as) =
        unlines $ ["Looping"] ++ bracket (map show as)
    show (Stuck as s) =
        unlines $ ["Stuck"] ++ bracket (map show as) ++ ["  " ++ showsPrec 11 s ""]
    show (DLScript as) =
        unlines $ ["DLScript"] ++ bracket (map show as)

bracket :: [String] -> [String]
bracket []  = ["  []"]
bracket [s] = ["  [" ++ s ++ "]"]
bracket (first:rest) = ["  ["++first++", "] ++
                       map (("   "++).(++", ")) (init rest) ++
                       ["   " ++ last rest ++ "]"]

-- Restricted calls are not generated by "AfterAny"; they are included
-- in tests explicitly using "After" in order to check specific
-- properties at controlled times, so they are likely to fail if
-- invoked at other times.

class (Typeable s, StateModel s) => DynLogicModel s where
    restricted :: Action s a -> Bool
    restricted _ = False

forAllUniqueScripts :: (DynLogicModel s, Testable a) =>
                          Int -> s -> DynFormula s -> (Actions s -> a) -> Property
forAllUniqueScripts n s f k =
  QC.withSize $ \sz -> let d = unDynFormula f sz in
  case generate chooseUniqueNextStep d n s 500 [] of
  Nothing   -> counterexample "Generating Non-unique script in forAllUniqueScripts" False
  Just test -> validDLTest d test .&&. (applyMonitoring d test . property $ k (scriptFromDL test))

forAllScripts :: (DynLogicModel s, Testable a) =>
                   DynFormula s -> (Actions s -> a) -> Property
forAllScripts f k =
    forAllMappedScripts id id f k

forAllScripts_ :: (DynLogicModel s, Testable a) =>
                   DynFormula s -> (Actions s -> a) -> Property
forAllScripts_ f k =
    QC.withSize $ \n -> let d = unDynFormula f n in
    forAll (sized $ generateDLTest d) $
        withDLScript d k

forAllMappedScripts ::
  (DynLogicModel s, Testable a, Show rep) =>
    (rep -> DynLogicTest s) -> (DynLogicTest s -> rep) -> DynFormula s -> (Actions s -> a) -> Property
forAllMappedScripts to from f k =
    QC.withSize $ \n -> let d = unDynFormula f n in
    forAllShrink (Smart 0 <$> (sized $ (from<$>) . generateDLTest d))
                 (shrinkSmart ((from<$>) . shrinkDLTest d . to)) $ \(Smart _ script) ->
        withDLScript d k (to script)

forAllMappedScripts_ ::
  (DynLogicModel s, Testable a, Show rep) =>
    (rep -> DynLogicTest s) -> (DynLogicTest s -> rep) -> DynFormula s -> (Actions s -> a) -> Property
forAllMappedScripts_ to from f k =
    QC.withSize $ \n -> let d = unDynFormula f n in
    forAll (sized $ (from<$>) . generateDLTest d) $
        withDLScript d k . to

withDLScript :: (DynLogicModel s, Testable a) => DynLogic s -> (Actions s -> a) -> DynLogicTest s -> Property
withDLScript d k test =
    validDLTest d test .&&. (applyMonitoring d test . property $ k (scriptFromDL test))

withDLScriptPrefix :: (DynLogicModel s, Testable a) => DynFormula s -> (Actions s -> a) -> DynLogicTest s -> Property
withDLScriptPrefix f k test =
    QC.withSize $ \n ->
    let d = unDynFormula f n
        test' = unfailDLTest d test
    in
    validDLTest d test' .&&. (applyMonitoring d test' . property $ k (scriptFromDL test'))

generateDLTest :: DynLogicModel s => DynLogic s -> Int -> Gen (DynLogicTest s)
generateDLTest d size = generate chooseNextStep d 0 (initialStateFor d) size []

generate :: (Monad m, DynLogicModel s)
         => (s -> Int -> DynLogic s -> m (NextStep s))
         -> DynLogic s
         -> Int
         -> s
         -> Int
         -> [TestStep s]
         -> m (DynLogicTest s)
generate chooseNextStepFun d n s size as =
    case badActions d s of
        [] ->
            if n > sizeLimit size then
                return $ Looping (reverse as)
            else do
                let preferred = if n > size then stopping d else noStopping d
                    useStep StoppingStep _ = return $ DLScript (reverse as)
                    useStep (Stepping (Do (var := act)) d') _ =
                      generate chooseNextStepFun
                               d'
                               (n+1)
                               (nextState s act var)
                               size
                               (Do (var := act):as)
                    useStep (Stepping (Witness a) d') _ =
                      generate chooseNextStepFun
                               d'
                               n
                               s
                               size
                               (Witness a:as)
                    useStep NoStep alt = alt
                foldr (\ step k -> do try <- chooseNextStepFun s n step; useStep try k)
                      (return $ Stuck (reverse as) s)
                      [preferred, noAny preferred, d, noAny d]
        bs -> return $ BadPrecondition (reverse as) bs s

sizeLimit :: Int -> Int
sizeLimit size = 2 * size + 20

initialStateFor :: StateModel s => DynLogic s -> s
initialStateFor _ = initialState

stopping :: DynLogic s -> DynLogic s
stopping EmptySpec     = EmptySpec
stopping Stop          = Stop
stopping (After act k) = After act k
stopping (AfterAny _)  = EmptySpec
stopping (Alt b d d')  = Alt b (stopping d) (stopping d')
stopping (Stopping d)  = d
stopping (Weight w d)  = Weight w (stopping d)
stopping (ForAll _ _)  = EmptySpec
stopping (Monitor f d) = Monitor f (stopping d)

noStopping :: DynLogic s -> DynLogic s
noStopping EmptySpec     = EmptySpec
noStopping Stop          = EmptySpec
noStopping (After act k) = After act k
noStopping (AfterAny k)  = AfterAny k
noStopping (Alt b d d')  = Alt b (noStopping d) (noStopping d')
noStopping (Stopping _)  = EmptySpec
noStopping (Weight w d)  = Weight w (noStopping d)
noStopping (ForAll q f)  = ForAll q f
noStopping (Monitor f d) = Monitor f (noStopping d)

noAny :: DynLogic s -> DynLogic s
noAny EmptySpec     = EmptySpec
noAny Stop          = Stop
noAny (After act k) = After act k
noAny (AfterAny _)  = EmptySpec
noAny (Alt b d d')  = Alt b (noAny d) (noAny d')
noAny (Stopping d)  = Stopping (noAny d)
noAny (Weight w d)  = Weight w (noAny d)
noAny (ForAll q f)  = ForAll q f
noAny (Monitor f d) = Monitor f (noAny d)

nextSteps :: DynLogic s -> [(Double, DynLogic s)]
nextSteps EmptySpec     = []
nextSteps Stop          = [(1, Stop)]
nextSteps (After act k) = [(1, After act k)]
nextSteps (AfterAny k)  = [(1, AfterAny k)]
nextSteps (Alt _ d d')  = nextSteps d ++ nextSteps d'
nextSteps (Stopping d)  = nextSteps d
nextSteps (Weight w d)  = [(w*w', s) | (w', s) <- nextSteps d, w*w' > never]
nextSteps (ForAll q f)  = [(1, ForAll q f)]
nextSteps (Monitor f d) = nextSteps d

chooseOneOf :: [(Double, DynLogic s)] -> Gen (DynLogic s)
chooseOneOf steps = frequency [(round (w/never), return s) | (w, s) <- steps]

never :: Double
never = 1.0e-9

data NextStep s = StoppingStep
                | Stepping (TestStep s) (DynLogic s)
                | NoStep

chooseNextStep :: DynLogicModel s => s -> Int -> DynLogic s -> Gen (NextStep s)
chooseNextStep s n d =
    case nextSteps d of
        [] -> return NoStep
        steps -> do
            chosen <- chooseOneOf steps
            case chosen of
                EmptySpec  -> return NoStep
                Stop       -> return StoppingStep
                After (Some a) k ->
                    return $ Stepping (Do $ Var n := a) (k (nextState s a (Var n)))
                AfterAny k -> do
                    m <- keepTryingUntil 100 (arbitraryAction s) $
                          \a -> case a of
                                  Some act -> precondition s act && not (restricted act)
                                  Error _  -> False
                    case m of
                        Nothing -> return NoStep
                        Just (Some a) ->
                            return $ Stepping (Do $ Var n := a)
                                              (k (nextState s a (Var n)))
                        Just Error{} -> error "impossible"
                ForAll q f -> do
                    x <- generateQ q
                    return $ Stepping (Witness x) (f x)
                After Error{} _ -> error "chooseNextStep: After Error"
                Alt{}           -> error "chooseNextStep: Alt"
                Stopping{}      -> error "chooseNextStep: Stopping"
                Weight{}        -> error "chooseNextStep: Weight"
                Monitor{}       -> error "chooseNextStep: Monitor"

chooseUniqueNextStep :: (MonadFail m, DynLogicModel s) => s -> Int -> DynLogic s -> m (NextStep s)
chooseUniqueNextStep s n d =
    case snd <$> nextSteps d of
        []                 -> return NoStep
        [EmptySpec]        -> return NoStep
        [Stop]             -> return StoppingStep
        [After (Some a) k] -> return $ Stepping (Do $ Var n := a) (k (nextState s a (Var n)))
        _                  -> fail "chooseUniqueNextStep: non-unique action in DynLogic"

keepTryingUntil :: Int -> Gen a -> (a -> Bool) -> Gen (Maybe a)
keepTryingUntil 0 _ _ = return Nothing
keepTryingUntil n g p = do
    x <- g
    if p x then return $ Just x else scale (+1) $ keepTryingUntil (n-1) g p


shrinkDLTest :: DynLogicModel s => DynLogic s -> DynLogicTest s -> [DynLogicTest s]
shrinkDLTest _ (Looping _) = []
shrinkDLTest d tc =
    [test | as' <- shrinkScript d (getScript tc),
            let test = makeTestFromPruned d (pruneDLTest d as'),
            -- Don't shrink a non-executable test case to an executable one.
            case (tc, test) of
                (DLScript _, _) -> True
                (_, DLScript _) -> False
                _               -> True]

shrinkScript :: DynLogicModel t => DynLogic t -> [TestStep t] -> [[TestStep t]]
shrinkScript d as = shrink' d as initialState
    where
        shrink' _ [] _ = []
        shrink' d (step:as) s =
          [] :
          reverse (takeWhile (not . null) [drop (n-1) as | n <- iterate (*2) 1]) ++
          case step of
            Do (Var i := act) ->
              [Do (Var i := act'):as | Some act' <- shrinkAction s act]
            Witness a ->
              -- When we shrink a witness, allow one shrink of the
              -- rest of the script... so assuming the witness may be
              -- used once to construct the rest of the test. If used
              -- more than once, we may need double shrinking.
              [Witness a':as' | a' <- shrinkWitness d a,
                                as' <- as:shrink' (stepDLtoDL d s (Witness a')) as s]
          ++ [step:as'
             | as' <- shrink' (stepDLtoDL d s step) as $
                        case step of
                          Do (var := act) -> nextState s act var
                          Witness _       -> s]

shrinkWitness :: (StateModel s, Typeable a) => DynLogic s -> a -> [a]
shrinkWitness (ForAll (q :: Quantification a) _) (a :: a') =
  case eqT @a @a' of
    Just Refl | isaQ q a -> shrinkQ q a
    _                    -> []
shrinkWitness (Alt _ d d') a = shrinkWitness d a ++ shrinkWitness d' a
shrinkWitness (Stopping d) a = shrinkWitness d a
shrinkWitness (Weight _ d) a = shrinkWitness d a
shrinkWitness (Monitor _ d)a = shrinkWitness d a
shrinkWitness _ _            = []

-- The result of pruning a list of actions is a list of actions that
-- could have been generated by the dynamic logic.
pruneDLTest :: DynLogicModel s => DynLogic s -> [TestStep s] -> [TestStep s]
pruneDLTest d test = prune [d] initialState test
  where
    prune [] _ _ = []
    prune _ _ [] = []
    prune ds s (Do (var := act):rest)
      | precondition s act =
        case [d' | d <- ds, d' <- stepDL d s (Do $ var := act)] of
          [] -> prune ds s rest
          ds' -> Do (var := act) :
            prune ds' (nextState s act var) rest
      | otherwise =
        prune ds s rest
    prune ds s (Witness a:rest) =
      case [d' | d <- ds, d' <- stepDL d s (Witness a)] of
        []  -> prune ds s rest
        ds' -> Witness a : prune ds' s rest

stepDL :: DynLogicModel s => DynLogic s -> s -> TestStep s -> [DynLogic s]
stepDL (After a k) s (Do (var := act))
  | a == Some act = [k (nextState s act var)]
stepDL (AfterAny k) s (Do (var := act))
  | not (restricted act) = [k (nextState s act var)]
stepDL (Alt _ d d') s step = stepDL d s step ++ stepDL d' s step
stepDL (Stopping d) s step = stepDL d s step
stepDL (Weight _ d) s step = stepDL d s step
stepDL (ForAll (q :: Quantification a) f) _ (Witness (a :: a')) =
  case eqT @a @a' of
    Just Refl -> [f a | isaQ q a]
    Nothing   -> []
stepDL (Monitor f d) s step = stepDL d s step
stepDL _ _ _ = []

stepDLtoDL :: DynLogicModel s => DynLogic s -> s -> TestStep s -> DynLogic s
stepDLtoDL d s step = case stepDL d s step of
                        [] -> EmptySpec
                        ds -> foldr1 (Alt Demonic) ds

propPruningGeneratedScriptIsNoop :: DynLogicModel s => DynLogic s -> Property
propPruningGeneratedScriptIsNoop d =
  forAll (sized $ \ n -> choose (1, max 1 n) >>= generateDLTest d) $ \test ->
    let script = case test of BadPrecondition s _ _ -> s
                              Looping s             -> s
                              Stuck s _             -> s
                              DLScript s            -> s
    in script == pruneDLTest d script

getScript :: DynLogicTest s -> [TestStep s]
getScript (BadPrecondition s _ _) = s
getScript (Looping s)             = s
getScript (Stuck s _)             = s
getScript (DLScript s)            = s

makeTestFromPruned :: DynLogicModel s => DynLogic s -> [TestStep s] -> DynLogicTest s
makeTestFromPruned d test = make d initialState test
  where make d s as | not (null bad) = BadPrecondition as bad s
          where bad = badActions d s
        make d s [] | stuck d s = Stuck [] s
                    | otherwise = DLScript []
        make d s (step:as) =
          case make (stepDLtoDL d s step)
                    (case step of
                       Do (var := act) -> nextState s act var
                       Witness _       -> s)
                    as
          of
            BadPrecondition as bad s -> BadPrecondition (step:as) bad s
            Stuck as s               -> Stuck (step:as) s
            DLScript as              -> DLScript (step:as)
            Looping{}                -> error "makeTestFromPruned: Looping"

-- | If failed, return the prefix up to the failure. Also prunes the test in case the model has
--   changed.
unfailDLTest :: DynLogicModel s => DynLogic s -> DynLogicTest s -> DynLogicTest s
unfailDLTest d test = makeTestFromPruned d $ pruneDLTest d as
    where
        as = case test of
                BadPrecondition as _ _ -> as
                Stuck as _             -> as
                DLScript as            -> as
                Looping as             -> as

stuck :: DynLogicModel s => DynLogic s -> s -> Bool
stuck EmptySpec    _ = True
stuck Stop         _ = False
stuck (After _ _)  _ = False
stuck (AfterAny _) s = not $ canGenerate 0.01 (arbitraryAction s)
                              (\a -> case a of
                                       Some act -> precondition s act
                                                && not (restricted act)
                                       Error _ -> False)
stuck (Alt Angelic d d') s = stuck d s && stuck d' s
stuck (Alt Demonic d d') s = stuck d s || stuck d' s
stuck (Stopping d) s       = stuck d s
stuck (Weight w d) s       = w < never || stuck d s
stuck (ForAll _ _) _       = False
stuck (Monitor _ d)s       = stuck d s

validDLTest :: StateModel s => DynLogic s -> DynLogicTest s -> Property
validDLTest _ (DLScript _)                = property True
validDLTest _ (Stuck as _)                = counterexample ("Stuck\n" ++ (unlines . map ("  "++) . lines $ show as)) False
validDLTest _ (Looping as)                = counterexample ("Looping\n" ++ (unlines . map ("  "++) . lines $ show as)) False
validDLTest _ (BadPrecondition as bads s) = counterexample ("BadPrecondition\n" ++ show as ++ "\n" ++ unlines (showBad <$> bads)) $ False
  where
    showBad (Error s) = s
    showBad a         = show a

scriptFromDL :: DynLogicTest s -> Actions s
scriptFromDL (DLScript s) = Actions [a | Do a <- s]
scriptFromDL _            = Actions []

badActions :: StateModel s => DynLogic s -> s -> [Any (Action s)]
badActions EmptySpec _    = []
badActions Stop      _    = []
badActions (After (Some a) _) s
  | precondition s a = []
  | otherwise        = [Some a]
badActions (After (Error m) _) s = [Error m]
badActions (AfterAny _) _ = []
badActions (Alt _ d d') s = badActions d s ++ badActions d' s
badActions (Stopping d) s = badActions d s
badActions (Weight w d) s = if w < never then [] else badActions d s
badActions (ForAll _ _) _ = []
badActions (Monitor _ d)s = badActions d s

applyMonitoring :: DynLogicModel s => DynLogic s -> DynLogicTest s -> Property -> Property
applyMonitoring d (DLScript s) p =
  case findMonitoring d initialState s of
    Just f  -> f p
    Nothing -> p
applyMonitoring _ Stuck{}           p = p
applyMonitoring _ Looping{}         p = p
applyMonitoring _ BadPrecondition{} p = p

findMonitoring :: DynLogicModel s => DynLogic s -> s -> [TestStep s] -> Maybe (Property -> Property)
findMonitoring Stop      s [] = Just id
findMonitoring (After (Some a) k) s (Do (var := a'):as)
  | Some a == Some a' = findMonitoring (k s') s' as
  where s' = nextState s a' var
findMonitoring (AfterAny k) s as@(Do (var := a):_)
  | not (restricted a) = findMonitoring (After (Some a) k) s as
findMonitoring (Alt b d d') s as =
  -- Give priority to monitoring matches to the left. Combining both
  -- results in repeated monitoring from always, which is unexpected.
  findMonitoring d s as <|> findMonitoring d' s as
findMonitoring (Stopping d) s as = findMonitoring d s as
findMonitoring (Weight _ d) s as = findMonitoring d s as
findMonitoring (ForAll (q :: Quantification a) k) s (Witness (a :: a'):as) =
  case eqT @a @a' of
    Just Refl -> findMonitoring (k a) s as
    Nothing   -> Nothing
findMonitoring (Monitor m d) s as =
  (m.) <$> findMonitoring d s as
findMonitoring _ _ _ = Nothing
