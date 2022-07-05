module Plutus.Contract.Test.Coverage.ReportCoverage(writeCoverageReport) where

import Control.Exception
import Control.Lens (view)
import Data.Function
import Data.List
import Data.Map qualified as Map
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (pack, unpack)
import HTMLEntities.Text (text)

import PlutusTx.Coverage

-- Position (in a file), and status (of a character)

type Pos = (Int,Int)   -- line, column

predPos, succPos :: Pos -> Pos

predPos (l,1) = (l-1,maxBound)
predPos (l,c) = (l,c-1)

succPos (l,c) = (l,c+1)

data CoverStatus = NotCovered | HasBeenHere | HasBeenFalse | HasBeenTrue | HasBeenBoth
  deriving (Eq, Ord, Show)

data IgnoreStatus = NotIgnored | IgnoredIfFalse | IgnoredIfTrue | AlwaysIgnored
  deriving (Eq, Ord, Show)

data Status = OnChain CoverStatus IgnoreStatus
  deriving (Eq, Ord, Show)

instance Semigroup CoverStatus where
  HasBeenBoth  <> _            = HasBeenBoth
  _            <> HasBeenBoth  = HasBeenBoth
  HasBeenFalse <> HasBeenTrue  = HasBeenBoth
  HasBeenTrue  <> HasBeenFalse = HasBeenBoth
  HasBeenFalse <> _            = HasBeenFalse
  _            <> HasBeenFalse = HasBeenFalse
  HasBeenTrue  <> _            = HasBeenTrue
  _            <> HasBeenTrue  = HasBeenTrue
  HasBeenHere  <> _            = HasBeenHere
  _            <> HasBeenHere  = HasBeenHere
  NotCovered   <> NotCovered   = NotCovered

instance Monoid CoverStatus where
  mempty = NotCovered

instance Semigroup IgnoreStatus where
  AlwaysIgnored  <> _              = AlwaysIgnored
  _              <> AlwaysIgnored  = AlwaysIgnored
  IgnoredIfFalse <> IgnoredIfTrue  = AlwaysIgnored
  IgnoredIfTrue  <> IgnoredIfFalse = AlwaysIgnored
  IgnoredIfTrue  <> _              = IgnoredIfTrue
  _              <> IgnoredIfTrue  = IgnoredIfTrue
  IgnoredIfFalse <> _              = IgnoredIfFalse
  _              <> IgnoredIfFalse = IgnoredIfFalse
  NotIgnored     <> NotIgnored     = NotIgnored

instance Monoid IgnoreStatus where
  mempty = NotIgnored

-- The Semigroup instance is used to combine swipes over identical ranges.
instance Semigroup Status where
  OnChain c i <> OnChain c' i' = OnChain (c <> c') (i <> i')

instance Monoid Status where
  mempty = OnChain mempty mempty

statusStyle :: Status -> String
statusStyle (OnChain HasBeenHere _)             = "background-color:white;color:black"
statusStyle (OnChain HasBeenBoth _)             = "background-color:white;color:black"
statusStyle (OnChain HasBeenTrue i)
  | elem i [IgnoredIfFalse, AlwaysIgnored]      = "background-color:white;color:lightgreen"
  | otherwise                                   = "background-color:lightgreen;color:black"
statusStyle (OnChain HasBeenFalse i)
  | elem i [IgnoredIfTrue, AlwaysIgnored]       = "background-color:white;color:lightpink"
  | otherwise                                   = "background-color:lightpink;color:black"
statusStyle (OnChain NotCovered NotIgnored)     = "background-color:black;color:orangered"
statusStyle (OnChain NotCovered IgnoredIfFalse) = "background-color:lightpink;color:black"
statusStyle (OnChain NotCovered IgnoredIfTrue)  = "background-color:lightgreen;color:black"
statusStyle (OnChain NotCovered AlwaysIgnored)  = "background-color:white;color:orangered"

offChainStyle :: String
offChainStyle = "background-color:lightgray;color:gray"

-- A "swipe" represents colouring a region of a file with a
-- status. Our overall approach is to convert coverage information
-- into a collection of non-overlapping, but possibly nested swipes,
-- and then converting this into an orderedlist of disjoint swipes
-- which can be used for generating colours.

data Swipe = Swipe { swipeStart  :: Pos,
                     swipeEnd    :: Pos,
                     swipeStatus :: Status }
  deriving (Eq, Show)

-- This surprising ordering on swipes has the property that if s1 is
-- nested within s2, then s1 <= s2. Given that no two swipes overlap,
-- then s1 <= s2 precisely if s1 precedes s2 entirely, or s1 is nested
-- within s2. It follow that, in a sorted list of swipes, the first
-- one has no other swipes nested within it, and therefore its colour
-- takes priority over all other swipes. We make use of this in
-- converting a set of swipes to a set of disjoint swipes with the
-- same coloration.

instance Ord Swipe where
  (<=) = (<=) `on` \(Swipe start end status) -> (end, Down start, status)

-- Is the first swipe nested within the second?

nested :: Swipe -> Swipe -> Bool
nested (Swipe from to _) (Swipe from' to' _) = from >= from' && to <= to'

-- Let the first swipe "swipe over" part of the second. The resulting
-- swipes do not overlap.  The first swipe must be nested within the
-- second.

combineNestedSwipes :: Swipe -> Swipe -> [Swipe]
combineNestedSwipes (Swipe from to s) (Swipe from' to' s')
  | (from, to) == (from', to') = [Swipe from to (s <> s')]
  | otherwise =
    [Swipe from' (predPos from) s' | from /= from'] ++
    [Swipe from to s] ++
    [Swipe (succPos to) to' s' | to /= to']

-- Flatten an ordered list of swipes, to get a non-overlapping list.
-- Nested swipes "swipe over" the outer swipe. Because of the custom
-- ordering on swipes, the first swipe in the list cannot have any
-- other swipe in the listed nested within it, which means that its
-- colour "wins" over all others.

flattenSwipes :: [Swipe] -> [Swipe]
flattenSwipes []          = []
flattenSwipes (sw:swipes) = swipeOver sw . flattenSwipes $ swipes

swipeOver :: Swipe -> [Swipe] -> [Swipe]
swipeOver sw [] = [sw]
swipeOver sw (sw':swipes)
  | swipeEnd sw < swipeStart sw' = sw:sw':swipes
  | swipeEnd sw' < swipeStart sw = sw':swipeOver sw swipes
  | nested sw sw'                = combineNestedSwipes sw sw' ++ swipes
  | otherwise                    = error . unlines $
                                     "swipeOver: precondition violated; swipes are not nested or disjoint.":
                                     map show (sw:sw':take 8 swipes)

-- Convert an ordered list of non-intersecting swipes that may swipe
-- any region in a file, into a list of swipes applied to each line.

type SwipesPerLine = [(Int,[Swipe])]

swipesByLine :: [Swipe] -> SwipesPerLine
swipesByLine = map addLine . groupBy ((==) `on` (fst.swipeStart)) . concatMap splitSwipe
  where splitSwipe s@(Swipe (fromLine,_) (toLine,_) _)
          | fromLine == toLine = [s]
          | otherwise          = s{swipeEnd = (fromLine,maxBound)}:
                                 splitSwipe s{swipeStart = (fromLine+1,1)}
        addLine swipes = (fst . swipeStart . head $ swipes, swipes)

-- Extend a list of swipes-per-line by including non-swiped lines that
-- are within windowLines of a swiped line.

windowLines :: Int
windowLines = 5

includeNearby ::  SwipesPerLine ->  SwipesPerLine
includeNearby swipes = excluding 1 swipes
  where excluding _ [] = []
        excluding n nSwipes
          | n > nextSwiped             = error . unlines $ ("Bad excluding: "++show n):map show (take 10 nSwipes)
          | n == nextSwiped            = including n nSwipes
          | n+windowLines < nextSwiped = excluding (nextSwiped-windowLines) nSwipes
          | otherwise                  = (n,[]):excluding (n+1) nSwipes
          where nextSwiped = fst (head nSwipes)
        including _ [] = []
        including n ((next,swipe):nSwipes)
          | n > next = error . unlines $ ("Bad including: "++show n):map show (take 10 ((next,swipe):nSwipes))
          | n == next =
              (next,swipe):including (n+1) nSwipes
          | n+windowLines >= next =
              (n,[]):including (n+1) ((next,swipe):nSwipes)
          | n+windowLines < next =
              [(i,[]) | i <- [n..n-1+windowLines]] ++ excluding (n+windowLines) ((next,swipe):nSwipes)
          | otherwise = error "impossible"

-- Extend a list of swipes-per-line to include non-swiped lines that
-- form a small gap between swiped blocks. Gaps are replaced by three
-- vertical dots; there is no sense in replacing a gap of three or
-- fewer lines this way.

fillSmallGaps :: SwipesPerLine ->  SwipesPerLine
fillSmallGaps ((n,swipes):(n',swipes'):nSwipes)
  | n+4 >= n' = (n,swipes):[(i,[]) | i <- [n+1..n'-1]] ++ fillSmallGaps ((n',swipes'):nSwipes)
  | otherwise = (n,swipes):fillSmallGaps ((n',swipes'):nSwipes)
fillSmallGaps swipes = swipes

-- Generate HTML elements

element :: String -> [(String,String)] -> String -> String
element name attrs body =
  "<"++name++" "++
  concat [a++"="++b++" " | (a,b) <- attrs]++
  ">"++body++"</"++name++">"

quote :: String -> String
quote s = q++s++q
  where q = "\""

encode :: String -> String
encode = unpack . text . pack

-- Read source files and extract coverage information.

data FileInfo = FileInfo
  { fiName    :: String
  , fiLines   :: [String]
  , fiAllAnns :: Set CoverageAnnotation
  , fiCovered :: Set CoverageAnnotation
  , fiIgnored :: Set CoverageAnnotation }

files :: CoverageReport -> IO [FileInfo]
files (CoverageReport ci@(CoverageIndex metadataMap) (CoverageData annots)) = sequence [file n | n <- fileNames ci]
  where file name = do
          body <- either (const "" :: IOException -> String) id <$>
                    try (readFile name)
          return FileInfo{ fiName    = name
                         , fiLines   = lines body
                         , fiAllAnns = covx name
                         , fiCovered = covs name
                         , fiIgnored = covi name }
        ignoredMap = Map.filter (Set.member IgnoredAnnotation . view metadataSet) metadataMap
        covx name = Set.filter ((==name) . _covLocFile . getCovLoc) . Map.keysSet $ metadataMap
        covs name = Set.filter ((==name) . _covLocFile . getCovLoc) annots
        covi name = Set.filter ((==name) . _covLocFile . getCovLoc) . Map.keysSet $ ignoredMap

fileNames :: CoverageIndex -> [String]
fileNames (CoverageIndex metadataMap) =
  Set.toList . Set.map (_covLocFile . getCovLoc) . Map.keysSet $ metadataMap

getCovLoc :: CoverageAnnotation -> CovLoc
getCovLoc (CoverLocation c) = c
getCovLoc (CoverBool c _)   = c

-- Generate the coverage report and write to an HTML file.

writeCoverageReport :: String -> CoverageReport -> IO ()
writeCoverageReport name cr = do
  fs <- files cr
  writeFile (name++".html") . coverageReportHtml $ fs

coverageReportHtml :: [FileInfo] -> String
coverageReportHtml fs = element "body" [] $ report
  where
    report = header ++ concat ["<hr>"++file name body covx annots covi | FileInfo name body covx annots covi <- fs]
    header =
      element "h1" [] "Files" ++
      element "ul" []
        (concat [element "li" [] . element "a" [("href",quote ("#"++name))] $ name
                | FileInfo{fiName = name} <- fs])
    file name body covx annots covi =
      let uncovered  = covx Set.\\ annots
          status ann = OnChain (ifM c covS) (ifM i ignS)
            where
              i = Set.member ann covi
              c = Set.notMember ann uncovered
              ifM False _ = mempty
              ifM True  m = m
              covS = case ann of
                CoverBool _ True  -> HasBeenTrue
                CoverBool _ False -> HasBeenFalse
                CoverLocation{}   -> HasBeenHere
              ignS = case ann of
                CoverBool _ True  -> IgnoredIfTrue
                CoverBool _ False -> IgnoredIfFalse
                CoverLocation{}   -> AlwaysIgnored
          swipe loc s = Swipe (_covLocStartLine loc, _covLocStartCol loc)
                              (_covLocEndLine   loc, _covLocEndCol   loc) s
          swipes = flattenSwipes . sort $
                   [ swipe (getCovLoc ann) $ status ann | ann <- Set.toList covx ]
      in
      element "h2" [("id",quote name)] name ++
      element "pre" [] (unlines
        (annotateLines
          (zip [1..] body)
          (fillSmallGaps . includeNearby . swipesByLine $ swipes)))

-- Apply swipes to the selected contents of a file

annotateLines :: [(Int,String)] -> SwipesPerLine -> [String]
annotateLines _ [] = []
annotateLines [] _ = [element "div" [("style","color:red")] "Source code not available"]
annotateLines ((n,line):nLines) ((n',swipes):nSwipes)
  | n < n'  = replicate 3 "." ++
              annotateLines (dropWhile ((<n').fst) nLines) ((n',swipes):nSwipes)
  | n == n' = annotateLine n line swipes:annotateLines nLines nSwipes
annotateLines xs ys =
  error $ "Non-exhaustive patterns in function annotateLines\n"++
  unlines (take 10 (map show xs) ++ ["----------"] ++ take 10 (map show ys))

-- Annotate a line with a list of swipes

annotateLine :: Int -> String -> [Swipe] -> String
annotateLine n line swipes =
  showLineNo n++"    "++swipeLine 1 line swipes

swipeLine :: Int -> String -> [Swipe] -> String
swipeLine _ line [] = element "span" [("style",offChainStyle)] $ encode line
swipeLine c line s@(Swipe (_,from) (_,to) stat:swipes)
  | c < from  = element "span" [("style",offChainStyle)] (encode $ take (from-c) line) ++
                swipeLine from (drop (from-c) line) s
  | otherwise = element "span" [("style",statusStyle stat)] (encode $ take (to+1-from) line) ++
                swipeLine (to+1) (drop (to+1-from) line) swipes

showLineNo :: Int -> String
showLineNo n = reverse . take 6 . reverse $ replicate 6 ' ' ++ show n

