{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeOperators, DataKinds, CPP, ScopedTypeVariables #-}
module Pure.Data.Txt.Search (Search(..),containsDef,containing) where

import Pure.Data.Txt as Txt

import Data.List as List
import Data.Monoid
import Data.IntMap as IntMap
import GHC.Generics as G
import GHC.TypeLits

-- for Search instances
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Int
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- This module is downright abusive and it probably doesn't do what I want.
-- Why even use types at this point? This library is hammer when you likely
-- need a screwdriver.
--
-- Note: Searches are case-agnostic.

-- $setup
-- data Test = X | Y { test :: Txt } deriving (Generic,Search,Show)

-- | Generic search facilities; search constructor names, fields names and field values.
--
-- >>> data Test = X | Y { test :: Txt } deriving (Generic,Search,Show)
--
-- >>> contains "X" X
-- True
--
-- >>> contains "x" X
-- False
--
-- >>> contains "X" (Y "")
-- False
--
-- >>> contains "X" [X]
-- True
--
-- >>> contains "X" (Y "X")
-- True
--
-- >>> contains "test" X -- X :: Test; datatypeName not searched
-- False
--
-- >>> contains "test" (Y "whatever")
-- True
--
-- >>> filter (contains "X") [X,Y "",Y "X"]
-- [X,Y {test = "X"}]
--
class Search a where
    contains :: Txt -> a -> Bool
    {-# INLINE contains #-}
    default contains :: (Generic a, GSearch (Rep a)) => Txt -> a -> Bool
    contains t = gcontains t . G.from

{-# INLINABLE containing #-}
containing :: forall b. Search b => Txt -> [b] -> [b] 
containing needle haystack
    | Txt.null needle = haystack
    | otherwise       = results
    where
        index :: [(Int,b)]
        index = List.zip [0..] haystack

        results :: [b]
        results = fmap (\(i,_) -> haystack !! i) sorted

        sorted :: [(Int,Int)]
        sorted = List.sortOn snd assocs

        assocs :: [(Int,Int)]
        assocs = IntMap.assocs merged

        merged :: IntMap Int
        merged = IntMap.unionsWith (+) indexes

        indexes :: [IntMap Int]
        indexes = create <$> (Txt.words needle)

        create :: Txt -> IntMap Int
        create w = IntMap.fromList $ fmap (\(i,_) -> (i,1)) (matches w)

        matches :: Txt -> [(Int,b)]
        matches w = Prelude.filter (\(i,x) -> (contains :: Txt -> b -> Bool) w (x :: b)) index

-- If we do this, newtype type, wrapper and unwrapper will not be searched.
-- instance {-# OVERLAPPABLE #-} (ToTxt a) => Search a where
--     contains = containsDef
-- Instead, we have the following primtive instances.

{-# INLINE containsDef #-}
containsDef :: ToTxt a => Txt -> a -> Bool
containsDef t = contains t . toTxt

-- overlaps the `(Search a,Foldable f) => Search (f a)` instance since 
-- we want to treat strings not structurally, but as textual values.
instance {-# OVERLAPPING #-} Search [Char] where
    {-# INLINE contains #-}
    contains = containsDef

instance Search Char where
    {-# INLINE contains #-}
    contains = containsDef

instance Search Int where
    {-# INLINE contains #-}
    contains = containsDef

instance Search Int8 where
    {-# INLINE contains #-}
    contains = containsDef

instance Search Int16 where
    {-# INLINE contains #-}
    contains = containsDef

instance Search Int32 where
    {-# INLINE contains #-}
    contains = containsDef

instance Search Int64 where
    {-# INLINE contains #-}
    contains = containsDef

instance Search Word where
    {-# INLINE contains #-}
    contains = containsDef

instance Search Word8 where
    {-# INLINE contains #-}
    contains = containsDef

instance Search Word16 where
    {-# INLINE contains #-}
    contains = containsDef

instance Search Word32 where
    {-# INLINE contains #-}
    contains = containsDef

instance Search Word64 where
    {-# INLINE contains #-}
    contains = containsDef

instance Search Integer where
    {-# INLINE contains #-}
    contains = containsDef

instance Search Float where
    {-# INLINE contains #-}
    contains = containsDef

instance Search Double where
    {-# INLINE contains #-}
    contains = containsDef

instance Search Bool where
    {-# INLINE contains #-}
    contains = containsDef

instance Search () where
    {-# INLINE contains #-}
    contains = containsDef

instance Search BSC.ByteString where
    {-# INLINE contains #-}
    contains = containsDef

instance Search BSLC.ByteString where
    {-# INLINE contains #-}
    contains = containsDef

#ifdef __GHCJS__
instance Search T.Text where
    {-# INLINE contains #-}
    contains = containsDef
#endif

instance Search TL.Text where
    {-# INLINE contains #-}
    contains = containsDef

-- This fix handles the case that the generic machinery of G.from 
-- wraps a primitive Txt(JSVal) into an object by re-extracting 
-- the primitive JSString when compiled with GHCJS.
#ifdef __GHCJS__
foreign import javascript unsafe
    "if ($1 === Object($1)) { $r = $1.d1 } else { $r = $1 }" fix_wrapped_string :: Txt -> Txt

instance {-# OVERLAPPING #-} Search Txt where
    {-# INLINE contains #-}
    contains n h = Txt.isInfixOf (Txt.toLower n) (Txt.toLower $ fix_wrapped_string h)
#else
instance {-# OVERLAPPING #-} Search Txt where
    {-# INLINE contains #-}
    contains n h = Txt.isInfixOf (Txt.toLower n) (Txt.toLower h)
#endif

-- rely on the generic machinery to permit searching for `Either`, `Left`, and `Right`.
instance (Search a, Search b) => Search (Either a b)

-- rely on the generic machinery to permit searching for `Maybe`, `Nothing`, and `Just`.
instance (Search a) => Search (Maybe a)

instance {-# OVERLAPPABLE #-} (Foldable f, Search a) => Search (f a) where
    {-# INLINE contains #-}
    contains t = getAny . foldMap (Any . contains t)

instance {-# OVERLAPPING #-} (Search a, Search b) => Search (a,b) where 
    {-# INLINE contains #-}
    contains t (a,b) = contains t a || contains t b

instance {-# OVERLAPPING #-} (Search a, Search b, Search c) => Search (a,b,c) where
    {-# INLINE contains #-}
    contains t (a,b,c) = contains t a || contains t (b,c)

instance {-# OVERLAPPING #-} (Search a, Search b, Search c, Search d) => Search (a,b,c,d) where
    {-# INLINE contains #-}
    contains t (a,b,c,d) = contains t a || contains t (b,c,d)

instance {-# OVERLAPPING #-} (Search a, Search b, Search c, Search d, Search e) => Search (a,b,c,d,e) where
    {-# INLINE contains #-}
    contains t (a,b,c,d,e) = contains t a || contains t (b,c,d,e)

instance {-# OVERLAPPING #-} (Search a, Search b, Search c, Search d, Search e, Search f) => Search (a,b,c,d,e,f) where
    {-# INLINE contains #-}
    contains t (a,b,c,d,e,f) = contains t a || contains t (b,c,d,e,f)

instance {-# OVERLAPPING #-} (Search a, Search b, Search c, Search d, Search e, Search f, Search g) => Search (a,b,c,d,e,f,g) where
    {-# INLINE contains #-}
    contains t (a,b,c,d,e,f,g) = contains t a || contains t (b,c,d,e,f,g)

class GSearch a where
    gcontains :: Txt -> a x -> Bool

instance (GSearch a, GSearch b) => GSearch (a :+: b) where
    gcontains t (L1 l) = gcontains t l
    gcontains t (R1 r) = gcontains t r

instance (GSearch a, GSearch b) => GSearch (a :*: b) where
    gcontains t (l :*: r) = gcontains t l || gcontains t r

instance (GSearch f, G.Datatype t) => GSearch (G.M1 D t f) where
    -- Don't search the `datatypeName` here because it's not especially useful.
    gcontains t d1@(G.M1 m) = gcontains t m

instance (GSearch f, G.Constructor t) => GSearch (G.M1 C t f) where
    gcontains t m1@(G.M1 m) =
        (||) 
            (contains t $ toTxt $ conName m1)
            (gcontains t m)

instance (GSearch f, G.Selector s) => GSearch (G.M1 S s f) where
    gcontains t s1@(G.M1 m) =
        (||)
            (contains t $ toTxt $ selName s1)
            (gcontains t m)

instance (Search a) => GSearch (G.K1 i a) where
    gcontains t (G.K1 a) = contains t a

instance GSearch G.U1 where
    gcontains _ _ = False
