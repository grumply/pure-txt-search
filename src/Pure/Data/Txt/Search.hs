{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeOperators, DataKinds, CPP #-}
module Pure.Data.Txt.Search where

import Pure.Data.Txt as Txt

import Data.Monoid

import GHC.Generics as G
import GHC.TypeLits

-- This module is downright abusive and it probably doesn't do what I want.
-- Why even use types at this point? This library is hammer when you likely
-- need a screwdriver.

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
-- >>> contains "test" X 
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

instance {-# OVERLAPPABLE #-} (ToTxt a) => Search a where
    contains t = contains t . toTxt

-- This fix handles the case that the generic machinery of G.from 
-- wraps a primitive Txt(JSVal) into an object by re-extracting 
-- the primitive JSString when compiled with GHCJS.
#ifdef __GHCJS__
foreign import javascript unsafe
    "if ($1 === Object($1)) { $r = $1.d1 } else { $r = $1 }" fix_wrapped_string :: Txt -> Txt

instance {-# OVERLAPPING #-} Search Txt where
    contains n h = Txt.isInfixOf n (fix_wrapped_string h)
#else
instance {-# OVERLAPPING #-} Search Txt where
    contains = Txt.isInfixOf
#endif

instance (Search a, Search b) => Search (Either a b) where
    contains t (Left  l) = contains t l
    contains t (Right r) = contains t r

instance {-# OVERLAPPABLE #-} (Foldable f, Search a) => Search (f a) where
    {-# INLINE contains #-}
    contains t = getAny . foldMap (Any . contains t)

instance {-# OVERLAPPING #-} Search [Char] where
    contains t = contains t . toTxt

instance {-# OVERLAPPING #-} (Search a, Search b) => Search (a,b) where 
    {-# INLINE contains #-}
    contains t (a,b) = contains t a || contains t b

instance {-# OVERLAPPING #-} (Search a, Search b, Search c) => Search (a,b,c) where
    {-# INLINE contains #-}
    contains t (a,b,x) = contains t (a,b) || contains t x

instance {-# OVERLAPPING #-} (Search a, Search b, Search c, Search d) => Search (a,b,c,d) where
    {-# INLINE contains #-}
    contains t (a,b,c,x) = contains t (a,b,c) || contains t x

instance {-# OVERLAPPING #-} (Search a, Search b, Search c, Search d, Search e) => Search (a,b,c,d,e) where
    {-# INLINE contains #-}
    contains t (a,b,c,d,x) = contains t (a,b,c,d) || contains t x

instance {-# OVERLAPPING #-} (Search a, Search b, Search c, Search d, Search e, Search f) => Search (a,b,c,d,e,f) where
    {-# INLINE contains #-}
    contains t (a,b,c,d,e,x) = contains t (a,b,c,d,e) || contains t x

instance {-# OVERLAPPING #-} (Search a, Search b, Search c, Search d, Search e, Search f, Search g) => Search (a,b,c,d,e,f,g) where
    {-# INLINE contains #-}
    contains t (a,b,c,d,e,f,x) = contains t (a,b,c,d,e,f) || contains t x

class GSearch a where
    gcontains :: Txt -> a x -> Bool

instance (GSearch f) => GSearch (G.M1 D t f) where
    gcontains t (G.M1 m) = gcontains t m

instance {-# OVERLAPPING #-} (GSearch f, G.Selector s) => GSearch (G.M1 S s f) where
    gcontains t s1@(G.M1 m) = 
        (||)
            (contains t $ toTxt $ selName s1)
            (gcontains t m)

instance (GSearch f, G.Constructor t) => GSearch (G.M1 C t f) where
    gcontains t m1@(G.M1 m) = 
        (||) 
            (contains t $ toTxt $ conName m1)
            (gcontains t m)

instance GSearch G.U1 where
    gcontains _ _ = False

instance (GSearch a, GSearch b) => GSearch (a :+: b) where
    gcontains t (L1 l) = gcontains t l
    gcontains t (R1 r) = gcontains t r

instance (GSearch a, GSearch b) => GSearch (a :*: b) where
    gcontains t (l :*: r) = gcontains t l || gcontains t r

instance (Search a) => GSearch (G.K1 i a) where
    gcontains t (G.K1 a) = contains t a
