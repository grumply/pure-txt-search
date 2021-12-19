{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeOperators, DataKinds, CPP, ScopedTypeVariables, StandaloneDeriving, RecordWildCards, DeriveGeneric #-}
module Pure.Data.Txt.Search (Search(..),SearchOptions(..),defaultSearchOptions,containsDef,containing) where

import Pure.Data.Txt as Txt
import Pure.Data.Default
import Pure.Data.Lifted
import Pure.Data.View

import Data.List as List
import Data.Maybe
import Data.Monoid
import GHC.Generics as G
import GHC.TypeLits

-- for Search instances
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Int
import Data.Ord
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
    contains :: SearchOptions -> Txt -> a -> Bool
    {-# INLINE contains #-}
    default contains :: (Generic a, GSearch (Rep a)) => SearchOptions -> Txt -> a -> Bool
    contains so t = gcontains so t . G.from

data SearchOptions
    = SearchOptions
        { constructorNames :: Bool
        , selectorNames :: Bool
        } deriving (Eq)

defaultSearchOptions :: SearchOptions
defaultSearchOptions = SearchOptions True True

instance Default SearchOptions where
    def = defaultSearchOptions

{-# INLINABLE containing #-}
containing :: forall b. Search b => SearchOptions -> Txt -> [b] -> [b] 
containing so needle haystack
    | Txt.null needle = haystack
    | otherwise       = results
    where
      needles :: [Txt]
      needles = Txt.words needle

      results :: [b]
      results = fmap snd sorted

      sorted :: [(Int,b)]
      sorted = List.sortBy (flip (comparing fst)) processed

      processed :: [(Int,b)]
      processed = mapMaybe process haystack

      process :: b -> Maybe (Int,b)
      process x = 
        let matches = List.length (mapMaybe match needles)
            match needle 
              | contains so needle x = Just ()
              | otherwise            = Nothing
         in if matches == 0 then Nothing else Just (matches,x)

-- If we do this, newtype type, wrapper and unwrapper will not be searched.
-- instance {-# OVERLAPPABLE #-} (ToTxt a) => Search a where
--     contains = containsDef
-- Instead, we have the following primtive instances.

{-# INLINE containsDef #-}
containsDef :: ToTxt a => SearchOptions -> Txt -> a -> Bool
containsDef so t = contains so t . toTxt

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
    contains so n h = Txt.isInfixOf (Txt.toLower n) (Txt.toLower $ fix_wrapped_string h)
#else
instance {-# OVERLAPPING #-} Search Txt where
    {-# INLINE contains #-}
    contains so n h = Txt.isInfixOf (Txt.toLower n) (Txt.toLower h)
#endif

-- rely on the generic machinery to permit searching for `Either`, `Left`, and `Right`.
instance (Search a, Search b) => Search (Either a b)

-- rely on the generic machinery to permit searching for `Maybe`, `Nothing`, and `Just`.
instance (Search a) => Search (Maybe a)

{-
instance {-# OVERLAPPABLE #-} (Foldable f, Search a) => Search (f a) where
    {-# INLINE contains #-}
    contains = containsDefFoldable
-}

instance {-# OVERLAPPABLE #-} Search a => Search [a] where
    contains = containsDefFoldable

containsDefFoldable so t = getAny . foldMap (Any. contains so t)

instance (Search a, Search b) => Search (a,b) where
    {-# INLINE contains #-}
    contains so t (a,b) = contains so t a || contains so t b

instance (Search a, Search b, Search c) => Search (a,b,c) where
    {-# INLINE contains #-}
    contains so t (a,b,c) = contains so t a || contains so t (b,c)

instance (Search a, Search b, Search c, Search d) => Search (a,b,c,d) where
    {-# INLINE contains #-}
    contains so t (a,b,c,d) = contains so t a || contains so t (b,c,d)

instance (Search a, Search b, Search c, Search d, Search e) => Search (a,b,c,d,e) where
    {-# INLINE contains #-}
    contains so t (a,b,c,d,e) = contains so t a || contains so t (b,c,d,e)

instance (Search a, Search b, Search c, Search d, Search e, Search f) => Search (a,b,c,d,e,f) where
    {-# INLINE contains #-}
    contains so t (a,b,c,d,e,f) = contains so t a || contains so t (b,c,d,e,f)

instance (Search a, Search b, Search c, Search d, Search e, Search f, Search g) => Search (a,b,c,d,e,f,g) where
    {-# INLINE contains #-}
    contains so t (a,b,c,d,e,f,g) = contains so t a || contains so t (b,c,d,e,f,g)

class GSearch a where
    gcontains :: SearchOptions -> Txt -> a x -> Bool

instance (GSearch a, GSearch b) => GSearch (a :+: b) where
    gcontains so t (L1 l) = gcontains so t l
    gcontains so t (R1 r) = gcontains so t r

instance (GSearch a, GSearch b) => GSearch (a :*: b) where
    gcontains so t (l :*: r) = gcontains so t l || gcontains so t r

instance (GSearch f, G.Datatype t) => GSearch (G.M1 D t f) where
    -- Don't search the `datatypeName` here because it's not especially useful.
    gcontains so t d1@(G.M1 m) = gcontains so t m

instance (GSearch f, G.Constructor t) => GSearch (G.M1 C t f) where
    gcontains so@SearchOptions {..} t m1@(G.M1 m) =
        (||)
            (constructorNames && (contains so t $ toTxt $ conName m1))
            (gcontains so t m)

instance (GSearch f, G.Selector s) => GSearch (G.M1 S s f) where
    gcontains so@SearchOptions {..} t s1@(G.M1 m) =
        (||)
            (selectorNames && (contains so t $ toTxt $ selName s1))
            (gcontains so t m)

instance (Search a) => GSearch (G.K1 i a) where
    gcontains so t (G.K1 a) = contains so t a

instance GSearch G.U1 where
    gcontains _ _ _ = False

instance Search Options where
    contains so@SearchOptions {..} t Options {..} =
        (constructorNames && contains so t "Options")
            || (selectorNames && contains so t "preventDef")
            || (selectorNames && contains so t "stopProp")
            || (selectorNames && contains so t "passive")

instance Search Target where
    contains so@SearchOptions {..} t tar = constructorNames &&
        case tar of
            ElementTarget  -> contains so t "ElementTarget"
            WindowTarget   -> contains so t "WindowTarget"
            DocumentTarget -> contains so t "DocumentTarget"

instance Search Listener where
    contains so@SearchOptions {..} t On {..} =
        (constructorNames && contains so t "On")
            || (selectorNames && contains so t "eventName")
            || contains so t eventName
            || (selectorNames && contains so t "eventTarget")
            || contains so t eventTarget
            || (selectorNames && contains so t "eventOptions")
            || contains so t eventOptions
            || (selectorNames && contains so t "eventAction")
            || (selectorNames && contains so t "eventStopper")

instance Search Lifecycle where
    contains so@SearchOptions {..} t HostRef {..} =
        (constructorNames && contains so t "HostRef")
            || (selectorNames && contains so t "withHost")

instance Search Features where
    contains so@SearchOptions {..} t Features_ {..} =
        (constructorNames && contains so t "Features")
            || (selectorNames && contains so t "classes")
            || containsDefFoldable so t classes
            || (selectorNames && contains so t "styles")
            || containsDefFoldable so t styles
            || (selectorNames && contains so t "attributes")
            || containsDefFoldable so t attributes
            || (selectorNames && contains so t "properties")
            || containsDefFoldable so t properties
            || (selectorNames && contains so t "listeners")
            || containsDefFoldable so t listeners
            || (selectorNames && contains so t "lifecycles")
            || containsDefFoldable so t lifecycles

instance Search View where
    contains so@SearchOptions {..} t v =
        case v of
            NullView {..} ->
                (constructorNames && contains so t "NullView")
                    || (selectorNames && contains so t "elementHost")
            TextView {..} ->
                (constructorNames && contains so t "TextView")
                    || (selectorNames && contains so t "textHost")
                    || (selectorNames && contains so t "content")
                    || contains so t content
            RawView {..} ->
                (constructorNames && contains so t "RawView")
                    || (selectorNames && contains so t "elementHost")
                    || (selectorNames && contains so t "tag")
                    || contains so t tag
                    || (selectorNames && contains so t "features")
                    || contains so t features
                    || (selectorNames && contains so t "content")
                    || contains so t content
            HTMLView {..} ->
                (constructorNames && contains so t "HTMLView")
                    || (selectorNames && contains so t "elementHost")
                    || (selectorNames && contains so t "tag")
                    || contains so t tag
                    || (selectorNames && contains so t "features")
                    || contains so t features
                    || (selectorNames && contains so t "children")
                    || containsDefFoldable so t children
            KHTMLView {..} ->
                (constructorNames && contains so t "KHTMLView")
                    || (selectorNames && contains so t "elementHost")
                    || (selectorNames && contains so t "tag")
                    || contains so t tag
                    || (selectorNames && contains so t "features")
                    || contains so t features
                    || (selectorNames && contains so t "keyedChildren")
                    || containsDefFoldable so t keyedChildren
            ComponentView {..} ->
                (constructorNames && contains so t "ComponentView")
                    || (selectorNames && contains so t "props")
                    || (selectorNames && contains so t "record")
                    || (selectorNames && contains so t "Ref")
                    || (selectorNames && contains so t "comp")
            SVGView {..} ->
                (constructorNames && contains so t "SVGView")
                    || (selectorNames && contains so t "elementHost")
                    || (selectorNames && contains so t "tag")
                    || contains so t tag
                    || (selectorNames && contains so t "features")
                    || contains so t features
                    || (selectorNames && contains so t "xlinks")
                    || containsDefFoldable so t xlinks
                    || (selectorNames && contains so t "children")
                    || containsDefFoldable so t children
            KSVGView {..} ->
                (constructorNames && contains so t "KSVGView")
                    || (selectorNames && contains so t "elementHost")
                    || (selectorNames && contains so t "tag")
                    || contains so t tag
                    || (selectorNames && contains so t "features")
                    || contains so t features
                    || (selectorNames && contains so t "xlinks")
                    || containsDefFoldable so t xlinks
                    || (selectorNames && contains so t "keyedChildren")
                    || containsDefFoldable so t keyedChildren
            SomeView {..} ->
                (constructorNames && contains so t "SomeView")
                    || (selectorNames && contains so t "renderable")
            PortalView {..} ->
                (constructorNames && contains so t "PortalView")
                    || (selectorNames && contains so t "portalProxy")
                    || (selectorNames && contains so t "portalDestination")
                    || (selectorNames && contains so t "portalView")
                    || contains so t portalView
            LazyView {..} ->
                (constructorNames && contains so t "LazyView")
                    || (selectorNames && contains so t "lazyFun")
                    || (selectorNames && contains so t "lazyArg")