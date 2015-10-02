{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module UniqMap
    ( UniqMap
    , buildUniqMap
    , (!)
    , lookupName
    , nameAt
    , exprAt
    , keys
    , elems
    , assocs
    , intersectUniq
    ) where

import Control.Lens
import Data.Either.Validation
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M


newtype UniqMap k v = UMap (Map k v)
  deriving (Eq,Ord,Monoid,Show,Functor,Foldable,Traversable)

type instance Index (UniqMap k v) = k
type instance IxValue (UniqMap k v) = v

instance Ord k => Ixed (UniqMap k v) where
    ix k f (UMap m) = case M.lookup k m of
     Just v  -> f v <&> \v' -> UMap (M.insert k v' m)
     Nothing -> pure (UMap m)
    {-# INLINE ix #-}

instance FunctorWithIndex k (UniqMap k)
instance FoldableWithIndex k (UniqMap k)
instance TraversableWithIndex k (UniqMap k) where
  itraverse f (UMap m) = UMap <$> M.traverseWithKey f m

buildUniqMap
    :: (Foldable f, Ord k)
    => f (k,v)
    -> Validation [k] (UniqMap k v)
buildUniqMap = fmap UMap . buildMap . map wrap . toList
  where
    wrap (k, v) = (k, Success (k, v))

    dedupKey (Success (k1, _)) (Success (k2, _)) = Failure [k1,k2]
    dedupKey (Success (k, _)) (Failure l) = Failure (k:l)
    dedupKey (Failure _) _ = error "Shouldn't happen."

    buildMap = traverse (fmap snd) . M.fromListWith dedupKey

(!) :: Ord k => UniqMap k v -> k -> v
UMap m ! k = m M.! k

lookupName :: Ord k => UniqMap k v -> k -> Maybe Int
lookupName (UMap m) = (`M.lookupIndex` m)

nameAt :: UniqMap k v -> Int -> k
nameAt (UMap m) i = fst $ M.elemAt i m

exprAt :: UniqMap k v -> Int -> v
exprAt (UMap m) i = snd $ M.elemAt i m

keys :: UniqMap k v -> [k]
keys (UMap m) = M.keys m

elems :: UniqMap k v -> [v]
elems (UMap m) = M.elems m

assocs :: UniqMap k v -> [(k, v)]
assocs (UMap m) = M.assocs m

intersectUniq
    :: (Ord k)
    => (a -> b -> c)
    -> UniqMap k a
    -> UniqMap k b
    -> Validation ([k], [k]) (UniqMap k c)
intersectUniq f (UMap m1) (UMap m2) = sequenceA . UMap $
    M.mergeWithKey combine (failed (,[])) (failed ([],)) m1 m2
  where
    combine _ x y = Just . Success $ f x y
    failed wrap = M.mapWithKey (\k _ -> Failure $ wrap [k])
