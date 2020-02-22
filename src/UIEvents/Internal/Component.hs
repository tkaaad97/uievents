{-# LANGUAGE FlexibleContexts #-}
module UIEvents.Internal.Component
    ( ComponentStore(..)
    , ComponentStoreState(..)
    , componentSize
    , newComponentStore
    , addComponent
    , removeComponent
    , readComponent
    , writeComponent
    , modifyComponent
    , getComponentSlice
    , cleanComponentStore
    , extendComponentStore
    , unsafeGetComponentVector
    ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.ST (RealWorld)
import Data.Hashable (Hashable(..))
import qualified Data.HashTable.IO as HT
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy(..))
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV

newtype ComponentStore v e a = ComponentStore
    { unComponentStore :: IORef (ComponentStoreState v e a)
    }

type HashTable k v = HT.BasicHashTable k v

data ComponentStoreState v e a = ComponentStoreState
    { componentStoreSize      :: !Int
    , componentStoreVec       :: !(v RealWorld a)
    , componentStoreEntityVec :: !(v RealWorld e)
    , componentStoreEntityMap :: !(HashTable e Int)
    }

newComponentStore :: (MV.MVector v a, MV.MVector v e) => Int -> Proxy (v RealWorld a) -> IO (ComponentStore v e a)
newComponentStore preserve _ = do
    v <- MV.new preserve
    ev <- MV.new preserve
    em <- HT.newSized preserve
    ref <- newIORef (ComponentStoreState 0 v ev em)
    return (ComponentStore ref)

componentSize :: ComponentStore v e a -> IO Int
componentSize =
    fmap componentStoreSize . readIORef . unComponentStore

addComponent :: (MV.MVector v a, MV.MVector v e, Eq e, Hashable e) => ComponentStore v e a -> e -> a -> IO ()
addComponent store e a = do
    s <- readIORef (unComponentStore store)
    maybeIndex <- HT.lookup (componentStoreEntityMap s) e
    maybe addNewEntity (\i -> writeComponentAt store i a) maybeIndex
    where
    addNewEntity = do
        (i, s) <- genComponentIndex store
        let vec = componentStoreVec s
            evec = componentStoreEntityVec s
            emap = componentStoreEntityMap s
        MV.unsafeWrite vec i a
        MV.unsafeWrite evec i e
        HT.insert emap e i

    genComponentIndex v = do
        s <- readIORef ref
        let currentSize = componentStoreSize s
            reservedSize = MV.length $ componentStoreVec s
        when (reservedSize <= currentSize) $
            extendComponentStore v =<< decideSize currentSize
        atomicModifyIORef' ref (genIdentifier currentSize)
        where
        ref = unComponentStore store
        genIdentifier i s =
            let s' = s { componentStoreSize = i + 1 }
            in (s', (i, s))

    decideSize :: Int -> IO Int
    decideSize current = do
        let limit = maxBound :: Int
        when (current >= limit)
            (throwIO . userError $ "max bound")
        return $ if current > limit `div` 2
            then limit
            else current * 2

removeComponent :: (MV.MVector v a, MV.MVector v e, Eq e, Hashable e) => ComponentStore v e a -> e -> IO Bool
removeComponent store e = do
    s <- readIORef (unComponentStore store)
    maybeIndex <- HT.lookup (componentStoreEntityMap s) e
    maybe (return False) relocate maybeIndex

    where
    ref = unComponentStore store
    decrementSize s =
        let currentSize = componentStoreSize s
            s' = s { componentStoreSize = currentSize - 1 }
        in (s', s')
    relocate i = do
        s <- atomicModifyIORef' ref decrementSize
        let j = componentStoreSize s
            vec = componentStoreVec s
            evec = componentStoreEntityVec s
            emap = componentStoreEntityMap s
        last' <- MV.unsafeRead evec j
        MV.unsafeSwap vec i j
        MV.unsafeSwap evec i j
        HT.mutate emap last' (const (Just i, ()))
        HT.delete emap e
        return True

readComponent :: (MV.MVector v a, Eq e, Hashable e) => ComponentStore v e a -> e -> IO (Maybe a)
readComponent store e = do
    s <- readIORef (unComponentStore store)
    let vec = componentStoreVec s
        emap = componentStoreEntityMap s
    maybeIndex <- HT.lookup emap e
    maybe (return Nothing)
        (fmap Just . MV.unsafeRead vec) maybeIndex

writeComponent :: (MV.MVector v a, Eq e, Hashable e) => ComponentStore v e a -> e -> a -> IO Bool
writeComponent store e a = do
    s <- readIORef (unComponentStore store)
    maybeIndex <- HT.lookup (componentStoreEntityMap s) e
    maybe (return False)
        (\i -> MV.unsafeWrite (componentStoreVec s) i a >> return True)
        maybeIndex

writeComponentAt :: (MV.MVector v a) => ComponentStore v e a -> Int -> a -> IO ()
writeComponentAt store i a = do
    vec <- componentStoreVec <$> readIORef (unComponentStore store)
    MV.unsafeWrite vec i a

modifyComponent :: (MV.MVector v a, Eq e, Hashable e) => ComponentStore v e a -> e -> (a -> a) -> IO Bool
modifyComponent store e f = do
    s <- readIORef (unComponentStore store)
    maybeIndex <- HT.lookup (componentStoreEntityMap s) e
    maybe (return False)
        (\i -> MV.unsafeModify (componentStoreVec s) f i >> return True)
        maybeIndex

getComponentSlice :: (MV.MVector v a) => ComponentStore v e a -> IO (v RealWorld a)
getComponentSlice store = do
    s <- readIORef (unComponentStore store)
    return $ MV.slice 0 (componentStoreSize s) (componentStoreVec s)

unsafeGetComponentVector :: (V.Vector v a) => ComponentStore (V.Mutable v) e a -> IO (v a, Int)
unsafeGetComponentVector store = do
    s <- readIORef (unComponentStore store)
    v <- V.unsafeFreeze (componentStoreVec s)
    return (v, componentStoreSize s)

cleanComponentStore :: (MV.MVector v a, MV.MVector v e) => ComponentStore v e a -> Int -> IO ()
cleanComponentStore store preserve = do
    v <- MV.new preserve
    ev <- MV.new preserve
    em <- HT.newSized preserve
    writeIORef (unComponentStore store) (ComponentStoreState 0 v ev em)

extendComponentStore :: (MV.MVector v a, MV.MVector v e) => ComponentStore v e a -> Int -> IO ()
extendComponentStore (ComponentStore ref) newSize = do
    s <- readIORef ref
    let currentVec = componentStoreVec s
        currentEVec = componentStoreEntityVec s
        currentSize = MV.length currentVec
    when (currentSize < newSize) $ do
        vec <- MV.new newSize
        MV.unsafeCopy (MV.unsafeTake currentSize vec) currentVec
        evec <- MV.new newSize
        MV.unsafeCopy (MV.unsafeTake currentSize evec) currentEVec
        atomicModifyIORef' ref (extendSize newSize vec evec)
    where
    extendSize n v ev state =
        let state' = state
                { componentStoreSize = n
                , componentStoreVec = v
                , componentStoreEntityVec = ev
                }
        in (state', ())
