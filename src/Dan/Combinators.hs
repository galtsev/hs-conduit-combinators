module Dan.Combinators where


import Prelude hiding (enumFromTo, repeat, iterate)
import Data.Conduit
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (Handle, hIsEOF)
import Data.ByteString (ByteString, hGet)
import qualified Data.ByteString as BS

yieldMany :: (Monad m, Foldable t) => t a -> ConduitT i a m ()
yieldMany t = mapM_ yield t


unfold :: (Monad m) => (b -> Maybe (a, b)) -> b -> ConduitT i a m ()
unfold g seed = do
    case g seed of
        Nothing -> return ()
        Just (a, s1) -> yield a >> unfold g s1


enumFromTo :: (Monad m, Enum a, Ord a) => a -> a -> ConduitT i a m ()
enumFromTo start end = do
    if start <= end 
        then do
            yield start
            enumFromTo (succ start) end 
        else return ()

iterate :: Monad m => (a -> a) -> a -> ConduitT i a m ()
iterate g a = do
    yield a
    iterate g (g a)

repeat :: Monad m => a -> ConduitT i a m ()
repeat a = do
    yield a
    repeat a

replicate :: Monad m => Int -> a -> ConduitT i a m ()
replicate n a = go n
    where
        go 0 = return ()
        go x = yield a >> go (x-1)

-- monadic

repeatM :: Monad m => m a -> ConduitT i a m ()
repeatM m = go
    where
        go = do
            a <- lift m
            yield a
            go

repeatWhileM :: Monad m => m a -> (a -> Bool) -> ConduitT i a m ()
repeatWhileM m g = go
    where
        go = do
            a <- lift m
            if g a then yield a >> go else return ()

replicateM :: Monad m => Int -> m a -> ConduitT i a m ()
replicateM n m = go n
    where
        go 0 = return ()
        go nx = do
            a <- lift m
            yield a
            go (nx - 1)

-- IO

-- sourceFile :: MonadResource m => FilePath -> ConduitT i ByteString m ()

sz :: Int
sz = 32 * 1024

sourceHandle :: MonadIO m => Handle -> ConduitT i ByteString m ()
sourceHandle h = go
    where
        go = do
            eof <- liftIO $ hIsEOF h
            if eof
                then return ()
                else do
                    buf <- liftIO $ hGet h sz
                    yield buf
                    go

-- Consumers
-- pure

drop :: Monad m => Int -> ConduitT i o m ()
drop n = go n
    where
        go 0 = return ()
        go x = do
            v <- await
            case v of
                Nothing -> return ()
                Just _ -> go (x-1)

dropWhile :: Monad m => (i -> Bool) -> ConduitT i o m ()
dropWhile g = go
    where
        go = do
            v <- await
            case v of
                Nothing -> return ()
                Just a -> if g a then go else leftover a

foldl :: Monad m => (r -> i -> r) -> r -> ConduitT i o m r
foldl g acc = go acc
    where
        go res = do
            v <- await
            case v of
                Nothing -> return res
                Just vv -> go (g res vv)