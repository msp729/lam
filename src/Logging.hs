module Logging (Logged, log, region, writeLog, forget) where

import Prelude hiding (log, lines)

data Logged s a = Log {ls :: [s], v :: a}

instance Functor (Logged s) where fmap f l = l{v = f (v l)}
instance Applicative (Logged s) where
    pure = Log []
    liftA2 f (Log l1 v1) (Log l2 v2) = Log (l2 ++ l1) (f v1 v2)
    Log l1 f <*> Log l2 x = Log (l2 ++ l1) (f x)

instance Monad (Logged s) where
    Log l1 x >>= f = (f x){ls = ls (f x) ++ l1}

log :: s -> Logged s ()
log s = Log [s] ()

region :: (s -> s) -> Logged s a -> Logged s a
region f x = x{ls = f <$> ls x}

writeLog :: Logged String a -> IO a
writeLog (Log lines val) = do
    sequence_ $ putStrLn <$> reverse lines
    pure val

forget :: Logged s a -> a
forget = v
