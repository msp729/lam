module Logging (Level (Zero, Low, High), Logged, logAt, logFrom, region, writeLog, forget) where

import Prelude hiding (lines, log)

-- | @Logged l s a@ represents a value of type @a@, with "log lines" of type @s@, depending on a "log level" of type @l@
data Logged l s a = Log {ls :: l -> [s], v :: a}

-- | The log-levels for this app
data Level = Zero | Low | High deriving (Eq, Enum, Show)

instance Ord Level where
    Zero <= _ = True
    _ <= Zero = False
    Low <= _ = True
    _ <= Low = False
    High <= _ = True

instance Functor (Logged l s) where fmap f l = l {v = f (v l)}

instance Applicative (Logged l s) where
    pure = Log (const [])
    liftA2 f (Log l1 v1) (Log l2 v2) = Log (liftA2 (++) l1 l2) (f v1 v2)
    Log l1 f <*> Log l2 x = Log (liftA2 (++) l1 l2) (f x)

instance Monad (Logged l s) where
    Log l1 x >>= f = (f x) {ls = liftA2 (++) l1 (ls (f x))}

logAt :: (Eq l) => l -> s -> Logged l s ()
logAt l s = Log (\x -> if x == l then [s] else []) ()

logFrom :: (Ord l) => l -> s -> Logged l s ()
logFrom l s = Log (\x -> if x >= l then [s] else []) ()

region :: (s1 -> s2) -> Logged l s1 a -> Logged l s2 a
region f x = x {ls = map f . ls x}

writeLog :: l -> Logged l String a -> IO a
writeLog l (Log lines val) = do
    sequence_ $ putStrLn <$> lines l
    pure val

forget :: Logged l s a -> a
forget = v
