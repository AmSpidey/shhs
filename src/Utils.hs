{-# LANGUAGE BangPatterns #-}
module Utils where

import System.Random
import Control.Monad
import Control.Monad.IO.Class

-- | The following 4 functions are adapted from the `extra` package.

-- | Like 'when', but where the test can be monadic.
whenM :: Monad m => m Bool -> m () -> m ()
whenM b t = ifM b t (pure ())

-- | Like 'unless', but where the test can be monadic.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b = ifM b (pure ())

-- | Like @if@, but where the test can be monadic.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b' <- b; if b' then t else f

-- | Like 'not', but where the test can be monadic.
notM :: Functor m => m Bool -> m Bool
notM = fmap not

-- like && but combining predicates.
(.&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .&& g = \a -> f a && g a

-- taken from Data.List.Extra
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x:xs) = Just (x:a, b)
    where Just (a,b) = unsnoc xs

joinByBackslash :: [String] -> [String]
joinByBackslash [] = []
joinByBackslash (line:rest) =
    let res = joinByBackslash rest in -- TODO make it tail recursive
        case unsnoc line of
            Nothing -> res
            Just (l, '\\') -> case res of
                [] -> [l]
                l':ls' -> (l ++ l'):ls'
            _ -> line:res

fixM :: (Monad m, Eq a) => (a -> m a) -> a -> m a
fixM f x = do
  fx <- f x
  if fx == x
    then return x
    else fixM f fx

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f l = concat <$> mapM f l

-- Count the number of times a predicate is true
-- From GHC implementation.

count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where go !n [] = n
        go !n (x:xs) | p x       = go (n+1) xs
                     | otherwise = go n xs

panic :: a
panic = error "This should never happen."

splitByPred :: (a -> Bool) -> [a] -> ([a], a, [a])
splitByPred f (x:xs)
  | f x = ([], x, xs)
  | otherwise =
    let (pref, x', suf) = splitByPred f xs
    in (x:pref, x', suf)
splitByPred _ [] = panic


fold1M :: Monad m => (a -> a -> m a) -> [a] -> m a
fold1M f (x:xs) = foldM f x xs
fold1M _ [] = error "fold1M"

randomString :: MonadIO m => Int -> m String
randomString i = liftIO $ replicateM i $ randomRIO ('a','z')

debug :: Bool
debug = True

dprint :: MonadIO m => String -> m ()
dprint = when debug . liftIO . putStrLn
