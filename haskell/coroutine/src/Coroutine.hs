module Coroutine
  ( Trampoline
  , bounce
  , return
  , (>>=)
  , lift
  , pause
  , run
  )
where

import           Data.Functor.Identity          ( Identity(..) )
import           Control.Applicative
import           Control.Monad                  ( liftM
                                                , liftM2
                                                , ap
                                                )
import           Control.Monad.Trans            ( MonadTrans(..) )

newtype Trampoline m r = Trampoline {
    bounce :: m (Either (Trampoline m r) r)
}

instance Monad m => Functor (Trampoline m) where
  fmap = liftM

instance Monad m => Applicative (Trampoline m) where
  pure  = return
  (<*>) = ap

-- (>>=) is the most important part.
--
-- We firstly call bounce on t to run the outer monad m. Think Trampoline as a list
-- of monads, this step evaluates the head element. If it produces m (Right r), then
-- we bind the result r to f and call bounce again, evaluating the bind result. In
-- case we use do notation to combine Trampolines, this causes recursive evaluation
-- of (>>) and (>>=) inside f's body. As long as it keep producing m (Right _), the
-- evaluation goes on until there is no more bind, and eventualy return m (Right r'),
-- which is then passed into the outer-most Trampoline constructor.
--
-- Now consider in one step, current monad m being evaluated is bind to pause. pause
-- produces a Trampoline monad in form of m (Left (m (Right ()))). Look at the handler
-- for Left in either, we see that the inner m (Right ()) is bind to current f. If you
-- look at f carefully, you see that f is always the remaining list of monads in the
-- original f, meaning f is the continuation of the current monad m. We bind the
-- evaluation result of the current monad to f but we don't evaluate it (no bounce is
-- called on it). Instead, we save it in a Trampoline monad as its Left part and
-- finish. The caller now have a Trampoline (m (Left continuation)) monad in hand and
-- it's free to run continuation at a later time.
--
-- By this implementation, it's clear that coroutine type is a Free monad transformer.

instance Monad m => Monad (Trampoline m) where
  return = Trampoline . return . Right
  t >>= f =
    Trampoline (bounce t >>= either (return . Left . (>>= f)) (bounce . f))

instance MonadTrans Trampoline where
  lift = Trampoline . liftM Right

pause :: Monad m => Trampoline m ()
pause = Trampoline (return $ Left $ return ())

run :: Monad m => Trampoline m r -> m r
run t = bounce t >>= either run return

-- In sight of a coroutine being a Free monad transformer and a Free monad is basically
-- a list of functors (monads in this case), interleaving multiple coroutine is just
-- merging mutiple lists of monads.

mzipWith
  :: Monad m
  => (a -> b -> c)
  -> Trampoline m a
  -> Trampoline m b
  -> Trampoline m c
mzipWith f t1 t2 = Trampoline $ liftM2 bind (bounce t1) (bounce t2) where
  bind (Left  a) (Left  b) = Left $ mzipWith f a b
  bind (Left  a) (Right b) = Left $ mzipWith f a (return b)
  bind (Right a) (Left  b) = Left $ mzipWith f (return a) b
  bind (Right a) (Right b) = Right $ f a b

interleave :: Monad m => [Trampoline m a] -> Trampoline m [a]
interleave = foldr (mzipWith (:)) (return [])

-- More commonly seen coroutines are generators and iteratees. The former send a value
-- back to the caller when suspend (yield), and the latter wait for a value when suspend
-- (await). We can easily modify the Trampoline type for them:
--
--newtype Generator a m r = Generator {
--  bounceGen :: m (Either (a, Generator a m r) r)
--}
--
--newtype Iteratee a m r = Iteratee {
--  bounceIter :: m (Either (a -> Iteratee a m r) r)
--}
--
-- The three versions of coroutine are very similar, and the only difference is the way
-- how the continuation is wrapped. However, all three wrappings are functors. With this
-- knowledge, it's the time to define the generic coroutine type.

newtype Coroutine s m r = Coroutine {
  resume :: m (Either (s (Coroutine s m r)) r)
}

instance (Functor s, Monad m) => Functor (Coroutine s m) where
  fmap = liftM

instance (Functor s, Monad m) => Applicative (Coroutine s m) where
  pure  = return
  (<*>) = ap

instance (Functor s, Monad m) => Monad (Coroutine s m) where
  return = Coroutine . return . Right
  c >>= f =
    Coroutine (resume c >>= either (return . Left . fmap (>>= f)) (resume . f))

instance Functor s => MonadTrans (Coroutine s) where
  lift = Coroutine . liftM Right

suspend :: (Functor s, Monad m) => s (Coroutine s m r) -> Coroutine s m r
suspend s = Coroutine $ return (Left s)

-- Now the three versions of coroutine can be defined as type aliases.

type Trampoline' m x = Coroutine Identity m x
type Generator a m x = Coroutine ((,) a) m x
type Iteratee a m x = Coroutine ((->) a) m x

yield :: Monad m => a -> Generator a m ()
yield x = suspend (x, return ())

await :: Monad m => Iteratee a m a
await = suspend return

-- We can further tune the interleave function to handle a pair of Generator and
-- Iteratee instead of Trampolines so that they can communicate by values. Since
-- the generator might produce less values than the iteratee requires, we wrap
-- the continuation of the iteratee requiring a Maybe a.

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = do
  a <- ma
  b <- mb
  f a b

pipe :: Monad m => Generator a m x -> Iteratee (Maybe a) m y -> m (x, y)
pipe g i = bindM2 proceed (resume g) (resume i) where
  proceed (Left  (a, c)) (Left  f) = pipe c (f $ Just a)
  proceed (Left  (a, c)) (Right y) = pipe c (return y)
  proceed (Right x     ) (Left  f) = pipe (return x) (f Nothing)
  proceed (Right x     ) (Right y) = return (x, y)

-- Finally, we can make one step further by allowing a coroutine to have both
-- yield and await operations. At one suspend point only one of them can be used,
-- of course. Such a coroutine is known as a transducer.

data EitherFunctor l r x = LeftF (l x) | RightF (r x)

instance (Functor l, Functor r) => Functor (EitherFunctor l r) where
  fmap f (LeftF  l) = LeftF (fmap f l)
  fmap f (RightF r) = RightF (fmap f r)

type Transducer a b m r = Coroutine (EitherFunctor ((->) (Maybe a)) ((,) b)) m r

yieldT :: Monad m => b -> Transducer a b m ()
yieldT x = suspend $ RightF (x, return ())

awaitT :: Monad m => Transducer a b m (Maybe a)
awaitT = suspend $ LeftF return

-- Beefed-up version of pipe as the scheduler of transducers.

