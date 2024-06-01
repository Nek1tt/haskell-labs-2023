
newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
    fmap f (State g) = State $ \s ->
        let (a, newState) = g s
        in (f a, newState)

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    (State sf) <*> (State sa) = State $ \s ->
        let (f, s1) = sf s
            (a, s2) = sa s1
        in (f a, s2)

instance Monad (State s) where
  return = pure
  (State sa) >>= f = State $ \s ->
    let (a, s1) = sa s
        (State sb) = f a
    in sb s1

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = do
    s <- get
    put (f s)

execState :: State s a -> s -> s
execState act s = snd (runState act s)

evalState :: State s a -> s -> a
evalState act s = fst (runState act s)

