module Stack where


-- Here's a "regular" stack.  We model it as a list of ints.
type Stack = [Int]


-- push and pop opertions must return the modified stack; it's the only way to
-- track its state.
pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Stack -> Int -> ((), Stack)
push stack a = ((), a:stack)

peek :: Stack -> Int
peek (x:_) = x

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
  ((), newStack1) = push stack 3
  (_, newStack2) = pop newStack1
  in pop newStack2


-- The State type.  This is already defined for us in libraries like mtl
-- (https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html#g:2)
-- but we'll define it here for pedagogical purposes.  Also, its definition in
-- MTL is more complicated than this one.
data State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) s = f s

-- Functor, Applicative and Monad instances for State.

instance Functor (State s) where
  fmap f (State g) = State (\s ->
                              let (x, s') = g s
                                  y = f x
                              in (y, s')
                           )

instance Applicative (State s) where
  pure x = State (\s -> (x, s))

  (State fsab) <*> (State f) =
    State (\s -> let (g, s') = fsab s
                     (x, s'') = f s'
                 in (g x, s'')
          )

instance Monad (State s) where
  (State g) >>= f =
    State (\s -> let (x, s') = g s
                     State h = f x
            in h s')

-- Let's write new versions of push, pop and peek for State.
pop' :: State Stack Int
pop' = State (\(x:xs) -> (x, xs))

push' :: Int -> State Stack ()
push' x = State (\xs -> ((), x:xs))

peek' :: State Stack Int
peek' = State (\(x:xs) -> (x, x:xs))
  
stackManip2 = do
  push' 3
  a <- pop'
  pop'
