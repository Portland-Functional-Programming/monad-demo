module Lib where

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Stack -> Int -> ((), Stack)
push stack a = ((), a:stack)

peek :: Stack -> Int
peek = undefined

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
  ((), newStack1) = push stack 3
  (a, newStack2) = pop newStack1
  in pop newStack2

data State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) s = f s --let (x, s') = f s in x

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

pop' :: State Stack Int
pop' = State (\(x:xs) -> (x, xs))

push' :: Int -> State Stack ()
push' x = State (\xs -> ((), x:xs))
  
stackManip2 = do
  push' 3
  a <- pop'
  pop'
