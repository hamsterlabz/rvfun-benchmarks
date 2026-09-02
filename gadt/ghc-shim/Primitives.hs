module Primitives (primCharLE, primCharEQ) where
primCharLE :: Char -> Char -> Bool
primCharLE a b = a <= b
primCharEQ :: Char -> Char -> Bool
primCharEQ a b = a == b
