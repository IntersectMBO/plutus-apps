{-# language ForeignFunctionInterface #-}
module Main where

import Foreign

foreign import ccall "wrapper" createAddPtr :: (Int -> Int -> Int) -> IO (FunPtr (Int -> Int -> Int))
foreign import ccall "fold" fold :: (FunPtr (Int -> Int -> Int)) -> Int -> Int

add :: Int -> Int -> Int
add x y = x+y

main :: IO ()
main = do
    addPtr <- createAddPtr add
    -- you can use addPtr like any other FunPtr (e.g. give it to foreign code)
    print $ foldl (+) 0 [(0::Int)..10]
    print $ fold addPtr 10
    -- you MUST free the FunPtr, otherwise it won't be collected
    freeHaskellFunPtr addPtr
