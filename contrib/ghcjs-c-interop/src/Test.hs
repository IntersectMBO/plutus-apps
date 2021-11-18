{-# language ForeignFunctionInterface #-}
module Test where

import Foreign

foreign import ccall "wrapper" createAddPtr :: (Int -> Int -> IO Int) -> IO (FunPtr (Int -> Int -> IO Int))
foreign import ccall "test_fold" test_fold :: (FunPtr (Int -> Int -> IO Int)) -> Int -> Int

add :: Int -> Int -> IO Int
add x y = do
    print $ (show x) ++ " + " ++ (show y) ++ " = " ++ (show $ x + y)
    return $ x+y
{-# NOINLINE add #-}

doFold :: IO ()
doFold = do
    addPtr <- createAddPtr add
    -- you can use addPtr like any other FunPtr (e.g. give it to foreign code)
    print $ foldl (+) 0 [(0::Int)..10]
    print $ test_fold addPtr 10
    -- you MUST free the FunPtr, otherwise it won't be collected
    freeHaskellFunPtr addPtr