import Data.Digest.CRC32

main :: IO ()
main = putStrLn "Hi there!"
    print $ crc32 [1,2,3,4,5]