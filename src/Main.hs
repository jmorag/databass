module Main where
import BTree
import Pipes
import Data.ByteString.Lazy

main :: IO ()
main = print "Hello Joseph!"

buildBTree :: IO (ByteString)
buildBTree =
  let leaves = [BLeaf "a" (1 :: Int), BLeaf "b" 2, BLeaf "c" 3] in
         fromOrderedToByteString 5 100 (each leaves)
