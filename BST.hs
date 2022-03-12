myTree = Nod 8
            (Nod 3
                Leaf
                (Nod 1 Leaf Leaf))
            (Nod 6 Leaf Leaf)

data Tree = Leaf | Nod Integer Tree Tree
    deriving (Eq, Show)

-- In order traversal
inOrder :: Tree -> [Integer]
inOrder Leaf = []
inOrder (Nod currentVal leftTree rightTree) = lst1 ++ [currentVal] ++ lst2
    where lst1 = inOrder leftTree
          lst2 = inOrder rightTree


-- Check if array is sorted
isSorted :: [Integer] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:(y:xs)) = (x<=y) && isSorted (y:xs)

-- Check if the tree is a BST
isBST :: Tree -> Bool
isBST xs = isSorted (inOrder xs)

-- Search in the BST
search :: Tree -> Integer -> Bool
search Leaf _ = False
search (Nod currentVal leftTree rightTree) searchedVal
    | currentVal < searchedVal  = search rightTree searchedVal
    | currentVal == searchedVal = True
    | currentVal > searchedVal  = search leftTree searchedVal

-- Insert in the BST
insBST :: Tree -> Integer -> Tree
insBST Leaf currentVal = Nod currentVal Leaf Leaf
insBST (Nod currentVal leftTree rightTree) insertedVal
    | currentVal < insertedVal  = Nod insertedVal (insert leftTree currentVal) rightTree
    | currentVal == insertedVal = Nod insertedVal leftTree rightTree
    | currentVal > insertedVal  = Nod insertedVal leftTree (insert rightTree currentVal)


main :: IO ()
main = do

   let myTreee = insBST myTree 7

   print (inOrder myTreee)

   print (val "c" [("a",5.13),("b",2.11)])
   print (val "c" [])

   return()
