module Section1

%default total

Key : Type
Key = String

data Tree a = Leaf
            | Branch (Tree a) Key a (Tree a)

empty : Tree a
empty = Leaf

insert : Key -> a -> Tree a -> Tree a
insert key val Leaf = Branch Leaf key val Leaf
insert key val (Branch l k v r) with (key `compare` k)
  | LT = Branch (insert key val l) k v r
  | EQ = Branch l k val r
  | GT = Branch l k v (insert key val r)

lookup : Key -> Tree a -> Maybe a
lookup key Leaf = Nothing
lookup key (Branch l k v r) with (key `compare` k)
  | LT = lookup key l
  | EQ = Just v
  | GT = lookup key r

TreeSet : Type
TreeSet = Tree ()

insertSet : Key -> TreeSet -> TreeSet
insertSet key = insert key ()

insertAll : Foldable f => f String -> TreeSet -> TreeSet
insertAll keys set = foldl (flip insertSet) set keys

-- 1 c.
unbalanced1 : TreeSet
unbalanced1 = insertAll ["t", "s", "p", "i", "p", "f", "b", "s", "t"] empty

unbalanced2 : TreeSet
unbalanced2 = insertAll ["a", "b", "c", "d", "e", "f", "g", "h", "i"] empty
