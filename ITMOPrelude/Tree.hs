{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show,Read,error, show)
import ITMOPrelude.Primitive


data Tree a = Node a (Tree a) (Tree a) 
            | Leaf                     
    deriving Show


emptyTree = Leaf

addToRoot x t = Node x t Leaf

addToLeft x (Node a l r) = Node a (addToLeft x l) r
addToLeft x Leaf = Node x Leaf Leaf

addToRight x (Node a l r) = Node a l (addToRight x r)
addToRight x Leaf = Node x Leaf Leaf

rotateLeft (Node x l (Node y l' r')) = Node y (Node x l l') r'

rotateRight (Node x (Node y l' r') r) = Node y l' (Node x r' r)

tmap :: (a -> b) -> Tree a -> Tree b
tmap f  Leaf = Leaf
tmap f (Node a l r) = Node (f a) (tmap f l) (tmap f r)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr f z Leaf = z
tfoldr f z (Node x l r) = f x (tfoldr f (tfoldr f z r) l)