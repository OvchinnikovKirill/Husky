{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length xs = case (xs) of
			Nil -> Zero
			Cons x xs -> d
				where d = Succ (length xs)

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ b = b
(Cons x xs) ++ b = Cons x (xs ++ b)

-- Список без первого элемента
tail :: List a -> List a
tail Nil = error "empty list"
tail (Cons x xs) = xs

-- Список без последнего элемента
init :: List a -> List a
init Nil = error "empty list"
init (Cons x Nil) = Nil
init (Cons x xs) = Cons x (init xs)

-- Первый элемент
head :: List a -> a
head Nil = error "empty list"
head (Cons x xs) = x

-- Последний элемент
last :: List a -> a
last Nil = error "empty list"
last (Cons x Nil) = x
last (Cons x xs) = last xs

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero xs = Nil
take (Succ _) Nil = Nil
take (Succ n) (Cons x xs) = Cons x (take n xs)


-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop Zero xs = xs
drop (Succ _) Nil = Nil
drop (Succ n) (Cons x xs) = drop n xs

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons x xs) = if' (p x) (Cons x (filter p xs)) (filter p xs)


-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter f Nil = Nil
gfilter f (Cons x xs) = case f x of
  Just y -> Cons y (gfilter f xs)
  Nothing -> gfilter f xs

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p Nil = Nil
takeWhile p (Cons x xs) = if' (p x) (Cons x (takeWhile p xs)) Nil

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p Nil = Nil
dropWhile p lst@(Cons x xs) = if' (p x) (dropWhile p xs) lst

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span p Nil = Pair Nil Nil
span p (Cons x xs) = if' (p x) 
			(let (Pair ts ds) = span p xs in Pair (Cons x ts) ds) 
		(Pair Nil (Cons x xs))

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p = span (not . p)

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons x xs) !! Zero = x
(Cons x xs) !! (Succ n) = xs !! n

-- Список задом на перёд
reverse :: List a -> List a
reverse = reverse' Nil
  where reverse' d (Cons x xs) = reverse' (Cons x d) xs
        reverse' d Nil = d

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) = subsequences xs ++ map (Cons x) (subsequences xs)

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations = undefined

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat x = Cons x $ repeat x

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f z (Cons x xs) = foldl f (f z x) xs
foldl f z Nil = z

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f z (Cons x xs) = (f z x) `Cons` (scanl f (f z x) xs)
scanl f z Nil = (Cons z Nil)

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z (Cons x xs) = f x $ foldr f z xs
foldr f z Nil = z

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr f z Nil = Cons z Nil
scanr f z (Cons x xs) = (f x $ head d) `Cons` d
  where d = scanr f z xs

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f = foldr helper Nil
  where helper a xs = (f a) `Cons` xs

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat = foldr (++) Nil

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap f (Cons x xs) = (f x) ++ (concatMap f xs)
concatMap f Nil = Nil

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip = zipWith Pair

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f (Cons x xs) (Cons y ys) = (f x y) `Cons` (zipWith f xs ys)
zipWith f Nil ys = Nil
zipWith f xs Nil = Nil