{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read)

---------------------------------------------
-- ��������� ������-���������

-- ������������� �����������
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- ����� ������������� �����������
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- ����������� ���������
undefined = undefined

-- ���� ������� ����������� ��� �����, ��������� �� undefined ��������.
-- ����� ����� ����� ������������ (natEq � natLt --- ������� ���������).

-------------------------------------------
-- ����������� ����

-- ��� � ������������ ���������
data Unit = Unit deriving (Show,Read)

-- ����, ������������
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- �������, ��������������
data Either a b = Left a | Right b deriving (Show,Read)

-- ������ ������� ������, ��������� Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- ������ ������� ������, ��������� Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- ������� ��������, ��� ���������� if � ���� Bool ������������ ������,
-- ���� case ������ ��������.

-- �� ��� ����� ����������� ���� if
if' True a b = a
if' False a b = b

-- ����������. ������������� ���, ������������ ��������� ���������
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- ������ ��������

-- ���������� "��"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- ���������� "�"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- ���������� "���"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- ����������� �����

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- ���������� ��� ����������� �����
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero			= EQ		
natCmp (Succ n) Zero 		= GT
natCmp Zero (Succ m) 		= LT
natCmp (Succ n) (Succ m)	= natCmp n m

-- n ��������� � m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n ������ m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

infixl 6 +.
-- �������� ��� ����������� �����
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- ��������� ��� ����������� �����
(-.) :: Nat -> Nat -> Nat
Zero     -. m = Zero
m -. Zero = m
(Succ n) -. (Succ m) = n -. m

infixl 7 *.
-- ��������� ��� ����������� �����
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- ����� � ������� �� ������� n �� m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n m = case  (natCmp n m) of
  LT -> Pair Zero n
  EQ -> Pair natOne Zero
  GT -> let rec = natDivMod (n -. m) m in
    Pair (Succ $ fst rec) (snd rec)
  
  
natDiv n = fst . natDivMod n -- �����
natMod n = snd . natDivMod n -- �������

-- ����� GCD ���������� ������� (������ �������� 2 (���������������� �����) + 1 (���) �������)
gcd :: Nat -> Nat -> Nat
gcd n Zero = n
gcd n m = gcd m (n `natMod` m)


-------------------------------------------
-- ����� �����

-- ���������, ����� ������������� ������� ����� ���� ������������
data Int = UNDEFINED deriving (Show,Read)

intZero   = NonNeg Zero   -- 0
intOne    = NonNeg $ Succ Zero    -- 1
intNegOne = Neg Zero -- -1

natural (NonNeg n) = n
natural (Neg _) = error "natural: negative Integer"

-- n -> - n
intNeg :: Int -> Int
intNeg (NonNeg (Succ n)) = Neg n
intNeg (Neg n) = NonNeg $ Succ n
intNeg (NonNeg Zero) = NonNeg Zero



-- ������ ����� ��� ��� �����������
intCmp :: Int -> Int -> Tri
intCmp (Neg n) (NonNeg m) = LT
intCmp (NonNeg n) (Neg m) = GT
intCmp (NonNeg n) (NonNeg m) = natCmp n m
intCmp (Neg n) (Neg m) = natCmp m n

intEq :: Int -> Int -> Bool
intEq n m = case intCmp n m of
  EQ -> True
  otherwise -> False

intLt :: Int -> Int -> Bool
intLt n m = case intCmp n m of
  LT -> True
  otherwise -> False

infixl 6 .+., .-.
-- � ���� ��� ������������ �������� ���� �� ��� �����
(.+.) :: Int -> Int -> Int
(NonNeg (Succ n)) .+. (Neg Zero) = NonNeg n -- -1
(NonNeg Zero) .+. n = n --0+something
(NonNeg (Succ n)) .+. (NonNeg (Succ m)) = NonNeg $ n +. m --2pos
(Neg (Succ n)) .+. (Neg (Succ m)) = Neg $ Succ $ n +. m --2neg
(NonNeg (Succ n)) .+. (Neg (Succ m)) = NonNeg n .+. Neg m --pos+neg
(Neg n) .+. (NonNeg m) = (NonNeg m) .+. (Neg n) --neg+pos

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
(NonNeg Zero) .*. (Neg _) = NonNeg Zero
(NonNeg n) .*. (NonNeg m) = NonNeg $ n *. m
(Neg n) .*. (Neg m) = NonNeg $ (Succ n) *. (Succ m)
(NonNeg n) .*. (Neg m) = Neg $ (n *. (Succ m)) -. natOne
n@(Neg _) .*. m@(NonNeg _) = m .*. n

intSign (NonNeg Zero) = intZero
intSign (NonNeg (Succ _)) = intOne
intSign (Neg _) = intNegOne

intDiv :: Int -> Int -> Int
intDiv (NonNeg n) (NonNeg m) = NonNeg $ natDiv n m
intDiv n m = intSign n .*. intSign m .*. (intAbs n `intDiv` intAbs m)

-------------------------------------------
-- ������������ �����

data Rat = Rat Int Nat

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- � ������������ ��� ���� �������� ��������
ratInv :: Rat -> Rat
ratInv (Rat x y) = Rat (intSign x .*. NonNeg y) (natural . intAbs $ x)

-- ������ ��� ������
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat n Zero) (Rat a b) = error "first"
ratCmp (Rat n m) (Rat a Zero) = error "second"
ratCmp (Rat n (Succ m)) (Rat a (Succ b)) = intCmp (n .*. NonNeg(Succ b)) (a .*. NonNeg(Succ m))

ratEq :: Rat -> Rat -> Bool
ratEq n m = case ratCmp n m of
  EQ -> True
  otherwise -> False

ratLt :: Rat -> Rat -> Bool
ratLt n m = case ratCmp n m of
  LT -> True
  otherwise -> False

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat n Zero) %+ (Rat a b) = error "first"
(Rat n m) %+ (Rat a Zero) = error "second"
(Rat n (Succ m)) %+ (Rat a (Succ b)) =  Rat (a .*. NonNeg (Succ b) .+. a .*. NonNeg (Succ m)) (Succ m *. Succ b)


(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat n Zero) %* (Rat a b) = error "first"
(Rat n m) %* (Rat a Zero) = error "second"
(Rat n (Succ m)) %* (Rat a (Succ b)) =  Rat (n .*. a) (Succ m *. Succ b)


(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- �������� ��� ���������.
-- ���������� �����, �� ������������ ����� � ����

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- ������������� �����������
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- � ��� ������������� �����������
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b