-- Extremely simple but monstrously stupid (and slow) monadic parser
-- combinator library

module Monstupar.Core
    ( ParseError(..)
    , Monstupar, runParser
    , ok, isnot, eof, (<|>), like
    ) where

--------------------------------------------------------------------------------
-- �����������

-- ���� ����� ����������� ����� �������� �� ��, ��� ���������

data ParseError = ParseError
                deriving (Show) -- ���� �� show ���

newtype Monstupar s a = Monstupar { runParser :: [s] -> Either ParseError ([s], a) }

instance Monad (Monstupar s) where
    return a = Monstupar $ \s -> Right (s , a)
    ma >>= f = Monstupar $ \s -> case runParser ma s of
		Left ParseError -> Left ParseError
		Right (ns, a) -> runParser (f a) ns 

--------------------------------------------------------------------------------
-- ����������� �������.
-- ����� � ��������� ������� ������ ������, ���� �����

-- �� ������
ok :: Monstupar s ()
ok = Monstupar $ \s -> Right (s , ())

-- �� ������ ��������� �������� p

isnot :: Monstupar s () -> Monstupar s ()
isnot p = Monstupar $ \s -> case runParser p s of
	Left e -> Right (s , ())
	Right _ -> Left ParseError

-- ����� �����

eof :: Monstupar s ()
eof = Monstupar $ \s -> case s of
	[] -> Right (s , ())
	_  -> Left ParseError

infixr 2 <|>

-- ������� ������ ������, ���� �� ��������, �� ������

(<|>) :: Monstupar s a -> Monstupar s a -> Monstupar s a
a <|> b = Monstupar $ \s -> case runParser a s of
	Left e -> runParser b s
	Right (ns, a) -> Right(ns, a)

-- � ������ ����� ������ �����, ��������������� p

like :: (s -> Bool) -> Monstupar s s
like p = Monstupar $ \s -> case s of
	[] -> Left ParseError
	(n:ns) -> if p n
		then Right (ns, n)
		else Left ParseError

-- ���� ����� ��������� ��� �����-�� ����������� �������
-- ���� ��� �����������