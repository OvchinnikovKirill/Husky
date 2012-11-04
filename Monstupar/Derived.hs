module Monstupar.Derived where

-- � ���� ������ �� ����� ��������� ���� Monstupar ��
import Monstupar.Core
-- ��������,
-- blah = Monstupar $ undefined
-- �� ��������������, ��������� ����������� Monstupar ���������,
-- ������� �������������� ������� ��� ����� ������ ��������� �����������
-- ������� �� Core.

--------------------------------------------------------------------------------
-- ������ ������� � �������� �����

-- �� �����

notok :: Monstupar s ()
notok = isnot ok

-- � ������ ����� ������ � �������� s

char :: Eq s => s -> Monstupar s s
char = like . (==)

-- � ������ ����� ������ ���-�� �� ������

oneOf :: Eq s => [s] -> Monstupar s s
oneOf l = like (`elem` l)

-- � �������� ������ ������ ����� ������ �����������

string :: Eq s => [s] -> Monstupar s [s]
string [] = return []
string (n:ns) = do
  nn <- char n
  nns <- string ns
  return $ nn:nns

-- "��������" -- ��������� ������ ������������ (���� ��� �����) ����� ��� �
-- ��������������� ����������

many :: Monstupar s a -> Monstupar s [a]
many p = many1 p <|> return []
  
-- ��������� � �����������! ������� �� ���, ����� � ��� ��-�� ������������� <|>
-- �� ��� � ������������� ����.

-- "������" -- ���� ��� ����� ���

many1 :: Monstupar s a -> Monstupar s [a]
many1 p = do
    e <- p
    es <- many p
    return (e:es)

-- "��������" -- ���� ��� ���� ���

optional :: Monstupar s a -> Monstupar s (Maybe a)
optional p = (p >>= return . Just) <|> return Nothing