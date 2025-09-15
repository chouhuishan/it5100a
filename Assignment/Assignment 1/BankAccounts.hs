module BankAccounts (BankAccount (..), accountId, balance, interest, deposit, deduct, applyInterest, transfer, compound, BSTMap (..), put, get, in', values) where

-- TODO: Define the BankAccount ADT.
data BankAccount
  = NormalAccount String Rational Rational
  | MinimalAccount String Rational Rational

applyInterest :: BankAccount -> BankAccount
applyInterest (NormalAccount i b r) = NormalAccount i (b * (1 + r)) r
applyInterest (MinimalAccount i b r) = MinimalAccount i (b * (1 + r)) r

-- TODO: Define the accountId function.
accountId :: BankAccount -> String
accountId (NormalAccount i _ _) = i
accountId (MinimalAccount i _ _) = i

-- TODO: Define the balance function.
balance :: BankAccount -> Rational
balance (NormalAccount _ b _) = b
balance (MinimalAccount _ b _) = b

-- TODO: Define the interest function.
interest :: BankAccount -> Rational
interest (NormalAccount _ _ r) = r
interest (MinimalAccount _ _ r) = r

-- TODO: Define the deposit function.
deposit :: Rational -> BankAccount -> BankAccount
deposit amt (NormalAccount i b r) = NormalAccount i (b + amt) r
deposit amt (MinimalAccount i b r) = MinimalAccount i (b + amt) r

-- TODO: Define the deduct function.
deduct :: Rational -> BankAccount -> (Bool, BankAccount)
deduct amt (NormalAccount i b r)
  | amt <= b = (True, NormalAccount i (b - amt) r)
  | otherwise = (False, NormalAccount i b r)
deduct amt (MinimalAccount i b r)
  | amt <= b = (True, MinimalAccount i (b - amt) r)
  | otherwise = (False, MinimalAccount i b r)

-- TODO: Define the transfer function.
transfer :: Rational -> BankAccount -> BankAccount -> (Bool, BankAccount, BankAccount)
-- Rational = amount, BankAccount = credit(FROM) account, BankAccount = debit(TO) account
-- Returns (Success/Fail, new_credit, new_debit)
transfer amt (NormalAccount i b r) debit
  | amt <= b = (True, NormalAccount i (b - amt) r, deposit amt debit)
  | otherwise = (False, NormalAccount i b r, debit)
transfer amt (MinimalAccount i b r) debit
  | amt <= b = (True, MinimalAccount i (b - amt) r, deposit amt debit)
  | otherwise = (False, MinimalAccount i b r, debit)

-- TODO: Define the compound function.
compound :: BankAccount -> BankAccount
compound (NormalAccount i b r) = NormalAccount i (b * (1 + r)) r
compound (MinimalAccount i b r)
  | b < 1000 =
      let balanceAfterFee =
            if b >= 20 then b - 20 else 0
       in MinimalAccount i (balanceAfterFee * (1 + r)) r
  | otherwise = MinimalAccount i (b * (1 + r)) r

-- TODO: Define the BSTMap ADT
data BSTMap k v -- BSTMap is a type constructor with parameters k, v
  = Empty -- represents empty tree
  | Node (BSTMap k v) k v (BSTMap k v) -- Node is a data constructor with 4 fields (curried function)
  -- Node :: BSTMap k, v (L sub-tree) -> k (key) -> v (value) -> BSTMap k v (R sub-tree) -> BSTMap k, v

-- TODO: Define the put function.
put :: (Ord k) => k -> v -> BSTMap k v -> BSTMap k v
put k v Empty = Node Empty k v Empty
put k v (Node l k' v' r)
  | k < k' = Node (put k v l) k' v' r
  | k > k' = Node l k' v' (put k v r)
  | otherwise = Node l k v r

-- TODO: Define the get function.
get :: (Ord k) => k -> BSTMap k v -> v
get _ Empty = undefined
get x (Node l k v r)
  | x == k = v
  | x < k = get x l
  | otherwise = get x r

-- possible??
-- get :: Ord k => k -> BSTMap k v -> Maybe v
-- get _ Empty = Nothing
-- get x (Node l k v r)
--  | x == k = Just v
--  | x < k = get x l
--  | otherwise = get x r

-- TODO: Define the in' function.
in' :: (Ord k) => k -> BSTMap k v -> Bool
in' _ Empty = False
in' x (Node l k _ r)
  | x == k = True
  | x < k = in' x l
  | otherwise = in' x r

-- TODO: Define the values function.
values :: BSTMap k v -> [v]
values Empty = []
values (Node l _ v r) = values l ++ (v : values r)

-- TODO: Define the Op ADT
data Op

-- TODO: Define the processOne function.
processOne = undefined

-- TODO: Define the processAll function.
processAll = undefined
