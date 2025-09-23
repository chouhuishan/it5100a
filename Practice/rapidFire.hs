data Ingredient = B | C | P | V | O | M deriving (Eq, Show)

price :: Ingredient -> Rational
price B = 0.5
price C = 0.8
price P = 1.5
price V = 0.7
price O = 0.4
price M = 0.9

burgerPrice :: [Ingredient] -> Rational
burgerPrice [] = 0
burgerPrice (x : xs) = price x + burgerPrice xs

parseBurger :: String -> Maybe [Ingredient]

data BankAccount
  = NormalAccount {accountId :: String, balance :: Rational, interestRate :: Rational}
  | MinimalAccount {accountId :: String, balance :: Rational, interestRate :: Rational}
  deriving (Eq, Show)

deposit :: Rational -> BankAccount -> BankAccount
deposit amt (NormalAccount i b r) = NormalAccount i (b + amt) r
deposit amt (MinimalAccount i b r) = MinimalAccount i (b + amt) r

deduct :: Rational -> BankAccount -> (Bool, BankAccount)
deduct amt (NormalAccount i b r)
  | b < 0 = (False, NormalAccount i b r)
  | amt <= b = (True, NormalAccount i (b - amt) r)
  | otherwise = (False, NormalAccount i b r)
deduct amt (MinimalAccount i b r)
  | b < 0 = (False, MinimalAccount i b r)
  | amt <= b = (True, MinimalAccount i (b - amt) r)
  | otherwise = (False, MinimalAccount i b r)

compound :: BankAccount -> BankAccount
compound (NormalAccount i b r) = NormalAccount i (b * (1 + r)) r
compound (MinimalAccount i b r)
  | b < 1000 = MinimalAccount i ((max 0 (b - 20)) * (1 + r)) r
  | otherwise = MinimalAccount i (b * (1 + r)) r

transfer :: Rational -> BankAccount -> BankAccount -> (Bool, BankAccount, BankAccount)
transfer amt (NormalAccount i b r) debit
  | amt <= b = (True, NormalAccount i (b - amt) r, deposit amt debit)
  | otherwise = (False, NormalAccount i b r, debit)
transfer amt (MinimalAccount i b r) debit
  | amt <= b = (True, MinimalAccount i (b - amt) r, deposit amt debit)
  | otherwise = (False, MinimalAccount i b r, debit)
