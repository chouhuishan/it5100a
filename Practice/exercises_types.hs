data Ingredients = B | C | P | V | O | M

price :: Ingredients -> Rational
price B = 0.5
price C = 0.8
price P = 1.5
price V = 0.7
price O = 0.4
price M = 0.9

type Burger = [Ingredients]

burgerPrice :: Burger -> Rational
burgerPrice [] = 0
burgerPrice (x : xs) = price x + burgerPrice xs

data BankAccount
  = NormalAccount {accountId :: String, balance :: Rational, interestRate :: Rational}
  | MinimalAccount {accountId :: String, balance :: Rational, interestRate :: Rational}
  deriving (Eq, Show)

applyInterest :: BankAccount -> BankAccount
applyInterest (NormalAccount i b r) = NormalAccount i (b * (1 + r)) r
applyInterest (MinimalAccount i b r) = MinimalAccount i (b * (1 + r)) r

-- deposit
deposit :: Rational -> BankAccount -> BankAccount
deposit amt (NormalAccount i b r) = NormalAccount i (b + amt) r
deposit amt (MinimalAccount i b r) = MinimalAccount i (b + amt) r

-- deduct
deduct :: Rational -> BankAccount -> (Bool, BankAccount)
deduct amt (NormalAccount i b r)
  | amt <= b = (True, NormalAccount i (b - amt) r)
  | otherwise = (False, NormalAccount i b r)
deduct amt (MinimalAccount i b r)
  | amt <= b = (True, MinimalAccount i (b - amt) r)
  | otherwise = (False, MinimalAccount i b r)

-- compounding interest
compound :: BankAccount -> BankAccount
compound (NormalAccount i b r) = NormalAccount i (b * (1 + r)) r
compound (MinimalAccount i b r)
  | b < 1000 =
      let balanceAfterFee = if b >= 20 then b - 20 else 0
       in MinimalAccount i (balanceAfterFee * (1 + r)) r
  | otherwise = MinimalAccount i (b * (1 + r)) r

-- bank transfer
transfer :: Rational -> BankAccount -> BankAccount -> (Bool, BankAccount, BankAccount)
transfer amt (NormalAccount i b r) debit
  | amt <= b = (True, NormalAccount i (b - amt) r, deposit amt debit)
  | otherwise = (False, NormalAccount i b r, debit)
transfer amt (MinimalAccount i b r) debit
  | amt <= b = (True, MinimalAccount i (b - amt) r, deposit amt debit)
  | otherwise = (False, NormalAccount i b r, debit)
