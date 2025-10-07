-- Solutions Manual
module BankAccounts (
    BankAccount (..),
    accountId,
    balance,
    interest,
    deposit,
    deduct,
    compound,
    transfer,
    BSTMap (..),
    put,
    get,
    in',
    values,
) where

{- | The 'BankAccount' type represents two kinds of bank accounts:
 'NormalAccounts' are just regular accounts, and 'MinimalAccounts'
 are accounts such that if its balance is below $1000,
 at the time of compounding an administrative fee of $20 will
 be deducted before compounding. Both constructors receive
 an account ID, the bank account balance
 and an interest rate.
-}
data BankAccount
    = -- | The 'NormalAccount' constructor creates a
      -- normal bank account.
      NormalAccount
        String
        -- ^ The account ID
        Rational
        -- ^ The account balance
        Rational
        -- ^ The account interest rate
        -- | The 'MinimalAccount' constructor creates
        -- a minimal bank account.
    | MinimalAccount
        String
        -- ^ The account ID
        Rational
        -- ^ The account balance
        Rational
        -- ^ The account interest rate

{- | 'accountId' retrieves a bank account's ID.
>>> accountId (NormalAccount "a" 1000 0.1)
"a"
-}
accountId :: BankAccount -> String
accountId (NormalAccount s _ _) = s
accountId (MinimalAccount s _ _) = s

{- | 'balance' retrieves a bank account's bank balance.
>>> balance (NormalAccount "a" 1000 0.1)
1000 % 1
-}
balance :: BankAccount -> Rational
balance (NormalAccount _ x _) = x
balance (MinimalAccount _ x _) = x

{- | 'interest' retrieves a bank account's interest rate
>>> interest (NormalAccount "a" 1000 0.1)
1 % 10
-}
interest :: BankAccount -> Rational
interest (NormalAccount _ _ x) = x
interest (MinimalAccount _ _ x) = x

{- | 'setBalance' sets the bank balance of a bank account

>>> x = NormalAccount "a" 1000 0.1
>>> setBalance 2000 x
NormalAccount "a" (2000 % 1) (1 % 10)
-}
setBalance ::
    -- | The new bank balance of the account
    Rational ->
    -- | The bank account to set the balance of
    BankAccount ->
    -- | The resulting bank account with the new balance
    BankAccount
setBalance x (NormalAccount a _ c) = NormalAccount a x c
setBalance x (MinimalAccount a _ c) = MinimalAccount a x c

{- | 'deposit' deposits money into a bank account

>>> x = NormalAccount "a" 1000 0.1
>>> deposit 1000 x
NormalAccount "a" (2000 % 1) (1 % 10)
-}
deposit ::
    -- | The deposit amount
    Rational ->
    -- | The account to deposit into
    BankAccount ->
    -- | The new state of the account with the deposited funds
    BankAccount
deposit x b =
    let newBalance = x + balance b
     in setBalance newBalance b

{- | 'deduct' attempts to deduct money from a bank account. The result is a pair describing
the outcome of the deduction, where the @Bool@ value describes the deduction actually happened,
and the 'BankAccount' value is the resulting state of the bank account after the deduction.
The deduction does not happen if the bank account does not have sufficient funds.

>>> x = NormalAccount "a" 1000 0.1
>>> deduct 1000 x
(True,NormalAccount "a" (0 % 1) (1 % 10))
>>> deduct 1001 x
(False,NormalAccount "a" (1000 % 1) (1 % 10))
-}
deduct ::
    -- | The amount to deduct from the bank account
    Rational ->
    -- | the bank account to deduct from
    BankAccount ->
    -- | the result of the deduction
    (Bool, BankAccount)
deduct x b
    | bal < x = (False, b)
    | otherwise = (True, setBalance (bal - x) b)
  where
    bal = balance b

{- | Performs a transfer of some amount from one bank account into another. The result is
a triplet describing the outcome of the transaction, where the @Bool@ value describes
whether the transfer has succeeded, and the two 'BankAccount's descibe the new state
of the bank account after the transaction. The transfer does not happen if the
credit account has insufficient funds.

>>> x = NormalAccount "a" 1000 0.1
>>> y = NormalAccount "b" 0 0.1
>>> transfer 1000 x y
(True,NormalAccount "a" (0 % 1) (1 % 10),NormalAccount "b" (1000 % 1) (1 % 10))
-}
transfer ::
    -- | The transfer amount
    Rational ->
    -- | The account from which funds will be transferred
    BankAccount ->
    -- | The account which funds will be transferred to
    BankAccount ->
    -- | The result of the transfer
    (Bool, BankAccount, BankAccount)
transfer x from to = (res, from', to')
  where
    (res, from') = deduct x from
    to'
        | res = deposit x to
        | otherwise = to

{- | 'compound' compounds a bank account based on the interest rate and
type of the account. 'MinimalAccount's will have an administrative
fee of $20 being deducted before compounding if its balance is below
-}
compound :: BankAccount -> BankAccount
compound (NormalAccount s bal ir) = NormalAccount s (bal + bal * ir) ir
compound (MinimalAccount s bal ir) = MinimalAccount s bal'' ir
  where
    bal'
        | bal < 1000 = max (bal - 20) 0
        | otherwise = bal
    bal'' = bal' + bal' * ir

-- | 'BSTMap' is a binary search tree (unique sorted keys) of key-value pairs.
data BSTMap k v
    = -- | The empty 'BSTMap'
      Empty
    | -- | A tree node
      Node
        (BSTMap k v)
        -- ^ The left subtree
        k
        -- ^ The key at this node
        v
        -- ^ The value at this node
        (BSTMap k v)
        -- ^ The right subtree

{- | Puts a key-value pair in a 'BSTMap'. If the key already exists
then this operation replaces the existing value with the new value.
-}
put ::
    forall α β.
    (Ord α) =>
    -- | The key of the key-value pair
    α ->
    -- | The value of the key-value pair
    β ->
    -- | The map to put the key-value pair in
    BSTMap α β ->
    -- | The resulting map with the key-value pair
    BSTMap α β
put k v Empty = Node Empty k v Empty
put k v (Node l k' v' r)
    | k == k' = Node l k v r
    | k < k' = Node (put k v l) k' v' r
    | otherwise = Node l k' v' (put k v r)

{-# WARNING get "This is a partial function; it is undefined when the key is not present in the map." #-}

-- | Retrieves the value corresponding to the key in a 'BSTMap'.
get ::
    forall α β.
    (Ord α) =>
    -- | The key of the pair to retrieve
    α ->
    -- | The map to retrieve the value from
    BSTMap α β ->
    -- | The value corresponding to the key
    β
get _ Empty = undefined
get k (Node l k' v r)
    | k == k' = v
    | k < k' = get k l
    | otherwise = get k r

-- | Determines whether a key is present in a 'BSTMap'.
in' ::
    forall α β.
    (Ord α) =>
    -- | The key to search for
    α ->
    -- | The map to search
    BSTMap α β ->
    -- | Whether the key is present in the 'BSTMap'
    Bool
in' _ Empty = False
in' k (Node l k' _ r)
    | k == k' = True
    | k < k' = in' k l
    | otherwise = in' k r

-- | Puts all the values of a 'BSTMap' into a list.
values :: forall α β. BSTMap α β -> [β]
values Empty = []
values (Node l _ v r) = values l ++ (v : values r)
