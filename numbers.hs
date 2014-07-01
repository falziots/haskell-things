data N = Zero | S N



nat :: Integer -> N
nat 0 = Zero
nat n | n>0       = S(nat (n-1))
      | otherwise = nat (-n)
      
instance Num N where
  Zero + b  = b
  a + Zero  = a
  (S a) + b = a + (S b) 

  Zero * b  = Zero
  a * Zero  = Zero
  (S a) * b = (a*b) + b

  abs a = a

  signum a = S Zero

  fromInteger = nat

  negate a = a



--instance Enum N where





show' Zero = 0
show' (S n) = 1+(show' n)
instance Show N where
  show = show.show'


instance Eq N where
  Zero == Zero = True
  (S a) == (S b) = a == b

instance Ord N where
  compare Zero Zero = EQ
  compare _ Zero    = GT
  compare Zero _    = LT
  compare (S n) (S m) = compare n m

infinity :: N
infinity = S infinity

-- Now, if you do:
-- infinity > (nat n), where n is any Integer
-- it will return True. And it will behave similarly with <
-- >implying this is interesing







