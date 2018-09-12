data Natural = Cero | Suc Natural deriving (Eq, Show)

mayorQue::Natural->Natural->Bool
mayorQue Cero Cero = False
mayorQue Cero _ = False
mayorQue _ Cero = True
mayorQue (Suc n) (Suc m) = menorque n m
