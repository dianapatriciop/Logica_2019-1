data Natural = Cero | Suc Natural deriving (Eq, Show)

mayorQue::Natural->Natural->Bool
mayorQue Cero Cero = False
mayorQue Cero _ = False
mayorQue _ Cero = True
mayorQue (Suc n) (Suc m) = mayorQue n m

menorQue::Natural->Natural->Bool
menorQue Cero Cero = False
menorQue Cero _ = True
menorQue _ Cero = False
menorQue (Suc n) (Suc m) = menorQue n m

igual::Natural->Natural->Bool
igual Cero Cero = True
igual Cero _ = False
igual _ Cero = False
igual (Suc n) (Suc m) = igual n m
