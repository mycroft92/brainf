module Token where

    data Token = Put
        | DecP
        | IncP
        | Plus
        | Minus
        | Get 
        | Loop [Token]
    
    instance Show Token where
        show Put   = "."
        show DecP  = "<"
        show IncP  = ">"
        show Plus  = "+"
        show Minus = "-"
        show Get   = ","
        show (Loop ts) = "[" ++ concatMap show ts ++"]"

    type Program = [Token]
    
    -- data Phrase =
    --     OP Token
    --     | Loop [Token] 
    -- instance Show Phrase where
    --     show (OP t)    = show t
    --     show (Loop ts) = "["++ concatMap show ts ++ "]"
    -- type Program = [Phrase]