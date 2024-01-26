module Evaluator where

    import Token
    import Data.Char (ord, chr)
    import Control.Monad.State ( MonadState(get, put), StateT (runStateT), MonadIO (liftIO) )
    import Control.Monad (when, void)

    data MachineState = MachineState {
        tape :: [Int],
        idx  :: Int
    }

    type Machine a = StateT MachineState IO a

    newMachine :: MachineState
    newMachine = MachineState (replicate 30000 0) 0

    writeAt :: [a] -> Int -> a -> [a]
    writeAt ls x i = let (fr, _:bk) = splitAt x ls in
            fr ++ i:bk


    putByte :: Int -> Machine ()
    putByte a = do
        st <- get
        put (st {tape = writeAt (tape st) (idx st) a})

    getByte :: Machine Int
    getByte = do
        st <- get
        return $ tape st !! idx st

    setByte :: Machine ()
    setByte = do
        b <- liftIO getChar
        putByte (ord b)

    showByte :: Machine ()
    showByte = do
        b <- getByte
        liftIO $ putChar (chr b)

    inc :: Machine ()
    inc = do
        v <- getByte
        putByte (v+1)

    dec :: Machine ()
    dec = do
        v <- getByte
        putByte (v-1)

    incP :: Machine ()
    incP = do
        st <- get
        put (st {idx = idx st+1})

    decP :: Machine ()
    decP = do
        st <- get
        put (st {idx = idx st-1})


    eval' :: Token -> Machine ()
    eval' Put   = showByte
    eval' Get   = setByte
    eval' DecP  = decP
    eval' IncP  = incP
    eval' Plus  = inc
    eval' Minus = dec
    eval' (Loop ts) = looper ts

    looper :: [Token] -> Machine ()
    looper ts = do
        val <- getByte
        when (val /= 0) $ do
            mapM_ eval' ts
            looper ts

    eval :: [Token] -> Machine ()
    eval ts = do
        mapM_ eval' ts

    runProgram :: Program -> IO ()
    runProgram p = void (runStateT (eval p) newMachine)

    -- check :: Machine Int
    -- check = do
    --     idx <$> get

    -- newtype StateEither s e a = StateEither { runStateEither :: s -> (s, Either e a)}  deriving Functor

    -- instance Applicative (StateEither s e) where
    --     pure a = StateEither $ \s -> (s, Right a)
    --     (<*>) = ap
    --     -- StateEither ff <*> StateEither fa =  StateEither $ \s0 ->
    --     --     case ff s0 of
    --     --         (s1, Left e)  -> (s1, Left e)
    --     --         (s1, Right f) -> 
    --     --             case fa s1 of
    --     --                 (s2, Left e) -> (s2, Left e)
    --     --                 (s2, Right a) -> (s2, Right (f a))

    -- instance Monad (StateEither s e) where
    --     return = pure
    --     StateEither p >>= g = StateEither $ \s ->
    --         case p s of
    --             (s1, Left e)  -> (s1, Left e)
    --             (s1, Right a) -> runStateEither (g a) s1

    -- modifyM :: (Monad (t m), MonadTrans t, MonadState a m) => (a -> t m a) -> t m ()
    -- modifyM f = do
    --     s0 <- lift get
    --     s1 <- f s0
    --     lift $ put $! s1