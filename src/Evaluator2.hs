module Evaluator2 where

    import Token
    import Data.Word (Word8)
    import Data.Char (ord, chr)
    import Control.Monad.State ( MonadState(get, put), StateT (runStateT), MonadIO (liftIO), gets )
    import Control.Monad (when, void)

    data Tape = Tape [Word8] Word8 [Word8]

    initialTape :: Tape
    initialTape = Tape (repeat 0) 0 (repeat 0)

    newtype MachineState = MachineState {
        tape :: Tape
        -- idx  :: Int
    }

    type Machine a = StateT MachineState IO a


    getC :: Tape -> Word8
    getC (Tape _ c _) = c

    incPtr :: Tape -> Tape
    incPtr (Tape ls x (r:rs)) = Tape (x:ls) r rs
    
    decPtr :: Tape -> Tape
    decPtr (Tape (x:ls) y rs) = Tape ls x (y:rs)

    newMachine :: MachineState
    newMachine = MachineState initialTape


    write :: Tape -> Word8 -> Tape
    write (Tape l _ r) x = Tape l x r


    putByte :: Word8 -> Machine ()
    putByte a = do
        st <- get
        put (st {tape = write (tape st) a})

    getByte :: Machine Word8
    getByte = gets (getC . tape)

    setByte :: Machine ()
    setByte = do
        b <- liftIO getChar
        putByte (fromIntegral $ ord b)

    showByte :: Machine ()
    showByte = do
        b <- getByte
        liftIO $ putChar (chr. fromIntegral $ b)

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
        put (st {tape = incPtr (tape st)})

    decP :: Machine ()
    decP = do
        st <- get
        put (st {tape = decPtr (tape st)})


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

