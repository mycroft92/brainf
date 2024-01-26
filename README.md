# BrainF Haskell
My first attempt at writing a Haskell evaluator for brainfuck language. This was primarily an attempt to get better at Haskell, by learning Monad Transformers. 

## List of Improvements
- Machine Tape can be made to a vector instead of lists
- Better testing (possibly with QuickCheck)
- Wrapping the evaluation in an ExceptionT Monad to account for failures
    a. The writeAt function in evaluator doesn't consider the limits of the tape

