
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Data.Word
import Data.List ( elemIndex )
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Map as M
import Data.Map ( Map )

import Text.Megaparsec hiding (parse, State(..) )
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Lexer as L
import Text.Printf

import System.Environment
import System.Exit
import System.IO

import Control.Monad
import Control.Monad.State.Lazy
import Control.Exception hiding (throw)


commands :: String
commands = "><+-.,[]"


-- | represent state of the interpreter
data InterpState = InterpState {
      dataPtr :: Int            -- ^ the data pointer
    , progPtr :: Int            -- ^ the instruction pointer
    , cells   :: Map Int Word8  -- ^ memory cells
    , prog    :: [Command]      -- ^ the program being executed
    , jumpLocs:: [(Int, Int)]   -- ^ "jump locations" - the indices of matching
                                 --   pairs of brackets, so we can loop.
  }
  deriving (Show, Eq)

adjustCell
  :: InterpState -> Int -> (Word8 -> Word8) -> InterpState
adjustCell interpSt idx f =
  let newCells = M.adjust f idx (cells interpSt)
  in  interpSt { cells = newCells }

alterCell
  :: InterpState -> Int -> (Maybe Word8 -> Maybe Word8) -> InterpState
alterCell interpSt idx f =
  let newCells = M.alter f idx (cells interpSt)
  in  interpSt { cells = newCells }

-- | mapping of tokens to commands
charToCommand = [
    ('>', DPtrInc)
  , ('<', DPtrDec)
  , ('+', ValInc)
  , ('-', ValDec)
  , ('.', Print)
  , (',', Get)
  , ('[', JmpFwd)
  , (']', JmpBak)
  ]

-- | megaparsec parser type
type Parser a = Parsec Dec String a

-- | parse a "token", return a command plus source position
fsckCommand :: Parser (Command, SourcePos)
fsckCommand = lexeme $ do
  c <- oneOf commands
  let cmd = fromJust $ lookup c charToCommand
  pos <- getPosition
  return (cmd, pos)

-- | 'whitespace' is simply all things that aren't commands
sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = void $ optional $ many $ noneOf commands

lexeme = L.lexeme sc

-- well, we don't allow a first comment, but we do
--  allow a last.
fsckProg :: Parser [(Command, SourcePos)]
fsckProg = some fsckCommand


parseFsckProg :: String -> [(Command, SourcePos)]
parseFsckProg str = 
  case MP.parse (fsckProg <* eof) "<file>" str of
    Left err -> error $  MP.parseErrorPretty err
    Right res -> res

-- | the interpreter commands
data Command = 
        DPtrInc     -- ^ increment data pointer
      | DPtrDec     -- ^ decrement data pointer
      | ValInc      -- ^ increment pointed-to cell
      | ValDec      -- ^ decrement pointed-to cell
      | Print       -- ^ print cell pointed to as ASCII, probably
      | Get         -- ^ get a byte from the user, store in current cell
      | JmpFwd      -- ^ jump forward (if cell val == 0) to whatever position
                    --   matches this one in our list of "jump locations"
      | JmpBak      -- ^ jump backward, if cell val /= 0
      | Halt        -- ^ used for debugging
  deriving (Eq)

instance Show Command where
  show DPtrInc = ">"
  show DPtrDec = "<"
  show ValInc  = "+"
  show ValDec  = "-"
  show Print   = "."
  show Get     = ","
  show JmpFwd  = "["
  show JmpBak  = "]"
  show Halt    = "HALT"

-- | construct initial interpreter state
initState :: [Command] -> IO InterpState
initState prog = do
  let cells = M.empty
  -- check that the brackets are properly nested,
  -- get a jump table if so.
  case jumpLocations prog of
    Left errMsg -> ioError $ userError errMsg
    Right locs  -> return $ InterpState 0 0 cells prog locs



-- debug flag
debug = False

--fnordWithA = runStateT comp 'a'
--  where
--    --comp :: State Char Integer
--    comp = do
--              -- putStrLn "hi" 
--              return (0 :: Integer)

progPtrMod :: MonadState InterpState m => (Int -> Int) -> m ()
progPtrMod f =
  modify' (\st -> st { progPtr = f (progPtr st) } )

-- | increment the instruction pointer
progPtrInc :: MonadState InterpState m => m ()
progPtrInc =
  progPtrMod (+1)

-- | modify the data pointer by applying some function
dataPtrMod :: MonadState InterpState m => (Int -> Int) -> m ()
dataPtrMod f =
  modify' (\st -> st { dataPtr = f (dataPtr st) })

-- | `cellMod cellIdx f` - modify the cell at position `cellIdx`
--  by applying function f. (cells are initially assumed to be 0)
cellMod
  :: MonadState InterpState m => Int -> (Word8 -> Word8) -> m ()
cellMod cellIdx f =
  --let f' currVal = case currVal of
  --                    Nothing -> Just $ f currVal
  -- modify' (\st -> adjustCell st cellIdx f)
  modify' (\st -> alterCell st cellIdx f')
  where
    f' currVal = case currVal of
                    Nothing -> Just $ f 0
                    Just v  -> Just $ f v

-- | get value of a cell
cellGet :: (MonadState InterpState m, MonadIO m) => Int -> m Word8
cellGet cellIdx = do
  theCells <- gets cells
  st <- get
  return $ M.findWithDefault 0 cellIdx theCells

-- | put value to a cell
cellPut :: MonadState InterpState m => Int -> Word8 -> m () 
cellPut cellIdx newVal =
  cellMod cellIdx (const newVal)

-- | given the interpreter state, find the destination of the
--  jump command we're sitting on.
getJmpDest
  :: (MonadState InterpState m, MonadIO m) => Command -> m Int
getJmpDest cmd = do
  pPtr <- gets progPtr
  locs <- gets jumpLocs
  -- find correct spot in the jump table
  let tableIdx = case cmd of 
                    JmpFwd -> elemIndex pPtr $ fst $ unzip locs 
                    JmpBak -> elemIndex pPtr $ snd $ unzip locs
                    _      -> error "can't call getJmpDest w/ non jump cmd" 
  -- handle bad indx
  tableIdx <- case tableIdx of
    Nothing -> throw$ "instruction ptr not found in jump table!: " ++ show pPtr
    Just idx -> return idx
  let newProgPtr = case cmd of
                    JmpFwd -> snd $ locs !! tableIdx 
                    JmpBak -> fst $ locs !! tableIdx 
  return newProgPtr

-- | throw an error in MonadIO
throw :: MonadIO m => String -> m a
throw message = liftIO $ ioError $ userError message

-- | execute one interpreter instruction from the program.
-- 
-- MonadIO is used for debugging, and early exits,
-- and doing "print" and "read" instructions.
-- But could easily be rewritten, so as to (e.g.) print and read
-- using a stream or list of bytes.
exec :: (MonadState InterpState m, MonadIO m) => m ()
exec = do
  pPtr    <-               gets progPtr
  progLen <- length    <$> gets prog
  cmd     <- (!! pPtr) <$> gets prog 
  when ((pPtr < 0)  || (pPtr >= progLen)) $ 
    throw $ "prog ptr out of bounds, is : " ++ show pPtr
  when debug $ 
    liftIO $ putStrLn $ "tick. pPtr = " ++ show pPtr ++ ", instr: " ++ show cmd

  cellIdx <- gets dataPtr
  -- cellVal <- cellGet cellIdx

  if cmd /= JmpFwd && cmd /= JmpBak
    -- process non-jump commands, and bump the instruction pointer
    then do case cmd of
                DPtrInc -> dataPtrMod (+1)
                DPtrDec -> dataPtrMod (\x -> x - 1)
                ValInc  -> cellMod cellIdx (+1) 
                ValDec  -> cellMod cellIdx (\x -> x - 1) 
                Print   -> do cellVal <- cellGet cellIdx
                              liftIO $ when debug $ do
                                putStrLn $ "\nprint val: " ++ show cellVal
                                putStrLn $ " at idx: " ++ show cellIdx
                              let asChar :: Char
                                  asChar = toEnum $ fromIntegral cellVal
                              liftIO $ putChar asChar
                Get     -> do aByte <- liftIO $ catch 
                                          (getChar)
                                          (\e -> do let e' = e :: IOException
                                                    return '\0')
                              --aByte <- liftIO getChar
                              let asWord8 = fromIntegral $ fromEnum aByte
                              cellPut cellIdx asWord8
                Halt    -> do st <- get
                              throw $ "hit halt instr. state = " ++ show st
            progPtrInc
    -- process jump commands
    else do cellVal <- cellGet cellIdx
            case cmd of
                JmpFwd  -> do when (cellVal == 0) $ do
                                  newProgPtr <- getJmpDest JmpFwd
                                  progPtrMod (const newProgPtr)
                              when (cellVal /= 0)  
                                progPtrInc
                JmpBak  -> do when (cellVal /= 0) $ do
                                  newProgPtr <- getJmpDest JmpBak
                                  progPtrMod (const newProgPtr)
                              when (cellVal == 0)  
                                progPtrInc


-- | print a program (list of commands) with "line numbers"
-- (well, instruction pointer numbers)
printProg prog = do
  let lineNums :: [Int]
      lineNums = [0..]
  forM_ (zip lineNums prog) $ \(lineNum, cmd) -> do
    putStrLn $ printf "%3d: %s" lineNum (show cmd)


    
-- | returns a zipped list of pairs of indices -
--   jump commands jump between fst and snd of a pair
jumpLocations :: [Command] -> Either [Char] [(Int, Int)]
jumpLocations cmds = doList (zip cmds [0..]) [] [] 
  where
    -- doList commands stack result 
    doList :: [(Command,Int)] -> [Int] -> [(Int,Int)] -> Either String [(Int, Int)]
    doList [] [] result = Right result
    doList [] stack result = Left $ "finished w/ stuff still in stack:" ++ show stack
    doList p@((JmpBak, _) : cmds) [] result = Left $ "JmpBak w/ no fwd. cur str: " ++ show p
    -- push
    doList ((JmpFwd, idx) : cmds) stack result = doList cmds (idx:stack) result 
    -- pop
    doList ((JmpBak, idx) : cmds) (i:is) result = doList cmds is ( (i,idx) : result)
    doList ((_,_) : cmds) stack result = doList cmds stack result


-- | parse and run a program, return the final interpreter state.
run :: String -> IO InterpState
run str = do
  let parseRes = parseFsckProg str
      theProg = fst $ unzip parseRes
      positions = snd $ unzip parseRes
  st <- initState theProg
  when debug $
    printProg theProg
  putStrLn ""
  let loop :: (MonadState InterpState m, MonadIO m) => m ()
      loop = do 
                st <- get
                --liftIO $ putStrLn $ "\nstart of loop. state = " ++ show st
                exec 
                st <- get
                --liftIO $ putStrLn $ "\ndone exec. state = " ++ show st
                -- see if instruct ptr has gone past end of prog
                pPtr <- gets progPtr
                if pPtr >= length theProg -- out of bounds
                  then return ()
                  else loop
  execStateT loop st




-- test programs from wikipedia article
-- https://en.wikipedia.org/wiki/Brainfuck 

testProg = unlines [
    "++       Cell c0 = 2"
  , "> +++++  Cell c1 = 5"
  , ""
  , "[        Start your loops with your cell pointer on the loop counter (c1 in our case)"
  , "< +      Add 1 to c0"
  , "> -      Subtract 1 from c1"
  , "]        End your loops with the cell pointer on the loop counter"
  , ""
  , "At this point our program has added 5 to 2 leaving 7 in c0 and 0 in c1"
  , "BUT we cannot output this value to the terminal since it's not ASCII encoded!"
  , ""
  , "To display the ASCII character 7 we must add 48 to the value 7!"
  , "48 = 6 * 8 so let's use another loop to help us!"
  , ""
  , "++++ ++++  c1 = 8 and this will be our loop counter again"
  , "["
  , "< +++ +++  Add 6 to c0"
  , "> -        Subtract 1 from c1"
  , "]"
  , "< .        Print out c0 which has the value 55 which translates to 7!"
  ]

testProg2 = unlines [
    "[ This program prints 'Hello World!' & a newln to the screen, its"
  , "  length is 106 active command characters. [is not the shortest.]"
  , ""
  , "  This loop is an 'initial comment loop', a simple way to add a comment"
  , "  such that you don't have to worry about any command"
  , "  characters. Any '.', ',', '+', '-', '<' and '>' char are simply"
  , "  ignored, the '[' and ']' characters just have to be balanced. This"
  , "  loop and the commands it contains are ignored because the current cell"
  , "  defaults to a value of 0; the 0 value causes this loop to be skipped."
  , "]"
  , "++++++++               Set Cell #0 to 8"
  , "["
  , "    >++++               Add 4 to Cell #1; will always set Cell #1 to 4"
  , "    [                   as the cell will be cleared by the loop"
  , "        >++             Add 2 to Cell #2"
  , "        >+++            Add 3 to Cell #3"
  , "        >+++            Add 3 to Cell #4"
  , "        >+              Add 1 to Cell #5"
  , "        <<<<-           Decrement the loop counter in Cell #1"
  , "    ]                   Loop till Cell #1 is 0; num of iters is 4"
  , "    >+                  Add 1 to Cell #2"
  , "    >+                  Add 1 to Cell #3"
  , "    >-                  Subtract 1 from Cell #4"
  , "    >>+                 Add 1 to Cell #6"
  , "    [<]                 Move back to the first 0 cell you find; this will"
  , "                        be Cell #1 which was cleared by prev loop"
  , "    <-                  Decrement the loop Counter in Cell #0"
  , "]                       Loop till Cell #0 is zero; num of iterns is 8"
  , ""
  , "The result of this is:"
  , "Cell No :   0   1   2   3   4   5   6"
  , "Contents:   0   0  72 104  88  32   8"
  , "Pointer :   ^"
  , ""
  , ">>.                     Cell #2 has value 72 which is 'H'"
  , ">---.                   Subtract 3 from Cell #3 to get 101 which is 'e'"
  , "+++++++..+++.           Likewise for 'llo' from Cell #3"
  , ">>.                     Cell #5 is 32 for the space"
  , "<-.                     Subtract 1 from Cell #4 for 87 to give a 'W'"
  , "<.                      Cell #3 was set to 'o' from the end of 'Hello'"
  , "+++.------.--------.    Cell #3 for 'rl' and 'd'"
  , ">>+.                    Add 1 to Cell #5 gives us an exclamation point"
  , ">++.                    And finally a newline from Cell #6"
  ]


main = do
  args <- getArgs  
  when (length args > 1) $ do
    hPutStrLn stderr "expected max. one arg (filename of program to run"
    exitFailure
  when (length args == 1) $ do
    fileConts <- readFile (args !! 0)
    res <- run fileConts
    when debug $
      print res
  when (length args == 0) $ do
    conts <- getContents
    res <- run conts
    when debug $
      print res

