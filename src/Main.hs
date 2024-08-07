module Main where

-------------------------------------------------
-- APOORVA ANAND - 2037610
-------------------------------------------------
import CSharp.Algebra
import CSharp.Analysis
import CSharp.CodeGen
import CSharp.AbstractSyntax
import CSharp.Parser

import SSM

import ParseLib.Abstract.Derived
import ParseLib.Error (ErrorsPretty)

import System.Environment
import System.FilePath
import Prelude hiding ((*>), (<$), (<*))
import CSharp.Parser (lexicalScanner)
import CSharp.CodeGen (codeAlgebra)
import qualified Data.Map as M

main :: IO ()
main = do
  -- get command line arguments
  args <- getArgs
  files <- case args of
    [] -> do
      putStrLn "no argument given; assuming example.cs"
      return ["example.cs"]
    xs -> return xs
  -- translate each of the files
  mapM_ processFile files

-- processFile compiles one file;
-- it take the name of the input file
processFile :: FilePath -> IO ()
processFile infile = do
  let outfile = addExtension (dropExtension infile) "ssm"
  xs <- readFile infile
  let program = run "parser" (pClass <* eof) . run "lexer" lexicalScanner $ xs 
  case foldCSharp analysisAlgebra program of
    False -> error "analysis failed"
    True -> do
      let folded = fst $ (foldCSharp codeAlgebra program) M.empty
      let ssm = formatCode $ folded
      writeFile outfile ssm
      putStrLn (outfile ++ " written")

run :: (ErrorsPretty s, Ord s, Show a) => String -> Parser s a -> [s] -> a
run s p x = fst . headOrError . parse (p <* eof) $ x
    where
  headOrError (x : xs) = x
  headOrError [] = error $ "The " <> s <> " returned no full parses."
