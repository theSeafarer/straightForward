{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.JSON.TH (
  JSONParse,
  mkJSON,
  parseJSON
) where


import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( void, forM )
import           Data.Functor                   ( ($>) )
import           Data.Semigroup                 ( mconcat, (<>) )
import qualified Text.Parsec                    as P
import qualified Text.Parsec.Char               as P
import qualified Data.Text                      as T
import qualified Language.Haskell.TH            as TH
import qualified Language.Haskell.TH.Quote      as TH
import qualified Language.Haskell.TH.Syntax     as TH

type Parser = P.Parsec T.Text ()
newtype Name = Name T.Text deriving (Eq, Ord, Show)

whitespace :: Parser ()
whitespace = void $ P.many $ P.oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

floatP :: Parser Float
floatP = lexeme $ fmap read $ P.try $ (++) <$> P.try int <*> decimal
  where
    intPart    = P.many1 P.digit
    int        = (P.char '+' >> intPart)
              <|> (:) <$> P.char '-' <*> intPart
              <|> intPart
    decimal    = (:) <$> P.char '.' <*> intPart

intP :: Parser Int
intP = lexeme $ read <$> int
  where
    intPart    = P.many1 P.digit
    int        = (P.char '+' >> intPart)
              <|> (:) <$> P.char '-' <*> intPart
              <|> intPart

strP :: Parser T.Text
strP = lexeme $ T.pack <$>
  do P.char '\"'; P.manyTill P.anyChar (P.try $ P.char '\"')

boolP :: Parser Bool
boolP = let true = P.string "true" $> True
            false = P.string "false" $> False
          in lexeme $ true <|> false

nameP :: Parser Name
nameP = lexeme $ do
  first <- P.oneOf ['a'..'z'] -- ++ ['A'..'Z']
  rest  <- P.many $ P.oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['_', '\'']
  return $ Name $ T.pack $ first : rest

arrP :: JSONParse a => Parser [a]
arrP = lexeme $
  P.between (lexeme $ P.char '[') (P.char ']') (P.sepBy1 jsonParse $ lexeme $ P.char ',')

class JSONParse a where
  jsonParse :: Parser a

instance JSONParse T.Text where
  jsonParse = strP
instance JSONParse Int where
  jsonParse = intP
instance JSONParse Float where
  jsonParse = floatP
instance JSONParse Bool where
  jsonParse = boolP
instance JSONParse a => JSONParse ([] a) where
  jsonParse = arrP
instance JSONParse a => JSONParse (Maybe a) where
  jsonParse = P.try (P.string "null") $> Nothing 
    <|> Just <$> jsonParse

parseJSON :: JSONParse a => T.Text -> Maybe a
parseJSON t = case res of
    (Left _) -> Nothing
    (Right ofA) -> Just ofA
  where res = P.parse jsonParse "JSON Parse" t

mkJSON :: TH.Name -> TH.DecsQ
mkJSON name = do
  TH.TyConI (TH.DataD _ _ _ _ cons' _) <- TH.reify name
  [d|
    instance JSONParse $(TH.conT name) where
      jsonParse = $(parseF cons')
   |]
  where
    parseF cons'' = case cons'' of
      [c] -> [e| $(parseC c) |]
      cs  -> foldl1 (\a b -> [e| $a <|> $b |]) $ (\t -> [e| P.try $(parseC t) |]) <$> cs
    parseC c = [e|
      lexeme $ P.between (lexeme $ P.char '{') (P.char '}') $ $(TH.runQ $ doC c) |]
      where
        fst3 (x, _, _) = x
        mkStmts (TH.NormalC cName ts) = return (fmap (\_ -> do
                                          v <- TH.newName "v"
                                          return ([TH.NoBindS (TH.VarE 'nameP),
                                            TH.NoBindS (TH.InfixE (Just (TH.VarE 'lexeme)) (TH.VarE '($)) (Just (TH.AppE (TH.VarE 'P.char) (TH.LitE (TH.CharL ':'))))),
                                            TH.BindS (TH.VarP v) (TH.VarE 'jsonParse),
                                            TH.NoBindS (TH.InfixE (Just (TH.VarE 'lexeme)) (TH.VarE '($)) (Just (TH.AppE (TH.VarE 'P.char) (TH.LitE (TH.CharL ',')))))], v)
                                                      ) ts, cName)
        mkStmts (TH.RecC cName ts) = return (fmap (\(nm, _, _) -> do
                                          v <- TH.newName "v"
                                          n <- TH.stringE $ TH.nameBase nm
                                          return ([TH.NoBindS (TH.InfixE (Just (TH.VarE 'lexeme)) (TH.VarE '($)) (Just (TH.AppE (TH.VarE 'P.string) n ))),
                                            TH.NoBindS (TH.InfixE (Just (TH.VarE 'lexeme)) (TH.VarE '($)) (Just (TH.AppE (TH.VarE 'P.char) (TH.LitE (TH.CharL ':'))))),
                                            TH.BindS (TH.VarP v) (TH.VarE 'jsonParse),
                                            TH.NoBindS (TH.InfixE (Just (TH.VarE 'lexeme)) (TH.VarE '($)) (Just (TH.AppE (TH.VarE 'P.char) (TH.LitE (TH.CharL ',')))))], v)
                                                      ) ts, cName)
        doC c = do
          (stmts'', cName) <- mkStmts c
          ran <- sequence stmts''
          let stmts' = mconcat $ fst <$> ran
          let stmts = take (length stmts' - 1) stmts'
          let vs = mconcat $ (\(_, v) -> [TH.VarE v]) <$> ran
          thisCons <- TH.runQ $ TH.conE cName
          return $ TH.DoE (stmts <> [TH.NoBindS $ TH.InfixE (Just (TH.VarE 'return)) (TH.VarE '($)) (Just $ foldl TH.AppE thisCons vs )])
