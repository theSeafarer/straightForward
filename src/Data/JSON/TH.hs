{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.JSON.TH (
  JSONParse,
  mkJSON,
  parseJSON
) where


import           Control.Applicative                ( (<|>) )
import           Control.Monad                      ( void )
import           Data.Functor                       ( ($>) )
import           Data.Semigroup                     ( mconcat, (<>) )
import           Data.Word8                         ( Word8, isHexDigit, isDigit )
import           Data.ByteString.Internal           ( c2w, w2c )
import           Data.Scientific                    ( Scientific, scientific )
import qualified Data.Attoparsec.ByteString         as P
import qualified Data.Attoparsec.ByteString.Char8   as PC
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Unsafe             as BS
import qualified Data.ByteString.Char8              as BSC
import qualified Language.Haskell.TH                as TH


whitespace :: P.Parser ()
whitespace = void $ P.many' $ oneOf " \n\t"

lexeme :: P.Parser a -> P.Parser a
lexeme p = p <* whitespace

digit :: P.Parser Word8
digit = P.satisfy $ isDigit

char :: Char -> P.Parser Word8
char = P.word8 . c2w
{-# INLINE char #-}

oneOf :: String -> P.Parser Word8
oneOf cs = P.satisfy $ \w -> w `elem` ws
  where ws = c2w <$> cs

byteRead :: Read a => BS.ByteString -> a
byteRead = read . BSC.unpack

wordListRead :: Read a => [Word8] -> a
wordListRead = read . fmap w2c

between :: P.Parser open -> P.Parser close -> P.Parser a -> P.Parser a
between open close p = open *> p <* close

floatP :: P.Parser Float
floatP = lexeme $ fmap wordListRead $ P.try $ (++) <$> P.try int <*> decimal
  where
    intPart    = P.many1 digit
    int        = char '+' *> intPart
              <|> (:) <$> char '-' <*> intPart
              <|> intPart
    decimal    =  (:) <$> char '.' <*> intPart

intP :: P.Parser Int
intP = lexeme $ wordListRead <$> int
  where
    intPart    = P.many1 digit
    int        = char '+' *> intPart
              <|> (:) <$> char '-' <*> intPart
              <|> intPart

strP :: P.Parser BS.ByteString
strP = BS.pack <$> between (lexeme $ char '"') (lexeme $ char '"') (P.many' charP)
    where charP =  (char '\\' *> escP)
               <|> (P.satisfy $ \c -> c /= (c2w '"') && c /= (c2w '\\'))
          escP  =  (c2w '"'  <$ char '"')
               <|> (c2w '\\' <$ char '\\')
               <|> (c2w '/'  <$ char '/' )
               <|> (c2w '\b' <$ char 'b' )
               <|> (c2w '\f' <$ char 'f' )
               <|> (c2w '\n' <$ char 'n' )
               <|> (c2w '\r' <$ char 'r' )
               <|> (c2w '\t' <$ char 't' )
             P.<?> "escape character"

betQuote :: BS.ByteString -> P.Parser ()
betQuote s = void $ between (char '\"') (char '\"') $ P.string s

anyBetQuote :: P.Parser ()
-- anyBetQuote = void $ between (P.word8 (c2w '\"')) (P.word8 (c2w '\"')) $ P.many1' P.anyWord8
anyBetQuote = void $ char '"' *> P.anyWord8 *> P.manyTill' P.anyWord8 (char '\"')

boolP :: P.Parser Bool
boolP = let true = P.string "true" $> True
            false = P.string "false" $> False
          in true <|> false

nameP :: P.Parser ()
nameP = lexeme $ do
  first <- oneOf ['a'..'z'] -- ++ ['A'..'Z']
  rest  <- P.many' $ oneOf $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['_', '\'']
  -- return $ Name $ T.pack $ first : rest
  return ()

arrP :: JSONParse a => P.Parser [a]
arrP = between
  (lexeme $ char '[') 
  (char ']') 
  (P.sepBy1 jsonParse $ lexeme $ char ',')

maybeP :: JSONParse a => P.Parser (Maybe a)
maybeP = nullP <|> valP
  where nullP = P.string "null" $> Nothing
        valP  = Just <$> jsonParse

class JSONParse a where
  jsonParse :: P.Parser a

instance JSONParse BS.ByteString where
  jsonParse = lexeme strP P.<?> "ByteString"

instance JSONParse Int where
  jsonParse = intP P.<?> "Integer"

instance JSONParse Float where
  jsonParse = floatP P.<?> "Float"

instance JSONParse Bool where
  jsonParse = lexeme boolP P.<?> "Bool"

instance JSONParse a => JSONParse ([] a) where
  jsonParse = lexeme arrP P.<?> "Array"

instance JSONParse a => JSONParse (Maybe a) where
  jsonParse = lexeme maybeP P.<?> "Maybe"



-- I have to use parseOnly otherwise I get a Partial, have to find out why
parseJSON :: JSONParse a => BSC.ByteString -> Either String a
parseJSON t = P.parseOnly jsonParse t

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
      cs  -> foldl1 (\a b -> [e| $a <|> $b |]) $ (\t -> [e| $(parseC t) |]) <$> cs
    parseC c = [e|
      lexeme $ between (lexeme (P.word8 (c2w '{'))) (P.word8 (c2w '}')) $ $(TH.runQ $ doC c) |]
      where
        fst3 (x, _, _) = x
        mkStmts (TH.NormalC cName ts) =
          return (fmap (\_ -> do
            v <- TH.newName "v"
            return ([TH.NoBindS
                (TH.AppE (TH.VarE 'lexeme) (TH.VarE 'anyBetQuote)),
              TH.NoBindS
                (TH.InfixE (Just (TH.VarE 'lexeme))
                  (TH.VarE '($))
                  (Just (TH.AppE (TH.VarE 'P.word8) (TH.AppE (TH.VarE 'c2w) (TH.LitE (TH.CharL ':')))))),
              TH.BindS
                (TH.VarP v)
                (TH.VarE 'jsonParse),
              TH.NoBindS
                (TH.InfixE (Just (TH.VarE 'lexeme))
                  (TH.VarE '($))
                  (Just (TH.AppE (TH.VarE 'P.word8) (TH.AppE (TH.VarE 'c2w) (TH.LitE (TH.CharL ','))))))], v)
                        ) ts, cName)
        mkStmts (TH.RecC cName ts) =
          return (fmap (\(nm, _, _) -> do
            v <- TH.newName "v"
            n <- TH.stringE $ TH.nameBase nm
            return ([TH.NoBindS
                (TH.InfixE (Just (TH.VarE 'lexeme))
                  (TH.VarE '($)) (Just (TH.AppE (TH.VarE 'betQuote) n ))),
              TH.NoBindS
                (TH.InfixE (Just (TH.VarE 'lexeme))
                  (TH.VarE '($))
                  (Just (TH.AppE (TH.VarE 'P.word8) (TH.AppE (TH.VarE 'c2w) (TH.LitE (TH.CharL ':')))))),
              TH.BindS (TH.VarP v) (TH.VarE 'jsonParse),
              TH.NoBindS
                (TH.InfixE (Just (TH.VarE 'lexeme))
                  (TH.VarE '($))
                  (Just (TH.AppE (TH.VarE 'P.word8) (TH.AppE (TH.VarE 'c2w) (TH.LitE (TH.CharL ','))))))], v)
                        ) ts, cName)
        doC c = do
          (stmts'', cName) <- mkStmts c
          ran <- sequence stmts''
          let stmts' = mconcat $ fst <$> ran
          let stmts = take (length stmts' - 1) stmts'
          let vs = mconcat $ (\(_, v) -> [TH.VarE v]) <$> ran
          thisCons <- TH.runQ $ TH.conE cName
          return $ TH.DoE
            (stmts <>
              [TH.NoBindS $
                TH.InfixE (Just (TH.VarE 'return)) (TH.VarE '($)) (Just $ foldl TH.AppE thisCons vs )])
