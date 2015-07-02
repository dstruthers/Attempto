{-# LANGUAGE OverloadedStrings #-}

module Attempto.Lexicon where
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator (sepBy)
import Data.ByteString.Internal (ByteString)

-- See: http://attempto.ifi.uzh.ch/site/resources/

data WordClass = -- <adv|adj> PositiveForm LogicalSymbol
                 Adverb                    String String
               | CompAdverb                String String
               | SupAdverb                 String String
               | IntransAdjective          String String
               | CompIntransAdjective      String String
               | SupIntransAdjective       String String
                 -- <adj> PositiveForm LogicalSymbol Preposition
               | TransAdjective            String String String
               | CompTransAdjective        String String String
               | SupTransAdjective         String String String
                 -- <noun> Form LogicalSymbol Gender
               | Noun                      String String Gender
               | PluralNoun                String String Gender
               | MassNoun                  String String Gender
                 -- <unit> Form LogicalSymbol
               | MeasurementNoun           String String
               | PluralMeasurementNoun     String String
                 -- <proper noun> Form LogicalSymbol Gender
               | ProperNoun                String String Gender
               | PluralProperNoun          String String Gender
               | ProperNounDef             String String Gender
               | PluralProperNounDef       String String Gender
                 -- <verb> Form LogicalSymbol
               | IntransVerb               String String
               | InfIntransVerb            String String
               | TransVerb                 String String
               | InfTransVerb              String String
               | PastParticipleTransVerb   String String
                 -- <verb> Form LogicalSymbol Preposition
               | DitransVerb               String String String
               | InfDitransVerb            String String String
               | PastParticipleDitransVerb String String String
                 -- <prep> Form LogicalSymbol
               | Preposition               String String
               deriving (Eq, Show)

data Gender = Undefined
            | Neuter
            | Human
            | Masculine
            | Feminine
            deriving (Eq, Show)

type Lexicon = [WordClass]

whitespace :: Parser String
whitespace = many' space

separator :: Parser String
separator = whitespace >> char ',' >> whitespace

parseACEForm :: Parser String
parseACEForm = do
  quote <- option "" $ string "'"
  form <- many' $ satisfy $ inClass "a-zA-Z0-9-_$°"
  string quote
  return form

parseLogicalForm :: Parser String
parseLogicalForm = parseACEForm

parse2PartEntry :: ByteString -> (String -> String -> WordClass) -> Parser WordClass
parse2PartEntry fn constructor = do
  string fn
  char '('
  whitespace
  form <- parseACEForm
  separator
  logical <- parseLogicalForm
  whitespace
  string ")."
  return $ constructor form logical

parse3PartEntry :: ByteString -> (String -> String -> String -> WordClass) -> Parser WordClass
parse3PartEntry fn constructor = do
  string fn
  char '('
  whitespace
  form <- parseACEForm
  separator
  logical <- parseLogicalForm
  separator
  extra <- parseACEForm
  whitespace
  string ")."
  return $ constructor form logical extra

parseGender :: Parser Gender
parseGender =
      (string "undef" >> return Undefined)
  <|> (string "neutr" >> return Neuter)
  <|> (string "human" >> return Human)
  <|> (string "masc"  >> return Masculine)
  <|> (string "fem"   >> return Feminine)

parseNounWithGender :: ByteString -> (String -> String -> Gender -> WordClass) -> Parser WordClass
parseNounWithGender fn constructor = do
  string fn
  char '('
  whitespace
  form <- parseACEForm
  separator
  logical <- parseLogicalForm
  separator
  gender <- parseGender
  whitespace
  string ")."
  return $ constructor form logical gender

parseLexEntry :: Parser WordClass
parseLexEntry =
      parse2PartEntry     "adv"          Adverb
  <|> parse2PartEntry     "adv_comp"     CompAdverb
  <|> parse2PartEntry     "adv_sup"      SupAdverb
  <|> parse2PartEntry     "adj_itr"      IntransAdjective
  <|> parse2PartEntry     "adj_itr_comp" CompIntransAdjective
  <|> parse2PartEntry     "adj_itr_sup"  SupIntransAdjective
  <|> parse3PartEntry     "adj_tr"       TransAdjective
  <|> parse3PartEntry     "adj_tr_comp"  CompTransAdjective
  <|> parse3PartEntry     "adj_tr_sup"   SupTransAdjective
  <|> parseNounWithGender "noun_sg"      Noun
  <|> parseNounWithGender "noun_pl"      PluralNoun
  <|> parseNounWithGender "noun_mass"    MassNoun
  <|> parse2PartEntry     "mn_sg"        MeasurementNoun
  <|> parse2PartEntry     "mn_pl"        PluralMeasurementNoun
  <|> parseNounWithGender "pn_sg"        ProperNoun
  <|> parseNounWithGender "pn_pl"        PluralProperNoun
  <|> parseNounWithGender "pndef_sg"     ProperNounDef
  <|> parseNounWithGender "pndef_pl"     PluralProperNounDef
  <|> parse2PartEntry     "iv_finsg"     IntransVerb
  <|> parse2PartEntry     "iv_infpl"     InfTransVerb
  <|> parse2PartEntry     "tv_finsg"     TransVerb
  <|> parse2PartEntry     "tv_infpl"     InfTransVerb
  <|> parse2PartEntry     "tv_pp"        PastParticipleTransVerb
  <|> parse3PartEntry     "dv_finsg"     DitransVerb
  <|> parse3PartEntry     "dv_infpl"     InfDitransVerb
  <|> parse3PartEntry     "dv_pp"        PastParticipleDitransVerb
  <|> parse2PartEntry     "prep"         Preposition

parseLexicon :: Parser Lexicon
parseLexicon = do
  entries <- parseLexEntry `sepBy` (char '\n')
  return entries

loadLexicon :: ByteString -> Either String Lexicon
loadLexicon = parseOnly parseLexicon
