module Helpers.Fixtures.Deck where

import Import

import qualified Data.Text as T

newtype Question =
  Question { questionCard :: Card }
  deriving (Eq, Show)

newtype Answer =
  Answer { answerCard :: Card }
  deriving (Eq, Show)

dropConsecutive :: Text -> [Text] -> [Text]
dropConsecutive droppee xs =
  snd $ foldr dropper (Nothing, []) xs
  where dropper t (Nothing, newXs) =
          (Just t, t : newXs)
        dropper t (Just prevVal, newXs) =
          if prevVal == droppee && t == droppee
          then (Just t, newXs)
          else (Just t, t : newXs)

parseQuestion' :: Text -> Either String Card
parseQuestion' t = do
  case dropConsecutive "" $ T.split (=='_') t of
    [] -> fail "Empty list result for row parse, could not make card"
    [a] ->
      return (Card False a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
    [a, b] ->
      return (Card False a (Just b) Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
    [a, b, c] ->
      return (Card False a (Just b) (Just c) Nothing Nothing Nothing Nothing Nothing Nothing)
    [a, b, c, d] ->
      return (Card False a (Just b) (Just c) (Just d) Nothing Nothing Nothing Nothing Nothing)
    [a, b, c, d, e] ->
      return (Card False a (Just b) (Just c) (Just d) (Just e) Nothing Nothing Nothing Nothing)
    [a, b, c, d, e, f] ->
      return (Card False a (Just b) (Just c) (Just d) (Just e) (Just f) Nothing Nothing Nothing)
    [a, b, c, d, e, f, g] ->
      return (Card False a (Just b) (Just c) (Just d) (Just e) (Just f) (Just g) Nothing Nothing)
    [a, b, c, d, e, f, g, h] ->
      return (Card False a (Just b) (Just c) (Just d) (Just e) (Just f) (Just g) (Just h) Nothing)
    [a, b, c, d, e, f, g, h, i] ->
      return (Card False a (Just b) (Just c) (Just d) (Just e) (Just f) (Just g) (Just h) (Just i))
    xs -> fail $ "List exceeded question card maximum of 4 splits: " <> show xs

parseQuestion :: Text -> Either String Question
parseQuestion t = Question <$> parseQuestion' t

parseAnswer :: Text -> Either String Answer
parseAnswer t = return $ Answer $ Card True t Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

decodeQuestions :: ByteString -> Either String (Vector Question)
decodeQuestions bs = do
  let t = decodeUtf8 bs
      rows = T.lines t
  fromList <$> traverse parseQuestion rows

decodeAnswers :: ByteString -> Either String (Vector Answer)
decodeAnswers bs = do
  let t = decodeUtf8 bs
      rows = T.lines t
  fromList <$> traverse parseAnswer rows
