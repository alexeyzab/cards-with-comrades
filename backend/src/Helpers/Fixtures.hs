module Helpers.Fixtures where

import Import
import Helpers.Fixtures.Deck (decodeAnswers, decodeQuestions, Question(..), Answer(..))

newtype UserFixtures =
  UserFixtures { allUsersF :: [Entity User] }
  deriving (Eq, Show)

newtype AdminFixtures =
  AdminFixtures { allAdminsF :: [Entity Admin] }
  deriving (Eq, Show)

newtype CardFixtures =
  CardFixtures { allCardsF :: [Entity Card] }
  deriving (Eq, Show)

newtype DeckFixtures =
  DeckFixtures { allDecksF :: [Entity Deck] }
  deriving (Eq, Show)

data Fixtures =
  Fixtures { userF     :: UserFixtures
           , adminF    :: AdminFixtures
           , cardF     :: CardFixtures
           , deckF     :: DeckFixtures
           }
  deriving (Eq, Show)

chrisEmail, chrisPassword :: Text
chrisEmail = "chris@lol.com"
chrisPassword = "chrisPass"

alexeyEmail, alexeyPassword :: Text
alexeyEmail = "alexey@lol.com"
alexeyPassword = "alexeyPass"

makeAccount :: Text -> Text -> DB (Entity User)
makeAccount email pass = do
  userEnt <- createUser email pass
  return userEnt

makeAccounts :: DB [Entity User]
makeAccounts = sequenceA [ makeAccount chrisEmail chrisPassword
                         , makeAccount alexeyEmail alexeyPassword ]

makeAdmin :: Key User -> DB (Entity Admin)
makeAdmin = createAdmin

makeAdmins :: [Key User] -> DB [Entity Admin]
makeAdmins = traverse makeAdmin

makeCard :: Card -> DB (Entity Card)
makeCard = createCard

makeCards :: [Card] -> DB [Entity Card]
makeCards = traverse makeCard

deck1, deck2 :: Text
deck1 = "Deck 1"
deck2 = "Deck 2"

makeDeck :: Text -> DB (Entity Deck)
makeDeck = createDeck

makeDecks :: [Text] -> DB [Entity Deck]
makeDecks = traverse makeDeck

makeDeckCard :: Key Deck -> Key Card -> DB (Entity DeckCard)
makeDeckCard = createDeckCard

makeDeckCards :: [Key Card] -> Key Deck -> DB [Entity DeckCard]
makeDeckCards xs d = traverse (makeDeckCard d) xs

{-# INLINABLE unsafeIdx #-}
unsafeIdx :: (MonoFoldable c) => c -> Integer -> Element c
unsafeIdx xs n
  | n < 0     = error "negative index"
  | otherwise = foldr (\x r k -> case k of
                                   0 -> x
                                   _ -> r (k-1)) (error ("index too large: " ++ show n))  xs n

insertFixtures :: DB Fixtures
insertFixtures = do
  questionsFile <- readFile "decks/cah-black.csv"
  answersFile <- readFile "decks/cah-white.csv"
  let (Right questionCards) = decodeQuestions questionsFile
      (Right answerCards) = decodeAnswers answersFile
  allCardsF <- makeCards $ toList . asVector $ map questionCard questionCards ++ map answerCard answerCards
  let listOfCardIds = map entityKey allCardsF
  deck <- makeDeck deck1
  allDecksF <- makeDecks [deck2]
  allDeckCardsF <- makeDeckCards listOfCardIds (entityKey deck)
  allUsersF <- makeAccounts
  let chris = unsafeIdx allUsersF 0
      alexey = unsafeIdx allUsersF 1
  allAdminsF <- makeAdmins [entityKey chris, entityKey alexey]
  let userF = UserFixtures {..}
      adminF = AdminFixtures {..}
      cardF = CardFixtures {..}
      deckF = DeckFixtures {..}
  return Fixtures {..}
