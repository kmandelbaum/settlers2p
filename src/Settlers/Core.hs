{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, GADTs, DataKinds #-}
module Settlers.Core where

import Data.Functor
import Data.Sequence

--import qualified Game as G
import Game as G
import qualified Engine as E
import Maps

data Settlers

data Dice = D1 | D2 | D3 | D4 | D5 | D6 deriving (Show, Eq, Ord, Enum)
data EventDice
type DiceRoll = (Dice, EventDice)

data ResourceType = RLumber | RWheat | RWool | ROre | RClay | RGold deriving (Show, Eq, Enum)

newtype ResourceAmount = ResourceAmount Int deriving (Eq, Show, Ord)
data ResourceAmountArea = RZero | ROne | RTwo | RThree

newtype UpDown a = UpDown (a,a)
data ZeroOneTwo a = Zero | One a | Two a a deriving (Show, Eq)

data EventCard = EventCard (Id Event)
data HandCard = AbilityCard (Id Ability) | 
                SettleExtensionCard (Id SettleExtension) |
                TownExtensionCard (Id TownExtension)

-- TODO
data SettleExtension
data TownExtension
data Ability
data Event

data ExtensionType = SettleExtType | TownExtType
data BuiltExtension :: ExtensionType -> * where
  BuiltSettleExt :: Id SettleExtension -> BuiltExtension a
  BuiltTownExt :: Id TownExtension -> BuiltExtension TownExtType

data Building = 
  Settlement (UpDown (Maybe (BuiltExtension SettleExtType))) |
  Town (UpDown (ZeroOneTwo (BuiltExtension TownExtType)))

data PlayerState = 
  PlayerState {
    psHand :: Seq HandCard,
    psBuildings :: Seq Building,
    psResourceLayout :: Seq (UpDown ResourceArea),
    psLeftRoad :: Bool,
    psRightRoad :: Bool
  }

data OponentState = 
  OponentState {
    osHandSize :: Int,
    osBuildings :: Seq Building,
    osResourceLayout :: Seq (UpDown ResourceArea),
    osLeftRoad :: Bool,
    osRightRoad :: Bool
  }

instance Game Settlers where

  data PlayerId Settlers = Player1 | Player2 deriving (Eq, Show, Enum)

  data GameState Settlers = 
    GameState { 
      gsTurnNo :: Int,
      gsCurPlayer :: PlayerId Settlers,
      gsPlayerStates :: ( PlayerState, PlayerState ),
      gsAbilityDecks :: Seq (Seq HandCard),
      gsResourceDeck :: Seq ResourceCard,
      gsEventsDeck :: Seq EventCard
    }

  data VisibleState Settlers = 
    VisibleState {
      vsTurnNo :: Int,
      vsCurPlayer :: PlayerId Settlers,
      vsMyState :: PlayerState,
      vsOponentState :: OponentState,
      vsAbilityDeckSizes :: Seq Int,
      vsResourceDecSize :: Int
    }

  data DataToPlayer Settlers = 
    ShowDeck (Seq DeckCard) ForChoice |
    UpdateState (VisibleState Settlers) |
    Message String

  data GameSettings Settlers = 
    GameSettings {
      cfgSettleExt :: IdMap SettleExtension,
      cfgTownExt :: IdMap TownExtension,
      cfgAbility :: IdMap Ability,
      cfgEvent :: IdMap Event,
      cfgHandCardsNo :: Int
    }

  data DataFromPlayer Settlers = PlayerChoice Int

data ResourceArea = ResourceArea ResourceType ResourceAmountArea
data ResourceCard = ResourceCard ResourceType Dice

data ForChoice = ForChoice Int

data DeckCard =
  DResourceCard ResourceCard | 
  DHandCard HandCard |
  DEventCard EventCard
