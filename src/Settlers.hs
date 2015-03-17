{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, GADTs, DataKinds #-}
module Settlers where

import Data.Functor
import Data.Sequence


data Dice = D1 | D2 | D3 | D4 | D5 | D6 deriving (Show, Eq, Ord, Enum)
data EventDice
type DiceRoll = (Dice, EventDice)

data ResourceType = RLumber | RWheat | RWool | ROre | RClay | RGold deriving (Show, Eq, Enum)

data PlayerId = Player1 | Player2

newtype ResourceAmount = ResourceAmount Int deriving (Eq, Show, Ord)
data ResourceAmountArea = RZero | ROne | RTwo | RThree

newtype UpDown a = UpDown (a,a)
data ZeroOneTwo a = Zero | One a | Two a a deriving (Show, Eq)

-- TODO
data EventCard
data AbilityCard
data SettlementExtension
data TownExtension
data ExtensionData

data ExtensionType = SettleExt | TownExt

data Extension :: ExtensionType -> * where
  SettlementExtension :: ExtensionData -> Extension a
  TownExtension :: ExtensionData -> Extension TownExt

data Building = Settlement (UpDown (Maybe (Extension SettleExt))) | Town (UpDown (ZeroOneTwo (Extension TownExt)))

data PlayerState = 
  PlayerState {
    psHand :: Seq AbilityCard,
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

data GameState = 
  GameState { 
    gsTurnNo :: Int,
    gsCurPlayer :: PlayerId,
    gsPlayerStates :: ( PlayerState, PlayerState ),
    gsAbilityDecks :: Seq (Seq AbilityCard),
    gsResourceDeck :: Seq ResourceCard,
    gsEventsDeck :: Seq EventCard
  }

data VisibleState = 
  VisibleState {
    vsTurnNo :: Int,
    vsCurPlayer :: PlayerId,
    vsMyState :: PlayerState,
    vsOponentState :: OponentState,
    vsAbilityDeckSizes :: Seq Int,
    vsResourceDecSize :: Int
  }

data ResourceArea = ResourceArea ResourceType ResourceAmountArea
data ResourceCard = ResourceCard ResourceType Dice

data DeckCard =
  DResourceCard ResourceCard | 
  DAbilityCard AbilityCard |
  DEventCard EventCard
