{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, GADTs, DataKinds, StandaloneDeriving , Rank2Types, TemplateHaskell #-}
module Settlers.Core where

import Data.Functor
import qualified Data.Sequence as S
import Data.Sequence (Seq)

import Maps
import qualified Data.Map as DM

import Control.Lens
import Control.Lens.TH
import Language.Haskell.TH

import qualified Engine as E

data Dice = D1 | D2 | D3 | D4 | D5 | D6 deriving (Show, Eq, Ord, Enum)
data EventDice
type DiceRoll = (Dice, EventDice)

data ResourceType = RLumber | RWheat | RWool | ROre | RClay | RGold deriving (Show, Eq, Enum)

newtype ResourceAmount = ResourceAmount Int deriving (Eq, Show, Ord)
data ResourceAmountArea = RZero | ROne | RTwo | RThree deriving Show

newtype UpDown a = UpDown (a,a) deriving Show
data ZeroOneTwo a = Zero | One a | Two a a deriving (Show, Eq)

data EventCard = EventCard (Id Event) deriving Show
data HandCard = AbilityCard (Id Ability) | 
                SettleExtensionCard (Id SettleExtension) |
                TownExtensionCard (Id TownExtension) deriving Show

-- TODO
data SettleExtension = DummySettleExtension
data TownExtension = DummyTownExtension
data Ability = DummyAbility
data Event = DummyEvent

data ExtensionType = SettleExtType | TownExtType deriving Show
data BuiltExtension :: ExtensionType -> * where
  BuiltSettleExt :: Id SettleExtension -> BuiltExtension a
  BuiltTownExt :: Id TownExtension -> BuiltExtension TownExtType

deriving instance Show (BuiltExtension a)

data Building = 
  Settlement (UpDown (Maybe (BuiltExtension SettleExtType))) |
  Town (UpDown (ZeroOneTwo (BuiltExtension TownExtType))) deriving Show

data PlayerState = 
  PlayerState {
    _psHand :: Seq HandCard,
    _psBuildings :: Seq Building,
    _psResourceLayout :: Seq (UpDown ResourceArea),
    _psLeftRoad :: Bool,
    _psRightRoad :: Bool
  } deriving Show

data OpponentState = 
  OpponentState {
    osHandSize :: Int,
    osBuildings :: Seq Building,
    osResourceLayout :: Seq (UpDown ResourceArea),
    osLeftRoad :: Bool,
    osRightRoad :: Bool
  } deriving Show


data VisibleState = 
  VisibleState {
    vsTurnNo :: Int,
    vsCurPlayer :: PlayerId,
    vsMyState :: PlayerState,
    vsOpponentState :: OpponentState,
    vsAbilityDeckSizes :: Seq Int,
    vsResourceDeckSize :: Int
  } deriving Show


data PlayerId = Player1 | Player2 deriving (Eq, Show, Enum, Ord)

data GameState = 
  GameState { 
    _gsTurnNo :: Int,
    _gsCurPlayer :: PlayerId,
    _gsPlayerStates :: DM.Map PlayerId PlayerState,
    _gsAbilityDecks :: Seq (Seq HandCard),
    _gsResourceDeck :: Seq ResourceCard,
    _gsEventsDeck :: Seq EventCard
  } deriving Show

data DataToPlayer = 
  ShowDeck (Seq DeckCard) ForChoice |
  UpdateState VisibleState |
  Message String deriving Show

data GameSettings = 
  GameSettings {
    cfgSettleExt :: IdMap SettleExtension,
    cfgTownExt :: IdMap TownExtension,
    cfgAbility :: IdMap Ability,
    cfgEvent :: IdMap Event,
    cfgHandCardsNo :: Int
  }

data DataFromPlayer = PlayerChoice Int

data ResourceArea = ResourceArea ResourceType ResourceAmountArea deriving Show
data ResourceCard = ResourceCard ResourceType Dice deriving Show

data ForChoice = ForChoice Int deriving Show

data DeckCard =
  DResourceCard ResourceCard | 
  DHandCard HandCard |
  DEventCard EventCard
  deriving Show

type PId = PlayerId
type GSt = GameState
type GCfg = GameSettings

--makeLensesFor (map (\x -> (x,'l':x)) ["gsAbilityDecks", "gsPlayerStates"]) ''GameState
--makeLensesFor (map (\x -> (x,'l':x)) ["psHand"]) ''PlayerState
makeLenses ''GameState
makeLenses ''PlayerState

lgsPlayerHand :: PId -> Traversal' GSt (Seq HandCard)
lgsPlayerHand p = gsPlayerStates . ix p . psHand
