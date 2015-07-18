{-# LANGUAGE TupleSections #-}
module Settlers.Settings where

import Game
import Maps
import Settlers.Core
import Data.Map as DM
import qualified Data.Sequence as S
import Data.Monoid

defaultSettings :: GameSettings Settlers
defaultSettings = GameSettings {
  cfgSettleExt = idMapFromList $ take 10 (repeat DummySettleExtension),
  cfgTownExt = idMapFromList $ take 5 (repeat DummyTownExtension),
  cfgAbility = idMapFromList $ take 6 (repeat DummyAbility),
  cfgEvent = idMapFromList $ take 7 (repeat DummyEvent),
  cfgHandCardsNo = 3
}

defaultPlayerState :: PlayerState
defaultPlayerState = PlayerState {
  psHand = S.empty,
  psBuildings = S.empty,
  psResourceLayout = S.empty,
  psLeftRoad = False,
  psRightRoad = False
}

allAbilityCards = fmap AbilityCard abilityIds <> fmap SettleExtensionCard settleIds <> fmap TownExtensionCard townIds
  where
    abilityIds = keys $ cfgAbility defaultSettings
    settleIds = keys $ cfgSettleExt defaultSettings
    townIds = keys $ cfgTownExt defaultSettings

defaultState :: GameState Settlers
defaultState = GameState {
  gsTurnNo = 0,
  gsCurPlayer = Player1,
  gsPlayerStates = DM.fromList $ fmap (,defaultPlayerState) [Player1, Player2],
  gsAbilityDecks = S.fromList $ fmap S.fromList $ splitInto 5 allAbilityCards,
  gsResourceDeck = S.empty,
  gsEventsDeck = S.empty
}

splitInto :: Int -> [a] -> [[a]]
splitInto 1 xs = [xs]
splitInto n xs = now : splitInto (n-1) go
  where (now, go) = splitAt ((length xs + n - 1) `div` n) xs
