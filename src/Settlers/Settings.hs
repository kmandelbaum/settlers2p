{-# LANGUAGE TupleSections #-}
module Settlers.Settings where

import Maps
import Settlers.Core
import Data.Map as DM
import qualified Data.Sequence as S
import Data.Monoid

defaultSettings :: GameSettings
defaultSettings = GameSettings {
  cfgSettleExt = idMapFromList $ take 10 (repeat DummySettleExtension),
  cfgTownExt = idMapFromList $ take 5 (repeat DummyTownExtension),
  cfgAbility = idMapFromList $ take 6 (repeat DummyAbility),
  cfgEvent = idMapFromList $ take 7 (repeat DummyEvent),
  cfgHandCardsNo = 3
}

defaultPlayerState :: PlayerState
defaultPlayerState = PlayerState {
  _psHand = S.empty,
  _psBuildings = S.empty,
  _psResourceLayout = S.empty,
  _psLeftRoad = False,
  _psRightRoad = False
}

allAbilityCards = fmap AbilityCard abilityIds <> fmap SettleExtensionCard settleIds <> fmap TownExtensionCard townIds
  where
    abilityIds = keys $ cfgAbility defaultSettings
    settleIds = keys $ cfgSettleExt defaultSettings
    townIds = keys $ cfgTownExt defaultSettings

defaultState :: GameState
defaultState = GameState {
  _gsTurnNo = 0,
  _gsCurPlayer = Player1,
  _gsPlayerStates = DM.fromList $ fmap (,defaultPlayerState) [Player1, Player2],
  _gsAbilityDecks = S.fromList $ fmap S.fromList $ splitInto 5 allAbilityCards,
  _gsResourceDeck = S.empty,
  _gsEventsDeck = S.empty
}

splitInto :: Int -> [a] -> [[a]]
splitInto 1 xs = [xs]
splitInto n xs = now : splitInto (n-1) go
  where (now, go) = splitAt ((length xs + n - 1) `div` n) xs
