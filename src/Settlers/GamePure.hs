module Settlers.GamePure where

import qualified Data.Map as DM
import Data.List (sort)
import qualified Data.Sequence as S


import Control.Lens
import Control.Monad

import Maps
import Game

import Settlers.Core

getVisibleState :: GSt -> PId -> VisibleState
getVisibleState g p = VisibleState {
  vsTurnNo = gsTurnNo g,
  vsCurPlayer = gsCurPlayer g,
  vsMyState = gsPlayerStates g DM.! p,
  vsOpponentState = toOpponentState (gsPlayerStates g DM.! opponent p),
  vsAbilityDeckSizes = fmap length $ gsAbilityDecks g,
  vsResourceDeckSize = length $ gsResourceDeck g
}
  where
    toOpponentState ps = OpponentState {
      osHandSize = length $ psHand ps,
      osBuildings = psBuildings ps,
      osResourceLayout = psResourceLayout ps,
      osLeftRoad = psLeftRoad ps,
      osRightRoad = psRightRoad ps
    }

opponent :: PId -> PId
opponent Player1 = Player2
opponent Player2 = Player1

drawAbilityCards :: PId -> Int -> [Int] -> GSt -> Maybe GSt
drawAbilityCards p deckNo cardsNo gs = do
  when (not $ checkUniq $ sCardsNo) Nothing
  cards <- S.fromList <$> mapM (\i -> gs ^? lDeck >>= (^? ix i)) sCardsNo
  return (gs & lDeck %~ (remove sCardsNo) & lgsPlayerHand p %~ (cards S.><))
  where 
    lDeck :: Traversal' GSt (S.Seq HandCard)
    lDeck = lgsAbilityDecks . ix deckNo
    sCardsNo = sort cardsNo

-- idxs should be sorted
remove :: [Int] -> S.Seq a -> S.Seq a
remove idxs sq = foldr remove1 sq idxs
 where 
   remove1 i s =
     let (l,r) = S.splitAt i s in (l S.>< S.drop 1 r) 

-- checks that there are no identical neighbours
checkUniq :: Eq a => [a] -> Bool
checkUniq [] = True
checkUniq [x] = True
checkUniq (x:y:xs) = x /= y && checkUniq (y:xs)
