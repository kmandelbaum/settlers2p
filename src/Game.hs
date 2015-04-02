{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
module Game where

class Eq (PlayerId g) => Game g where
  type GameState g
  type PlayerId g
  type VisibleState g
  type DataToPlayer g
  type DataFromPlayer g

class (Game g, Game g', 
       PlayerId g ~ PlayerId g',
       VisibleState g ~ VisibleState g',
       DataToPlayer g ~ DataToPlayer g',
       DataFromPlayer g ~ DataFromPlayer g',
       GameState g ~ GameState g') => EqGames g g'
