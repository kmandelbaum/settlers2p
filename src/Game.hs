{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
module Game where

class Eq (PlayerId g) => Game g where
  data GameState g
  data GameSettings g
  data PlayerId g
  data VisibleState g
  data DataToPlayer g
  data DataFromPlayer g
