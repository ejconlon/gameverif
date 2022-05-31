module Gameverif.Ecsy.Base where

data Lit =
    LitInt !Integer
  | LitBool !Bool
  deriving stock (Eq, Show)

data LitTy =
    LitTyInt
  | LitTyBool
  deriving stock (Eq, Show)

inferLitTy :: Lit -> LitTy
inferLitTy = \case
  LitInt _ -> LitTyInt
  LitBool _ -> LitTyBool
