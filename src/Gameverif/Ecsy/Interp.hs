{-# LANGUAGE OverloadedStrings #-}

module Gameverif.Ecsy.Interp where

import Control.Exception (Exception)
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Gameverif.Ecsy.Base (ArchDecl (..), ArchName (..), CompDecl (..), CompName (..), FuncDecl (..), FuncName (..),
                            InvDecl (..), InvName (..), MethDecl, ProgDecl (..), QueryDecl (..), QueryName (..),
                            ResDecl (..), ResName (..), SysDecl (..), SysName (..))
import Gameverif.Ecsy.Plain (PlainDecl, PlainProg)
import qualified Gameverif.Viper.Plain as VP

newtype Name = Name { unName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

data NamedVar v =
    NamedVarBound !Name
  | NamedVarFree !v
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

mainName :: Name
mainName = Name "main"

type PlainMethDecl v = MethDecl VP.Exp v

data SomeDecl v =
    SomeDeclTop !(PlainDecl v)
  | SomeDeclMeth !ResName
  | SomeDeclField !CompName
  deriving stock (Eq, Show)

projectDecls :: PlainDecl v -> [(Name, SomeDecl v)]
projectDecls pd =
  let td = SomeDeclTop pd
  in case pd of
    ProgDeclFunc fd -> [(Name (unFuncName (funcDeclName fd)), td)]
    ProgDeclRes rd -> [(Name (unResName (resDeclName rd)), td)]
    -- TODO add method decls
    ProgDeclSys sd -> [(Name (unSysName (sysDeclName sd)), td)]
    ProgDeclQuery qd -> [(Name (unQueryName (queryDeclName qd)), td)]
    ProgDeclComp cd -> [(Name (unCompName (compDeclName cd)), td)]
    -- TODO add field decls
    ProgDeclArch ad -> [(Name (unArchName (archDeclName ad)), td)]
    ProgDeclInv vd -> [(Name (unInvName (invDeclName vd)), td)]
    ProgDeclMain _ -> [(mainName, td)]

type Resolver v = Map Name (SomeDecl v)

data DeclExistsError v = DeclExistsError
  { deeName :: !Name
  , deeTypeExists :: !(SomeDecl v)
  , deeTypeProposed :: !(SomeDecl v)
  } deriving stock (Eq, Show)

instance (Show v, Typeable v) => Exception (DeclExistsError v)

initResolver :: PlainProg v -> Either (DeclExistsError v) (Resolver v)
initResolver = foldM go1 Map.empty where
  go1 res decl =
    let pairs = projectDecls decl
    in foldM go2 res pairs
  go2 res (name, decl) =
    case Map.lookup name res of
      Just exists -> Left (DeclExistsError name exists decl)
      Nothing -> Right (Map.insert name decl res)

rewriteVar :: (v -> Text) -> Resolver v -> v -> NamedVar v
rewriteVar toText res v =
  let name = Name (toText v)
  in if Map.member name res
    then NamedVarBound name
    else NamedVarFree v

rewriteResolver :: (v -> Text) -> Resolver v -> Resolver (NamedVar v)
rewriteResolver txtize res = fmap go res where
  toVar = rewriteVar txtize res
  go = \case
    SomeDeclTop pd -> SomeDeclTop (fmap toVar pd)
    SomeDeclMeth rn -> SomeDeclMeth rn
    SomeDeclField cn -> SomeDeclField cn

newtype ResolveError = ResolveError { unResolveError :: Name }
  deriving stock (Show)
  deriving newtype (Eq)

instance Exception ResolveError

resolve :: Name -> Resolver v -> Either ResolveError (SomeDecl v)
resolve n = maybe (Left (ResolveError n)) Right . Map.lookup n
