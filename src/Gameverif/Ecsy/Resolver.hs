{-# LANGUAGE OverloadedStrings #-}

module Gameverif.Ecsy.Resolver where

import Control.Exception (Exception)
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Gameverif.Ecsy.Base (ArchDecl (..), ArchName (..), CompDecl (..), CompName (..), FuncDecl (..), FuncName (..),
                            InvDecl (..), InvName (..), MethDecl, ProgDecl (..), QueryDecl (..), QueryName (..),
                            ResDecl (..), ResName (..), SysDecl (..), SysName (..), VarName (..))
import Gameverif.Ecsy.Plain (PlainDecl, PlainProg)
import qualified Gameverif.Viper.Plain as VP

newtype ScopedName = ScopedName { unScopedName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

data ResolvedName =
    ResolvedNameBound !ScopedName
  | ResolvedNameFree !VarName
  deriving stock (Eq, Ord, Show)

mainName :: ScopedName
mainName = ScopedName "main"

type PlainMethDecl v = MethDecl VP.Exp v

data SomeDecl v =
    SomeDeclTop !(PlainDecl v)
  | SomeDeclMeth !ResName
  | SomeDeclField !CompName
  deriving stock (Eq, Show)

projectDecls :: PlainDecl v -> [(ScopedName, SomeDecl v)]
projectDecls pd =
  let td = SomeDeclTop pd
  in case pd of
    ProgDeclFunc fd -> [(ScopedName (unFuncName (funcDeclName fd)), td)]
    ProgDeclRes rd -> [(ScopedName (unResName (resDeclName rd)), td)]
    -- TODO add method decls
    ProgDeclSys sd -> [(ScopedName (unSysName (sysDeclName sd)), td)]
    ProgDeclQuery qd -> [(ScopedName (unQueryName (queryDeclName qd)), td)]
    ProgDeclComp cd -> [(ScopedName (unCompName (compDeclName cd)), td)]
    -- TODO add field decls
    ProgDeclArch ad -> [(ScopedName (unArchName (archDeclName ad)), td)]
    ProgDeclInv vd -> [(ScopedName (unInvName (invDeclName vd)), td)]
    ProgDeclMain _ -> [(mainName, td)]

type Resolver v = Map ScopedName (SomeDecl v)

data DeclExistsError v = DeclExistsError
  { deeName :: !ScopedName
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

rewriteVar :: Resolver Text -> Text -> ResolvedName
rewriteVar res v =
  let name = ScopedName v
  in if Map.member name res
    then ResolvedNameBound name
    else ResolvedNameFree (VarName v)

rewriteResolver :: Resolver Text -> Resolver ResolvedName
rewriteResolver res = fmap go res where
  toVar = rewriteVar res
  go = \case
    SomeDeclTop pd -> SomeDeclTop (fmap toVar pd)
    SomeDeclMeth rn -> SomeDeclMeth rn
    SomeDeclField cn -> SomeDeclField cn

newtype ResolveError = ResolveError { unResolveError :: ScopedName }
  deriving stock (Show)
  deriving newtype (Eq)

instance Exception ResolveError

resolve :: ScopedName -> Resolver ResolvedName -> Either ResolveError (SomeDecl ResolvedName)
resolve n = maybe (Left (ResolveError n)) Right . Map.lookup n
