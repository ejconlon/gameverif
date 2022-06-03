{-# LANGUAGE OverloadedStrings #-}

module Gameverif.Ecsy.Resolver where

import Control.Exception (Exception)
import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Gameverif.Ecsy.Base (ArchDecl (..), ArchName (..), CompDecl (..), CompField (..), CompName (..), FieldName (..),
                            FuncDecl (..), FuncName (..), InvDecl (..), InvName (..), MethDecl (..), MethName (..),
                            ProgDecl (..), QueryDecl (..), QueryName (..), ResDecl (..), ResName (..), SysDecl (..),
                            SysName (..), VarName (..))
import Gameverif.Ecsy.Concrete (AnnProg, AnnProgDecl)
import Gameverif.Util.Ann (Located (..))

newtype ScopedName = ScopedName { unScopedName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

data ResolvedName =
    ResolvedNameBound !ScopedName
  | ResolvedNameFree !VarName
  deriving stock (Eq, Ord, Show)

mainName :: ScopedName
mainName = ScopedName "main"

data SomeDecl p v =
    SomeDeclTop !(AnnProgDecl p v)
  | SomeDeclMeth !ResName
  | SomeDeclField !CompName
  deriving stock (Eq, Show)

projectDecls :: AnnProgDecl p v -> [(ScopedName, SomeDecl p v)]
projectDecls pd =
  let td = SomeDeclTop pd
  in case locVal pd of
    ProgDeclFunc fd -> [(ScopedName (unFuncName (funcDeclName fd)), td)]
    ProgDeclRes rd ->
      let n = resDeclName rd
          x = (ScopedName (unResName n), td)
          xs = [(ScopedName (unMethName (methDeclName md)), SomeDeclMeth n) | md <- toList (resDeclMethods rd)]
      in x:xs
    ProgDeclSys sd -> [(ScopedName (unSysName (sysDeclName sd)), td)]
    ProgDeclQuery qd -> [(ScopedName (unQueryName (queryDeclName qd)), td)]
    ProgDeclComp cd ->
      let n = compDeclName cd
          x = (ScopedName (unCompName n), td)
          xs = [(ScopedName (unFieldName (compFieldName fd)), SomeDeclField n) | fd <- toList (compDeclFields cd)]
      in x:xs
    ProgDeclArch ad -> [(ScopedName (unArchName (archDeclName ad)), td)]
    ProgDeclInv vd -> [(ScopedName (unInvName (invDeclName vd)), td)]
    ProgDeclMain _ -> [(mainName, td)]

type Resolver p v = Map ScopedName (SomeDecl p v)

data DeclExistsError p v = DeclExistsError
  { deeName :: !ScopedName
  , deeTypeExists :: !(SomeDecl p v)
  , deeTypeProposed :: !(SomeDecl p v)
  } deriving stock (Eq, Show)

instance (Show p, Typeable p, Show v, Typeable v) => Exception (DeclExistsError p v)

initResolver :: AnnProg p v -> Either (DeclExistsError p v) (Resolver p v)
initResolver = foldM go1 Map.empty where
  go1 res decl =
    let pairs = projectDecls decl
    in foldM go2 res pairs
  go2 res (name, decl) =
    case Map.lookup name res of
      Just exists -> Left (DeclExistsError name exists decl)
      Nothing -> Right (Map.insert name decl res)

rewriteVar :: Resolver p Text -> Text -> ResolvedName
rewriteVar res v =
  let name = ScopedName v
  in if Map.member name res
    then ResolvedNameBound name
    else ResolvedNameFree (VarName v)

rewriteResolver :: Resolver p Text -> Resolver p ResolvedName
rewriteResolver res = fmap goSome res where
  goDecl = fmap (fmap (rewriteVar res))
  goSome = \case
    SomeDeclTop pd -> SomeDeclTop (goDecl pd)
    SomeDeclMeth rn -> SomeDeclMeth rn
    SomeDeclField cn -> SomeDeclField cn

newtype ResolveError = ResolveError { unResolveError :: ScopedName }
  deriving stock (Show)
  deriving newtype (Eq)

instance Exception ResolveError

resolve :: ScopedName -> Resolver p ResolvedName -> Either ResolveError (SomeDecl p ResolvedName)
resolve n = maybe (Left (ResolveError n)) Right . Map.lookup n
