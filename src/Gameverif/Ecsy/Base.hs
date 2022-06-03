module Gameverif.Ecsy.Base where

import Data.Bifunctor (Bifunctor (..))
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Text (Text)

data Lit =
    LitUnit
  | LitInt !Integer
  | LitBool !Bool
  deriving stock (Eq, Show)

data LitTy =
    LitTyUnit
  | LitTyInt
  | LitTyBool
  deriving stock (Eq, Show)

inferLitTy :: Lit -> LitTy
inferLitTy = \case
  LitUnit -> LitTyUnit
  LitInt _ -> LitTyInt
  LitBool _ -> LitTyBool

data Ty =
    TyLit !LitTy
  | TyComp !CompName
  deriving stock (Eq, Show)

data Access =
    AccessConst
  | AccessMut
  deriving stock (Eq, Ord, Show, Enum, Bounded)

newtype FieldName = FieldName { unFieldName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype FuncName = FuncName { unFuncName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype MethName = MethName { unMethName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype ResName = ResName { unResName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype SysName = SysName { unSysName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype QueryName = QueryName { unQueryName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype CompName = CompName { unCompName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype ArchName = ArchName { unArchName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype InvName = InvName { unInvName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype VarName = VarName { unVarName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

data BoundRes = BoundRes
  { boundResVar :: !VarName
  , boundResName :: !ResName
  , boundResAccess :: !Access
  } deriving stock (Eq, Show)

data BoundArg = BoundArg
  { boundArgVar :: !VarName
  , boundArgTy :: !Ty
  } deriving stock (Eq, Show)

data FuncDecl u s v = FuncDecl
  { funcDeclName :: !FuncName
  , funcDeclArgs :: !(Seq BoundArg)
  , funcDeclResources :: !(Seq BoundRes)
  , funcDeclRetTy :: !Ty
  , funcDeclRequires :: !(Seq (u v))
  , funcDeclEnsures :: !(Seq (u v))
  , funcDeclBody :: !(Maybe (s v))
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

funcDeclMapBoth :: (u v -> w v) -> (s v -> t v) -> FuncDecl u s v -> FuncDecl w t v
funcDeclMapBoth onProp onStmt (FuncDecl name args resources retTy requires ensures body) =
  FuncDecl name args resources retTy (fmap onProp requires) (fmap onProp ensures) (fmap onStmt body)

data MethDecl u v = MethDecl
  { methDeclName :: !MethName
  , methDeclAccess :: !Access
  , methDeclArgs :: !(Seq BoundArg)
  , methDeclRetTy :: !Ty
  , methDeclRequires :: !(Seq (u v))
  , methDeclEnsures :: !(Seq (u v))
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

methDeclMapProp :: (u v -> w v) -> MethDecl u v -> MethDecl w v
methDeclMapProp onProp (MethDecl name access args retTy requires ensures) =
  MethDecl name access args retTy (fmap onProp requires) (fmap onProp ensures)

data ResDecl u v = ResDecl
  { resDeclName :: !ResName
  , resDeclCtorArgs :: !(Seq BoundArg)
  , resDeclMethods :: !(Seq (MethDecl u v))
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

resDeclMapProp :: (u v -> w v) -> ResDecl u v -> ResDecl w v
resDeclMapProp onProp (ResDecl name ctorArgs methods) =
  ResDecl name ctorArgs (fmap (methDeclMapProp onProp) methods)

data SysDecl u s v = SysDecl
  { sysDeclName :: !SysName
  , sysDeclParams :: !(Seq BoundArg)
  , sysDeclResources :: !(Seq BoundRes)
  , sysDeclQueries :: !(Seq QueryName)
  , sysDeclRequires :: !(Seq (u v))
  , sysDeclEnsures :: !(Seq (u v))
  , sysDeclBody :: !(s v)
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

sysDeclMapBoth :: (u v -> w v) -> (s v -> t v) -> SysDecl u s v -> SysDecl w t v
sysDeclMapBoth onProp onStmt (SysDecl name args resources queries requires ensures body) =
  SysDecl name args resources queries (fmap onProp requires) (fmap onProp ensures) (onStmt body)

data QueryAttr = QueryAttr
  { queryAttrComp :: !CompName
  , queryAttrAccess :: !Access
  } deriving stock (Eq, Show)

data QueryDecl = QueryDecl
  { queryDeclName :: !QueryName
  , queryDeclAttrs :: !(Seq QueryAttr)
  } deriving stock (Eq, Show)

data CompField = CompField
  { compFieldName :: !FieldName
  , compFieldTy :: !Ty
  } deriving stock (Eq, Show)

data CompDecl = CompDecl
  { compDeclName :: !CompName
  , compDeclFields :: !(Seq CompField)
  } deriving stock (Eq, Show)

data ArchDecl = ArchDecl
  { archDeclName :: !ArchName
  , archDeclComps :: !(Seq CompName)
  } deriving stock (Eq, Show)

data InvDecl u v = InvDecl
  { invDeclName :: !InvName
  , invDeclBody :: !(u v)
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

invDeclMapProp :: (u v -> w v) -> InvDecl u v -> InvDecl w v
invDeclMapProp onProp (InvDecl name body) = InvDecl name (onProp body)

data MainDecl u s v = MainDecl
  { mainDeclEnsures :: !(Seq (u v))
  , mainDeclBody :: !(s v)
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

mainDeclMapBoth :: (u v -> w v) -> (s v -> t v) -> MainDecl u s v -> MainDecl w t v
mainDeclMapBoth onProp onStmt (MainDecl ensures body) = MainDecl (fmap onProp ensures) (onStmt body)

data ProgDecl u s v =
    ProgDeclFunc !(FuncDecl u s v)
  | ProgDeclRes !(ResDecl u v)
  | ProgDeclSys !(SysDecl u s v)
  | ProgDeclQuery !QueryDecl
  | ProgDeclComp !CompDecl
  | ProgDeclArch !ArchDecl
  | ProgDeclInv !(InvDecl u v)
  | ProgDeclMain !(MainDecl u s v)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

progDeclMapBoth :: (u v -> w v) -> (s v -> t v) -> ProgDecl u s v -> ProgDecl w t v
progDeclMapBoth onProp onStmt = \case
  ProgDeclFunc fd -> ProgDeclFunc (funcDeclMapBoth onProp onStmt fd)
  ProgDeclRes rd -> ProgDeclRes (resDeclMapProp onProp rd)
  ProgDeclSys sd -> ProgDeclSys (sysDeclMapBoth onProp onStmt sd)
  ProgDeclQuery qd -> ProgDeclQuery qd
  ProgDeclComp cd -> ProgDeclComp cd
  ProgDeclArch ad -> ProgDeclArch ad
  ProgDeclInv vd -> ProgDeclInv (invDeclMapProp onProp vd)
  ProgDeclMain md -> ProgDeclMain (mainDeclMapBoth onProp onStmt md)

data BuiltOp =
    BuiltOpEquals
  | BuiltOpNotEquals
  | BuiltOpNot
  | BuiltOpAnd
  | BuiltOpOr
  | BuiltOpAdd
  | BuiltOpSub
  | BuiltOpMul
  | BuiltOpDiv
  | BuiltOpMod
  | BuiltOpGt
  | BuiltOpGte
  | BuiltOpLt
  | BuiltOpLte
  deriving stock (Eq, Show)

opArity :: BuiltOp -> Int
opArity = \case
  BuiltOpEquals -> 2
  BuiltOpNotEquals -> 2
  BuiltOpNot -> 1
  BuiltOpAnd -> 2
  BuiltOpOr -> 2
  BuiltOpAdd -> 2
  BuiltOpSub -> 2
  BuiltOpMul -> 2
  BuiltOpDiv -> 2
  BuiltOpMod -> 2
  BuiltOpGt -> 2
  BuiltOpGte -> 2
  BuiltOpLt -> 2
  BuiltOpLte -> 2

data Fixity =
    FixityPre
  | FixityIn
  deriving stock (Eq, Show)

opFixity :: BuiltOp -> Fixity
opFixity = \case
  BuiltOpEquals -> FixityIn
  BuiltOpNotEquals -> FixityIn
  BuiltOpNot -> FixityPre
  BuiltOpAnd -> FixityIn
  BuiltOpOr -> FixityIn
  BuiltOpAdd -> FixityIn
  BuiltOpSub -> FixityIn
  BuiltOpMul -> FixityIn
  BuiltOpDiv -> FixityIn
  BuiltOpMod -> FixityIn
  BuiltOpGt -> FixityIn
  BuiltOpGte -> FixityIn
  BuiltOpLt -> FixityIn
  BuiltOpLte -> FixityIn

data Op v =
    OpFree !v
  | OpBuilt !BuiltOp
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data ExpF v a =
    ExpVarF !v
  | ExpLitF !Lit
  | ExpCondF a a a
  | ExpAppF !(Op v) !(Seq a)
  | ExpFieldF a !FieldName
  | ExpLetF !v a a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Bifunctor ExpF where
  bimap f g = goExp where
    goExp = \case
      ExpVarF v -> ExpVarF (f v)
      ExpLitF l -> ExpLitF l
      ExpCondF a b c -> ExpCondF (g a) (g b) (g c)
      ExpAppF o args -> ExpAppF (fmap f o) (fmap g args)
      ExpFieldF a n -> ExpFieldF (g a) n
      ExpLetF v a b -> ExpLetF (f v) (g a) (g b)

data Local e v = Local
  { localName :: !VarName
  , localAccess :: !Access
  , localType :: !(Maybe Ty)
  , localBody :: !(e v)
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

localMapExp :: (e v -> f v) -> Local e v -> Local f v
localMapExp onExp (Local n a t e) = Local n a t (onExp e)

data Action u e v =
    ActionAssert !(u v)
  | ActionAssignVars !(Seq v) !(e v)
  | ActionAssignField !v !FieldName !(e v)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

actionMapBoth :: (u v -> w v) -> (e v -> f v) -> Action u e v -> Action w f v
actionMapBoth onProp onExp = \case
  ActionAssert uv -> ActionAssert (onProp uv)
  ActionAssignVars vs ev -> ActionAssignVars vs (onExp ev)
  ActionAssignField v fn ev -> ActionAssignField v fn (onExp ev)

data StmtF u e v a =
    StmtLocalF !(Seq (Local e v)) a
  | StmtIfF !(e v) a !(Maybe a)
  | StmtWhileF !(e v) !(Seq (u v)) a
  | StmtActionF !(Action u e v)
  deriving stock (Eq, Show, Functor)

instance (Functor u, Functor e) => Bifunctor (StmtF u e) where
  bimap f g = goStmt where
    goStmt = \case
      StmtLocalF ds a -> StmtLocalF (fmap goDecl ds) (g a)
      StmtIfF e a mb -> StmtIfF (goExp e) (g a) (fmap g mb)
      StmtWhileF e ps a -> StmtWhileF (goExp e) (fmap goProp ps) (g a)
      StmtActionF k -> StmtActionF (fmap f k)
    goExp = fmap f
    goProp = fmap f
    goDecl = fmap f

stmtMapBoth :: (u v -> w v) -> (e v -> f v) -> StmtF u e v a -> StmtF w f v a
stmtMapBoth onProp onExp = \case
  StmtLocalF ds a -> StmtLocalF (fmap (localMapExp onExp) ds) a
  StmtIfF e a b -> StmtIfF (onExp e) a b
  StmtWhileF e ps a -> StmtWhileF (onExp e) (fmap onProp ps) a
  StmtActionF k -> StmtActionF (actionMapBoth onProp onExp k)

data StmtSeqF u e v a =
    StmtSeqNilF
  | StmtSeqConsF !(StmtF u e v a) a
  deriving stock (Eq, Show, Functor)

instance (Functor u, Functor e) => Bifunctor (StmtSeqF u e) where
  bimap f g = \case
    StmtSeqNilF -> StmtSeqNilF
    StmtSeqConsF s a -> StmtSeqConsF (bimap f g s) (g a)

stmtSeqMapBoth :: (u v -> w v) -> (e v -> f v) -> StmtSeqF u e v a -> StmtSeqF w f v a
stmtSeqMapBoth onProp onExp = \case
  StmtSeqNilF -> StmtSeqNilF
  StmtSeqConsF sf a -> StmtSeqConsF (stmtMapBoth onProp onExp sf) a
