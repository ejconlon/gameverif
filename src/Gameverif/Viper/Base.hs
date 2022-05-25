module Gameverif.Viper.Base where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Text (Text)

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

newtype VarName = VarName { unVarName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype TyName = TyName { unTyName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype FieldName = FieldName { unFieldName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype AxName = AxName { unAxName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype FuncName = FuncName { unFuncName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype MethName = MethName { unMethName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

newtype PredName = PredName { unPredName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

data Ty =
    TyRef
  | TyLabel
  | TyLit !LitTy
  | TyDomain !TyName
  | TyFun !(Seq Ty) !Ty
  | TyMeth !(Seq Ty) !(Seq Ty)
  | TyTup !(Seq Ty)
  deriving stock (Eq, Show)

data BuiltOp =
    BuiltOpEquals
  | BuiltOpNotEquals
  | BuiltOpImplies
  | BuiltOpIff
  | BuiltOpNot
  | BuiltOpAnd
  | BuiltOpOr
  | BuiltOpAdd
  | BuiltOpSub
  | BuiltOpMul
  | BuiltOpDiv
  | BuiltOpMod
  | BuiltOpWand
  | BuiltOpGt
  | BuiltOpGte
  | BuiltOpLt
  | BuiltOpLte
  deriving stock (Eq, Show)

opArity :: BuiltOp -> Int
opArity = \case
  BuiltOpEquals -> 2
  BuiltOpNotEquals -> 2
  BuiltOpImplies -> 2
  BuiltOpIff -> 2
  BuiltOpNot -> 1
  BuiltOpAnd -> 2
  BuiltOpOr -> 2
  BuiltOpAdd -> 2
  BuiltOpSub -> 2
  BuiltOpMul -> 2
  BuiltOpDiv -> 2
  BuiltOpMod -> 2
  BuiltOpWand -> 2
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
  BuiltOpImplies -> FixityIn
  BuiltOpIff -> FixityIn
  BuiltOpNot -> FixityPre
  BuiltOpAnd -> FixityIn
  BuiltOpOr -> FixityIn
  BuiltOpAdd -> FixityIn
  BuiltOpSub -> FixityIn
  BuiltOpMul -> FixityIn
  BuiltOpDiv -> FixityIn
  BuiltOpMod -> FixityIn
  BuiltOpWand -> FixityIn
  BuiltOpGt -> FixityIn
  BuiltOpGte -> FixityIn
  BuiltOpLt -> FixityIn
  BuiltOpLte -> FixityIn

data Op v =
    OpFree !v
  | OpBuilt !BuiltOp
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data Quant = QuantExists | QuantForall
  deriving stock (Eq, Show)

type Trigger a = Seq a

data QuantVar = QuantVar
  { quantVarName :: !VarName
  , quantVarTy :: !Ty
  } deriving stock (Eq, Show)

data ExpF v a =
    ExpVarF !v
  | ExpLitF !Lit
  | ExpCondF a a a
  | ExpAppF !(Op v) !(Seq a)
  | ExpFieldF a !FieldName
  | ExpQuantF !Quant !(Seq QuantVar) !(Seq (Trigger a)) a
  | ExpOldF !(Maybe v) a
  | ExpUnfoldingF a a
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
      ExpQuantF qu vs alts c -> ExpQuantF qu vs (fmap goTrig alts) (g c)
      ExpOldF mv a -> ExpOldF (fmap f mv) (g a)
      ExpUnfoldingF a b -> ExpUnfoldingF (g a) (g b)
      ExpLetF v a b -> ExpLetF (f v) (g a) (g b)
    goTrig = fmap g

instance Bifoldable ExpF where
  bifoldr f g = goExp where
    goExp z = \case
      ExpVarF a -> f a z
      ExpLitF _ -> z
      ExpCondF a b c -> g a (g b (g c z))
      ExpAppF o args -> foldr f (foldr g z args) o
      ExpFieldF a _ -> g a z
      ExpQuantF _ _ alts c -> foldr goTrig (g c z) alts
      ExpOldF mv a -> let w = g a z in maybe w (`f` w) mv
      ExpUnfoldingF a b -> g a (g b z)
      ExpLetF v a b -> f v (g a (g b z))
    goTrig = flip (foldr g)

instance Bitraversable ExpF where
  bitraverse f g = goExp where
    goExp = \case
      ExpVarF v -> ExpVarF <$> f v
      ExpLitF l -> pure (ExpLitF l)
      ExpCondF a b c -> ExpCondF <$> g a <*> g b <*> g c
      ExpAppF o args -> ExpAppF <$> traverse f o <*> traverse g args
      ExpFieldF a n -> (`ExpFieldF` n) <$> g a
      ExpQuantF qu vs alts c -> ExpQuantF qu vs <$> traverse goTrig alts <*> g c
      ExpOldF mv a -> ExpOldF <$> traverse f mv <*> g a
      ExpUnfoldingF a b -> ExpUnfoldingF <$> g a <*> g b
      ExpLetF v a b -> ExpLetF <$> f v <*> g a <*> g b
    goTrig = traverse g

data Local e v = Local
  { localName :: !VarName
  , localType :: !Ty
  , localBody :: !(Maybe (e v))
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

localMapExp :: (e v -> f v) -> Local e v -> Local f v
localMapExp g (Local n t me) = Local n t (fmap g me)

data ProofAction =
    ProofActionInhale
  | ProofActionExhale
  | ProofActionAssert
  | ProofActionAssume
  deriving stock (Eq, Show)

data Action e v =
    ActionLabel !v
  | ActionProof !ProofAction !(e v)
  | ActionAssignVars !(Seq v) !(e v)
  | ActionAssignField !v !FieldName !(e v)
  | ActionUnfold !(e v)
  | ActionFold !(e v)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

actionMapExp :: (e v -> f v) -> Action e v -> Action f v
actionMapExp g = \case
  ActionLabel lb -> ActionLabel lb
  ActionProof pa e -> ActionProof pa (g e)
  ActionAssignVars vs e -> ActionAssignVars vs (g e)
  ActionAssignField v fn e -> ActionAssignField v fn (g e)
  ActionUnfold e -> ActionUnfold (g e)
  ActionFold e -> ActionFold (g e)

data StmtF e v a =
    StmtLocalF !(Seq (Local e v)) a
  | StmtIfF !(e v) a !(Maybe a)
  | StmtWhileF !(e v) !(Seq (e v)) a
  | StmtActionF !(Action e v)
  deriving stock (Eq, Show, Functor)

instance Functor e => Bifunctor (StmtF e) where
  bimap f g = goStmt where
    goStmt = \case
      StmtLocalF ds a -> StmtLocalF (fmap goDecl ds) (g a)
      StmtIfF e a mb -> StmtIfF (goExp e) (g a) (fmap g mb)
      StmtWhileF e ps a -> StmtWhileF (goExp e) (fmap goExp ps) (g a)
      StmtActionF s -> StmtActionF (goAction s)
    goExp = fmap f
    goDecl = fmap f
    goAction = fmap f

instance Foldable e => Bifoldable (StmtF e) where
  bifoldr f g = goStmt where
    goStmt z = \case
      StmtLocalF ds a -> foldr goDecl (g a z) ds
      StmtIfF e a mb -> goExp e (g a (foldr g z mb))
      StmtWhileF e ps a -> goExp e (foldr goExp (g a z) ps)
      StmtActionF s -> goAction s z
    goExp = flip (foldr f)
    goDecl = flip (foldr f)
    goAction = flip (foldr f)

instance Traversable e => Bitraversable (StmtF e) where
  bitraverse f g = goStmt where
    goStmt = \case
      StmtLocalF ds a -> StmtLocalF <$> traverse goDecl ds <*> g a
      StmtIfF e a b -> StmtIfF <$> goExp e <*> g a <*> traverse g b
      StmtWhileF e ps a -> StmtWhileF <$> goExp e <*> traverse goExp ps <*> g a
      StmtActionF s -> StmtActionF <$> goAction s
    goExp = traverse f
    goDecl = traverse f
    goAction = traverse f

stmtMapExp :: (e v -> f v) -> StmtF e v a -> StmtF f v a
stmtMapExp g = \case
  StmtLocalF ds a -> StmtLocalF (fmap (localMapExp g) ds) a
  StmtIfF e a b -> StmtIfF (g e) a b
  StmtWhileF e ps a -> StmtWhileF (g e) (fmap g ps) a
  StmtActionF s -> StmtActionF (actionMapExp g s)

data StmtSeqF e v a =
    StmtSeqNilF
  | StmtSeqConsF !(StmtF e v a) a
  deriving stock (Eq, Show, Functor)

stmtSeqMapExp :: (e v -> f v) -> StmtSeqF e v a -> StmtSeqF f v a
stmtSeqMapExp g = \case
  StmtSeqNilF -> StmtSeqNilF
  StmtSeqConsF sf a -> StmtSeqConsF (stmtMapExp g sf) a

instance Functor e => Bifunctor (StmtSeqF e) where
  bimap f g = \case
    StmtSeqNilF -> StmtSeqNilF
    StmtSeqConsF s a -> StmtSeqConsF (bimap f g s) (g a)

instance Foldable e => Bifoldable (StmtSeqF e) where
  bifoldr f g z = \case
    StmtSeqNilF -> z
    StmtSeqConsF s a -> bifoldr f g (g a z) s

instance Traversable e => Bitraversable (StmtSeqF e) where
  bitraverse f g = \case
    StmtSeqNilF -> pure StmtSeqNilF
    StmtSeqConsF s a -> StmtSeqConsF <$> bitraverse f g s <*> g a

data ArgDecl = ArgDecl
  { argDeclName :: !VarName
  , argDeclTy :: !Ty
  } deriving stock (Eq, Show)

data FieldDecl = FieldDecl
  { fieldDeclName :: !FieldName
  , fieldDeclTy :: !Ty
  } deriving stock (Eq, Show)

data AxiomDecl e v = AxiomDecl
  { axiomDeclName :: !(Maybe AxName)
  , axiomDeclBody :: !(e v)
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

data DomDecl e v = DomDecl
  { domDeclName :: !TyName
  , domDeclDomFuncs :: !(Seq (DomFuncDecl v))
  , domDeclAxioms :: !(Seq (AxiomDecl e v))
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

data DomFuncDecl v = DomFuncDecl
  { domFuncDeclName :: !FuncName
  , domFuncDeclArgs :: !(Seq ArgDecl)
  , domFuncDeclRetTy :: !Ty
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

data FuncDecl e v = FuncDecl
  { funcDeclName :: !FuncName
  , funcDeclArgs :: !(Seq ArgDecl)
  , funcDeclRetTy :: !Ty
  , funcDeclRequires :: !(Seq (e v))
  , funcDeclEnsures :: !(Seq (e v))
  , funcDeclBody :: !(e v)
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

data MethDecl e s v = MethDecl
  { methDeclName :: !MethName
  , methDeclArgs :: !(Seq ArgDecl)
  , methDeclRets :: !(Seq ArgDecl)
  , methDeclRequires :: !(Seq (e v))
  , methDeclEnsures :: !(Seq (e v))
  , methDeclBody :: !(s v)
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

data PredDecl e v = PredDecl
  { predDeclName :: !PredName
  , predDeclArgs :: !(Seq ArgDecl)
  , predDeclBody :: !(e v)
  } deriving stock (Eq, Show, Functor, Foldable, Traversable)

data ProgDecl e s v =
    ProgDeclField !FieldDecl
  | ProgDeclDom !(DomDecl e v)
  | ProgDeclFunc !(FuncDecl e v)
  | ProgDeclMeth !(MethDecl e s v)
  | ProgDeclPred !(PredDecl e v)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)
