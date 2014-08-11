module Syntax where

type VarID = String
type ConID = String

data Type
  = FType Type Type
  | TType Type [Type]
  | TyVar VarID
  | TyCon ConID
  | Tuple [Type]
  | List Type
  | Unit
  deriving (Eq, Ord, Show)
  
type Var = (VarID, Maybe Type, Maybe Expr)
type Variant = (ConID, [Type])
type SimpleType = (ConID, [VarID])

toType :: SimpleType -> Type
toType (name, apps) = TType (TyCon name) (map TyVar apps)

data Decl
  = FunDecl VarID [Var] Expr
  | ConDecl ConID [Var] Expr
  | TypeDecl SimpleType Type
  | NewtypeDecl SimpleType Type
  | ClassDecl SimpleType SimpleType [Decl]
  | DataDecl SimpleType [Decl]
  | ConstantDecl VarID (Maybe Type) Expr
  | VariantDecl SimpleType [Variant]
  | VarDecl Var
  deriving (Eq, Ord, Show)

data Expr
  = Floating Double
  | Integral Integer
  | BinOp String Expr Expr
  | UnOp String Expr
  | Var String
  | Call VarID [Expr]
  | Construct ConID [Expr]
  | Stmts [Expr]
  | Assign String Expr
  deriving (Eq, Ord, Show)
  
