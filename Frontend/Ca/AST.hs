{-# LANGUAGE DeriveFunctor #-}
module Frontend.Ca.AST (
  Program(..),
  ToplevelDef(..),
  Func(..),
  Data(..),
  Scope(..),
  Stmt(..),
  Expr(..),
  isLVal,
  Lit(..),
  BinaryOp(..),
  UnaryOp(..),
  JumpInfo,
  JumpFlag(..),
  StorageType(..),
  gcptr, -- :: StorageType
  MemClass,
  u8, u16, u32, u64, i8, i16, i32, i64, f32, f64, -- :: MemClass
  memWidth,
  memSigned,
) where

-- Frontend.Ca: a C-alternative language suitable for functional
-- programming language to use as an IR.

newtype Program a
  = MkProgram { toplevel :: [ToplevelDef a] }
  deriving (Show, Eq, Functor)

data ToplevelDef a
  = FuncDef (Func a)
  | DataDef (Data a)
  | ScopeDef (Scope a)
  deriving (Show, Eq, Functor)

data Func a
  = Func {
    fName :: a,
    fArgs :: [(StorageType, a)],
    fBody :: Stmt a
  }
  deriving (Show, Eq, Functor)

data Stmt a
  = SAssign (Expr a) (Expr a)
  | SVarDecl StorageType a
  | SIf (Expr a) (Stmt a) (Stmt a)
  | SWhile (Expr a) (Stmt a)
  | SBlock [Stmt a]
  | SReturn (Expr a) -- Normal return
  | SJump (Expr a) -- ECall, a local jump/call/tail call
  | SLabel a
  deriving (Show, Eq, Functor)

data Expr a
  = ELit Lit -- A source language literal
  | EVar a -- A variable, whose information is described in tyVar a
  | EBinary BinaryOp (Expr a) (Expr a) -- arith/rel/bool
  | EUnary UnaryOp (Expr a) -- not/neg
  | ECall JumpInfo (Expr a) [Expr a] -- func [args..]
  deriving (Show, Eq, Functor)

data Lit
  = LInt Integer
  | LStr String
  | LFlo Double
  deriving (Show, Eq, Ord)

data BinaryOp
  -- Arith
  = AAdd | ASub | AMul | ADiv | AMod
  -- Rel
  | RLt | RLe | RGt | RGe | REq | RNe
  -- Logic
  | LAnd | LOr
  -- Bitwise
  | BAnd | BOr | BXor | BShr | BShl
  deriving (Show, Eq)

data UnaryOp
  = ANeg -- Arith neg
  | LNot -- Logical not
  | BNot -- Bitwise not
  | MRef MemClass -- Mem
  deriving (Show, Eq)

data Data a
  = LiteralData StorageType a Lit
  deriving (Show, Eq, Functor)

data Scope a
  = Extern a
  | ExternReg a String
  deriving (Show, Eq, Functor)

-- Supplementary types and functions
isLVal :: Expr a -> Bool
isLVal e = case e of
  EVar _ -> True
  EUnary (MRef _) _ -> True
  _ -> False

type JumpInfo = [JumpFlag]

data JumpFlag
  = LocalJump
  | CCall
  | TailCall
  | CallerSave
  deriving (Show, Eq, Ord)

data StorageType
  = MkStorage {
    memClass :: MemClass,
    needGc :: Bool
  }
  deriving (Show, Eq)

gcptr = MkStorage i64 True

type MemClass = (Char, Int) -- operand type, width
u8, u16, u32, u64, i8, i16, i32, i64, f32, f64:: MemClass
[u8, u16, u32, u64] = zip (repeat 'u') [8, 16, 32, 64]
[i8, i16, i32, i64] = zip (repeat 'i') [8, 16, 32, 64]
[f32, f64] = zip (repeat 'f') [32, 64]

memWidth :: MemClass -> Int
memWidth = snd

memSigned :: MemClass -> Bool
memSigned = (==) 'i' . fst

