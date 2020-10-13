{-|
Module      : SchemeTypes
Description : Datatypes of r5rs-denot.

As defined in the R5RS standard.
-}
module SchemeTypes where

import           Data.List.NonEmpty

-- |Locations
type L = Int

-- |Natural numbers
newtype N =
  N Integer

-- |Boolean
type T = Bool

-- |Symbol
type Q = String

-- |Characters
type H = Char

-- |Numbers
type R = Integer

-- |Strings
type Str = String

-- |Constants
data Con
  = Symbol Q
  | String Str
  | Boolean T
  | Number R
  | Character H
  | Nil
  deriving (Show, Eq)

-- |Identifiers
type Ide = String

-- |Expressions
data Expr
  = Const Con
  | Id Ide
  | App Expr
        [Expr]
  | Lambda [Ide]
           [Com]
           Expr
  | LambdaV [Ide]
            Ide
            [Com]
            Expr
  | LambdaVV Ide
             [Com]
             Expr
  | If Expr
       Expr
       Expr
  | IfPartial Expr
              Expr
  | Set Ide
        Expr
  deriving (Show)

-- |Commands
type Com = Expr

-- |Definitions
-- Defn1 x e                == (define x e)
-- Defn2 x [y1, y2 ...] e   == (define (x y1 y2 ...) e)
-- Defn3 x [y1, y2 ...] r e == (define (x y1 y2 ... . r) e)
data Defn = Defn1 Ide Expr
          | Defn2 Ide [Ide] [Com] Expr
          | Defn3 Ide [Ide] Ide [Com] Expr
          deriving (Show)

-- body -> definition* sequence
-- sequence -> command* expression
data Body = Body [Defn] [Com] Expr

type Program = NonEmpty (Either Com Defn)
