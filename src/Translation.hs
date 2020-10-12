{-# LANGUAGE OverloadedStrings #-}
module Translation where

import           Control.Monad.Except
import           Data.List.NonEmpty
import qualified Data.Map                      as M
import qualified Data.Text.Lazy                as T
import           Language.JavaScript.Parser.AST
import qualified Language.JavaScript.Parser.Parser
                                               as P
import           SchemeTypes

-- Language.JavaScript.Parser.AST has a lot of annoying annotations to
-- fill in, so we put JSNoAnnot for the annotation
nn = JSNoAnnot

jsTernary a b = JSExpressionTernary a nn b nn
jsString s = JSStringLiteral nn (show s)
jsLit = JSLiteral nn
jsDec = JSDecimal nn . show
jsIdent = JSIdentifier nn
jsReturn e = JSReturn nn (Just (jsParens e)) JSSemiAuto
jsSet s = JSAssignExpression (jsIdent s) (JSAssign nn)
jsExprStmt e = JSExpressionStatement e JSSemiAuto
jsParens e = JSExpressionParen nn e nn
jsBinOp op l = JSExpressionBinary l op
jsArrow params =
  JSArrowExpression (JSParenthesizedArrowParameterList nn params nn) nn

jsCommaList :: [a] -> JSCommaList a
jsCommaList = foldl f JSLNil
 where
  f :: JSCommaList a -> a -> JSCommaList a
  f JSLNil e = JSLOne e
  f r      e = JSLCons r nn e

toProgram e = JSAstProgram [jsExprStmt e] nn

binOps = M.fromList [("+", add), ("*", mul), ("-", sub), ("=", eq)]
 where
  add [] = pure (jsDec 0)
  add l  = foldr1 (jsBinOp (JSBinOpPlus nn)) <$> traverse convert l
  mul [] = pure (jsDec 1)
  mul l  = foldr1 (jsBinOp (JSBinOpTimes nn)) <$> traverse convert l
  sub [] = throwError "Wrong number of arguments to -"
  sub l  = foldr1 (jsBinOp (JSBinOpMinus nn)) <$> traverse convert l

  eq [a, b] = jsParens <$> (jsBinOp (JSBinOpEq nn) <$> convert a <*> convert b)
  eq _      = throwError "Wrong number of arguments to ="


convert :: Expr -> Either T.Text JSExpression
convert (Const Nil              ) = pure (jsLit "null")
convert (Const (Character c    )) = pure (jsString [c])
-- Symbols become strings
convert (Const (Symbol    q    )) = pure (jsString q)
convert (Const (Number    n    )) = pure (jsDec n)
convert (Const (String    s    )) = pure (jsString s)
convert (Const (Boolean   True )) = pure (jsLit "true")
convert (Const (Boolean   False)) = pure (jsLit "false")
convert (If p c a) = jsTernary <$> convert p <*> convert c <*> convert a
convert (IfPartial p c) =
  jsTernary <$> convert p <*> convert c <*> pure (jsLit "null")
convert (Id q) = pure (jsIdent q)
convert (App f e)
  | Id i <- f, Just op <- binOps M.!? i = op e
  | otherwise = do
    f' <- convert f
    e' <- jsCommaList <$> traverse convert e
    pure (JSMemberExpression f' nn e' nn)
convert (Lambda params [] e) = do
  params' <- jsCommaList <$> traverse (convert . Id) params
  b       <- convert e
  pure (jsParens (jsArrow params' (jsExprStmt b)))
convert (Lambda params l e) = do
  params' <- jsCommaList <$> traverse (convert . Id) params
  b       <- convertExprStmt l e
  pure (jsParens (jsArrow params' b))
convert (Set s e) = jsSet s <$> convert e

convert _         = throwError "Not supported"

convertExprStmt :: [Expr] -> Expr -> Either T.Text JSStatement
convertExprStmt l e = do
  e'    <- convert e
  forms <-
    (<> [jsReturn e'])
      <$> (((`JSExpressionStatement` JSSemi nn) <$>) <$> traverse convert l)
  pure (JSStatementBlock nn forms nn JSSemiAuto)

jsVarInit = JSVarInit nn
jsVar i e = JSVariable
  nn
  (JSLOne (JSVarInitExpression (jsIdent i) (jsVarInit e)))
  JSSemiAuto

jsFunc f params body = JSFunction nn
                                  (JSIdentName nn f)
                                  nn
                                  (jsCommaList params)
                                  nn
                                  (JSBlock nn body nn)
                                  JSSemiAuto


convertP :: Program -> Either T.Text JSAST
convertP p = (`JSAstProgram` nn) <$> traverse f (toList p)
 where
  f (Left  e                    ) = jsExprStmt <$> convert e
  f (Right (Defn1 f e          )) = jsVar f <$> convert e
  f (Right (Defn2 f args cmds e)) = jsVar f <$> convert (Lambda args cmds e)

