{-# LANGUAGE OverloadedStrings #-}
module Translation where

import SchemeTypes
import qualified Language.JavaScript.Parser.Parser as P
import Language.JavaScript.Parser.AST
import Control.Monad.Except
import Data.List.NonEmpty
import qualified Data.Text.Lazy as T

-- Language.JavaScript.Parser.AST has a lot of annoying annotations to
-- fill in, so we put JSNoAnnot for the annotation
nn = JSNoAnnot

jsTernary a b c = JSExpressionTernary a nn b nn c
jsString s = JSStringLiteral nn (show s)
jsLit s = JSLiteral nn s
jsDec n = JSDecimal nn (show n)
jsIdent i = JSIdentifier nn i
jsReturn e = JSReturn nn (Just (jsParens e)) JSSemiAuto
jsSet s e = JSAssignExpression (jsIdent s) (JSAssign nn) e
jsExprStmt e = JSExpressionStatement e JSSemiAuto
jsParens e = JSExpressionParen nn e nn
jsBinOp op l r = JSExpressionBinary l op r
jsArrow params b = JSArrowExpression (JSParenthesizedArrowParameterList nn params nn) nn b

jsCommaList :: [a] -> JSCommaList a
jsCommaList l = foldl f JSLNil l
  where
    f :: JSCommaList a -> a -> JSCommaList a
    f JSLNil e = JSLOne e
    f r e = JSLCons r nn e

toProgram e = JSAstProgram [jsExprStmt e] nn

binOps = [("+", add), ("*", mul), ("-", sub), ("=", eq)]
  where
    add [] = pure (jsDec 0)
    add l = foldr1 (\a r -> jsBinOp (JSBinOpPlus nn) a r) <$> (traverse convert l)
    mul [] = pure (jsDec 1)
    mul l = foldr1 (\a r -> jsBinOp (JSBinOpTimes nn) a r) <$> (traverse convert l)

    sub [] = throwError "Wrong number of arguments to -"
    sub l = foldr1 (\a r -> jsBinOp (JSBinOpMinus nn) a r) <$> (traverse convert l)

    eq [a,b] = do a' <- convert a
                  b' <- convert b
                  pure (jsParens (jsBinOp (JSBinOpEq nn) a' b'))
    eq _ = throwError "Wrong number of arguments to ="


convert :: Expr -> Either T.Text JSExpression
convert (Const Nil) = pure (jsLit "null")
convert (Const (Character c)) = pure (jsString [c])
-- Symbols become strings
convert (Const (Symbol q)) = pure (jsString q)
convert (Const (Number n)) = pure (jsDec n)
convert (Const (String s)) = pure (jsString s)
convert (Const (Boolean True)) = pure (jsLit "true")
convert (Const (Boolean False)) = pure (jsLit "false")
convert (If p c a) = do p' <- convert p
                        c' <- convert c
                        a' <- convert a
                        pure (jsTernary p' c' a')
convert (IfPartial p c) = do p' <- convert p
                             c' <- convert c
                             pure (jsTernary p' c' (jsLit "null"))
convert (Id q) = pure (jsIdent q)
convert (App f e) | Id i <- f, Just op <- lookup i binOps =  op e
                  | otherwise =
                    do f' <- convert f
                       e' <- jsCommaList <$> (traverse convert e)
                       pure (JSMemberExpression f' nn e' nn)
convert (Lambda params [] e) = do params' <- jsCommaList <$> (traverse (convert . Id) params)
                                  b <- convert e
                                  pure (jsParens (jsArrow params' (jsExprStmt b)))
convert (Lambda params l e) = do params' <- jsCommaList <$> (traverse (convert . Id) params)
                                 b <- convertExprStmt l e
                                 pure (jsParens (jsArrow params' b))
convert (Set s e) = jsSet s <$> convert e

convert _ = throwError "Not supported"

convertExprStmt :: [Expr] -> Expr -> Either T.Text JSStatement
convertExprStmt l e = do forms <- f (((`JSExpressionStatement` (JSSemi nn)) <$>) <$> traverse convert l)
                         pure (JSStatementBlock nn forms nn JSSemiAuto)
  where
    f b = do e' <- convert e
             ((++ [jsReturn e']) <$> b)

jsVarInit e = JSVarInit nn e
jsVar i e = JSVariable nn (JSLOne (JSVarInitExpression (jsIdent i) (jsVarInit e))) JSSemiAuto

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
    f (Left e) = jsExprStmt <$> convert e
    f (Right (Defn1 f e)) = do e' <- convert e
                               pure (jsVar f e')
    f (Right (Defn2 f args cmds e)) = do e' <- convert (Lambda args cmds e)
                                         pure (jsVar f e')

