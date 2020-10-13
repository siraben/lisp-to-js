{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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

jsTernary a b = JSExpressionTernary (jsParens a) nn (jsParens b) nn
jsString s = JSStringLiteral nn (show s)
jsLit = JSLiteral nn
jsDec = JSDecimal nn . show
jsIdent = JSIdentifier nn . sanitizeIdent
jsReturn e = JSReturn nn (Just (jsParens e)) JSSemiAuto
jsSet s = JSAssignExpression (jsIdent s) (JSAssign nn)
jsExprStmt e = JSExpressionStatement e JSSemiAuto
jsParens e = JSExpressionParen nn e nn
jsBinOp op l = JSExpressionBinary l op
jsArrow params =
  JSArrowExpression (JSParenthesizedArrowParameterList nn params nn) nn
-- jsCons car cdr = JSMemberExpression (jsIdent "Object") nn (jsCommaList [JSObjectLiteral nn ])

jsCons car cdr = JSMemberExpression
  (jsIdent "Object")
  nn
  (JSLOne
    (JSObjectLiteral
      nn
      (JSCTLNone
        (jsCommaList
          [ JSPropertyNameandValue (JSPropertyIdent nn "car") nn [car]
          , JSPropertyNameandValue (JSPropertyIdent nn "cdr") nn [cdr]
          ]
        )
      )
      nn
    )
  )
  nn

jsMemberDot o p = JSMemberDot o nn (jsIdent p)

jsCommaList :: [a] -> JSCommaList a
jsCommaList = foldl f JSLNil
 where
  f :: JSCommaList a -> a -> JSCommaList a
  f JSLNil e = JSLOne e
  f r      e = JSLCons r nn e

toProgram e = JSAstProgram [jsExprStmt e] nn

replace a b = fmap $ \c -> if c == a then b else c
sanitizeIdent = foldr ((.) . uncurry replace) id subs
  where subs = [('-', '_'), ('?', 'p')]

binOps = M.fromList
  [ ("+"            , add)
  , ("string-append", stringApp)
  , ("*"            , mul)
  , ("-"            , sub)
  , ("="            , eq)
  , ("<"            , lt)
  , ("cons"         , cons)
  , ("car"          , car)
  , ("cdr"          , cdr)
  , ("list"         , list)
  , ("modulo"       , modulo)
  , ("eqv?"         , eqv)
  , ("null?"        , nullp)
  , ("symbol?"      , symbolp)
  ]
 where
  wrongArgsError s = throwError ("Wrong number of arguments to " <> s)
  add [] = pure (jsDec 0)
  add l  = foldr1 (jsBinOp (JSBinOpPlus nn)) <$> traverse convert l
  stringApp [] = pure (jsString "")
  stringApp l  = foldr1 (jsBinOp (JSBinOpPlus nn)) <$> traverse convert l
  mul [] = pure (jsDec 1)
  mul l  = foldr1 (jsBinOp (JSBinOpTimes nn)) <$> traverse convert l
  sub [] = wrongArgsError "-"
  sub l  = foldr1 (jsBinOp (JSBinOpMinus nn)) <$> traverse convert l

  eq [a, b] = jsBinOp (JSBinOpEq nn) <$> convert a <*> convert b
  eq _      = wrongArgsError "="

  lt [a, b] = jsBinOp (JSBinOpLt nn) <$> convert a <*> convert b
  lt _      = wrongArgsError "<"


  cons [a, b] = jsCons <$> convert a <*> convert b
  cons _      = wrongArgsError "cons"

  car [a] = jsMemberDot <$> convert a <*> pure "car"
  car _   = wrongArgsError "car"

  cdr [a] = jsMemberDot <$> convert a <*> pure "cdr"
  cdr _   = wrongArgsError "cdr"

  nullp [a] = convert (App (Id "eqv?") [a, Const Nil])
  nullp _   = wrongArgsError "null?"

  list l = foldr jsCons (jsLit "null") <$> traverse convert l

  modulo [a, b] = jsBinOp (JSBinOpMod nn) <$> convert a <*> convert b
  modulo _      = wrongArgsError "modulo"

  eqv [a, b] = jsBinOp (JSBinOpEq nn) <$> convert a <*> convert b
  eqv _      = wrongArgsError "eqv?"

  symbolp [a] =
    jsBinOp (JSBinOpEq nn) <$> convert (App (Id "typeof") [a]) <*> convert
      (Const (String "string"))
  symbolp _ = wrongArgsError "symbol?"


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
-- Need to fix this later, look up in binOps and generate variadic lambda.
convert (Id "eqv?") =
  convert (Lambda ["x", "y"] [] (App (Id "eqv?") [Id "x", Id "y"]))
convert (Id "car") = convert (Lambda ["x"] [] (App (Id "car") [Id "x"]))
convert (Id "cdr") = convert (Lambda ["x"] [] (App (Id "cdr") [Id "x"]))
convert (Id q    ) = pure (jsIdent q)
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

