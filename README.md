# Compiling Lisp to JavaScript
Currently based on [r5rs-denot](github.com/siraben/r5rs-denot), but
will diverge significantly in code generation and core syntax.  Also,
there is no evaluator here, just a transpiler from Lisp to JavaScript. 

Excluding the variadic lambda forms, all the syntax below can
currently be converted to JavaScript.

```text
(if <expr> <expr> <expr>)
(if <expr> <expr>)
(set! <id> <expr>)
(lambda <id>* <expr>*)
(lambda <id> <expr>*)
(lambda (<id>* . <id>) <expr>*)
(<expr> <expr>*)
```

Some of the following derived forms have been implemented.

```text
(define <id> <expr>)
(define (<id> <id>*) <expr>*)
(define (<id>* . <id>) <expr>*)
'<expr>
(begin <expr>*)
(cond (<expr> <expr>)*)
(let ((<id> <expr>)*) <expr>*)
(letrec ((<id> <expr>)) <expr>*)
(and <expr>*)
(or <expr>*)
```

Ensure Cabal is installed and build this project by running `cabal
run`.  The REPL will boot up.  Type an expression and hit ENTER to
evaluate it.

### Usage Examples
```js
lisp-to-js> (define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 6)
var fact=((n)=>(n==0)?(1):n*fact(n-1));fact(6)
lisp-to-js> (define a 1) (define b 3) a
var a=1,b=3;a
lisp-to-js> (define a 3) (set! a 10) a
var a=3;a=10;a
lisp-to-js> (cond ((= 1 2) "hello") ((= 3 1) "foo") (#t "bar"))
(1==2)?("hello"):(3==1)?("foo"):(true)?("bar"):(false)?(null):null
```
## Scheme AST as a Haskell Datatype
The Scheme program:
```scheme
((lambda (x) (+ x x)) 10)
```

Can be written in the Haskell `Expr` datatype as:

```haskell
prog = App (Lambda ["x"] [] (App (Id "+") [Id "x", Id "x"])) [Const (Number 10)]
```

Which will then be converted into a minified JavaScript AST.

```haskell
JSAstProgram [JSExpressionStatement (JSMemberExpression (JSExpressionParen JSNoAnnot (JSArrowExpression (JSParenthesizedArrowParameterList JSNoAnnot (JSLOne (JSIdentifier JSNoAnnot "x")) JSNoAnnot) JSNoAnnot (JSExpressionStatement (JSExpressionBinary (JSIdentifier JSNoAnnot "x") (JSBinOpPlus JSNoAnnot) (JSIdentifier JSNoAnnot "x")) JSSemiAuto)) JSNoAnnot) JSNoAnnot (JSLOne (JSDecimal JSNoAnnot "10")) JSNoAnnot) JSSemiAuto] JSNoAnnot
```

Which gets pretty-printed into a JavaScript program.

```js
((x)=>x+x)(10)
```
