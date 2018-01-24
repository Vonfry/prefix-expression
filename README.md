![lang: haskell](https://img.shields.io/badge/lang-haskell-brightgreen.svg)
[![Travis Master](https://img.shields.io/travis/VonFry/prefix-expression/master.svg?label=master)](https://travis-ci.org/VonFry/prefix-expression)
[![Travis Dev](https://img.shields.io/travis/VonFry/prefix-expression/develop.svg?label=develop)](https://travis-ci.org/VonFry/prefix-expression)
[![Hackage](https://img.shields.io/hackage/v/prefix-expression.svg)](https://hackage.haskell.org/package/prefix-expression)

# ParseExpression

It converts infix expression to prefix expression.

# Usage

```
import Text.Exp.Prefix (fromInfix)

fromInfix "1 + 3"

-- return: "+ 1 3"
```


