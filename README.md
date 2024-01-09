# Saft

Saft is my custom programming language.

## Syntax

The syntax in Saft is very inspired by rust, but instead using the `:=` operator for declarations instead of the `let` keyword.
Since Saft is not yet typed all types are omitted from declarations, parameters and return values.

### EBNF

```
program = { statement }

statement = item
          | stmt , ';'

item = fn

fn = 'fn' , '(', [ Ident , {',' , Ident } ] ')' block

stmt = 'return' , expr
     | declaration
     | expr


declaration = Ident , ':=' , expr

block = '{' , { statement } , [ expr ] '}'

expr = p_none

p_none = p_assign
p_assign = p_or | p_assign , '=' , p_or
p_or = p_and | p_or , 'or' , p_and
p_and = p_eq | p_and , 'and' , p_eq
p_eq = p_cmp | p_eq , ( '==' | '!=' ) , p_cmp
p_cmp = p_term | p_cmp , ( '<' | '<=' | '>' | '>=' ) , p_term
p_term = p_fac | p_term , ('+' | '-' ) , p_fac
p_fac = p_unary | p_fac ( '*' | '-' ) , p_unary
p_unary = p_exp | ( '-' | '!' ) , p_unary
p_exp = p_call | p_call , '^' , p_exp
p_call = p_primary | p_call , '(' [ expr { ',' expr } ] ')'
p_primary = primary_expr

primary_expr = Ident
     | Float
     | Integer
     | 'nil'
     | 'true'
     | 'false'
     | String
     | list
     | '(' , expr , ')'
     | 'break' , expr
     | 'loop' , expr , block
     | if

list = '[' [ expr { ',' , expr } ] ']'

if = 'if' , expr , block , [ 'else' , ( block | if ) ]
```

## Goals

- Bytecode VM
- Type system (HM)

### Near Goals

### Future Goals
