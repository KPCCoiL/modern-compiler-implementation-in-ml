module Tiger.Lexer

import Text.Lexer

%default total

%access private

public export
data TigerToken = TypeDec
                | VarDec
                | FuncDec
                | Break
                | Of
                | End
                | In
                | Nil
                | Let
                | Do
                | To
                | For
                | While
                | If
                | Then
                | Else
                | Array
                | Assign
                | Or
                | And
                | TGE
                | TGT
                | TLE
                | TLT
                | TNEQ
                | TEQ
                | Divide
                | Times
                | Plus
                | Minus
                | Dot
                | RBrace
                | LBrace
                | RBracket
                | LBracket
                | RParen
                | LParen
                | Semicolon
                | Colon
                | Comma
                | Comment
                | StrLit String
                | IntLit Int
                | Ident String

public export
Show TigerToken where
  show TypeDec = "type"
  show VarDec = "var"
  show FuncDec = "function"
  show Break = "break"
  show Of = "of"
  show End = "end"
  show In = "in"
  show Nil = "nil"
  show Let = "let"
  show Do = "do"
  show To = "to"
  show For = "for"
  show While = "while"
  show If = "if"
  show Then = "then"
  show Else = "else"
  show Array = "array"
  show Assign = ":="
  show Or = "|"
  show And = "&"
  show TGE = ">="
  show TGT = ">"
  show TLE = "<="
  show TLT = "<"
  show TNEQ = "<>"
  show TEQ = "="
  show Divide = "/"
  show Times = "*"
  show Plus = "+"
  show Minus = "-"
  show Dot = "."
  show RBrace = "}"
  show LBrace = "{"
  show RBracket = "]"
  show LBracket = "["
  show RParen = ")"
  show LParen = "("
  show Semicolon = ";"
  show Colon = ":"
  show Comma = ","
  show Comment = "/* comment */"
  show (StrLit str) = "\"" ++ str ++ "\""
  show (IntLit n) = "(IntLit " ++ show n ++ ")"
  show (Ident str) = "(Ident " ++ str ++ ")"


extractLiteral : String -> String
extractLiteral s with (length s)
  | Z = ""
  | (S n) = substr 1 n s

comment : Lexer
comment = opt spaces <+>
          blockComment (exact "/*") (exact "*/") <+>
          opt spaces
          <|> spaces

identChar : Lexer
identChar = alphaNum <|> is '_'

identifier : Lexer
identifier = alpha <+> many identChar

reserved : String -> TigerToken -> (Lexer, String -> TigerToken)
reserved str token = (exact str <+> reject identChar, const token)

tigerSymbol : String -> TigerToken -> (Lexer, String -> TigerToken)
tigerSymbol sym token = (exact sym, const token)

tigerLexer : TokenMap TigerToken
tigerLexer = [ (comment, const Comment)
             , reserved "type" TypeDec
             , reserved "var" VarDec
             , reserved "function" FuncDec
             , reserved "break" Break
             , reserved "of" Of
             , reserved "end" End
             , reserved "in" In
             , reserved "nil" Nil
             , reserved "let" Let
             , reserved "do" Do
             , reserved "to" To
             , reserved "for" For
             , reserved "while" While
             , reserved "if" If
             , reserved "then" Then
             , reserved "else" Else
             , reserved "array" Array
             , tigerSymbol ":=" Assign
             , tigerSymbol "|" Or
             , tigerSymbol "&" And
             , tigerSymbol ">=" TGE
             , tigerSymbol ">" TGT
             , tigerSymbol "<=" TLE
             , tigerSymbol "<" TLT
             , tigerSymbol "<>" TNEQ
             , tigerSymbol "=" TEQ
             , tigerSymbol "/" Divide
             , tigerSymbol "*" Times
             , tigerSymbol "+" Plus
             , (intLit, IntLit . cast{to=Int})
             , tigerSymbol "-" Minus
             , tigerSymbol "." Dot
             , tigerSymbol "}" RBrace
             , tigerSymbol "{" LBrace
             , tigerSymbol "]" RBracket
             , tigerSymbol "[" LBracket
             , tigerSymbol ")" RParen
             , tigerSymbol "(" LParen
             , tigerSymbol ";" Semicolon
             , tigerSymbol ":" Colon
             , tigerSymbol "," Comma
             , (stringLit, StrLit . extractLiteral)
             , (identifier, Ident)
             ]

export
lexTiger : String -> (List (TokenData TigerToken), Int, Int, String)
lexTiger = lex tigerLexer

testLex : String -> IO ()
testLex s = do
  let (token, a, b, rest) = lexTiger s
  printLn $ map tok token
  printLn (a, b)
  putStrLn rest
