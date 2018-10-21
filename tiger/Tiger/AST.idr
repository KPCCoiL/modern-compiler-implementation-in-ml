module Tiger.AST

%default total
%access public export

Identifier : Type
Identifier = String

TypeID : Type
TypeID = String

record TypedField where
  constructor MkTyField
  field : Identifier
  fieldType : TypeID

TypedFields : Type
TypedFields = List (TypedField)

data TigerType = TypeName TypeID
               | RecordType TypedFields
               | ArrayOf TypeID

mutual
  data LValue = Var Identifier
              | GetField Identifier Identifier
              | ArrayRef Identifier Expression
  
  data Expression = Refer LValue
                  | Nil
                  | Seq Expression Expression
                  | NoValue
                  | IntLit Int
                  | StrLit String
                  | Negate Expression
                  | FunCall Identifier (List Expression)
                  | Infix Expression Identifier Expression
                  | NewRecord TypeID (List (Identifier, Expression))
                  | NewArray TypeID Expression Expression
                  | Assign LValue Expression
                  | IfElse Expression Expression Expression
                  | When Expression Expression
                  | While Expression Expression
                  | For Identifier Expression Expression Expression
                  | Break
                  | Let (List Declaration) (List Expression)

  data Declaration = TypeDec TypeID TigerType
                   | VarDec Identifier (Maybe TypeID) Expression
                   | FuncDec Identifier TypedFields (Maybe TypeID) Expression

TigerAST : Type
TigerAST = Expression
