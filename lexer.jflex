package cup.example;

import java_cup.runtime.Symbol;
%%

%unicode
%class Lexer
%cup
%implements sym

%line
%column

%{
public static final int SEMICOLON = 1;
public static final int COMMA = 1;
public static final int LBRACE = 1;
public static final int RBRACE = 1;
public static final int LPAREN = 1;
public static final int RPAREN = 1;
public static final int LBRACK = 1;
public static final int RBRACK = 1;
public static final int SUB = 1;
public static final int ADD = 1;
public static final int MULT = 1;
public static final int DIV = 1;
public static final int MOD = 1;
public static final int LT = 1;
public static final int GT = 1;
public static final int AND = 1;
public static final int OR = 1;
public static final int NOT = 1;
public static final int EQL = 1;
public static final int NE_OP = 1;
public static final int LE_OP = 1;
public static final int GE_OP = 1;
public static final int STRING = 1;
public static final int INT = 1;
public static final int REAL = 1;
public static final int IF = 1;
public static final int PROGRAM = 1;
public static final int ENDPROGRAM = 1;
public static final int FUNCTION = 1;
public static final int ENDFUNCTION = 1;
public static final int THEN = 1;
public static final int ELSE = 1;
public static final int ENDIF = 1;
public static final int WHILE = 1;
public static final int FOR = 1;
public static final int TO = 1;
public static final int DO = 1;
public static final int ENDDO = 1;
public static final int ENDFOR = 1;
public static final int RETURN = 1;
public static final int READ = 1;
public static final int MOVE = 1;
public static final int CLEAR = 1;
public static final int DRAW = 1;
private Symbol symbol(int sym) {
    return new Symbol(sym, yyline+1, yycolumn+1);
}

private Symbol symbol(int sym, Object val) {
   return new Symbol(sym, yyline+1, yycolumn+1, val);
}

private void error(String message) {
   System.out.println("Error at line "+ (yyline+1) + ", column " + (yycolumn+ 1)+ " : "+message);
}
%}

LineEnd = [\r\n]|\r\n
Character = [^\r\n]
WhiteSpace = {LineEnd} | [ \t\f]

LineComment = "//" {Character}* {LineEnd}
CStyleComment = "/*" ~"*/"
Comment = {LineComment} | {CStyleComment}

String = "\"" ~"\""

Identifier = [:jletter:][:jletterdigit:]*

NumericConstant = [0-9]+

%%
<YYINITIAL> {

   /* Whitespace */
  {WhiteSpace} { /* ignore */ }



   /* Separators */
   ";" { return symbol(SEMICOLON); }
   "," { return symbol(COMMA); }
   "{" { return symbol(LBRACE); }
   "}" { return symbol(RBRACE); }
   "(" { return symbol(LPAREN); }
   ")" { return symbol(RPAREN); }
   "[" { return symbol(LBRACK); }
   "]" { return symbol(RBRACK); }

   /* Arithmetic Operations */
   "-" { return symbol(SUB);}
   "+" { return symbol(ADD); }
   "*" { return symbol(MULT); }
   "/" { return symbol(DIV); }
   "%" { return symbol(MOD); }

   "<" { return symbol(LT); }
   ">" { return symbol(GT); }
   "and" { return symbol(AND); }
   "or" { return symbol(OR); }
   "not" { return symbol(NOT); }
   "=" { return symbol(EQL); }
   "<>" { return symbol(NE_OP); }
   "<=" { return symbol(LE_OP); }
   ">=" { return symbol(GE_OP); }

   /* Keywords */
   "String" { return symbol(STRING); }
   "integer" { return symbol(INT);}
   "real"   { return symbol(REAL);}

   "if" { return symbol(IF); }
   "program" { return symbol(PROGRAM); }
   "endprogram" { return symbol(ENDPROGRAM); }
   "function" { return symbol(FUNCTION); }
   "endfunction" { return symbol(ENDFUNCTION); }
   "then" { return symbol(THEN); }
   "else" { return symbol(ELSE); }
   "endif" { return symbol(ENDIF); }
   "while" { return symbol(WHILE); }
   "for" { return symbol(FOR); }
   "to" { return symbol(TO); }
   "do" { return symbol(DO); }
   "enddo" { return symbol(ENDDO); }
   "endfor" { return symbol(ENDFOR); }
   "return"  { return symbol(RETURN); }

    "read" { return symbol(READ); }
    "move" { return symbol(MOVE); }
    "clear" { return symbol(CLEAR); }
    "draw" { return symbol(DRAW); }
    "write" { System.out.println("STRING"); }
    "set color" { return symbol(COLOR); }
    "set line"  { return symbol(LINE); }

   {String}  { System.out.println("STRING"); }
   {Identifier} { return symbol(ID, yytext());}
   {NumericConstant} { return symbol(NUMERIC_CONSTANT, new Integer(Integer.parseInt(yytext()))); }
   {WhiteSpace} { /* Ignore */ }

 }

.|\n { System.out.println("ERROR");error(yytext());}
