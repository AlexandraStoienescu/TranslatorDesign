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
