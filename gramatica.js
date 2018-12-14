// Gramatica para el lenguaje Flecha
// ==========================
// Nestor Muñoz


//*****************************************************************//
// Funciones auxiliares
//*****************************************************************//
{  
    function anidarLambdas(lambdasIds, exp){
      if(lambdasIds){
          if(lambdasIds.length == 1){
              return ["ExprLambda", lambdasIds[0][1], exp];
            }else{
              return ["ExprLambda", lambdasIds[0][1], anidarLambdas(lambdasIds.slice(1), exp)];
          }
      }else{
          return exp;
        }
    }

    function aplicarPrecedencia(exps){
      exps = aplicarConstructor(exps);
      exps = aplicarPrecedenciaExprApply(exps);
      exps = aplicarPrecedenciaOperadoresUnarios(exps, ["UMINUS"]);
      exps = aplicarPrecedenciaOperadoresBinarios(exps, ["DIV", "MOD"]);
      exps = aplicarPrecedenciaOperadoresBinarios(exps, ["MUL"]);
      exps = aplicarPrecedenciaOperadoresBinarios(exps, ["ADD", "SUB"]);
      exps = aplicarPrecedenciaOperadoresBinarios(exps, ["EQ", "NE", "LE", "GE", "GT", "LT"]);
      exps = aplicarPrecedenciaOperadoresUnarios(exps, ["NOT"]);
      exps = aplicarPrecedenciaOperadoresBinarios(exps, ["AND"]);
      exps = aplicarPrecedenciaOperadoresBinarios(exps, ["OR"]);
      exps = aplicarPrecedenciaExprApply(exps);
      return exps;
    }

    function aplicarPrecedenciaOperadoresBinarios(exps, operadores){
      var i = 0;
      while(i < exps.length){
        if(operadores.indexOf(exps[i][1]) != -1){
          var exprApply = ["ExprApply", ["ExprApply", exps[i], exps[i-1]], exps[i+1]];
          exps[i-1] = exprApply;
          exps.splice(i,2);
        }else{
          i++;
        }
      }
      while(exps.length == 1){
        exps = exps[0];
      }
      return exps;      
    }

    function aplicarPrecedenciaOperadoresUnarios(exps, operadores){
      var i = 0;
      while(i < exps.length){
        if(operadores.indexOf(exps[i][1]) != -1){
          var exprApply = ["ExprApply", exps[i], exps[i+1]];
          exps[i] = exprApply;
          exps.splice(i+1,1);
        }else{
          i++;
        }
      }
      while(exps.length == 1){
        exps = exps[0];
      }
      return exps;  
    }

    function aplicarPrecedenciaExprApply(exps){
      var i = 0;
      while(i + 1 < exps.length && exps[i] != "ExprApply"){
        if((exps[i][0] === "ExprVar" && exps[i][1].toUpperCase() != exps[i][1]) ||
            exps[i][0] === "ExprApply" || exps[i][0] === "ExprLambda"){
          // es una aplicacion
          // chequeo lo que hay a la derecha          
          if((exps[i+1][0] === "ExprVar" && exps[i+1][1].toUpperCase() != exps[i+1][1]) ||// es una variable
              exps[i+1][0] === "ExprNumber" ||
              exps[i+1][0] === "ExprChar" ||
              exps[i+1][0] === "ExprApply"){            
            var exprApply = ["ExprApply", exps[i], exps[i+1]];
            exps[i] = exprApply;
            exps.splice(i+1,1);
          }else{
            i++;
          }
        }else{
          i++;
        }
      }
      while(exps.length == 1){
        exps = exps[0];
      }
      return exps;
    }

    function aplicarConstructor(exps){
      var i = 0;
      while(i + 1 < exps.length){
        if(esConstructorOAplicacionDeConstructor(exps[i])){
          // es constructor o aplicacion de constructor
          // chequeo lo que hay a la derecha          
          if((exps[i+1][0] === "ExprVar" && exps[i+1][1].toUpperCase() != exps[i+1][1]) ||// es una variable
              exps[i+1][0] === "ExprNumber" ||
              exps[i+1][0] === "ExprChar" ||
              exps[i+1][0] === "ExprApply" ||
              exps[i+1][0] === "ExprConstructor"){
            var exprApply = ["ExprApply", exps[i], exps[i+1]];
            exps[i] = exprApply;
            exps.splice(i+1,1);
          }else{
            i++;
          }
        }else{
          i++;
        }
      }
      while(exps.length == 1){
        exps = exps[0];
      }
      return exps; 
    }

    function esConstructorOAplicacionDeConstructor(exp){      
      return exp[0] === "ExprConstructor" || (exp[0] === "ExprApply" && esConstructorOAplicacionDeConstructor(exp[1]));
    }
    
}

//*****************************************************************//
// GRAMATICA
//*****************************************************************//
// Nota: ya que Pegjs no soporta recursion a la izquierda, se utiliza recursion a la derecha

// programa -> ɛ | programa definicion
Programa
  = _? d:Definicion _? p:Programa?
  { return p? [["Def", d.id, d.exp]].concat(p) : [["Def", d.id, d.exp]]; }
  / _?

//definicion -> <DEF> <LOWERID> <parámetros> <DEFEQ> <expresión>  
Definicion
  = DEF _ l:LOWERID _ p:Parametros? _? DEFEQ _? e:Expresion
  { 
    return { id: l[1], exp: anidarLambdas(p, e) }; 
  }
        
// parametros -> ɛ | <LOWERID> parametros
Parametros
  = l:LOWERID _? p:Parametros?
  {
    if(p){
        p.unshift(l)
        return p;     
    }else{
      return [l];
    }
  }
    
// expresión -> expresiónExterna | expresiónExterna <SEMICOLON> expresión
Expresion
  = ee:ExpresionExterna _? SEMICOLON _? e:Expresion { return ["ExprLet", "_", ee, e] }
  / ExpresionExterna 

// expresionExterna -> expresionIf | expresionCase | expresionLet | expresionLambda | expresionInterna
ExpresionExterna
  = ExpresionIf / ExpresionCase / ExpresionLet / ExpresionLambda
  / ei:ExpresionInterna { return aplicarPrecedencia(ei) }

// expresionIf -> <IF> expresionInterna <THEN> expresionInterna ramasElse
ExpresionIf
  = IF _ eiIf:ExpresionInterna _ THEN _ eiT:ExpresionInterna _ re:RamasElse
  { return ["ExprCase", aplicarPrecedencia(eiIf), ["CaseBranch", "True", [], aplicarPrecedencia(eiT)], ["CaseBranch", "False", [], re]]; }

// ramasElse -> <ELIF> expresionInterna <THEN> expresionInterna ramasElse | <ELSE> expresionInterna
RamasElse
  = ELIF _ eiElif:ExpresionInterna _ THEN _ eiT:ExpresionInterna _ re:RamasElse
  { return ["ExprCase", aplicarPrecedencia(eiElif), ["CaseBranch", "True", [], aplicarPrecedencia(eiT)], ["CaseBranch", "False", [], re]]; }
  / ELSE _ ei:ExpresionInterna { return aplicarPrecedencia(ei); }

// expresionCase -> <CASE> expresionInterna ramasCase
// reemplazo expresionInterna por <LOWERID>
// expresionCase -> <CASE> <LOWERID> ramasCase
ExpresionCase
  = CASE _ l:LOWERID _ r:RamasCase { return ["ExprCase", l, r] }

// ramasCase -> ɛ | ramaCase ramasCase
RamasCase
  = r:RamaCase _? rs:RamasCase? { return rs?[r, rs]:r; }

// ramaCase -> <PIPE> <UPPERID> parámetros <ARROW> expresionInterna
RamaCase
  = PIPE _ u:UPPERID _? p:Parametros? _? ARROW _? ei:ExpresionInterna
  { return ["CaseBranch", u[1], p?p.map(function(e){ return e[1] }):[], ei.length==1?ei[0]:aplicarPrecedencia(ei)] }

// expresionLet -> <LET> <ID> parámetros <DEFEQ> expresiónInterna <IN> expresiónExterna
ExpresionLet
  = LET _ l:LOWERID _ p:Parametros? _? DEFEQ _ ei:ExpresionInterna _ IN _ ee:ExpresionExterna
  { return ["ExprLet", l[1], anidarLambdas(p, aplicarPrecedencia(ei)), ee] }

// expresionLambda -> <LAMBDA> parámetros <ARROW> expresiónExterna
ExpresionLambda
  = LAMBDA _? p:Parametros _? ARROW _? ee:ExpresionExterna
  {    
    return anidarLambdas(p, ee);
  }

// expresionInterna -> expresiónAplicación | expresiónInterna operadorBinario expresiónInterna | operadorUnario expresiónInterna
// elimino recursion a izquierda
// expresionInterna ->  expresiónAplicación
//                      | expresiónAplicación operadorBinario expresiónInterna
//                      | operadorUnario expresiónInterna operadorBinario expresiónInterna
//                      | operadorUnario expresiónInterna
ExpresionInterna
  = ea:ExpresionAplicacion _? ob:OperadorBinario _? ei:ExpresionInterna
  { return ea.concat([ob]).concat(ei) }
    / ExpresionAplicacion
    / ou:OperadorUnario _? ei1:ExpresionInterna _? ob:OperadorBinario _? ei2:ExpresionInterna
    { return [ou].concat(ei1).concat(ob).concat(ei2) }
    / ou:OperadorUnario _? ei:ExpresionInterna 
    { return [ou].concat(ei) }

Operador
  = OperadorBinario / OperadorUnario

// operadorBinario -> <AND> | <OR> | <EQ> | <NE> | <GE> | <LE> | <GT> | <LT> | <PLUS> | <MINUS> | <TIMES> | <DIV> | <MOD>
OperadorBinario
  = AND / OR / EQ / NE / GE / LE / GT / LT / PLUS
  / MINUS { return ["ExprVar", "SUB"] }
  / TIMES / DIV / MOD

// operadorUnario -> <NOT> | <MINUS>
OperadorUnario
  = NOT
    / MINUS { return ["ExprVar", "UMINUS"] }

// expresionAplicacion -> expresiónAtómica | expresiónAplicación expresiónAtómica
// paso recursion izq a der
// expresionAplicacion -> expresiónAtómica | expresiónAtómica expresiónAplicación
ExpresionAplicacion
  = eat:ExpresionAtomica _ eap:ExpresionAplicacion
  { return [eat].concat(eap) }
  / ea:ExpresionAtomica { return [ea] }

// expresionAtomica -> <LOWERID> | <UPPERID> | <NUMBER> | <CHAR> | <STRING> | <LPAREN> expresión <RPAREN>
ExpresionAtomica
  = l:LOWERID
  / UPPERID / NUMBER
    / CHAR / STRING
    / LPAREN _? e:Expresion _? RPAREN { return e; }
  
_ "whitespace"
  = [ \t\n\r]+ / "--"[^\n\r]* {}

//*************************************************************//
// SIMBOLOS TERMINALES
//*************************************************************//
  
LOWERID = !(KEYWORD _) h:[a-z] t:[_a-zA-Z0-9]*
    { return ["ExprVar", h + t.join("")] }

UPPERID = h:[A-Z] t:[_a-zA-Z0-9]*
    { return ["ExprConstructor", h + t.join("")] }

NUMBER = n:[0-9]+ { return ["ExprNumber", parseInt(n.join(""), 10)] }

CHAR = "'" c:([ a-zA-Z0-9] / "") "'" { return ["ExprChar", c.charCodeAt(0)]}
      / "'" c:SPECIALCHAR "'" { return ["ExprChar", c.charCodeAt(0)]}

SPECIALCHAR = "\\'" / '\\"' { return '\"' } / "\\\\" / "\\t" / "\\r"
      / "\\n" { return '\n' }

STRING = '"' s:([ a-zA-Z0-9] / SPECIALCHAR)* '"'
  {     
      return s.reduceRight(function(r, e){
          return ["ExprApply",["ExprApply", ["ExprConstructor", "Cons"], ["ExprChar", e.charCodeAt(0)]], r]
        }, ["ExprConstructor", "Nil"])
    }

KEYWORD = DEF / IF / THEN / ELIF / ELSE / CASE / LET / IN

DEF = "def"

IF = "if"
THEN = "then"
ELIF = "elif"
ELSE = "else"

CASE = "case"

LET = "let"
IN = "in"

DEFEQ = "="
SEMICOLON = ";"
LPAREN = "("
RPAREN = ")"
LAMBDA = "\\"
PIPE = "|"
ARROW = "->"

AND = "&&" { return ["ExprVar", "AND"] }
OR = "||" { return ["ExprVar", "OR"] }
NOT = "!" { return ["ExprVar", "NOT"] }

EQ = "==" { return ["ExprVar", "EQ"] }
NE = "!=" { return ["ExprVar", "NE"] }
GE = ">=" { return ["ExprVar", "GE"] }
LE = "<=" { return ["ExprVar", "LE"] }
GT = ">" { return ["ExprVar", "GT"] }
LT = "<" { return ["ExprVar", "LT"] }

PLUS = "+" { return ["ExprVar", "ADD"] }
MINUS = "-"
TIMES = "*" { return ["ExprVar", "MUL"] }
DIV = "/" { return ["ExprVar", "DIV"] }
MOD = "%" { return ["ExprVar", "MOD"] }
