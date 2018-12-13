const parser = require("./parser.js")

const fs = require('fs');

// Crea el archivo en el directorio actual
function crearArchivo(nombreArchivo, contenido){
	fs.writeFile("./" + nombreArchivo, contenido, function(err) {
	    if(err) {
	        return console.log(err);
	    }
	    console.log("The " + nombreArchivo + " file was saved!");
	});
}

module.exports = 

(function() {
  "use strict";

  const INT = "Int";
  const CHAR = "Char";
  const CLOSURE = "Closure";

  const FUN_GLOBAL = "@fun";
  const FUN_LOCAL = "$fun";
  const ARG_GLOBAL = "@arg";
  const ARG_LOCAL = "$arg";
  const RES_GLOBAL = "@res";
  const RES_LOCAL = "$res";

  function compilarAST(jsonAST){  	
  	//console.log(JSON.stringify(jsonAST));
  	var tags = armarTablaDeTags(jsonAST);
  	var env = inicializarEntorno(jsonAST);
  	var instrucciones = [gen_jump("main")];
  	var reg = 0;
  	var r;
  	for (var i = 0; i < jsonAST.length; i++) {
  		r = compilarDefinicion(tags, env, reg, jsonAST[i]);
  		env = r.env;
  		reg = r.reg;
  		instrucciones = instrucciones.concat(r.instrucciones);
  	}
  	return instrucciones;
  }

  function compilarDefinicion(tags, env, reg, def){
  	var r = compilarExpresion(tags, env, def[2], reg);
  	r.instrucciones = [declararEtiqueta(def[1])].concat(r.instrucciones);
    if(def[1] != "main"){
      r.instrucciones.push(gen_mov_reg(env[def[1]], "$r" + reg));
      r.instrucciones.push(gen_return());
    }
  	return r;
  }

  // compilarExpresion :: Env -> Expr -> Reg -> [Instruccion]
  function compilarExpresion(tags, env, exp, reg){
  	//console.log(exp);
  	//console.log(env);
  	const r = "$r" + reg;
  	const t = "$t";
  	var rtn_i = "rtn_" + reg;
  	switch(exp[0]){
  		case "ExprChar":
		  	var instrucciones = [  			
	  			gen_alloc(r, 2),// celda de 2 slots
	  			gen_mov_int(t, tags[CHAR]),
	  			gen_store(r, 0, t),
	  			gen_mov_int(t, exp[1]),
	  			gen_store(r, 1, t),
	  		];
	  		return {
	  			instrucciones: instrucciones,
	  			env: env,
	  			reg: reg + 1
	  		};
	  		break;
	  	case "ExprApply":
	  		// caso especial de aplicacion de unsafePrintChar y unsafePrintInt
		  	if(exp[1][0] == "ExprVar" && 
		  		(exp[1][1] == "unsafePrintChar" || exp[1][1] == "unsafePrintInt")){
		  		var comp = compilarExpresion(tags, env, exp[2], reg);
		  		var r1 = "$r" + comp.reg;
		  		var print;
		  		if(exp[1][1] == "unsafePrintChar"){
		  			print = gen_print_char(r1);
		  		}else{
		  			print = gen_print(r1);
		  		}
		  		var instrucciones = comp.instrucciones.concat([
		  			gen_load(r1, r, 1),
		  			print
		  		]);
		  		return {
		  			instrucciones: instrucciones,
		  			env: env,
		  			reg: comp.reg + 1
		  		};
		  	}
		  	// aplicacion de expresion lambda
		  	if(exp[1][0] == "ExprLambda"){
		  		var comp_e1 = compilarExpresion(tags, env, exp[1], reg + 1);
	  			var comp_e2 = compilarExpresion(tags, env, exp[2], comp_e1.reg);	  			
	  			var r1 = "$r" + (reg + 1);
	  			var rtn_i = "rtn_" + (reg + 1);
	  			var r2 = "$r" + comp_e1.reg;
	  			//var r3 = "$r" + comp_e2.reg;
	  			//var r4 = "$r" + comp_e2.reg;
	  			var variablesLibres = getVariablesLibres(exp[1][0]); // TODO
	  			const start = "start_" + (reg + 1);
	  			var instrucciones = [
	  				gen_jump(start),
	  				...comp_e1.instrucciones,
		  			declararEtiqueta(start),
		  			gen_alloc(r, 2 + variablesLibres.length),
		  			gen_mov_int(t, tags[CLOSURE]),
		  			gen_store(r, 0, t),
		  			gen_mov_label(t, rtn_i),
		  			gen_store(r, 1, t),
	  				...comp_e2.instrucciones,
	  				//gen_load(r3, FUN_GLOBAL, 1),
	  				gen_mov_reg(FUN_GLOBAL, r1),
	  				gen_mov_reg(ARG_GLOBAL, r2),
	  				//gen_icall(r3),
	  				gen_call(rtn_i),
	  				gen_mov_reg(r, RES_GLOBAL)
	  			];
		  		return {
		  			instrucciones: instrucciones,
		  			env: env,
		  			reg: comp_e2.reg
		  		};		  		
		  	}
		  	if(exp[1][0] == "ExprVar"){
		  		var comp_e1 = compilarExpresion(tags, env, exp[1], reg + 1);
		  		var comp_e2 = compilarExpresion(tags, env, exp[2], comp_e1.reg);
		  		var r1 = "$r" + (reg + 1);
		  		var r2 = "$r" + comp_e1.reg;
		  		var instrucciones = [
		  			...comp_e1.instrucciones,
		  			...comp_e2.instrucciones,
	  				gen_mov_reg(FUN_GLOBAL, r1),
	  				gen_mov_reg(ARG_GLOBAL, r2),
	  				gen_load(t, r1, 1),
	  				gen_icall(t),
	  				gen_mov_reg(r, RES_GLOBAL)
		  		];
		  		return {
		  			instrucciones: instrucciones,
		  			env: env,
		  			reg: comp_e2.reg
		  		};	
		  	}
		  	break;
  		case "ExprNumber":
		  	var instrucciones = [  			
	  			gen_alloc(r, 2),// celda de 2 slots
	  			gen_mov_int(t, tags[INT]),
	  			gen_store(r, 0, t),
	  			gen_mov_int(t, exp[1]),
	  			gen_store(r, 1, t),
	  		];
	  		return {
	  			instrucciones: instrucciones,
	  			env: env,
	  			reg: reg + 1
	  		};
	  		break;
  		case "ExprConstructor":
		  	var instrucciones = [  			
	  			gen_alloc(r, 1),// celda de 1 slot
	  			gen_mov_int(t, tags[exp[1]]),
	  			gen_store(r, 0, t)
	  		];
	  		return {
	  			instrucciones: instrucciones,
	  			env: env,
	  			reg: reg + 1
	  		};
	  		break;
	  	case "ExprVar":	  		
	  		if(env[exp[1]]){
          var instrucciones = [];
          if(env[exp[1]].startsWith("@G_")){ // si es una funcion tengo que compilar call()
            instrucciones.push(gen_call(exp[1]));
          }
          instrucciones.push(gen_mov_reg(r, env[exp[1]]));
		  		return {
		  			instrucciones: instrucciones,
		  			env: env,
		  			reg: reg + 1
		  		};
	  		}else{
	  			console.log("NO ENCONTRE LA VARIABLE " + exp[1]);
	  			//console.log(env);
	  			//console.log(exp);
	  		}
	  		break;
	  	case "ExprLet":
	  		var comp_e1 = compilarExpresion(tags, env, exp[2], reg);
	  		var ext = [];
	  		ext[exp[1]] = r;
  			var ext_env = extenderEntorno(env, ext);
  			var comp_e2 = compilarExpresion(tags, ext_env, exp[3], comp_e1.reg);
	  		return {
	  			instrucciones: comp_e1.instrucciones.concat(comp_e2.instrucciones),
	  			env: env,
	  			reg: comp_e2.reg
	  		};
	  		break;
	  	case "ExprLambda":	  		
  			var ext = [];
  			ext[exp[1]] = ARG_LOCAL; // exp[1] es el nombre del parametro que recibe la funcion lambda
	  		var ext_env = extenderEntorno(env, ext);
	  		var comp = compilarExpresion(tags, ext_env, exp[2], reg+1);
	  		var instrucciones = [
	  			declararEtiqueta(rtn_i),
	  			gen_mov_reg(FUN_LOCAL, FUN_GLOBAL), // Mover el parámetro @fun a un registro local.
	  			gen_mov_reg(ARG_LOCAL, ARG_GLOBAL), // Mover el parámetro @arg a un registro local.
	  			...comp.instrucciones, // expando las instrucciones del cuerpo de la funcion
	  			gen_mov_reg(RES_LOCAL, "$r" + (reg + 1)),
	  			gen_mov_reg(RES_GLOBAL, RES_LOCAL),
	  			gen_return()
	  			
	  		];
	  		return {
	  			instrucciones: instrucciones,
	  			env: env,
	  			reg: comp.reg + 1
	  		};

	  		// TODO: TEST09
  	}

  }

  function getVariablesLibres(exp){
  	// TODO
  	return [];
  }

  function extenderEntorno(env, ext){
  	var r = [];
  	Object.keys(env).forEach(function(e){
  		r[e] = env[e];
  	});
  	Object.keys(ext).forEach(function(e){
  		r[e] = ext[e];
  	});
  	return r;
  }

  function inicializarEntorno(jsonAST){
    var env = [];
    jsonAST.forEach(function(e){
      env[e[1]] = "@G_" + e[1];
    });
    return env;
  }

  // TABLA DE TAGS
  function armarTablaDeTags(jsonAST){
  	// no recolecta los constructores dentro de case, TODO
  	var tablaDeTags = {"Int": 1, "Char": 2, "Closure": 3};
  	var constructores = getConstructores(jsonAST);
  	var i = 4;
  	constructores.forEach(function(e){
  		tablaDeTags[e] = i++;
  	});
  	return tablaDeTags;
  }

  function getConstructores(json){
  	var r = [];
  	json.forEach(function(e){
  		if(Array.isArray(e)){
  			if(e[0] == "ExprConstructor"){
  				r.push(e[1]);
  			}else{
  				r = r.concat(getConstructores(e));
  			}
  		}
  	});
  	return r;
  }

  // GENERACION DE INSTRUCCIONES

  function declararEtiqueta(nombre){
  	return nombre + ":";
  }

  // mov_reg(r1 : Reg, r2 : i64)
  // r1 := r2
  function gen_mov_reg(r1, r2){
  	return "mov_reg(" + r1 + ", " + r2 + ")";
  }

  // mov int(r : Reg, n : i64)
  // r := VInt(n)
  function gen_mov_int(r, n){
  	return "mov_int(" + r + ", " + n + ")";
  }

  // mov_label(r : Reg, l : Label)
  // r := VLoc(p), donde p es la locación de la etiqueta l en el código fuente
  function gen_mov_label(r, l){
  	return "mov_label(" + r + ", " + l + ")";
  }

  // alloc(r : Reg, n : u64)
  // r := VPtr(p), donde p es un puntero a una celda de memoria nueva con n slots
  function gen_alloc(r, n){
  	return "alloc(" + r + ", " + n + ")";
  }

  // load(r1 : Reg, r2 : Reg, i : u64)
  // r 1 := r 2 [i]
  function gen_load(r1, r2, n){
  	return "load(" + r1 + ", " + r2 + ", " + n + ")";
  }

  // store(r 1 : Reg, i : u64, r 2 : Reg)
  // r 1 [i] := r 2
  function gen_store(r1, n, r2){
  	return "store(" + r1 + ", " + n + ", " + r2 + ")";
  }

  // print(r : Reg)
  // Imprime en la salida el valor almacenado en r.
  function gen_print(r, l){
  	return "print(" + r + ")";
  }

  // print char(r : Reg)
  // Imprime en la salida el caracter almacenado en r.
  function gen_print_char(r){
  	return "print_char(" + r + ")";
  }

  // jump(l : Label)
  // Salta a l.
  function gen_jump(l){
  	return "jump(" + l + ")";
  }

  // jump eq(r 1 : Reg, r 2 : Reg, l : Label)
  // Si r 1 == r 2 , salta a l.
  function gen_jump_eq(r1, r2, l){
  	return "jump_eq(" + r1 + ", " + r2 + ", " + l + ")";
  }

  // jump lt(r 1 : Reg, r 2 : Reg, l : Label)
  // Si r 1 < r 2 , salta a l.
  function gen_jump_lt(r1, r2, l){
  	return "jump_lt(" + r1 + ", " + r2 + ", " + l + ")";
  }

  // add(r 1 : Reg, r 2 : Reg, r 3 : Reg)
  // r 1 := r 2 + r 3
  function gen_add(r1, r2, r3){
  	return "add(" + r1 + ", " + r2 + ", " + r3 + ")";
  }

  // sub(r 1 : Reg, r 2 : Reg, r 3 : Reg)
  // r 1 := r 2 − r 3
  function gen_sub(r1, r2, r3){
  	return "sub(" + r1 + ", " + r2 + ", " + r3 + ")";
  }

  // mul(r 1 : Reg, r 2 : Reg, r 3 : Reg)
  // r 1 := r 2 ∗ r 3
  function gen_mul(r1, r2, r3){
  	return "mul(" + r1 + ", " + r2 + ", " + r3 + ")";
  }

  // div(r 1 : Reg, r 2 : Reg, r 3 : Reg)
  // r 1 := r 2 div r 3
  function gen_div(r1, r2, r3){
  	return "div(" + r1 + ", " + r2 + ", " + r3 + ")";
  }

  // mod(r 1 : Reg, r 2 : Reg, r 3 : Reg)
  // r 1 := r 2 mod r 3
  function gen_mod(r1, r2, r3){
  	return "mod(" + r1 + ", " + r2 + ", " + r3 + ")";
  }

  // call(l : Label)
  // Guarda la dirección de retorno y el entorno local actual en la pila.
  // Crea un nuevo entorno local y salta a la locación de la etiqueta l.
  function gen_call(l){
  	return "call(" + l + ")";
  }

  // icall(r : Reg)
  // Similar a call, pero salta a la locación almacenada en r.
  function gen_icall(r){
  	return "icall(" + r + ")";
  }

  // return()
  // Restaura la dirección de retorno y el entorno local de la pila,
  // retornando a la posición del último call/icall.
  function gen_return(){
  	return "return()";
  }

  // CASOS DE TEST

  // Toma el input desde la carpeta test_codegen que tiene que estar en el directorio actual y
  // pone la salida en la carpeta test_outputs, que si no existe la crea
  function test(n){
    var input = fs.readFileSync("./test_codegen/test" + n + ".fl", "utf8");
    //return JSON.stringify(renombrarVariable(parser.parse(input)[0][2], "x", "wachin"));
    var instrucciones = compilarAST(parser.parse(input));
    if (!fs.existsSync("./test_outputs")) {
      fs.mkdirSync("./test_outputs");
    }
    crearArchivo("test_outputs/test" + n + ".mam", instrucciones.join("\n"));
    return instrucciones;
  }

  function testHasta(n){
  	for (var i = 1; i < n + 1; i++) {
  		if(i < 10){
  			test("0" + i);
  		}else{
  			test(i);
  		}
  		
  	}
  }

  function testConstructorAislado(){
    var input = "def constructor = Constructor";
    var instrucciones = compilarAST(parser.parse(input));
    crearArchivo("testConstructorAislado.mam", instrucciones.join("\n"));
    return instrucciones;
  }

  function testExprNumber(){
    var input = "def foo = 42";
    var instrucciones = compilarAST(parser.parse(input));
    crearArchivo("testExprNumber.mam", instrucciones.join("\n"));
    return instrucciones;
  }

  return {
    test01: (function(){ return test("01")}),
    test02: (function(){ return test("02")}),   
    testConstructorAislado: testConstructorAislado,
    testExprNumber: testExprNumber,
    test03: (function(){ return test("03")}),
    test04: (function(){ return test("04")}),
    test05: (function(){ return test("05")}),
    test06: (function(){ return test("06")}),
    test07: (function(){ return test("07")}),
    test08: (function(){ return test("08")}),    
    testHasta: testHasta,
    test09: (function(){ return test("09")}),
  };
})();
