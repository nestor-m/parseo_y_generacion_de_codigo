# Generador de código Flecha → Mamarracho

#### Dependencias

Node.js 0.10.0+

### Instalación

Se recomienda clonar el repositorio https://github.com/nestor-m/parseo_y_generacion_de_codigo para tener todo lo necesario para hacer las pruebas.

### Ejecución

Es necesario tener en el directorio actual los archivos code_generator.js, parser.js, y a.out. Tambien es necesaria la carpeta test_codegen con los casos de test. Para ejecutar los tests abrir una consola node en el directorio actual.

``` bash
# Entrar en consola de Node
$ node
# Instanciar el generador de codigo
$ var c = require("./code_generator.js")
# Ejecutar por ejemplo test01
$ c.test01()
# O ejecutar tests del 1 al 10
$ c.testHasta(10)
```

La ejecución de los tests genera los archivos .mam en la carpeta test_outputs. Los mismos se pueden ejecutar de la manera habitual

``` bash
# En el directorio actual
$ ./a.out test_outputs/test10.mam
```

