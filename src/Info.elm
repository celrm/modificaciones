module Info exposing (informacion)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Markdown
import Styles exposing (..)
import Types exposing (..)


informacion : Html Msg
informacion =
  Markdown.toHtml
    ( textStyle "20px"
    ++
    [ style "text-align" "justify"
    , style "width" "650px"
    , style "margin-left" "auto"
    , style "margin-right" "auto"
    ]) texto


texto =
    """

# Más información:

## Instrucciones.

  1. Antes de subir cualquier partitura, ésta se debe abrir en el programa _Musescore_ y guardar como _Archivo Musescore sin Comprimir_ (&ast;.mscx), que no es la opción predeterminada, sino la segunda.

  2. Después, se podrá hacer click en el botón __Examinar__ para buscar las partituras, o bien arrastrarlas y soltarlas en el recuadro rayado. Se pueden modificar varias partituras a la vez. Una vez subidas, se pueden descargar tantas modificaciones de ellas como se quiera.

  3. Se escoge entonces la escala deseada mediante los selectores y/o los cuadros de texto, además del nombre de la escala, si procede.

  4. Por último, se descargan los nuevos archivos mediante el botón __Descargar__, con el nombre original de cada partitura concatenado al nombre de la escala.

## ¿Cómo se modifican las partituras?

  La altura de cada nota se modifica independientemente según la escala escogida. Se puede escoger una escala predefinida o definir una propia. El primer recuadro indica a qué nota se envía cada Do de la partitura; el segundo, adónde se envía cada Do#; el tercero para el Re, etc.

  Por ejemplo, la escala cromática `0 1 2 3 4 5 6 7 8 9 10 11` manda cada nota 0 (Do) al 0, cada nota 1 (Do#) al 1, y así sucesivamente; es decir, es la función identidad. En cambio, la escala `0 0 0 0 0 0 0 0 0 0 0 0` envía cada nota al Do más cercano a ella inferiormente.

  Se pueden usar todo tipo de números enteros: positivos o negativos, y de cualquier tamaño. La escala `12 13 14 15 16 17 18 19 20 21 22 23` enviaría cada nota a su octava superior; la escala `-12 -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1` enviaría cada nota a su octava inferior.

  Al definir una escala hay que tener en cuenta que no solo se escogen las notas que aparecerán en ella, sino también cuántas veces estarán éstas repetidas y de qué manera se distribuyen por la partitura. Aquí la definición de _escala_ no es la usual. Es, más bien, una función transformativa definida directamente para todas sus entradas.

## ¿Por qué este convenio? ¿Por qué no el natural?

  El convenio más natural para modificar cada nota sería indicar cuántos semitonos se le quiere sumar o restar. Las series de números no serían entonces escalas, sino instrucciones individuales.

  Aunque cada nota se sigue modificando individualmente, el sentido de la modificación debe ser siempre global. Algo como `0 -1 0 -1 0 -1 -1 0 -1 0 -1 +1` no da apenas información sobre lo que se está modificando. De hecho, un mero intercambio entre dos números haría que la escala resultante fuese totalmente distinta, con otra sonoridad. Sería un gran cambio en la nueva pieza sin ser un gran cambio en la serie de números.

  En cambio, aporta más perspectiva y significado ver las modificaciones como parte de un todo, con visión de conjunto. Por un lado, se aprecia cuántas veces aparece cada nota y de cuántas notas estará compuesta la partitura. Por otro, se ve más fácilmente si una escala es conocida o no lo es. La serie de notas anterior formaba la escala Pentatónica, pero no se podía averiguar a simple vista sin calcular el otro formato.

## ¿Cuáles son las escalas predefinidas?

  La escala __Pentatónica__ es la pentatónica mayor: Do Re Mi Sol La (Do).

    0 0 2 2 4 4 7 7 7 9 9 12

  La escala __Hexatónica__ es la escala de tonos enteros: Do Re Mi Fa# Sol# La#.

    0 0 2 2 4 4 6 6 8 8 10 10

  La escala __Heptatónica__ es el modo jónico en Do, la escala diatónica en Do, o Do Mayor: Do Re Mi Fa Sol La Si.

    0 0 2 2 4 5 5 7 7 9 9 11

## ¿Qué formatos se pueden subir?

  Solamente se puede aplicar el algoritmo a los _Archivos Musescore sin Comprimir_ (&ast;.mscx). Los _Archivos Musescore_ (&ast;.mscz) están comprimidos, por lo que no se pueden editar directamente. La manera de descomprimirlos es mediante su propio software.

  No son aceptados otros formatos de otros programas de notación musical, porque no deseo apoyar software restrictivo. Aquel que desee exportar a otros formatos deberá hacerlo mediante _Musescore_.

## ¿Para qué modificar las notas de una partitura?

  En el comienzo de mi trabajo sobre dodecafonismo decidí experimentar con la idea del serialismo, despojándolo de la disonancia pero manteniendo la estructura matemática de las series y sus transformaciones.

  Ya que la disonancia suele provenir del cromatismo, se me ocurrió serializar con otras escalas que no fueran cromáticas, o que tuvieran un menor número de intervalos de semitono. La Pentatónica, la Hexatónica y la Heptatónica fueron mis tres intentos.

  Tenía dos opciones: o componer una nueva obra _enefonista_ por mi cuenta, o modificar una obra dodecafónica con una estructura ya ideada por otro compositor. Lo segundo resultaba más sencillo, pero la primera vez tuve que modificar a mano la partitura, nota a nota.

  Esta herramienta evita ese trabajo tedioso y mecánico, pero también sirve para otros propósitos. Por ejemplo, sirve para cambiar una partitura de mayor a menor, o viceversa.

"""
