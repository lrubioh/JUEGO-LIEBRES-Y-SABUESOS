object IASabuesos:
  //Para la IA de los sabuesos podemos utilizar como base la IA de la liebre, de tal forma que usaremos una tupla con la que evaluar los movimientos
  //a diferencia de la IA de la liebre en esta tupla el mejor movimiento sera aquel con menor valor
  def evaluarMovimientoSabuesos(tablero: TableroJuego, estado:Estado, movimientos:Set[(Posicion,Posicion)]): Map[Posicion,(Int,Int)] =
    // esta funcion evaluara cada uno de los movimiento con una tupla de tal forma que cuanto menor sea el valor mejor sera el
    // movimiento, el primer elemento de la tupla sera el numero de sabuesos rebasados (entendiendo por rebasados tambien que
    // esten en la misma columna que la liebre) y el segundo elemento sera el numero de posiciones posibles que tendra la liebre
    // cuando el sabueso se mueva a la posicion destino, al igual que el primer elemento cuanto menor sea su valor mejor movimiento sera
    //devolvera un map donde el indice es la posicion de destino y contiene las tuplas para cada uno de los movimientos

    //primero convierto los Set de movimientos en una lista para poder guardar por separado el origen y el destino
    val listaMov = movimientos.toList
    val origen= listaMov(0)
    val destino = listaMov(1)

    //guardo el nuevo estado de los sabuesos con los dos que no se mueven tal cual y quitandole el origen del que se mueve y sumandole el destino
    val nuevoEstadoSabuesos = estado.sabuesos -origen + destino

    //para guardar el primer elemento de la tupla debo contar el numero de sabuesos rebasados en las nuevas posiciones (en el nuevo estado)

    val sabuesosrebasados= nuevoEstadoSabuesos.foldLeft(0)((acum, sabuesos)=> if estado.liebre.x <= sabuesos.x then acum +1 else acum)

    //ya tenemos calculado el primer elemento de la tupla, ahora debo calcular el segundo para luego guardarlos

    //ahora hay que calcular los movimientos posibles de la liebre con el nuevo estado de los sabuesos
    //necesito crear el nuevo estado de la partida para con el ver los mov de la liebre
    val nuevoestado=Estado(
      liebre = TableroClasicoLyS.posicionInicialLiebre,
      sabuesos = TableroClasicoLyS.posicionesInicialesSabuesos,
      turno = estado.turno
    )
    //movimientos liebre es un Set[Posicion]
    val movimientosLiebre = MovimientoLiebre.movimientosPosibles(tablero, nuevoestado)
    //para saber si longitud (segundo elemento de la tupla) lo convertire en lista y vere la longitud de esta
    val nummovLiebre=movimientosLiebre.toList.length
    //devuelvo la tupla
    (sabuesosrebasados,nummovLiebre)
    //creo y devuelvo un mapa para facilitar luego la eleccion de movimientos
    val mapaEvaluaciones= Map()

  // ahora con el mapa que tengo en que los indices son los destinos y el "contenido" son las tuplas que hace la funcion evaluarMovimientoSabuesos
  //esta lo que hara sera buscar el elemento que tenga el menor valor en el primer valor de la tupla, en caso de haber mas de uno
  //con el mismo primer valor guardare ambos en una lista y usare la funcion desempate
  def encontrarMejorMovimiento(mapa: Map[Posicion,(Int,Int)]):List[(Posicion,(Int,Int))]=
    //hago una funcion recursiva que recorra el mapa lista por lista y en una nueva lista(mejores) guarde los mejores elementos de movimientos
    def recorrerMapa(movimientos: List[(Posicion, (Int, Int))], valorMin: Int, mejores: List[(Posicion, (Int, Int))]): List[(Posicion, (Int, Int))] = movimientos match
      case Nil => mejores
      case (posicion,tupla):: cola =>
        //quiero la tupla con menor valor en su primer elemento
        val primerelementotupla = tupla._1
        if primerelementotupla < valorMin then
          //si el primer elemento es menor que el valorMin(sera el Int.ManValue, el mayor entero posible) entonces guardo esa tupla en mejores
          //(si hubiese otra tendria menor valor con lo que la sustituyo) y vuelvo a llamar a la funcion recorrer con el resto de la tupla
          recorrerMapa(cola, primerelementotupla,List((posicion,tupla)))

        else if primerelementotupla == valorMin then
          //si el primer elemento es igual que uno que ya habia no lo sustituyo sino que lo añado
          recorrerMapa(cola, primerelementotupla,(posicion,tupla)::mejores)

        else
          //si no es menor ni igual no hago nada con el, sigo recorriendo el mapa con el resto y con valorMin
          recorrerMapa(cola, valorMin,mejores)

    //inicializo la funcion recorrer mapa

    recorrerMapa(mapa.toList, Int.MaxValue, List())

  //en caso de que encontrarMejorMovimiento me devuelva una lista con mas de un posible movimiento necesito una funcion que me ayude a
  //quedarme solo con uno, esta funcion va a devolver SI o SI una lista con un solo elemento, es decir un solo destino posible
  //ya que desempate va a mirar de entre las tuplas que me ha devuelto encontrarMejorMovimiento, todas tendran el mismo primer valor,
  //la funcion desempate me va a devolver aquella que tenga menor segundo valor, solo me ha devolver una ya que en caso de haber dos
  //con el mismo segundo elemento serian exactamente iguales con lo que daria igual cual coger, me quedo con la que primero evalue

  //le paso la lista que me devuelve encontrarMejoresMovimientos y me devuelve una lista con un solo elemento (Posicion, (Int, Int)), me dice a que pos mover el sabueso
  def desempate(listamejoresmovimientos: List[(Posicion,(Int,Int))]) :List[(Posicion, (Int, Int))] =
    def recorrerdesempate( lista:List[(Posicion, (Int, Int))], valorMin: Int, mejor: List[(Posicion, (Int, Int))]): List[(Posicion, (Int, Int))] = lista match
      case Nil => mejor
      case (posicion,tupla)::cola =>
        //voy a mirar si ese primer elemento es menor que el valor min (lo guardo/sustituyo) o si es mayor(no hago nada con el)
        val segundoelementotupla= tupla._2
        if segundoelementotupla< valorMin then
          recorrerdesempate(cola,segundoelementotupla,List((posicion,tupla)))

        else
          recorrerdesempate(cola, valorMin,mejor)

    //incicializo recorrerdesempate
    recorrerdesempate(listamejoresmovimientos,Int.MaxValue, List())

  //para implementar el como elegir cual es el mejor movimiento en el bucle de juego y que sea mas sencilla creamos una funcion
  //que use las dos ultimas implementaciones para decidir cual es el mejor movimiento y quu devuelva cual es el destino final
  def eleccionMejorMovimiento(movimientos:Set[Posicion],tablero:TableroJuego,estado: Estado):Posicion=
    

