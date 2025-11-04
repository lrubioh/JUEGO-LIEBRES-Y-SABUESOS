//tengo una interfaz que define el comportamiento general de la ficha, luego en las case
//object de liebre y sabuesos especifico los movimientos posibles de casa uno de ellos
sealed trait MovimientoFicha:
  //metodo general
  def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion]

  //defino el metodo concreto para cada una de las object class
case object MovimientoLiebre extends MovimientoFicha:
    //la liebre puede moverse a cualquier casilla libre de todas las que haya en su lista
    //de adyacencias, para definir sus movimientos posibles necesito tener su lista de adyacencias
    //y a esas restarle las casillas ocupadas, las casillas que queden en la lista son todos los
    //movimientos posibles de la liebre
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
  //las casillas adyacentes todas aquellas que me devuelve la funcion movimientosDesde, donde
  //estado.liebre me devuelve la casilla donde esta la liebre por ejemplo D2M donde se me devolveria
  //como casillas posibles D1A, D1M, D1B
    val adyacentes = tablero.movimientosDesde(estado.liebre)
    //a todas esas casillas les quito todas aquellas que esten ocupadasy ya tengo todos los movimientos posibles de la liebre
    adyacentes.diff(estado.ocupadas)

case object MovimientoSabueso extends MovimientoFicha:
  override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
    //lo primero a tener en cuenta es que los sabuesos solo pueden avanzar(hacia la derecha) de tal forma que
    //debo eliminar de la lista de adyacencias todas las posiciones que esten mas a la izquiera, ademas de
    //todas las casillas ya ocupadas, con esto ya tendria la lista de posiciones a las que puede avanzar,
    //lo debo hacer para cada uno de los sabuesos, hago una variacion del metodo principal.
    movimientosPosiblesPorSabueso(tablero, estado).map(_._2)


  def movimientosPosiblesPorSabueso(tablero: TableroJuego, estado: Estado): Set[(Posicion, Posicion)]=
    //me va a devolver por cada sabueso una lista con sus posibles movimientos, para saber de que sabueso se trata
    //esto lo va a hacer de forma que el primer elemento de la tupla sea la posicion inicial(me ayuda a identificar
    //además de que sabueso se trata) y el segundo una de las posiciones a las que se puede mover, habra tantos
    //pares como estos como movimientos posibles
    //los indices se ponen en bucle juego
    //un ejemplo sería:
    //1 -> Sabueso en D1.M se mueve a D1.B
    //2 -> Sabueso en I1.A se mueve a M.A
    //3 -> Sabueso en D1.M se mueve a D2.M

    //guardo las casillas ocupadas para quitar esas de los movimientos posibles
    val ocupadas = estado.ocupadas
    //necesito una funcion auxiliar recursiva para ir guardando en un set posicion las posibilidades
    def recorrerSabuesos(sabrestantes: List[Posicion], acum : Set[(Posicion,Posicion)]): Set[(Posicion,Posicion)]= sabrestantes match
      //si no me quedan sabuesos por recorrer en mi lista devuelvo el acum que es el Set con las posiciones iniciales y finales(a donde se mueven)
      case Nil => acum
      //en caso de que tenga un sabueso en la cabeza de sabrestantes y luego la cola guardo las posibles posiciones para ese sabueso
      case sabueso :: cola =>
        // busco todos los movimientos posibles (todas las casillas adyacentes a la posicion del sabueso
        val adyacentes = tablero.movimientosDesde(sabueso)
        //como el sabueso no puede retroceder guardo solo aquellas casillas de la lista de adyacencias que esten mas a la derecha o en la misma columna
        //estoy filtrando que la coordenada x de las adyacentes sea mayor o igual que la coord x de la posicion del sabueso
        val derechaoigual = adyacentes.filter(_.x >= sabueso.x)
        //quito las posiciones ocupadas:
        val movimientosFinales = derechaoigual.diff(ocupadas)
        //creo las tuplas con (posicionsabueso, destino) destino seran las posiciones posibles tras aplicar las restricciones (que no este ocupada,
        //y que no retroceda)
        val tuplasmovimientos = movimientosFinales.map(destino => (sabueso, destino))

        recorrerSabuesos(cola , acum ++ (tuplasmovimientos))

      recorrerSabuesos(estado.sabuesos.toList, Set.empty)

//ademas de imprimir los movimientos con los indices la voy a aprovechar para que me devuelva un Map con el incice asociado a la tupla de posiciones
def indicesMovimientosSabuesos(movimientos: Set[(Posicion,Posicion)]): Map[Int, (Posicion, Posicion)] =
  //me va a poner indice a cada una de las tuplas del set
  //necesito una funcion auxiliar recursiva
  def indicesAux(lista:List[(Posicion,Posicion)], indice:Int,mapa: Map[Int, (Posicion, Posicion)]) : Map[Int, (Posicion, Posicion)]= lista match
    case Nil => mapa
    case (origen, destino) :: cola =>
      println(s" $indice -> Sabueso en $origen se mueve a $destino")
      //llamada recursiva que vaya acumulando en el mapa las tuplas
      indicesAux(cola,indice+1, mapa + (indice -> (origen,destino)))
      
    //inicializo la fAux con indice 1 y mapa vacio
    indicesAux(movimientos.toList , 1, Map.empty)

//ademas de imprimir los movimientos con los indices la voy a aprovechar para que me devuelva un Map con el incice asociado a la tupla de posiciones
def indicesMovimientosLiebre(movimientos: Set[Posicion]): Map[Int, Posicion] =
  //me va a poner indice a cada una de las tuplas del set
  //necesito una funcion auxiliar recursiva
  def indicesAux(lista:List[Posicion], indice:Int,mapa: Map[Int, Posicion]) : Map[Int,Posicion ]= lista match
    case Nil => mapa
    case posicionfinal :: cola =>
      println(s" $indice -> Liebre se mueve a $posicionfinal")
      //llamada recursiva que vaya acumulando en el mapa las tuplas
      indicesAux(cola,indice+1, mapa + (indice -> posicionfinal))

  //inicializo la fAux con indice 1 y mapa vacio
  indicesAux(movimientos.toList , 1, Map.empty)




