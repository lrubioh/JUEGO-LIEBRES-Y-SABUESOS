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
    //un ejemplo sería: sabueso 1 tiene dos opciones de movimientos:(I1M, MM),(I1M, MB),
    // sabueso 2 tiene una opcion de movimiento: (MM, D1M) y por último sabueso 3 tiene dos opciones de
    // movimientos:(D1M, D2M), (D1M, D1B), todos estos dentro de un set

    //creo un "acumulador" en el que ir guardando los movimientos
    var resultado = Set.empty[(Posicion, Posicion)]
    val ocupadas= estado.ocupadas //casillas ocupadas por la liebre y los demas sabuesos
    //voy a ir recorriendo cada sabueso y guardando su posicion junto con sus posibles movimientos
    for sabueso <- estado.sabuesos do //sabueso es la posicion del sabueso que estoy recorriendo
      val adyacentes = tablero.movimientosDesde(sabueso)
      //en vez de eliminar las posiciones a la izquierda voy a guardar las de la derecha:
      val derecha = adyacentes.filter(_.x >= sabueso.x)
      //quito las posiciones ocupadas:
      val movimientosFinales = derecha.diff(ocupadas)

      for destino <- movimientosFinales do
        resultado = resultado + ((sabueso, destino))

    resultado //devuelvo los pares finales de movimientos posibles


