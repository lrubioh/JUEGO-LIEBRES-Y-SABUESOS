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
      val adyacentes = tablero.movimientosDesde(estado.liebre)
      adyacentes.diff(estado.ocupadas)
      
  case object MovimientoSabueso extends MovimientoFicha:
    override def movimientosPosibles(tablero: TableroJuego, estado: Estado): Set[Posicion] =
      //lo primero a tener en cuenta es que los sabuesos solo pueden avanzar(hacia la derecha) de tal forma que 
      //debo eliminar de la lista de adyacencias todas las posiciones que esten mas a la izquiera, ademas de 
      //todas las casillas ya ocupadas, con esto ya tendria la lista de posiciones a las que puede avanzar,
      //lo debo hacer para cada uno de los sabuesos, hago una variacion del metodo principal.
      def movimientosPosiblesPorSabueso(tab: TableroJuego, est: Estado): Set[(Posicion, Posicion)] =
        
        
        
      
      

