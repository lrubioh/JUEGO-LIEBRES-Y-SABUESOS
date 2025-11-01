//solo almacena la posicion de las fichas no tiene como tal el estado de la partida

object TableroClasicoLyS extends TableroJuego:
  
  val I1A = Posicion(Columna.I1, Fila.A)
  val MA = Posicion(Columna.M, Fila.A)
  val D1A = Posicion(Columna.D1, Fila.A)


  val I2M = Posicion(Columna.I2, Fila.M)
  val I1M = Posicion(Columna.I1, Fila.M)
  val MM = Posicion(Columna.M, Fila.M)
  val D1M = Posicion(Columna.D1, Fila.M)
  val D2M = Posicion(Columna.D2, Fila.M)


  val I1B = Posicion(Columna.I1, Fila.B)
  val MB = Posicion(Columna.M, Fila.B)
  val D1B = Posicion(Columna.D1, Fila.B)
  
//hacemos un objeto que extienda nuestra clase tablero y haga concretamente nuestro tablero

  // Lista de adyacencias (grafo)
  private val grafo: Map[Posicion, Set[Posicion]] = Map(
    I1A -> Set(MA, I1M),
    MA -> Set(I1A, I1M, MM, D1A),
    D1A -> Set(MA, D1M),


    I2M -> Set(I1M),
    I1M -> Set(I2M, I1A, MA, MM, I1B),
    MM -> Set(I1M, MA, D1M, MB),
    D1M -> Set(MM, D1A, D2M, D1B),
    D2M -> Set(D1M),


    I1B -> Set(I1M, MB),
    MB -> Set(I1B, MM, D1B),
    D1B -> Set(MB, D1M)
  )

  //tengo que implementar
  //me dice las casillas posibles a las que me puedo mover
  override def movimientosDesde(p: Posicion): Set[Posicion] =
    grafo.getOrElse(p, Set.empty)

  override def posicionInicialLiebre: Posicion = D2M

  override def posicionesInicialesSabuesos: Set[Posicion] = Set(I1A, I2M, I1B)

  override def posicionMetaLiebre: Posicion = I2M

  // --- Pintado ---
  private def pintarNodo(p: Posicion, estado: Estado): String =
    val RESET = "\u001B[0m"
    val ROJO = "\u001B[31m"
    val AZUL = "\u001B[34m"
    val BLANCO = "\u001B[37m"
  
    if (estado.liebre == p) s"${ROJO}L${RESET}"
    else if (estado.sabuesos.contains(p)) s"${AZUL}S${RESET}"
    else s"${BLANCO}o${RESET}"
  
  override def pintarTablero(estado: Estado): Unit =
    val s = (p: Posicion) => pintarNodo(p, estado)
    println(s"        ${s(I1A)}-----${s(MA)}-----${s(D1A)}")
    println("     ╱  |  \\  |  /  | \\")
    println(s"    ${s(I2M)}---${s(I1M)}-----${s(MM)}-----${s(D1M)}---${s(D2M)}")
    println("     \\  |  /  |  \\  |  /")
    println(s"        ${s(I1B)}-----${s(MB)}-----${s(D1B)}")

  override def esFinPartida(estado: Estado): Option[Jugador] = None 

