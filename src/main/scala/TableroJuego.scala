//hemos creado un tablero general para que se puedan implementar distintos tableros
//en nuestro propio lo crearemos extendiendo este
trait TableroJuego:

  // --- Devuelve las posiciones accesibles desde una posición ---
  def movimientosDesde(p: Posicion): Set[Posicion]

  // --- Posiciones iniciales de la liebre y sabuesos ---
  def posicionInicialLiebre: Posicion

  def posicionesInicialesSabuesos: Set[Posicion]

  def posicionMetaLiebre: Posicion

  // --- Pinta el tablero para un estado dado
  def pintarTablero(estado: Estado): Unit

  // --- Comprueba si ha terminado la partida ---
  def esFinPartida(estado: Estado): Option[Jugador]
//luego con un override vemos si estado.liebre= I2M o si la liebre no se puede
//mover, los sabuesos la han acorralado

