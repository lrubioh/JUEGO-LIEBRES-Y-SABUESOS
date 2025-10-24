//representamos el tablero como un grado donde cada nodo tiene una posición
// en función a su fila y su columna
//tenemos 3 filas: A, M, B (la A arriba, M en el medio y la B abajo)

//defino las filas, columnas y sus posiciones posibles
enum Fila(val v: Int):
  case A extends Fila(1)
  case M extends Fila(0)
  case B extends Fila(-1)
//tanto en fila como en columna considero el centro el (0,0)
enum Columna(val v: Int):
  case I2 extends Columna(-2)
  case I1 extends Columna(-1)
  case M extends Columna(0)
  case D1 extends Columna(1)
  case D2 extends Columna(2)


//para representar las casillas necesitas crear la clase posicion
//esta debe ser una case class para poder comparar dos posiciones directamente con ==
case class Posicion(col: Columna, fila: Fila):
  def x: Int = col.v
  def y: Int = fila.v
  //con el override consigo que se imprima por ejemplo M.A como posición
  override def toString : String = s"${col}.${fila}"


//hemos creado un tablero general para que se puedan implementar distintos tableros
"entiendo que hay que rellenar las definiciones"
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

"cosas que ha puesto chatgpt que vamos a necesitar luego"
enum Jugador:
  case Liebre, Sabuesos


case class Estado(liebre: Posicion, sabuesos: Set[Posicion], turno: Jugador):
  def ocupadas: Set[Posicion] = sabuesos + liebre


object Estado:
  // helper para construir un estado (se puede redefinir más adelante)
  def apply(liebre: Posicion, sabuesos: Set[Posicion], turno: Jugador): Estado =
    new Estado(liebre, sabuesos, turno)


"esto ya es mio/profe"
//hacemos un objeto que extienda nuestra clase tablero y haga concretamente nuestro tablero
//solo almacena la posicion de las fichas no tiene como tal el estado de la partida

object TableroClasicoLyS extends TableroJuego:
  "relleno por chatgpt"
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

  //tengo que implementar 
  override def movimientosDesde(p: Posicion): Set[Posicion] = ???

  override def posicionInicialLiebre: Posicion = ???

  override def posicionesInicialesSabuesos: Set[Posicion] = ???

  override def posicionMetaLiebre: Posicion = ???

  override def pintarTablero(estado: Estado): Unit = ???

  override def esFinPartida(estado: Estado): Option[Jugador] = ???


