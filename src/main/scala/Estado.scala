import scala.util.Random
//Random.nextBoolean() devuelve true o false, true -> empieza la liebre, false los sabuesos

def sortearTurno(): Jugador =
  if Random.nextBoolean() then Jugador.Liebre else Jugador.Sabuesos

//guarda donde estan los sabuesos, la liebre y a quien le toca jugar
case class Estado(liebre: Posicion, sabuesos: Set[Posicion], turno: Jugador):
  //metodo que devuelva las casillas ocupadas
  def ocupadas: Set[Posicion] = sabuesos + liebre
//con estado.liebre me devuelve la casilla de la liebre y con estado.sabuesos
//me devuelve las casillas de los sabuesos, no necesito separarlo

object Estado:
  // companion object para construir un estado
  def apply(liebre: Posicion, sabuesos: Set[Posicion], turno: Jugador): Estado =
    new Estado(liebre, sabuesos, turno)
