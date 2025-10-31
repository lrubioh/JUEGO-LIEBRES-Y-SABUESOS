import scala.*

object miPrograma extends App
@main def runJuegoLiebreYSabuesos(): Unit =
  // Crear el estado inicial
  val estadoInicial = Estado(
    liebre = TableroClasicoLyS.posicionInicialLiebre,
    sabuesos = TableroClasicoLyS.posicionesInicialesSabuesos,
    turno = Jugador.Liebre
  )

  // Pintar el tablero inicial
  println("Estado inicial del juego:\n")
  TableroClasicoLyS.pintarTablero(estadoInicial)