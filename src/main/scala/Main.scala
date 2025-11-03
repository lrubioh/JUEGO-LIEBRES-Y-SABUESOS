import TableroClasicoLyS.{D1B, MA, MM}

import scala.*


object miPrograma extends App
//prueba para ver que el tablero se imprime
@main def runJuegoLiebreYSabuesos(): Unit =
  // Crear el estado inicial
  val estadoInicial = Estado(
    liebre = TableroClasicoLyS.posicionInicialLiebre,
    sabuesos = TableroClasicoLyS.posicionesInicialesSabuesos,
    turno = Jugador.Liebre
  )

  // Pintar el tablero inicial
  println("===ESTADO INICIAL DE LA PARTIDA===")
  TableroClasicoLyS.pintarTablero(estadoInicial)

//prueba para ver que funcionen bien los posibles movimientos
@main def probarMovimientoFicha(): Unit =
    // Estado inicial: liebre y sabuesos en sus posiciones de inicio
    val estadoInicial = Estado(
      liebre = TableroClasicoLyS.posicionInicialLiebre,
      sabuesos = TableroClasicoLyS.posicionesInicialesSabuesos,
      turno = Jugador.Sabuesos
    )

    // Calculamos los movimientos posibles de los sabuesos
    val movimientos = MovimientoSabueso.movimientosPosiblesPorSabueso(TableroClasicoLyS, estadoInicial)
    // calculamos los movimientos posibles de la liebre
    val movimientosliebre = MovimientoLiebre.movimientosPosibles(TableroClasicoLyS, estadoInicial )

    // Mostramos los resultados de los sabuesos
    println("=== MOVIMIENTOS POSIBLES DE LOS SABUESOS ===")
    movimientos.foreach { case (origen, destino) =>
      println(s"Sabueso en ${origen} puede moverse a ${destino}")
    }

    //Mostramos los resultados de la liebre como un set con todas las posibles casillas a las que moverse
    println("=== MOVIMIENTOS POSIBLES DE LA LIEBRE ===")
    println(movimientosliebre)

//PRUEBA DEL BUCLE DE JUEGO
//sin el modo de IA, se juega con los dos personajes
//@main def mainJuegoLiebreSabuesos(): Unit =
//  println(" *** JUEGO DE LA LIEBRE Y LOS SABUESOS *** ")
//
//  val turnoInicial = sortearTurno()
//  println(s"\n Empieza jugando: ${turnoInicial}")
//
//  val estadoInicial = Estado(
//    liebre = TableroClasicoLyS.posicionInicialLiebre,
//    sabuesos = TableroClasicoLyS.posicionesInicialesSabuesos,
//    turno = turnoInicial
//  )
//
//  val ganador = BucleJuego.bucleJuego(TableroClasicoLyS, estadoInicial)
//
//  println(s"\n Fin de la partida. El ganador es: ${ganador} ")

//PRUEBA DEL BUCLE DE JUEGO CON LA IA DE LA LIEBRE
    
@main def mainJuegoLiebreSabuesos(): Unit=
  println(" *** JUEGO DE LA LIEBRE Y LOS SABUESOS *** ")

  val turnoInicial = sortearTurno()
  println(s"\n Empieza jugando: ${turnoInicial}")

  val estadoInicial = Estado(
    liebre = TableroClasicoLyS.posicionInicialLiebre,
    sabuesos = TableroClasicoLyS.posicionesInicialesSabuesos,
    turno = turnoInicial
  )
  val modoIA = true //si o si juego con los sabuesos y la liebre juega "sola"
  val ganador = BucleJuego.bucleJuego(TableroClasicoLyS, estadoInicial, modoIA)

  println(s"\n Fin de la partida. El ganador es: ${ganador} ")

