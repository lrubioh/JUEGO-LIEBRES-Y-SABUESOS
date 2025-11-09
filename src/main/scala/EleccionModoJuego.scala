object EleccionModoJuego:
  //creamos una funcion que pregunte al usuario que modo de juego quiere y comience el bucle de juego con el modo elegido
  def modoJuego(): Unit =
    //primero muestra por pantalla este mensaje tras el usuario responder que si que quiere jugar
    println("\n ¡GENIAL!, elige un modo de juego:  \n ")
    //imprime todas las opciones de modo de juego que hay para que el usuario introduzca el numero de su eleccion
    println("1 -> Jugar con los sabuesos, Liebre IA")
    println("2 -> Jugar con la liebre, sabuesos IA")
    println("3 -> Jugar con ambos(modo 2 jugadores)")
    println("4 -> Liebre y Sabuesos IA")

    val eleccion = scala.io.StdIn.readLine("\nIntroduce el número del modo que deseas jugar: ").toInt

    println("\n Se ha configurado correctamente el juego con su eleccion ")
    println("\n*** COMIENZA EL JUEGO DE LA LIEBRE Y LOS SABUESOS *** \n")

    //quiero que cuando ya haya seleccionado y comience el juego me diga con que personaje estoy jugando:
    eleccion match
      case 1 =>
        println("Vas a jugar con los sabuesos")
      case 2 =>
        println("Vas a jugar con la liebre")
      case 3 =>
        println("Vas a jugar con ambos")
      case 4 =>
        println("Vas a ver la partida IALiebres VS IASabuesos")

    //sorteo el turno y creo el estado inicial de la partida, lo necesito para inicializar el bucle de juego
    val turnoInicial = sortearTurno()
    println(s"\nEmpieza jugando: ${turnoInicial} \n")

    //en modo juego tengo guardado el numero seleccionado, voy a crear el bucle de juego en funcion al modo seleccionado,
    //pero primero debo establecer el estado inicial del juego
    val estadoInicial = Estado(
      liebre = TableroClasicoLyS.posicionInicialLiebre,
      sabuesos = TableroClasicoLyS.posicionesInicialesSabuesos,
      turno = turnoInicial,
      movimientosLiebre = 0
    )

    //para cada uno de los 4 posibles modos creo su bucle de juego
    eleccion match
      case 1 =>
        //CASO 1 -> JUEGA CON LOS SABUESOS Y LA LIEBRE ES IA
        val modoIA: Set[Jugador] = Set(Jugador.Liebre)
        BucleJuego.bucleJuego(TableroClasicoLyS, estadoInicial, modoIA)

      case 2 =>
        //CASO 2 -> JUEGA CON LA LIEBRE Y LOS SABUESOS SON IA
        val modoIA: Set[Jugador] = Set(Jugador.Sabuesos)
        BucleJuego.bucleJuego(TableroClasicoLyS, estadoInicial, modoIA)

      case 3 =>
        //CASO 3 -> JUGAN DOS JUGADORES CON AMBOS PERSONAJES
        val modoIA: Set[Jugador] = Set()
        BucleJuego.bucleJuego(TableroClasicoLyS, estadoInicial, modoIA)

      case 4 =>
        //AMBOS PERSONAJES SON IA
        val modoIA: Set[Jugador] = Set(Jugador.Liebre, Jugador.Sabuesos)
        BucleJuego.bucleJuego(TableroClasicoLyS, estadoInicial, modoIA)

