import Jugador.Liebre

object BucleJuego:
  def bucleJuego(tablero: TableroJuego, estado: Estado, modoIA: Set[Jugador]): Jugador=
    //1 pintar tablero
    println("\n")
    tablero.pintarTablero(estado)
    
    //el siguiente paso sería calcula los movimientos posibles del jugador al que
    // le toca el turno, pero para eso tengo que comprobar si la partida ha acabado
    //o continua, si ha acabado no le toca turno a nadie
    tablero.esFinPartida(estado) match
      //en el caso de que haya ganador imprimo el mensaje del ganador
      case Some(ganador) =>
        println ("*********************** EL GANADOR ES  *******************")
        println(s"*                         $ganador                       *  ")
        println(" **********************************************************")
    
        //quiero que cuando la partida acabe me de la opcion de jugar otra partida, si la respuesta es que no se acaba el juego
        //si la respuesta es que si le vuelvo a dar a elegir el modo de juego
        val Jugarotravez= scala.io.StdIn.readLine("\n¿quieres jugar otra partida? : (1 -> si , 2-> no) :  ").toInt
        if Jugarotravez == 1 then
          EleccionModoJuego.modoJuego()
        ganador



      //en caso de que nadie haya ganado imprimo los posibles movimientos del
      //jugador al que le toca el turno
      case None =>
        if estado.turno == Jugador.Liebre then
          //si es el turno de la liebre primero miro el contador, si lleva ya 25 mov entonces ha ganado
          if estado.movimientosLiebre >=1 then
            println("\n*** La liebre ha sobrevivido 25 turnos. ¡Gana la liebre! ***")
            Jugador.Liebre
          //si lleva menos movimientos, sigue jugando con normalidad
          else
            //calculo los movimientos posibles de la liebre
            val movimientos = MovimientoLiebre.movimientosPosibles(tablero, estado)

            if  modoIA(Jugador.Liebre)==true then

              println (s"\n Es el turno de la liebre \n")
              /**MODO IA -> LA LIEBRE SE MUEVE SOLA A LA POSICION MAS OPTIMA PARA GANAR**/
              //quiero evaluar los movimientos en funcion a la tupla de evaluar movimientos
              //movimientos es un Set[Posicion] que no sabemos cuantos elementos tiene, pero necesito acceder a cada uno de ellos
              //para evaluarlos y luego quedarme con el que tenga el primer elemento mayor, en caso de haber dos iguales
              //me quedo con el que tenga el mayor en el segundo elemento
              val destino = IALiebre.eleccionMejorMovimiento(movimientos,estado, tablero)

              //establezco el nuevo estado de la partida con la liebre ya en su nueva posicion y vuelvo a llamar al bucleJuego
              val nuevoEstado = Estado(
                liebre = destino,
                sabuesos = estado.sabuesos,
                turno = Jugador.Sabuesos,
                movimientosLiebre = estado.movimientosLiebre + 1
              )

              bucleJuego(tablero,nuevoEstado, modoIA)

            else
               /**FUNCIONAMIENTO NORMAL, IMPRIME LOS MOVIMIENTOS Y SE ELIGEN POR NUMERO**/
              //imprimo los movimientos posibles de la liebre
              println(s"\n Turno de la Liebre: ")

              //imprimo el tablero con las posiciones para cuando se de a elegir se vea cuales son
              println(s"\n Las posiciones del tablero son:\n ")
              println(s"            I1A------MA-----D1A  ")
              println("          ╱   |  \\   |  /    |   \\")
              println(s"      I2M---I1M------MM-----D1M---D2M  ")
              println("         \\   |   /   |  \\   |  / ")
              println(s"            I1B------MB-----D1B   \n")

              println(s"\n Los posibles movimientos de la liebre son: \n")

              val opciones= indicesMovimientosLiebre(MovimientoLiebre.movimientosPosibles(tablero,estado))

              //ahora hay que pedir al jugador el numero que esta asociado al movimiento que quiere hacer
              val eleccion = scala.io.StdIn.readLine("\n Elige movimiento:").toInt
              //guardo en destino el valor asociado al numero elegido (accedo a posicion final atraves de el)
              val destino = opciones(eleccion)

              //ya tenemos el movimiento guardado, ahora creamos un nuevo estado con la nueva casilla de la liebre,
              //el cambio de turno al proximo jugador(sabuesos) y las casillas de los sabuesos que se habran mantenido
              val nuevoEstado = Estado(
                liebre = destino,
                sabuesos = estado.sabuesos,
                turno = Jugador.Sabuesos,
                movimientosLiebre= estado.movimientosLiebre + 1
              )

              // vuelvo a llamar al bucle de juego con su nuevo estado para continuar la partida
              bucleJuego(tablero, nuevoEstado, modoIA)

        // ahora hago el mismo proceso pero con los sabuesos
        else
          if modoIA(Jugador.Sabuesos)==true then
            println(s" \n Turno de los sabuesos: ")
            //primero calculo los movimientos posibles de los sabuesos
            val movimientosSabuesos= MovimientoSabueso.movimientosPosiblesPorSabueso(tablero,estado)
            //con la funcion eleccionMejorMovimiento me devuelve una tupla (origen,destino) ya habiendo calculado
            //cual es el mejor movimiento de todos
            val (origen,destino)= IASabuesos.eleccionMejorMovimiento(movimientosSabuesos,tablero,estado)
            val nuevoestadosabuesos= estado.sabuesos-origen+destino
            val nuevoEstado= Estado(
              liebre = estado.liebre,
              sabuesos = nuevoestadosabuesos,
              turno = Jugador.Liebre,
              movimientosLiebre = estado.movimientosLiebre
            )
            //vuelvo a llamar al bucle de juego con el mismo tablero, el nuevo estado(el sabueso movido) y el mismo modoIA
            bucleJuego(tablero,nuevoEstado,modoIA)

          else
            /**MODO HUMANO , IA == FALSE **/
            //imprimo el tablero con las posiciones para cuando se de a elegir se vea cuales son
            println(s"\n Las posiciones del tablero son:\n ")
            println(s"            I1A------MA-----D1A  ")
            println("          ╱   |  \\   |  /    |   \\")
            println(s"      I2M---I1M------MM-----D1M---D2M  ")
            println("         \\   |   /   |  \\   |  / ")
            println(s"            I1B------MB-----D1B   ")
            val movimientos = MovimientoSabueso.movimientosPosiblesPorSabueso(tablero, estado)

            println(s" \n Turno de los sabuesos: ")
            val opciones = indicesMovimientosSabuesos(movimientos)

            val eleccion = scala.io.StdIn.readLine("Elige movimiento (introduciendo el numero) :").toInt
            val (origen, destino) = opciones(eleccion)
            val nuevoEstado = Estado(
              liebre = estado.liebre,
              //al estado de los sabuesos le quito la posicion de origen del sabueso que se va a mover (- origen)
              //y le sumo como nueva posicion el destino a donde se va a mover el sabueso elegido
              sabuesos = estado.sabuesos - origen + destino,
              turno = Jugador.Liebre,
              movimientosLiebre=estado.movimientosLiebre
            )
            // vuelvo a llamar al bucle de juego con su nuevo estado para continuar la partida
            bucleJuego(tablero, nuevoEstado, modoIA)

