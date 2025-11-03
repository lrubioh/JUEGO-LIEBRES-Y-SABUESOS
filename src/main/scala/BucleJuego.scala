import IALiebre._
object BucleJuego:
  def bucleJuego(tablero: TableroJuego, estado: Estado, modoIA: Boolean): Jugador=
    //1 pintar tablero
    tablero.pintarTablero(estado)

    //el siguiente paso sería calcula los movimientos posibles del jugador al que
    // le toca el turno, pero para eso tengo que comprobar si la partida ha acabado
    //o continua, si ha acabado no le toca turno a nadie
    tablero.esFinPartida(estado) match
      //en el caso de que haya ganador imprimo el mensaje del ganador
      case Some(ganador) =>
        println(s"\n ***PARTIDA TERMINADA, EL GANADOR ES : $ganador ***")
        ganador

      //en caso de que nadie haya ganado imprimo los posibles movimientos del
      //jugador al que le toca el turno
      case None =>
        if estado.turno == Jugador.Liebre then
            //calculo los movimientos posibles de la liebre
            val movimientos= MovimientoLiebre.movimientosPosibles(tablero, estado)
            if modoIA == true then
              /**MODO IA -> LA LIEBRE SE MUEVE SOLA A LA POSICION MAS OPTIMA PARA GANAR**/
              //quiero evaluar los movimientos en funcion a la tupla de evaluar movimientos
              //movimientos es un Set[Posicion] que no sabemos cuantos elementos tiene, pero necesito acceder a cada uno de ellos
              //para evaluarlos y luego quedarme con el que tenga el primer elemento mayor, en caso de haber dos iguales
              //me quedo con el que tenga el mayor en el segundo elemento
              
              //guardo todas las tuplas en un map con "indice" destino(Posicion)
              val evaluaciones = movimientos.map(destino => (destino, IALiebre.evaluarMovimiento(tablero, estado, destino))).toMap
              
              //he creado una funcion en la IALiebre que es encontrarMejorMovimiento, la cual con el mapa evaluaciones,
              //me devolvera una lista con un valor que es el destino asociado a la tupla con sus valores, gracias a la funcion
              //en mi lista solo quedaras los que tengan el valor mas grande en el primer elemento de la tupla, si solo hay uno
              //ya tengo a donde se va a mover la liebre si hubiese mas uso la funcion desempate que hace lo mismo pero con el segundo elemento de la tupla
              
              val mejoresMovimientos= encontrarMejorMovimiento(evaluaciones)
              //necesito saber cuantos elementos tiene el map, para eso uso la funcion size 
              if mejoresMovimientos.length == 1 then 
                //si mejoresMovimientos solo tiene un elemento solo hay una opcion a la que moverse, la liebre se mueve ahi
                //guardo esa posicion en destino 
                val (posicion,tupla)= mejoresMovimientos.head 
                val destino = posicion
                destino 
              //si tiene mas de un elemento hago desempate y ahora ya cuando se quede con un solo elemento obtengo el valor de destino igual que antes
              else 
                //mejor movimiento ahora es una lista con un solo elemento de la forma List[(Posicion, (Int, Int))] 
                val mejorMovimiento= desempate(mejoresMovimientos)
                val (posicion, tupla) = mejorMovimiento.head
                val destino = posicion
              
                
              //establezco el nuevo estado de la partida con la liebre ya en su nueva posicion y vuelvo a llamar al bucleJuego
              val nuevoEstado = Estado(
                liebre = destino,
                sabuesos = estado.sabuesos,
                turno = Jugador.Sabuesos
              )
              val modoIA = true

              bucleJuego(tablero,nuevoEstado, modoIA)


            else if modoIA == false then
              /***FUNCIONAMIENTO NORMAL, IMPRIME LOS MOVIMIENTOS Y SE ELIGEN POR NUMERO**/
              //imprimo los movimientos posibles de la liebre
              println(s"\n Turno de la Liebre: elige un movimiento (introduciendo el número)")
              //creo un indice que se va a ir incrementando por cada movimiento que recorra
              var indicemovimientos = 1
              //creo un Map vacio con el inidice y la posicion a la que se va a mover la liebre (asi puedo asociar el indice con la nueva posicion)
              var opciones = Map.empty[Int, Posicion]
              //recorro los movimientos de uno en uno y les voy dando un numero
              for (posicionfinal <- movimientos) do
                // 1 -> liebre se mueve a D1M (por ejemplo)
                println(s" $indicemovimientos -> Liebre se mueve a $posicionfinal ")
                //añade al mapa opciones: el par con el indice y la posicion final
                opciones += (indicemovimientos -> posicionfinal)
                //incrementa el indice para el siguiente movimiento
                indicemovimientos += 1
    
              //ahora hay que pedir al jugador el numero que esta asociado al movimiento que quiere hacer
              val  eleccion = scala.io.StdIn.readLine("Elige movimiento:").toInt
              //guardo en destino el valor asociado al numero elegido (accedo a posicion final atraves de el)
              val destino = opciones(eleccion)
    
              //ya tenemos el movimiento guardado, ahora creamos un nuevo estado con la nueva casilla de la liebre,
              //el cambio de turno al proximo jugador(sabuesos) y las casillas de los sabuesos que se habran mantenido
              val nuevoEstado = Estado(
                liebre = destino,
                sabuesos = estado.sabuesos,
                turno = Jugador.Sabuesos
              )
              val modoIA= true
              // vuelvo a llamar al bucle de juego con su nuevo estado para continuar la partida 
              bucleJuego(tablero,nuevoEstado, modoIA)

        // ahora hago el mismo proceso pero con los sabuesos
        else
          val movimientos = MovimientoSabueso.movimientosPosiblesPorSabueso(tablero,estado)

          println(s" \n Turno de los sabuesos: elige un movimiento (introduciendo el número)")
          var indicemovimientos = 1
          var opciones : Map[Int, (Posicion,Posicion)] = Map.empty

          for ((origen,destino)<- movimientos) do
            println(s" $indicemovimientos -> Sabueso en $origen se mueve a $destino")
            //guardo en el map el movimiento asociado a su índice
            opciones = opciones + (indicemovimientos -> (origen,destino))
            indicemovimientos += 1

          val eleccion = scala.io.StdIn.readLine("Elige movimiento:").toInt
          val (origen, destino) = opciones(eleccion)
          val nuevoEstado = Estado(
            liebre = estado.liebre,
            //al estado de los sabuesos le quito la posicion de origen del sabueso que se va a mover (- origen)
            //y le sumo como nueva posicion el destino a donde se va a mover el sabueso elegido
            sabuesos = estado.sabuesos -origen + destino,
            turno = Jugador.Liebre
          )
          val modoIA = true 
          // vuelvo a llamar al bucle de juego con su nuevo estado para continuar la partida 
        bucleJuego(tablero, nuevoEstado, modoIA)



