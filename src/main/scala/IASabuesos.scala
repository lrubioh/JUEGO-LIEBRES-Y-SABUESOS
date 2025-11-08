object IASabuesos:
  //Para la IA de los sabuesos podemos utilizar como base la IA de la liebre, de tal forma que usaremos una tupla con la que evaluar los movimientos
  //a diferencia de la IA de la liebre en esta tupla el mejor movimiento sera aquel con menor valor
  def evaluarMovimientoSabuesos(tablero: TableroJuego, estado:Estado, movimientos:Set[(Posicion,Posicion)]): Map[(Posicion,Posicion),(Int,Int)] =
    // esta funcion evaluara cada uno de los movimiento con una tupla de tal forma que cuanto menor sea el valor mejor sera el
    // movimiento, el primer elemento de la tupla sera el numero de sabuesos rebasados (entendiendo por rebasados tambien que
    // esten en la misma columna que la liebre) y el segundo elemento sera el numero de posiciones posibles que tendra la liebre
    // cuando el sabueso se mueva a la posicion destino, al igual que el primer elemento cuanto menor sea su valor mejor movimiento sera
    //devolvera un map donde el indice es la posicion de destino y contiene las tuplas para cada uno de los movimientos
    def recorrerPosiblesMovimientos(lista:List[(Posicion,Posicion)], mapa:Map[(Posicion,Posicion),(Int,Int)]): Map[(Posicion,Posicion),(Int,Int)]=lista match
      //esta funcion va a convertir los Set de los movimientos en una lista que voy a ir recorriendo a la vez que para cada movimiento guardo en un 
      //mapa donde la clave es una tupla con el origen y el destino y esta asociada con su tupla evaluada
      
      case Nil => mapa //si ya no quedan movimientos por recorrer devuelvo el mapa 
      //si tengo 
      case (origen,destino)::cola=> 
        //si tengo un movimiento y luego la cola evaluo ese movimiento, para eso necesito "mover" el sabueso al destino para ver las evaluaciones en el destino
        val nuevoSabuesos= estado.sabuesos -origen + destino
        //calculo el primer elemento => num de sabuesos rebasados
        val sabuesosrebasados= nuevoSabuesos.foldLeft(0)((acum,sab)=> if estado.liebre.x <= sab.x then acum + 1 else acum)
        //calculo el segundo valor de la tupla : movimientos posibles de la liebre
        //primero necesito poner un nuevo estado a partir del que calcular lo mov posibles de la liebre
        val nuevoEstado = Estado(
          liebre = estado.liebre,
          sabuesos = nuevoSabuesos,
          turno = estado.turno
        )
        val movimientosLiebre= MovimientoLiebre.movimientosPosibles(tablero,nuevoEstado).toList
        val numMovLiebre= movimientosLiebre.length
        
        val evaluacion = (sabuesosrebasados, numMovLiebre)
        //llamo a la funcion con el resto de mov (cola) , y añado al mapa el mov que he evaluado 
        recorrerPosiblesMovimientos(cola,mapa+ ((origen,destino)->evaluacion ))
        
    recorrerPosiblesMovimientos(movimientos.toList,Map.empty)
  
  // ahora con el mapa que tengo en que los indices son los destinos y el "contenido" son las tuplas que hace la funcion evaluarMovimientoSabuesos
  //esta lo que hara sera buscar el elemento que tenga el menor valor en el primer valor de la tupla, en caso de haber mas de uno
  //con el mismo primer valor guardare ambos en una lista y usare la funcion desempate
  def encontrarMejorMovimiento(mapa: Map[(Posicion,Posicion),(Int,Int)]):List[((Posicion,Posicion),(Int,Int))]=
    //hago una funcion recursiva que recorra el mapa lista por lista y en una nueva lista(mejores) guarde los mejores elementos de movimientos
    def recorrerMapa(movimientos: List[((Posicion,Posicion), (Int, Int))], valorMin: Int, mejores: List[((Posicion,Posicion), (Int, Int))]): List[((Posicion,Posicion), (Int, Int))] = movimientos match
      case Nil => mejores
      case (tuplaposiciones,tuplaevaluaciones):: cola =>
        //quiero la tupla con menor valor en su primer elemento
        val primerelementotuplaevaluaciones = tuplaevaluaciones._1
        if primerelementotuplaevaluaciones < valorMin then
          //si el primer elemento es menor que el valorMin(sera el Int.ManValue, el mayor entero posible) entonces guardo esa tupla en mejores
          //(si hubiese otra tendria menor valor con lo que la sustituyo) y vuelvo a llamar a la funcion recorrer con el resto de la tupla
          recorrerMapa(cola, primerelementotuplaevaluaciones,List((tuplaposiciones,tuplaevaluaciones)))

        else if primerelementotuplaevaluaciones == valorMin then
          //si el primer elemento es igual que uno que ya habia no lo sustituyo sino que lo añado
          recorrerMapa(cola, primerelementotuplaevaluaciones,(tuplaposiciones,tuplaevaluaciones)::mejores)

        else
          //si no es menor ni igual no hago nada con el, sigo recorriendo el mapa con el resto y con valorMin
          recorrerMapa(cola, valorMin,mejores)

    //inicializo la funcion recorrer mapa

    recorrerMapa(mapa.toList, Int.MaxValue, List())

  //en caso de que encontrarMejorMovimiento me devuelva una lista con mas de un posible movimiento necesito una funcion que me ayude a
  //quedarme solo con uno, esta funcion va a devolver SI o SI una lista con un solo elemento, es decir un solo destino posible
  //ya que desempate va a mirar de entre las tuplas que me ha devuelto encontrarMejorMovimiento, todas tendran el mismo primer valor,
  //la funcion desempate me va a devolver aquella que tenga menor segundo valor, solo me ha devolver una ya que en caso de haber dos
  //con el mismo segundo elemento serian exactamente iguales con lo que daria igual cual coger, me quedo con la que primero evalue

  //le paso la lista que me devuelve encontrarMejoresMovimientos y me devuelve una lista con un solo elemento (Posicion, (Int, Int)), me dice a que pos mover el sabueso
  def desempate(listamejoresmovimientos: List[((Posicion,Posicion),(Int,Int))]) :List[((Posicion,Posicion), (Int, Int))] =
    def recorrerdesempate( lista:List[((Posicion,Posicion), (Int, Int))], valorMin: Int, mejor: List[((Posicion,Posicion), (Int, Int))]): List[((Posicion,Posicion), (Int, Int))] = lista match
      case Nil => mejor
      case (tuplaposiciones,tuplaevaluaciones)::cola =>
        //voy a mirar si ese primer elemento es menor que el valor min (lo guardo/sustituyo) o si es mayor(no hago nada con el)
        val segundoelementotuplaevaluaciones= tuplaevaluaciones._2
        if segundoelementotuplaevaluaciones< valorMin then
          recorrerdesempate(cola,segundoelementotuplaevaluaciones,List((tuplaposiciones,tuplaevaluaciones)))

        else
          recorrerdesempate(cola, valorMin,mejor)

    //incicializo recorrerdesempate
    recorrerdesempate(listamejoresmovimientos,Int.MaxValue, List())

  //para implementar el como elegir cual es el mejor movimiento en el bucle de juego y que sea mas sencilla creamos una funcion
  //que use las dos ultimas implementaciones para decidir cual es el mejor movimiento y quu devuelva cual es el destino final
  def eleccionMejorMovimiento(movimientos:Set[(Posicion,Posicion)],tablero:TableroJuego,estado: Estado):Posicion=
    // guardo el mapa de evaluaciones para usar encontrarMejorMovimiento y que me devuelva el mejor/mejores movimientos 
    val evaluaciones = evaluarMovimientoSabuesos(tablero,estado,movimientos)
    val mejoresMovimientos= encontrarMejorMovimiento(evaluaciones)
    
    //si mejoresMovimientos es un lista con un solo movimiento ya tengo a donde moverme 
    if mejoresMovimientos.length==1 then 
      //el destino es el segundo elemento(._2) de la primer tupla(._1) del primer y unico elemento de la lista mejoresMovimientos(0)
      val destino = mejoresMovimientos(0)._1._2
      destino
      
    //si mejoresMovimientos tiene mas de un elemento uso desempate
    else 
      //mejorMovimiento es una lista con un solo movimiento, cojo el destino y ya tengo a donde se mueve el sabueso
      val mejorMovimiento= desempate(mejoresMovimientos)
      //el destino el segundo elemento(._2) de la primer tupla(._1) del primer y unico elemento de la lista mejorMovimiento(0)
      val destino = mejorMovimiento(0)._1._2
      destino 
      
      
    
    

