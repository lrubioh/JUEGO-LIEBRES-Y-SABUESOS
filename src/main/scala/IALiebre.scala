object IALiebre:
  //la estrategia de la liebre será rebasar sabuesos teniendo en cuenta dejar cierta distancia con ellos, para eso,
  //para cada uno de los movimientos posibles usaremos la funcion evaluarMovimiento la cual me devuelve una tupla con
  //en el primer elemento si la liebre aun no ha rebasado ningun sabueso el numero de sabuesos que rebasara al moverse, o en
  //caso contrario(en su estado inicial ya ha rebasado a algun sabueso) me devolvera como de cerca esta de la meta
  //en el segundo elemento me devolvera la suma de las distancias con cada uno de los sabuesos

  //rebasar a un sabueso es que la posición x de la liebre este mas a la izq que la del sabueso


  def evaluarMovimiento(tablero: TableroJuego, estado: Estado, destino: Posicion) : (Int, Int)=
    val meta = tablero.posicionMetaLiebre

    //vemos primero cuantos sabuesos ha rebasado la liebre en su posicion actual

    val rebasadosestadoInicial : Int =
      //usamos foldLeft para que cuando se cumple la condicion se le sume 1 al acc que va ser el total de sabuesos rebasados
      estado.sabuesos.foldLeft(0)((acc,sab)=> if estado.liebre.x < sab.x then acc + 1 else acc)

    //calculo los sabuesos que habra rebasado si se mueve a destino

    val rebasadosPorMovimiento : Int =
      estado.sabuesos.foldLeft(0)((acc: Int, sab: Posicion) => if destino.x < sab.x then acc + 1 else acc )

    //Calculo la metrica que devolvera con respecto de la meta si ya había rebasado alguno en la poscion inicial
    val metricaMeta : Int = -destino.manhattan(meta)
    //pongo - la distancia para que como luego me voy a quedar con el valor mas grande de ese elemento de la tupla
    //yo la que me quiero quedar es la mas pequeña entonces con el menos la mas pequeña sera la mas grande

    //si no ha rebasado ninguno, devuelvo cuantos rebasara en la posicion destino
    val primerelemento: Int=
      if rebasadosestadoInicial == 0 then
        rebasadosPorMovimiento
      //en caso de que ya haya rebasado alguno devuelvo la metrica
      else metricaMeta

    //calculo la suma de distancias respecto de los sabuesos para ponerla en el segundo elemento
    //tengo que convertir el estado de los sabuesos en una lista para poder acceder a cada uno de los valores y sumarlos
    val List (sab1, sab2, sab3) = estado.sabuesos.toList
    val sumaDistancias = destino.manhattan(sab1) + destino.manhattan(sab2) + destino.manhattan(sab3)

    //devuelvo la tupla
    (primerelemento, sumaDistancias)


  //La funcion encontrarMejorMovimiento me devuelve una lista de la forma List[(Posicion, (Int, Int))] en la que tenemos dos opciones
  // 1) hay un solo elemento ya que el primer elemento de la tupla es mayor que todos los demas, en cuyo caso ya tenemos la posicion a la que se movera la liebre
  //2) que havaya varios elementos con el mismo valor en el primer elemento de la tupla, en cuyo caso, tendre que hacer un desempate (creare la funcion )
  def encontrarMejorMovimiento(mapa: Map[Posicion, (Int, Int)]): List[(Posicion, (Int, Int))] =
    //uso uan funcion recursiva que recorra el mapa como una lista
    def recorrerMapa(movimientos: List[(Posicion, (Int, Int))], valorMax: Int, mejores: List[(Posicion, (Int, Int))]): List[(Posicion, (Int, Int))] = movimientos match
      //necesito un valor max para ir añadiendo a mejores solo aquellos que tengan el primer valor mayor
      case Nil => mejores //cuando no queden elementos en movimientos devuelvo los que haya guadado en mejores
      case (posicion, tupla):: cola =>
        //me interesa coger la tupla con mayor valor en su primer elemento,
        val primerelementotupla = tupla._1
        if primerelementotupla > valorMax then
          //si el primer elemento es mayor que el valor max esa tupla la meto en la lista de mejores y vuelvo a llamar a
          //la funcion recorrer mapa con el los movimientos restantes(cola) y ahora el valorMax sera el primer elemento
          recorrerMapa(cola, primerelementotupla, List((posicion,tupla)))

        //si el primer elemento es igual que el valor max debo guardarlo sin sustituirlo para luego desempatar
        else if primerelementotupla == valorMax then
          recorrerMapa(cola,primerelementotupla,(posicion,tupla)::mejores)

        else
          //en este caso el primer elemento de la tupla es menor que el valorMax o que el primer elemento de la tupla que ya esta guardada
          //con lo que volvere a llamar a la funcion para seguir buscando la tupla con mayor valor en el primer elemento,
          //como mayor valor pondre el valorMax que ya estaba guardado y la lista la dejare como estuviese antes, la añado nada
          recorrerMapa(cola, valorMax,mejores)

    //inicializo la función recorrerMapa con el valorMax como el número mas pequeño posible para no arriesgarme a poner un numero
    // y que luego ninguno de los primeros elementos sea mayor que ese entonces no estaria seleccionando el mas grande
    recorrerMapa(mapa.toList, Int.MinValue, List())



  //hago un segundo metodo para usar en el caso de que la lista mejores movimientos tenga mas de 1 elemento
  //desempate lo que hace es coger la lista con los candidatos a mejores elementos, todos tendran el mismo valor en el primer elemento de la tupla
  //y para decidir con cual quedarse recorre la lista y añade a la nueva el que mayor valor tenga, si
  //encuentra otra tupla con mayor valor en el segundo elemento esta sustituye a la anterior, de forma que solo puede quedar
  //un tupla la cual va asociada a una posicion la cual será la posicion a la que se mueva la liebre

  // esta funcion solo puede devolver una lista con un SOLO elemento no hay otra opcion por eso luego en eleccionMejorMovimiento no tengo que
  //tener en cuenta la posibilidad de que la lista este vacia ya que o va a tener un solo elemento en cuyo caso encontrarMejorMovimiento ya me ha devuelto
  //un solo elemento con la posicion correcta a la que moverse o si la lista tiene mas usare esta funcion desempate pero NUNCA puede estar vacia siempre va a haber minimo un elemento

  //solo va a haber un elemento ya que esta funcion solo se va a aplicar cuando la lista mejores que me devuelve encontrarMejorMovimiento tenga mas de un movimiento dentro, para decidir
  //entre esos voy a buscar la tupla que tenga mayor valor en su segundo elemento, si hubiese dos iguales seria por que dos movimientos
  //tienen exactamente la misma evaluacion por lo que daria igual a cual moverse, con lo que unicamente se guarda uno de ellos
  def desempate (listamejoresmovimientos: List[(Posicion,(Int,Int))]) :List[(Posicion, (Int, Int))] =
    def recorrerdesempate( lista:List[(Posicion, (Int, Int))], valorMax: Int, mejor: List[(Posicion, (Int, Int))]): List[(Posicion, (Int, Int))] = lista match
      case Nil => mejor //no quedar elementos por mirar, devuelvo el que ya habia encontrado mejor (con valor mas alto)
      case (posicion, tupla) :: cola =>
        //en el caso de tener un primer elemento y luego una cola miro ese elemento
        //guardo el valor del segundo elemento de la tupla que estoy mirando
        val segundoelementotupla = tupla._2
        if segundoelementotupla > valorMax then 
          //si el elemento es el mayor sustituyo al que ya habia guardado por ese
          recorrerdesempate(cola, segundoelementotupla, List((posicion,tupla)))
        else 
          recorrerdesempate(cola, valorMax, mejor)
          
    //inicializo la funcion recorrerdesempate
    recorrerdesempate(listamejoresmovimientos, Int.MinValue, List())
  

  //la lista movimientos no puede estar nunca vacia por lo explicado arriba
  def eleccionMejorMovimiento (movimientos: Set[Posicion],estado :Estado, tablero: TableroJuego): Posicion =
    //guardo todas las tuplas en un map con "indice" destino(Posicion)
    val evaluaciones = movimientos.map(destino => (destino, IALiebre.evaluarMovimiento(tablero, estado, destino))).toMap

    //he creado una funcion en la IALiebre que es encontrarMejorMovimiento, la cual con el mapa evaluaciones,
    //me devolvera una lista con un valor que es el destino asociado a la tupla con sus valores, gracias a la funcion
    //en mi lista solo quedaran los que tengan el valor mas grande en el primer elemento de la tupla, si solo hay uno
    //ya tengo a donde se va a mover la liebre si hubiese mas uso la funcion desempate que hace lo mismo pero con el segundo elemento de la tupla


    val mejoresMovimientos = encontrarMejorMovimiento(evaluaciones)
    //necesito saber cuantos elementos tiene el map, para eso uso la funcion lenght
    //si en mejoresMovimientos solo hay uno entonces devuelve directamente ese
    if mejoresMovimientos.length == 1 then
      //si mejoresMovimientos solo tiene un elemento solo hay una opcion a la que moverse, la liebre se mueve ahi
      //guardo esa posicion en destino
      val (posicion, tupla) = mejoresMovimientos.head
      val destino = posicion
      destino
    //si tiene mas de un elemento hago desempate y ahora ya cuando se quede con un solo elemento obtengo el valor de destino igual que antes
    else
      //mejormovimiento sera una lista con un solo elemento de la forma List[(Posicion, (Int, Int))] al aplicar desempate
      val mejorMovimiento = desempate(mejoresMovimientos)
      val (posicion, tupla) = mejorMovimiento.head
      val destino = posicion
      destino









