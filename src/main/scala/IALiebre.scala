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


    //hago una funcion que me devuelva la tupla con mayor primer elemento en una lista por si hubiese mas tuplas con
    //el mismo primer elemento 
  def encontrarMejorMovimiento(mapa: Map[Posicion, (Int, Int)]): List[(Posicion, (Int, Int))] =
    //Pongo 0 como valor maximo para que siempre que encuentre uno mas grande que ese lo sustituyo
    var Valormax = 0
    var mejorMovimiento: List[(Posicion, (Int, Int))] = List()
    //valor es la tupla con primerelemento y sumaDistancias
    for ((pos, tupla) <- mapa) do
      //si el primer valor del primer elemento de la tupla que estoy mirando es mayor que 0, lo reemplazo
      if (tupla._1 > Valormax) then
        Valormax = tupla._1
        mejorMovimiento = List((pos, tupla))
      //si estan en empate la añado  
      else if (tupla._1 == Valormax)
        mejorMovimiento = (pos, tupla) :: mejorMovimiento

      else //tupla._1 (que es primer elemento) es menor que el que ya tengo guardado entonces mejorMovimiento se queda igual
        mejorMovimiento = mejorMovimiento

    //devuelvo la lista mejor Movimiento que puede tener un solo elemento en cuyo caso ya se a donde mover la liebre
    //o puede tener varios en cuyo caso me quedo con el que tenga mayor sumaDistancias(Segundo elemento de la tupla)    
    mejorMovimiento 
  
  //hago un segundo metodo para usar en el caso de que la lista mejores movimientos tenga mas de 1 elemento
  //desempate lo que hace es coger la lista con los candidatos a mejores elementos, todos tendran el mismo valor en el primer elemento
  //y para decidir con cual quedarme recorre la lista como en la anterior funcion y añade a la nueva el que mayor valor tenga, si
  //encuentra otra tupla con mayor valor en el segundo elemento esta sustituye a la anterior, de forma que solo puede quedar
  //un tupla la cual va asociada a una posicion la cual sera la posicion a la que se mueva la liebre  
  def desempate (listamejoresmovimientos: List[(Posicion,(Int,Int))]) :List[(Posicion, (Int, Int))] =
    var Valormax=0
    var mejorMovimiento: List[(Posicion, (Int, Int))] = List()
    for ((pos, tupla) <- listamejoresmovimientos) do
      if (tupla._2 > Valormax) then 
        Valormax= tupla._2
        mejorMovimiento = List((pos,tupla))

      else 
        mejorMovimiento
        
    mejorMovimiento
      
    

  

    
  
    
  
