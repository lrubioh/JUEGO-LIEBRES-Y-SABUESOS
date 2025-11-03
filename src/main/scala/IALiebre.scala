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
      
      

  

    
  
    
  
