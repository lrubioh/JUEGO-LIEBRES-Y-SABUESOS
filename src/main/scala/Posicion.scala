//para representar las casillas necesitamos crear la clase posicion
//esta debe ser una case class para poder comparar dos posiciones directamente con ==
case class Posicion(col: Columna, fila: Fila):
  def x: Int = col.valor
  def y: Int = fila.valor
  //con el override consigo que se imprima por ejemplo M.A como posición
  override def toString : String = s"${col}.${fila}"
  
  //se usa poniendo puntoinicial.manhattan(puntofinal) donde punto inicial y punto final son los puntos entre los que 
  //quiero saber la distancia
  def manhattan(other: Posicion): Int =
    math.abs(this.x - other.x) + math.abs(this.y - other.y)

