//para representar las casillas necesitas crear la clase posicion
//esta debe ser una case class para poder comparar dos posiciones directamente con ==
case class Posicion(col: Columna, fila: Fila):
  def x: Int = col.valor
  def y: Int = fila.valor
  //con el override consigo que se imprima por ejemplo M.A como posición
  override def toString : String = s"${col}.${fila}"
