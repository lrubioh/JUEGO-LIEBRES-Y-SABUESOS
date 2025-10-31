//defino las filas, columnas y sus posiciones posibles
enum Fila(val valor: Int):
  case A extends Fila(1)
  case M extends Fila(0)
  case B extends Fila(-1)