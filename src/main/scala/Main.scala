
object Juego extends App
//
@main def JUEGOLIEBREYSABUESOS():Unit=
  println("                                                      ==========================================================")
  println("                                                      |     Bienvenido al juego de la liebre y los sabuesos    |")
  println("                                                      ==========================================================\n")

  val instrucciones = scala.io.StdIn.readLine("Sabes jugar(1) o quieres ver una breve descripción del juego(2)->Introduce tu eleccion:").toInt
  //solo muestra las instrucciones si el usuario asi lo decide, hemos tomado esta decisicon ya que si es por ejemplo la tercera
  //partida no tiene sentido mostrarlo siempre al comienzo de una partida
  if instrucciones == 2 then
    //pongo una breve introduccion que permita entender mejor nuestro juego
    println("==============================================================INSTRUCCIONES===========================================================================")
    println("||  El objetivo de la liebre es llegar a la casilla de mas a la izquierda (I2M), la liebre tiene libertad de movimiento hacia todas las direcciones ||")
    println("||  Los sabuesos deben acorralar a la liebre, pero solo pueden avanzar o mantenerse en su misma columna                                             ||")
    println("======================================================================================================================================================")

  //quiera o no ver las instrucciones, si quiere justo despues de mostrarlas y sino directamente se muestra este mensaje
  //de eleccion para que el usuario decida si quiere jugar o no, en caso de no querer mostrara un mensaje y en caso de
  //querer usaremos la funcion modoJuego para darle al usuario a elegir en que modo de juego quiere jugar, la propia funcion
  //inicializa ya el bucle en juego en funcion a lo que el usuario decida
  //imprimo la pregunta de si quieres jugar como botones para que quede mas tipo juego
  println(" ¿QUIERES JUGAR A LA LIEBRE Y LOS SABUESOS?")
  println("     =============                   ===============")
  println("     |  (1)->SI  |                   |   (2)->NO   |")
  println("     =============                   ===============")

  val quieresjugar= scala.io.StdIn.readLine("\nINTRODUCE EL NUMERO DE TU ELECCION:  ").toInt

  if quieresjugar== 1 then
    EleccionModoJuego.modoJuego()
  else
    println ("Proceso de juego finalizado")


