
object Juego extends App
//
@main def JUEGOLIEBREYSABUESOS():Unit=
  //guardo los valores de los colores para imprimir la bienvenida al juego en rojo 
  val ROJO   = "\u001B[31m"
  val RESET = "\u001B[0m"
  println(s"     ${ROJO}==========================================================${RESET}")
  println(s"     ${ROJO}|${RESET}     Bienvenido al juego de la liebre y los sabuesos    ${ROJO}|${RESET}")
  println(s"     ${ROJO}==========================================================${RESET}\n")

  
  //dos "botones" : se jugar , quiero ver el manual de instrucciones
  println("     ====================                   ==================================================")
  println("     |  (1)-> SE JUGAR  |                   |   (2)->QUIERO VER EL MANUAL DE INSTRUCCIONES   |")
  println("     ====================                   ==================================================\n")

  val instrucciones = scala.io.StdIn.readLine("Introduce tu eleccion:").toInt
  //solo muestra las instrucciones si el usuario asi lo decide, hemos tomado esta decisicon ya que puede que el usuario no conozca el juego
  if instrucciones == 2 then
    //pongo una breve introduccion que permita entender mejor nuestro juego
    println("==============================================================INSTRUCCIONES===========================================================================")
    println("||  El objetivo de la liebre es llegar a la casilla de mas a la izquierda (I2M), la liebre tiene libertad de movimiento hacia todas las direcciones ||")
    println("||  Los sabuesos deben acorralar a la liebre, pero solo pueden avanzar o mantenerse en su misma columna                                             ||")
    println("======================================================================================================================================================\n")

  //quiera o no ver las instrucciones, si quiere justo despues de mostrarlas y sino directamente se muestra este mensaje
  //de eleccion para que el usuario decida si quiere jugar o no, en caso de no querer mostrara un mensaje y saldrá y en caso de
  //querer usaremos la funcion modoJuego para darle al usuario a elegir en que modo de juego quiere jugar, la propia funcion
  //inicializa ya el bucle en juego en funcion a lo que el usuario decida
    
  //imprimo la pregunta de si quieres jugar como botones para que quede mas estetico
  println("       ¿QUIERES JUGAR A LA LIEBRE Y LOS SABUESOS?\n")
  println("     =============                   ===============")
  println("     |  (1)->SI  |                   |   (2)->NO   |")
  println("     =============                   ===============")

  val quieresjugar= scala.io.StdIn.readLine("\nINTRODUCE EL NUMERO DE TU ELECCION:  ").toInt
  //si quiere juagr le doy a elegir el modo
  if quieresjugar== 1 then
    EleccionModoJuego.modoJuego()
  //si no quiere salgo
  else
    println ("Proceso de juego finalizado")


