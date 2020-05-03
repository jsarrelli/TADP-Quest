import scala.util.Success

case object LaTaberna {
  val misiones: List[Mision] = ???

  /**
   *   No creo que sea lo mas performante, no me gusta el echo de reccorrer dos veces la lista.
   *   Pero por otro lado, asi es bastante sencillo
   */
  def elegirMision(equipo: Equipo, preferencia: (Equipo, Equipo) => Boolean, misiones: List[Mision] = misiones): Option[Mision] =
    misiones.filter(_.realizarMision(equipo).isSuccess)
      .reduceOption((mision1, mision2) => (mision1.realizarMision(equipo), mision2.realizarMision(equipo)) match {
        case (Success(equipo1), Success(equipo2)) => if (preferencia(equipo1, equipo2)) mision1 else mision2
      })

  def entrenar(equipo: Equipo, preferencia: (Equipo, Equipo) => Boolean): Equipo = {

    @scala.annotation.tailrec
    def realizarMisiones(misiones: List[Mision], equipo: Equipo): Equipo = misiones match {
      case x :: xs => elegirMision(equipo, preferencia, misiones) match {
        case Some(mision) =>
          realizarMisiones(dropMision(misiones, mision), mision.realizarMision(equipo).get) //aca hago un .get porque ya me asegure que la mision la pueda hacer
        case None => realizarMisiones(Nil, equipo)
      }
      case Nil => equipo
    }
    realizarMisiones(misiones, equipo)
  }

  private def dropMision(misiones: List[Mision], mision: Mision) = misiones.filterNot(_.equals(mision))
}
