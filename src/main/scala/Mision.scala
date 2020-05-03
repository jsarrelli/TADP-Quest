import scala.util.Try

case class Mision(
                   tareas: List[Tarea],
                   recompesa: Equipo => Equipo
                 ) {

  def realizarMision(equipo: Equipo): Try[Equipo] = Try {
    val equipoResult = tareas.foldRight(equipo)((tarea, equipo) => {
      val heroeElegido = heroeMasApto(equipo, tarea)
        .getOrElse(throw new Exception(s"No pudo completar la tarea $tarea. El equipo queda en estado $equipo"))
      val heroeResult = tarea.tarea(heroeElegido)
      equipo.reemplazarMiembro(heroeResult, heroeElegido)
    })
    recompesa(equipoResult)
  }

  private def heroeMasApto(equipo: Equipo, tarea: Tarea) =
    equipo.integrantes.filter(tarea.facilidad(equipo, _).isDefined).maxByOption(tarea.facilidad(equipo, _))


}
