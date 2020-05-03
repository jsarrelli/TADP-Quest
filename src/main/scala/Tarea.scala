case class Tarea(tarea: Heroe => Heroe, facilidad: (Equipo, Heroe) => Option[Int])


object pelearContraMonstruo extends Tarea(
  tarea = {
    case x: Heroe if x.stats.Fuerza < 20 => x.copy(vida = x.vida - 1)
    case x: Heroe => x
  },
  facilidad = (e: Equipo, _: Heroe) =>
    e.lider match {
      case Some(Heroe(_, _, Some(Guerrero), _, _)) => Some(20)
      case _ => Some(10)
    }
)

object forzarPuerta extends Tarea(
  tarea = (h: Heroe) =>
    if (h.es(Mago) || h.es(Ladron)) h
    else h.copy(statsBase = h.statsBase + Stats(HP = -5, Fuerza = 1)),
  facilidad = (e: Equipo, heroe: Heroe) => Some(
    heroe.stats.Fuerza + e.integrantes.count(_.trabajo.contains(Ladron))
  )
)

object robarTalisman extends Tarea(
  tarea = (h: Heroe) => h.equiparItem(talismanDedicacion),
  facilidad = (e: Equipo, _: Heroe) => e.lider.collect { case h: Heroe if h.es(Ladron) => h.stats.Velocidad }
)