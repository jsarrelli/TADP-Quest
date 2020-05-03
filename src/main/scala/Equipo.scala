case class Equipo(nombre: String, integrantes: List[Heroe], pozoComun: Int = 0) {

  def mejorHeroe(f: (Heroe => Int)) = integrantes.maxByOption(f)

  def obtenerItem(item: Item): Equipo = {
    val integrantesConItem: List[(Heroe, Int)] = integrantes.map(heroe =>
      heroe -> (heroe.equiparItem(item).statPrincipal.getOrElse(0) - heroe.statPrincipal.getOrElse(0))
    )

    if (integrantesConItem.forall(kv => kv._2 <= 0)) copy(pozoComun = pozoComun + item.precio)
    else {
      val heroeElegido = integrantesConItem.maxBy(_._2)._1
      reemplazarMiembro(heroeElegido.equiparItem(item), heroeElegido)
    }
  }

  def reemplazarMiembro(in: Heroe, out: Heroe) = copy(integrantes =
    integrantes.map {
      case `out` => in
      case x => x
    }
  )

  def obtenerMiembro(h: Heroe) = copy(integrantes = h :: integrantes)

  //no me convence
  def lider = integrantes.sortBy(_.statPrincipal).reverse match {
    case x :: y :: _ if x.statPrincipal == y.statPrincipal => None
    case Nil => None
    case x :: _ => Some(x)
  }

}
