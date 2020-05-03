case class Inventario(
                       cabeza: Option[Item] = None,
                       cuerpo: Option[Item] = None,
                       manos: List[Item] = List(),
                       gemas: List[Item] = List()
                     ) {

  def equipar(item: Item): Inventario = item.parteDelCuerpo match {
    case Cabeza => copy(cabeza = Some(item))
    case Cuerpo => copy(cuerpo = Some(item))
    case UnaMano => copy(manos = equiparArmaSimple(item))
    case DosManos => copy(manos = List(item))
    case Gema => copy(gemas = item :: gemas)
  }

  private def equiparArmaSimple(item: Item) = manos match {
    case List(_, x) => List(item, x)
    case List(Item(_, _, UnaMano, _)) => item :: manos
    case _ => List(item)
  }

  def stats(heroe: Heroe): Option[Stats] =
    items.map(_.efecto(heroe)).reduceLeftOption(_ + _)

  def items: List[Item] =
    List(gemas, manos, cabeza.toList, cuerpo.toList).flatten
}
