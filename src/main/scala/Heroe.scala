case class Heroe(
                  nombre: String,
                  statsBase: Stats,
                  trabajo: Option[Trabajo] = None,
                  inventario: Inventario = Inventario(),
                  vida: Int = 100
                ) {


  def equiparItem(item: Item) = if (item.restriccion(this)) copy(inventario = inventario.equipar(item)) else this

  def statPrincipal =
    trabajo.map(_.statPrincipal(stats))

  def stats = (statsBase :: inventario.stats(this).toList ::: trabajo.map(_.stats).toList).reduce(_ + _)

  def es(t: Trabajo) = trabajo.exists(_.equals(t))

}
