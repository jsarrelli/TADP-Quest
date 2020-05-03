case class Item(
                 efecto: Heroe => Stats,
                 restriccion: Heroe => Boolean = _ => true,
                 parteDelCuerpo: ParteDelCuerpo,
                 precio: Int = 0
               )

object cascoVikingo extends Item(
  _ => Stats(HP = 10),
  (h: Heroe) => h.statsBase.Fuerza > 30,
  parteDelCuerpo = Cabeza
)

object palitoMagico extends Item(
  _ => Stats(Inteligencia = 20),
  restriccion = (h: Heroe) => h.trabajo match {
    case Some(Mago) => true
    case Some(Ladron) => h.statsBase.Inteligencia > 30
    case _ => false
  },
  parteDelCuerpo = UnaMano
)

object ArmaduraEleganteSport extends Item(
  efecto = _ => Stats(Velocidad = 30, HP = -30),
  parteDelCuerpo = Cuerpo
)

object arcoViejo extends Item(
  efecto = _ => Stats(Fuerza = 2),
  parteDelCuerpo = DosManos
)

object escudoAntiRobo extends Item(
  efecto = _ => Stats(HP = 20),
  restriccion = (h: Heroe) => if (h.es(Ladron)) false else h.statsBase.Fuerza > 20,
  parteDelCuerpo = UnaMano
)

object talismanDedicacion extends Item(
  efecto = (h: Heroe) => {
    val incremento = (h.trabajo.map(_.statPrincipal(h.stats)).getOrElse(0) * 0.1).toInt
    h.statsBase.afectarATodos(_ => incremento)
  },
  parteDelCuerpo = Gema
)

object talismanMinimalismo extends Item(
  efecto = (h: Heroe) => Stats(HP = 30 - (h.inventario.items.size * 10)),
  parteDelCuerpo = Gema
)

object vinchaBufaloAgua extends Item(
  efecto = (h: Heroe) =>
    if (h.statsBase.Fuerza > h.statsBase.Inteligencia)
      Stats(Inteligencia = 30)
    else
      Stats(Fuerza = 10, Velocidad = 10, HP = 10)
  ,
  restriccion = (h: Heroe) => h.trabajo.isDefined,
  parteDelCuerpo = Cabeza
)

object talismanMaldito extends Item(
  efecto = (h: Heroe) => h.statsBase.afectarATodos(x => -x + 1),
  parteDelCuerpo = Gema
)

object espadaDeLaVida extends Item(
  efecto = (h: Heroe) => Stats(Fuerza = -h.statsBase.Fuerza + h.statsBase.HP),
  parteDelCuerpo = UnaMano)