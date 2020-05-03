
case class Trabajo(
                    statPrincipal: Stats => Int,
                    stats: Stats
                  )

object Guerrero extends Trabajo(
  (s: Stats) => s.Fuerza,
  Stats(HP = 10, Inteligencia = 10, Fuerza = 15)
)

object Mago extends Trabajo(
  (s: Stats) => s.Inteligencia,
  Stats(Inteligencia = 20)
)

object Ladron extends Trabajo(
  (s: Stats) => s.Velocidad,
  Stats(Velocidad = 10, HP = 5)
)