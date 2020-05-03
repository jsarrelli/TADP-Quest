
case class Stats(
                  HP: Int = 0,
                  Inteligencia: Int = 0,
                  Fuerza: Int = 0,
                  Velocidad: Int = 0
                ) {

  def afectarATodos(f: Int => Int): Stats =
    copy(
      HP = f(HP),
      Inteligencia = f(Inteligencia),
      Fuerza = f(Fuerza),
      Velocidad = f(Velocidad)
    )

  def +(stat: Stats): Stats =
    copy(
      HP = suma(HP, stat.HP),
      Inteligencia = suma(Inteligencia, stat.Inteligencia),
      Fuerza = suma(Fuerza, stat.Fuerza),
      Velocidad = suma(Velocidad, stat.Velocidad)
    )

  private def suma(x: Int, y: Int) = if (x + y >= 0) x + y else 1
}