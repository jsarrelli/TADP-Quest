import org.scalatest.FunSpec
import org.scalatest.Matchers._

class InvetarioSpec extends FunSpec {

  val inventario = Inventario()
  val statBase = Stats(Fuerza = 20, HP = 10, Inteligencia = 15, Velocidad = 10);
  val heroe1 = Heroe("pepe", statBase)

  describe("equipar") {
    it("equipar una armadura deberia equiparse en el cuerpo") {
      val armadura = ArmaduraEleganteSport
      val result = inventario.equipar(armadura)
      result.cuerpo shouldBe Some(armadura)
    }

    it("equipar un casco y despues un sombrero deberia reemplazar el anterior") {
      val casco = cascoVikingo
      val result = inventario.equipar(casco)
      result.cabeza shouldBe Some(casco)
      val vincha = vinchaBufaloAgua
      val result2 = result.equipar(vincha)
      result2.cabeza shouldBe Some(vincha)
    }

    it("equipar un arma de una sola mano con las dos manos vacias") {
      val arma = palitoMagico
      val result = inventario.equipar(arma)
      result.manos shouldBe List(arma)
    }

    it("equipar dos armas simples con las manos vacias") {
      val arma1 = palitoMagico
      val arma2 = escudoAntiRobo
      val result = inventario.equipar(arma1).equipar(arma2)
      result.manos.size shouldBe 2
    }

    it("equipar un arma doble cuando hay una arma simple deberia quedarse solo con la doble") {
      val armaSimple = palitoMagico
      val armaDoble = arcoViejo
      val result = inventario.equipar(armaSimple).equipar(armaDoble)
      result.manos shouldBe List(armaDoble)
    }

    it("equipar un arma simple cuando hay una arma doble deberia quedarse solo con la simple") {
      val armaSimple = palitoMagico
      val armaDoble = arcoViejo
      val result = inventario.equipar(armaDoble).equipar(armaSimple)
      result.manos shouldBe List(armaSimple)
    }
  }

  describe("items") {
    it("deberia devolverte una lista con todos los items") {
      val casco = cascoVikingo
      val armaSimple = palitoMagico
      val armadura = ArmaduraEleganteSport
      val gema1 = talismanDedicacion
      val gema2 = talismanMaldito
      val result = inventario.equipar(casco).equipar(armaSimple).equipar(armadura).equipar(gema1).equipar(gema2)
      result.items.size shouldBe 5
    }
  }


  describe("stats") {
    it("deberia devolverte la suma de todos los stats") {
      val casco = cascoVikingo
      val armaSimple = palitoMagico
      val armadura = ArmaduraEleganteSport

      val inventario1 = inventario.equipar(casco)
      inventario1.stats(heroe1) shouldBe Some(Stats(HP = 10))

      val inventario2 = inventario1.equipar(armaSimple)
      inventario2.stats(heroe1) shouldBe Some(Stats(HP = 10, Inteligencia = 20))

      val inventario3 = inventario2.equipar(armadura)
      inventario3.stats(heroe1) shouldBe Some(Stats(HP = 1, Inteligencia = 20, Velocidad = 30))

    }
  }
}
