import org.scalatest.FunSpec
import org.scalatest.Matchers._

class HeroeSpec extends FunSpec {

  val statBase = Stats(HP = 10, Inteligencia = 15, Fuerza = 20, Velocidad = 10)
  val heroe1 = Heroe("pepe", statBase)

  describe("equipar") {
    it("un ladron no deberia poder equipar un escudo antiRobo") {
      val ladron = Heroe("pepe", statBase, Some(Ladron))
      ladron.equiparItem(escudoAntiRobo).inventario.items.contains(escudoAntiRobo) shouldBe false
    }

    it("un mago deberia poder equiparse un palito magico") {
      val mago = Heroe("pepe", statBase, Some(Mago))
      mago.equiparItem(palitoMagico).inventario.items.contains(palitoMagico) shouldBe true
    }
  }

  describe("los stats del Heroe") {
    describe("si no tiene trabajo ni items en el inventario") {
      val heroe = Heroe("pepe", statBase, None, Inventario())
      it("deberian ser solos los statsBase") {
        heroe.stats shouldBe statBase
      }
    }

    describe("si no tiene trabajo pero si items") {
      val arma = arcoViejo
      val armadura = ArmaduraEleganteSport
      val heroe = Heroe("pepe", statBase, None, Inventario()).equiparItem(arma).equiparItem(armadura)
      it("son los stats base mas los items") {
        val expectedStat = statBase + (arma.efecto(heroe) + armadura.efecto(heroe))
        heroe.stats shouldBe expectedStat
      }

      describe("si tiene trabajo e items en el inventario") {
        val arma = arcoViejo
        val armadura = ArmaduraEleganteSport
        val heroe = Heroe("pepe", statBase, Some(Ladron), Inventario()).equiparItem(arma).equiparItem(armadura)
        it("son los stats base mas los items") {
          val expectedStat = statBase + (arma.efecto(heroe) + armadura.efecto(heroe)) + Ladron.stats
          heroe.stats shouldBe expectedStat
        }
      }
    }
  }

}
