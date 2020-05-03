import org.scalatest.FunSpec
import org.scalatest.Matchers._

class MisionSpec extends FunSpec {
  val statBase = Stats(Fuerza = 20, HP = 10, Inteligencia = 15, Velocidad = 10);

  describe("realizarMision") {
    val mago = Heroe("Mago", statBase, Some(Mago))
    val guerrero = Heroe("Guerrero", statBase, Some(Guerrero))
    val equipo = Equipo("equipo", List(mago, guerrero))

    describe("si un equipo no puede realizar la mision") {
      val recompensa = (e: Equipo) => e
      val mision = Mision(List(pelearContraMonstruo, robarTalisman), recompensa)
      it("deberia retornar el equipo sin afectar y la tarea que no pudo realizar") {
        mision.realizarMision(equipo).isFailure shouldBe true
      }
    }

    describe("si un equipo puede realizar la mision") {
      val recompensa = (e: Equipo) => e.copy(pozoComun = 122)
      val mision = Mision(List(pelearContraMonstruo, forzarPuerta), recompensa)
      it("deberia retornar el equipo afectado y con la recompensa cobrada") {
        //TODO no tengo idea de como testear bien el either
        val result = mision.realizarMision(equipo)
        result.isSuccess shouldBe true
        val equipoResult = result.get
        equipoResult.integrantes shouldNot be(equipo.integrantes)
        equipoResult.pozoComun shouldBe 122
      }
    }

  }
}
