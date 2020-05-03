import org.scalatest.FunSpec
import org.scalatest.Matchers._

class EquipoSpec extends FunSpec {

  val heroe1 = Heroe("1", Stats(Fuerza = 20, HP = 10, Inteligencia = 15, Velocidad = 10), Some(Ladron))
  val heroe2 = Heroe("2", Stats(Fuerza = 20, HP = 10, Inteligencia = 40, Velocidad = 10), Some(Mago))
  val heroe3 = Heroe("3", Stats(Fuerza = 20, HP = 10, Inteligencia = 15, Velocidad = 10), Some(Guerrero))

  describe("el lider de un equipo deberia ser") {
    val equipo = Equipo("nombre", List(heroe1, heroe2, heroe3))
    it("el que tiene el que tiene mayor stac principal") {
      equipo.lider shouldBe Some(heroe2)
    }

    describe("si hay dos lideres") {
      val heroe1 = Heroe("1", Stats(Fuerza = 20, HP = 10, Inteligencia = 15, Velocidad = 20), Some(Ladron))
      val heroe2 = Heroe("2", Stats(Fuerza = 20, HP = 10, Inteligencia = 10, Velocidad = 10), Some(Mago))
      val heroe3 = Heroe("3", Stats(Fuerza = 1, HP = 10, Inteligencia = 15, Velocidad = 10), Some(Guerrero))
      val equipo = Equipo("nombre", List(heroe1, heroe2, heroe3))
      it("no deberia haber un lider definido") {
        equipo.lider shouldBe None
      }
    }

    describe(" si no hay intengrante en el equipo") {
      val equipo = Equipo("afa", List())
      it("no deberia haber ningun lider") {
        equipo.lider shouldBe None
      }
    }
  }

  describe("obtener miembro") {
    it("deberia devolver el equipo con un integrante mas") {
      val equipo = Equipo("nombre", List(heroe1, heroe2))
      equipo.obtenerMiembro(heroe3).integrantes.contains(heroe3) shouldBe true
    }
  }

  describe("reemplezar un integrante") {
    val heroe4 = Heroe("4", Stats(Fuerza = 1, HP = 10, Inteligencia = 15, Velocidad = 10), Some(Mago))

    it("deberia reemplazar el integrante si existe") {
      val equipo = Equipo("nombre", List(heroe1, heroe2, heroe3))
      val equipoNuevo = equipo.reemplazarMiembro(heroe4, heroe3)
      equipoNuevo.integrantes.contains(heroe4) shouldBe true
      equipoNuevo.integrantes.contains(heroe3) shouldBe false
    }

    it("no deberia reemplazarlo si no existe") {
      val equipo = Equipo("nombre", List(heroe1, heroe2))
      val equipoNuevo = equipo.reemplazarMiembro(heroe4, heroe3)
      equipoNuevo.integrantes.contains(heroe4) shouldBe false
      equipoNuevo.integrantes.contains(heroe3) shouldBe false
    }
  }

  describe("obtener item") {
    describe("se obtiene un item que favorece los stats de los heroes") {
      val heroe1 = Heroe("1", Stats(Fuerza = 20, HP = 10, Inteligencia = 15, Velocidad = 20), Some(Ladron))
      val heroe2 = Heroe("2", Stats(Fuerza = 20, HP = 10, Inteligencia = 10, Velocidad = 10), Some(Mago))
      val heroe3 = Heroe("3", Stats(Fuerza = 1, HP = 10, Inteligencia = 15, Velocidad = 10), Some(Guerrero))
      val equipo = Equipo("nombre", List(heroe1, heroe2, heroe3))
      it("se lo deberia dar al heroe que mas le favorezca su stat princpipal") {
        val result = equipo.obtenerItem(ArmaduraEleganteSport)
        result.integrantes.find(_.nombre.equals("1")).get.inventario.items.contains(ArmaduraEleganteSport) shouldBe true
      }
    }

    describe("si obtienen un item que no le favorece a nadie") {
      val equipo = Equipo("nombre", List(heroe1, heroe2, heroe3))
      val item = talismanMaldito.copy(precio = 10)
      it("lo deberia vender incrementando el pozo del equipo") {
        equipo.obtenerItem(item).pozoComun shouldBe equipo.pozoComun + item.precio
      }
    }
  }


  describe("mejor heroe segun") {
    val heroe1 = Heroe("1", Stats(Fuerza = 20, HP = 10, Inteligencia = 80, Velocidad = 20), Some(Ladron))
    val equipo = Equipo("nombre", List(heroe1, heroe2, heroe3))
    it("te devuelve el mejor heroe en base a la funcion") {
      val masInteligente = (h: Heroe) => h.stats.Inteligencia
      equipo.mejorHeroe(masInteligente) shouldBe Some(heroe1)
    }
  }

}
