package o3.optimizadorTest

import o3.Programa
import o3.ReglasPredefinidas.{ComparacionesSinSentido, ReglaNoRestarCero, ReglaNoSumarCero}
import o3.expresiones._
import o3.motores.Optimizador
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class optimizadorSpec extends AnyFunSpec with Matchers {

  it("Optimización con ReglaNoSumarCero: convertir 1+0 en 1") {
    val optimizador = Optimizador(ReglaNoSumarCero())
    assert(optimizador.optimizar(Programa(Suma(Numero(0), Numero(1)))) == Programa(Numero(1)))
  }
  it("convertir (1+0)+0 en 1") {
    val optimizador = Optimizador(ReglaNoSumarCero())
    val p = Programa(Suma(Suma(Numero(1), Numero(0)), Numero(0)))
    assert(optimizador.optimizar(p) == Programa(Numero(1)))
  }

  it("Optimización con ReglaNoRestarCero: convertir 1-0 en 1") {
    val optimizador = Optimizador(ReglaNoRestarCero())
    assert(optimizador.optimizar(Programa(Resta(Numero(1), Numero(0)))) == Programa(Numero(1)))
  }

  it("Optimización con ComparacionesSinSentido: convertir 1>0 en true") {
    val optimizador = Optimizador(ComparacionesSinSentido())
    assert(optimizador.optimizar(Programa(Mayor(Numero(1), Numero(0)))) == Programa(Booleano(true)))
  }

  it("convertir 1==1 en true") {
    val optimizador = Optimizador(ComparacionesSinSentido())
    assert(optimizador.optimizar(Programa(Igual(Numero(1), Numero(1)))) == Programa(Booleano(true)))
  }

  it("convertir 1==2 en false") {
    val optimizador = Optimizador(ComparacionesSinSentido())
    assert(optimizador.optimizar(Programa(Igual(Numero(1), Numero(2)))) == Programa(Booleano(false)))
  }
  it("convertir true==true en true") {
    val optimizador = Optimizador(ComparacionesSinSentido())
    assert(optimizador.optimizar(Programa(Igual(Booleano(true), Booleano(true)))) == Programa(Booleano(true)))
  }

  it("convertir true>true en false") {
    val optimizador = Optimizador(ComparacionesSinSentido())
    assert(optimizador.optimizar(Programa(Mayor(Booleano(true), Booleano(true)))) == Programa(Booleano(false)))
  }
}
