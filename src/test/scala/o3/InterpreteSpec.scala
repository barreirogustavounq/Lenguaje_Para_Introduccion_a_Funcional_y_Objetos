package o3

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class InterpreteSpec extends AnyFunSpec with Matchers {

  describe("Interprete") {
    it("interprete ejecuta operación suma") {
      val interprete = new Interprete
      var operaciones : List[Operacion] = List()
      operaciones = operaciones.appended(Suma(Numero(2), Numero(3)))
      operaciones = operaciones.appended(Resta(Numero(4), Numero(2)))
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = interprete.ejecutar(programa)
      resultadoDeEjecutarPrograma(0) should equal(Numero(5))
      resultadoDeEjecutarPrograma(1) should equal(Numero(2))
    }
    it("Analiza las operaciones ") {
      val analizador = new Analizador
      var operaciones : List[Operacion] = List()
      operaciones = operaciones.appended(Suma(Numero(2), Numero(0)))
      operaciones = operaciones.appended(Resta(Numero(4), Numero(0)))
      operaciones = operaciones.appended(Multiplicacion(Numero(1), Numero(4)))
      operaciones = operaciones.appended(Division(Numero(4), Numero(0)))
      operaciones = operaciones.appended(Division(Numero(4), Numero(2)))
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = analizador.analizar(programa).map(listaRespuestas => listaRespuestas.map(r => r.gravedad))
      resultadoDeEjecutarPrograma(3).contains(Error) should equal(true)
      resultadoDeEjecutarPrograma(0).contains(Advertencia) should equal(true)
      resultadoDeEjecutarPrograma(1).contains(Advertencia) should equal(true)
      resultadoDeEjecutarPrograma(2).contains(Advertencia) should equal(true)
    }

//    it("interprete ejecuta operación resta") {
//      val interprete = new Interprete
//      val programa = Programa(Resta(Numero(10), Numero(6)))
//
//      interprete.ejecutar(programa) should equal(Numero(4))
//    }
//
//    it("interprete ejecuta operación multiplicacion") {
//      val interprete = new Interprete
//      val programa = Programa(Multiplicacion(Numero(2), Numero(3)))
//
//      interprete.ejecutar(programa) should equal(Numero(6))
//    }
//
//    it("interprete ejecuta operación Division") {
//      val interprete = new Interprete
//      val programa = Programa(Division(Numero(10), Numero(2)))
//
//      interprete.ejecutar(programa) should equal(Numero(5))
//    }

    it("optimizar operaciones de suma") {
      val optimizador = new Optimizador
      val operacion = Suma(Numero(10), Numero(0))
      val operacion2 = Suma(Numero(0), Numero(10))

      optimizador.optimizarSuma(operacion) should equal(Numero(10))
      optimizador.optimizarSuma(operacion) should equal(Numero(10))
    }

    it("optimizar operaciones de resta") {
      val optimizador = new Optimizador
      val operacion = Resta(Numero(10), Numero(0))

      optimizador.optimizarResta(operacion) should equal(Numero(10))
    }
    
    it("optimizar operaciones de multiplicacion") {
      val optimizador = new Optimizador
      val operacion = Multiplicacion(Numero(10), Numero(0))
      val operacion2 = Multiplicacion(Numero(0), Numero(10))
      val operacion3 = Multiplicacion(Numero(1), Numero(10))
      val operacion4 = Multiplicacion(Numero(10), Numero(1))    

      optimizador.optimizarMultiplicacion(operacion) should equal(Numero(0))
      optimizador.optimizarMultiplicacion(operacion2) should equal(Numero(0))      
      optimizador.optimizarMultiplicacion(operacion3) should equal(Numero(10))
      optimizador.optimizarMultiplicacion(operacion4) should equal(Numero(10))      
    }

    it("optimizar operaciones de division") {
      val optimizador = new Optimizador
      val operacion = Division(Numero(0), Numero(10))
      val operacion2 = Division(Numero(10), Numero(1))      

      optimizador.optimizarDivision(operacion) should equal(Numero(0))
      optimizador.optimizarDivision(operacion2) should equal(Numero(10))
    }

    it("optimizar comparaciones") {
      val optimizador = new Optimizador
      val comparacion = Igual(Numero(2), Numero(2))
      val comparacion2 = Mayor(Numero(3), Numero(1))
      val comparacion3 = Menor(Numero(2), Numero(3))
      val comparacion4 = Mayor(Numero(3), Numero(4))
      val comparacion5 = Menor(Numero(3), Numero(2))
      

      optimizador.optimizarIgualdad(comparacion) should equal(Booleano(true))
      optimizador.optimizarMayor(comparacion2) should equal(Booleano(true))
      optimizador.optimizarMenor(comparacion3) should equal(Booleano(true))
      optimizador.optimizarMayor(comparacion4) should equal(Booleano(false))
      optimizador.optimizarMenor(comparacion5) should equal(Booleano(false))
    }
        
  }
}
