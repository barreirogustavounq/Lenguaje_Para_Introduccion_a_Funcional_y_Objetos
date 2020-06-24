package o3.interpreteTest

import o3.Programa
import o3.expresiones._
import o3.motores.Interprete
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class InterpreteSpec extends AnyFunSpec with Matchers {

  describe("Interprete") {
    it("interprete ejecuta operación suma") {
      val interprete = new Interprete
      val operaciones: List[Operacion] = List(
        Suma(Numero(2), Numero(3)),
        Suma(Numero(4), Numero(2))
      )
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = interprete.ejecutar(programa)
      resultadoDeEjecutarPrograma(0) should equal(Numero(5))
      resultadoDeEjecutarPrograma(1) should equal(Numero(6))
    }
    it("interprete ejecuta operación resta") {
      val interprete = new Interprete
      val operaciones: List[Operacion] = List(
        Resta(Numero(7), Numero(3)),
        Resta(Numero(4), Numero(2))
      )
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = interprete.ejecutar(programa)
      resultadoDeEjecutarPrograma(0) should equal(Numero(4))
      resultadoDeEjecutarPrograma(1) should equal(Numero(2))
    }
    it("interprete ejecuta operación Multuplicacion") {
      val interprete = new Interprete
      val operaciones: List[Operacion] = List(
        Multiplicacion(Numero(7), Numero(3)),
        Multiplicacion(Numero(4), Numero(2))
      )
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = interprete.ejecutar(programa)
      resultadoDeEjecutarPrograma(0) should equal(Numero(21))
      resultadoDeEjecutarPrograma(1) should equal(Numero(8))
    }
    it("interprete ejecuta operación Division") {
      val interprete = new Interprete
      val operaciones: List[Operacion] = List(
        Division(Numero(12), Numero(3)),
        Division(Numero(4), Numero(2))
      )
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = interprete.ejecutar(programa)
      resultadoDeEjecutarPrograma(0) should equal(Numero(4))
      resultadoDeEjecutarPrograma(1) should equal(Numero(2))
    }
    it("interprete ejecuta operación Mayor, Menor, Igual, Distinto, MayorOIgual, MenorOIgual") {
      val interprete = new Interprete
      val operaciones: List[Operacion] = List(
        Mayor(Numero(12), Numero(3)),
        Mayor(Numero(4), Numero(8)),
        Menor(Numero(12), Numero(15)),
        Menor(Numero(4), Numero(1)),
        Igual(Numero(12), Numero(12)),
        Igual(Numero(4), Numero(1)),
        Distinto(Numero(11), Numero(12)),
        Distinto(Numero(4), Numero(4)),
        MayorOIgual(Numero(12), Numero(12)),
        MayorOIgual(Numero(12), Numero(11)),
        MayorOIgual(Numero(2), Numero(6)),
        MenorOIgual(Numero(11), Numero(11)),
        MenorOIgual(Numero(1), Numero(11)),
        MenorOIgual(Numero(28), Numero(6))
      )
      val programa = Programa(operaciones)
      val resultadoDeEjecutarPrograma = interprete.ejecutar(programa)
      resultadoDeEjecutarPrograma(0) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(1) should equal(Booleano(false))
      resultadoDeEjecutarPrograma(2) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(3) should equal(Booleano(false))
      resultadoDeEjecutarPrograma(4) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(5) should equal(Booleano(false))
      resultadoDeEjecutarPrograma(6) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(7) should equal(Booleano(false))
      resultadoDeEjecutarPrograma(8) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(9) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(10) should equal(Booleano(false))
      resultadoDeEjecutarPrograma(11) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(12) should equal(Booleano(true))
      resultadoDeEjecutarPrograma(13) should equal(Booleano(false))
    }
    it("interprete ejecuta operación desconocida") {
      val interprete = new Interprete
      case class Desconocido(n1: Numero, n2: Numero) extends Operacion(n1, n2)
      val operaciones: List[Operacion] = List(
        Desconocido(Numero(12), Numero(4))
      )
      val programa = Programa(operaciones)
      assertThrows[UnsupportedOperationException] {
        interprete.ejecutar(programa)
      }
    }
  }
  describe("Interprete con variables") {
    it("interprete ejecuta operaciones con Variables (al ejecutar se guarda una variable)") {
      val interprete = new Interprete
      val operaciones: List[Expresion] = List(Variable("edad", Numero(29)))
      val programa = Programa(operaciones)
      interprete.ejecutar(programa)
      println(programa.variables)
      programa.variables.size should equal(1)
      programa.variables.head should equal(Variable("edad", Numero(29)))
    }
    it("probando la funcionalidad de Asignar"){
      val interprete = new Interprete
      val operaciones: List[Expresion] = List(Variable("edad", Numero(29)), Asignar(Referencia("edad"), Numero(30)))
      val programa = Programa(operaciones)
      interprete.ejecutar(programa)
      println(programa.variables)
      programa.variables.size should equal(1)
      programa.variables.head.valor should equal(Numero(30))
    }

    it("probando referencia con operacion Resta"){
      val interprete = new Interprete
      val operaciones: List[Expresion] = List(Variable("edad", Numero(29)),
                                              Variable("anioActual", Numero(2020)),
                                              Resta(Referencia("anioActual"), Referencia("edad")))
      val programa = Programa(operaciones)
      interprete.ejecutar(programa).last should equal(Numero(1991))
      println(programa.variables)
      programa.variables.size should equal(2)
    }
    it("probando referencia con operacion Suma"){
      val interprete = new Interprete
      val operaciones: List[Expresion] = List(Variable("edad", Numero(29)),
        Variable("anioNacimiento", Numero(1991)),
        Suma(Referencia("anioNacimiento"), Referencia("edad")))
      val programa = Programa(operaciones)
      interprete.ejecutar(programa).last should equal(Numero(2020))
      println(programa.variables)
      programa.variables.size should equal(2)
    }
    it("probando referencia con operacion multiplicacion"){
      val interprete = new Interprete
      val operaciones: List[Expresion] = List(Variable("hora", Numero(4)),
        Variable("minuto", Numero(60)),
        Multiplicacion(Referencia("hora"), Referencia("minuto")))
      val programa = Programa(operaciones)
      interprete.ejecutar(programa).last should equal(Numero(240))
      println(programa.variables)
      programa.variables.size should equal(2)
    }
    it("probando referencia con operacion Division"){
      val interprete = new Interprete
      val operaciones: List[Expresion] = List(Variable("pizza", Numero(8)),
        Variable("personas", Numero(2)),
        Division(Referencia("pizza"), Referencia("personas")))
      val programa = Programa(operaciones)
      interprete.ejecutar(programa).last should equal(Numero(4))
      println(programa.variables)
      programa.variables.size should equal(2)
    }
    it("probando referencia con operacion Mayor"){
      val interprete = new Interprete
      val operaciones: List[Expresion] = List(Variable("8", Numero(8)),
        Variable("2", Numero(2)), Variable("dos", Numero(2)),
        Mayor(Referencia("8"), Referencia("2")))
      val programa = Programa(operaciones)
      interprete.ejecutar(programa).last should equal(Booleano(true))
      println(programa.variables)
      programa.variables.size should equal(3)
    }
    it("probando referencia con operacion Menor"){
      val interprete = new Interprete
      val operaciones: List[Expresion] = List(Variable("8", Numero(8)),
        Variable("2", Numero(2)), Variable("dos", Numero(2)),
        Menor(Referencia("8"), Referencia("2")))
      val programa = Programa(operaciones)
      interprete.ejecutar(programa).last should equal(Booleano(false))
      println(programa.variables)
      programa.variables.size should equal(3)
    }
    it("probando referencia con operacion Igual"){
      val interprete = new Interprete
      val operaciones: List[Expresion] = List(Variable("8", Numero(8)),
        Variable("2", Numero(2)), Variable("dos", Numero(2)),
        Igual(Referencia("8"), Referencia("2")))
      val programa = Programa(operaciones)
      interprete.ejecutar(programa).last should equal(Booleano(false))
      println(programa.variables)
      programa.variables.size should equal(3)
    }
    it("probando referencia con operacion Distinto"){
      val interprete = new Interprete
      val operaciones: List[Expresion] = List(Variable("8", Numero(8)),
        Variable("2", Numero(2)), Variable("dos", Numero(2)),
        Distinto(Referencia("8"), Referencia("2")))
      val programa = Programa(operaciones)
      interprete.ejecutar(programa).last should equal(Booleano(true))
      println(programa.variables)
      programa.variables.size should equal(3)
    }
    it("probando referencia con operacion MayorOIgual"){
      val interprete = new Interprete
      val operaciones: List[Expresion] = List(Variable("8", Numero(8)),
        Variable("2", Numero(2)), Variable("dos", Numero(2)),
        MayorOIgual(Referencia("8"), Referencia("2")))
      val programa = Programa(operaciones)
      interprete.ejecutar(programa).last should equal(Booleano(true))
      println(programa.variables)
      programa.variables.size should equal(3)
    }
    it("probando referencia con operacion MenorOIgual"){
      val interprete = new Interprete
      val operaciones: List[Expresion] = List(Variable("8", Numero(8)),
        Variable("2", Numero(2)), Variable("dos", Numero(2)),
        MenorOIgual(Referencia("8"), Referencia("2")))
      val programa = Programa(operaciones)
      interprete.ejecutar(programa).last should equal(Booleano(false))
      println(programa.variables)
      programa.variables.size should equal(3)
    }
    it("probando referencia con operacion desconocida"){
      val interprete = new Interprete
      val operaciones: List[Expresion] = List(Variable("8", Numero(8)),
        Variable("2", Numero(2)), Variable("dos", Numero(2)),
        Menor(Referencia("8"), Referencia("no existe")))
      val programa = Programa(operaciones)
      assertThrows[UnsupportedOperationException] {
        interprete.ejecutar(programa)
      }
    }
  }
}
