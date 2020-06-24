package o3.variableTest

import o3.Programa
import o3.expresiones.{EliminadorDeVariablesSinUso, _}
import o3.gravedad.Advertencia
import o3.motores.Interprete
import o3.problemas.Problema
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class VariableSpec extends AnyFunSpec with Matchers {

  describe("Variables") {
    it("declaro dos variables con el mismo nombre") {
      val analizadorChequeoDuplicado = new AnalizadorVariable()
      val expresiones : List[Expresion] = List(Variable("edad", Numero(27)), Variable("edad", Numero(28)),Variable("anio", Numero(2020)) )
      val programa : Programa = Programa(expresiones)

      val respuesta : List[Problema] = analizadorChequeoDuplicado.analizarVariablesDuplicadas(programa)
      println(respuesta)
      println(programa.variables)
      respuesta.head should equal(Problema(Advertencia, "variable duplicada", Variable("edad", Numero(28))))
    }

    it("Intento referencar una variable antes de declararla ") {
      val analizadorVariable = new AnalizadorVariable()
      val expresiones : List[Expresion] = List(Referencia("noInicializada"), Variable("edad", Numero(27)),Referencia("edad"), Variable("edad", Numero(28)),Variable("anio", Numero(2020)), Variable("noInicializada", Numero(0)))
      val programa : Programa = Programa(expresiones)
      val respuesta : List[Problema] = analizadorVariable.analizarUsoDeVariablesAntesDeSuDeclaracion(programa)
      println(respuesta)
      println(programa.variables)
      respuesta.head should equal(Problema(Advertencia, "Esta referencia no se encuentra definida en una variable", Referencia("noInicializada")))

    }

    it("detecto las varables que no se usan") {
      val analizadorVariable = new AnalizadorVariable()
      val expresiones : List[Expresion] = List(Referencia("noInicializada"),
                                               Variable("edad", Numero(27)),
                                               Referencia("edad"),
                                               Variable("anio", Numero(2020)),
                                               Variable("noInicializada", Numero(0))
                                               )
      val programa : Programa = Programa(expresiones)
      val respuesta : List[Variable] = analizadorVariable.analizarVariablesDelaradasSinUso(programa)
      println(respuesta)
      println(programa.variables)
      respuesta.head should equal(Variable("anio", Numero(2020)))
    }
    it("Chequeo Referencias Validas") {
      val analizadorVariable = new AnalizadorVariable()
      val expresiones : List[Expresion] = List(Referencia("noInicializada"),
        Variable("edad", Numero(27)),
        Referencia("edad"),
        Variable("anio", Numero(2020)),
        Variable("noInicializadas", Numero(0))
      )
      val programa : Programa = Programa(expresiones)
      val respuesta : List[Problema] = analizadorVariable.analizarReferenciaValida(programa)
      println(respuesta)
      println(programa.variables)
      respuesta.head should equal(Problema(Advertencia, "Esta referencia no se encuentra definida en una variable", Referencia("noInicializada")))
    }
  }
  describe("Optimizador de Variables") {
    it("Elimino variables Repetidas") {
      val analizadorVarible = new AnalizadorVariable()
      val expresiones : List[Expresion] = List(Variable("edad", Numero(27)),Variable("anio", Numero(2020)), Referencia("edad") )
      val programa : Programa = Programa(expresiones)


      EliminadorDeVariablesSinUso.optimizar(programa, analizadorVarible)
      println(programa.variables)

      programa.variables.head should equal(Variable("edad", Numero(27)))
    }
  }
}

