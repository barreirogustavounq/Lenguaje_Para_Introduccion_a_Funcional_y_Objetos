package o3.reglamento

import o3.ReglasPredefinidas.{ComparacionesSinSentido, DividirPorCero, OperacionRedundante}

/****************REGLAMENTO******************************/
object  Reglamento {
  var reglas : List[Regla] = List(OperacionRedundante, DividirPorCero, ComparacionesSinSentido)
  def agregarRegla(regla : Regla): Unit ={
    reglas = reglas.appended(regla)
  }
}
