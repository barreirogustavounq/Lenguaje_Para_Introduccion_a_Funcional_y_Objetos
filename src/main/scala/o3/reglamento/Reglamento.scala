package o3.reglamento

import o3.ReglasPredefinidas.{DividirPorCero, OperacionRedundante}

/****************REGLAMENTO******************************/
object  Reglamento {
  var reglas : List[Regla] = List(DividirPorCero, OperacionRedundante)
  def agregarRegla(regla : Regla): Unit ={
    reglas = reglas.appended(regla)
  }
}