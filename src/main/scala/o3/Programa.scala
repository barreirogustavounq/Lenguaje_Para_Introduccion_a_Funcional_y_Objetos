package o3


case class Programa(elementos : List[Operacion]) {

  trait elementos {
    def operaciones() : List[Operacion]
  }

}

