package o3.expresiones

class Comparacion(_a: Valor, _b: Valor, _fn: (Literal, Literal) => Booleano)
  extends OperacionBinaria[Literal, Literal, Booleano](_a, _b, _fn)

case class Mayor(a: Valor, b: Valor) extends Comparacion(a, b, {
  (a: Literal, b: Literal) => (a, b) match {
    case (a: Booleano, b: Booleano) => Booleano(a.valor > b.valor)
    case (a: Numero, b: Numero) => Booleano(a.valor > b.valor)
  }
})

case class Menor(a: Valor, b: Valor) extends Comparacion(a, b, {
  (a: Literal, b: Literal) => (a, b) match {
    case (a: Booleano, b: Booleano) => Booleano(a.valor < b.valor)
    case (a: Numero, b: Numero) => Booleano(a.valor < b.valor)
  }
})

case class Igual(a: Valor, b: Valor) extends Comparacion(a, b, {
  (a: Literal, b: Literal) => (a, b) match {
    case (a: Booleano, b: Booleano) => Booleano(a.valor == b.valor)
    case (a: Numero, b: Numero) => Booleano(a.valor == b.valor)
  }
})

case class Distinto(a: Valor, b: Valor) extends Comparacion(a, b, {
  (a: Literal, b: Literal) => (a, b) match {
    case (a: Booleano, b: Booleano) => Booleano(a.valor != b.valor)
    case (a: Numero, b: Numero) => Booleano(a.valor != b.valor)
  }
})

case class MayorOIgual(a: Numero, b: Numero) extends Comparacion(a, b, {
  (a: Literal, b: Literal) => (a, b) match {
    case (a: Booleano, b: Booleano) => Booleano(a.valor >= b.valor)
    case (a: Numero, b: Numero) => Booleano(a.valor >= b.valor)
  }
})

case class MenorOIgual(a: Numero, b: Numero) extends Comparacion(a, b, {
  (a: Literal, b: Literal) => (a, b) match {
    case (a: Booleano, b: Booleano) => Booleano(a.valor <= b.valor)
    case (a: Numero, b: Numero) => Booleano(a.valor <= b.valor)
  }
})
