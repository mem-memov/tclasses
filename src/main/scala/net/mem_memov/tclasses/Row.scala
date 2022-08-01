package net.mem_memov.tclasses

case class Row(product: String, brand: String, size: String, country: String)

object Row:
  val empty: Row = Row("", "", "", "")
