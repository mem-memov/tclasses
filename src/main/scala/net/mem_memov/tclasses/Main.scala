package net.mem_memov.tclasses

import Domain.*
import Row.*
import Encoder.*
import Decoder.*

@main
def main(): Unit =

  val product = Product("кеды", Size("XXL"), Brand("Adidas", Country("USA")))
  val row = product.enc(Row.empty)
  println(row) // Row(кеды,Adidas,XXL,USA)

  val restoredProduct = row.dec[Product]
  println(restoredProduct) // Product(кеды,Size(XXL),Brand(Adidas,Country(USA)))