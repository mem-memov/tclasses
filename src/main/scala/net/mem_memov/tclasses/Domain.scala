package net.mem_memov.tclasses

object Domain:
  case class Country(name: String)
  case class Brand(name: String, country: Country)
  case class Size(value: String)
  case class Product(name: String, size: Size, brand: Brand)
