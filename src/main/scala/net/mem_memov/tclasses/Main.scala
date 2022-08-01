package net.mem_memov.tclasses

// domain classes
case class Country(name: String)
case class Brand(name: String, country: Country)
case class Size(value: String)
case class Product(name: String, size: Size, brand: Brand)

// result type
case class Row(product: String, brand: String, size: String, country: String)
object Row:
  val empty: Row = Row("", "", "", "")

// type class
trait Encoder[A, B]:

  def encode(a: A, b: B): B

//  extension (a: A)
//    def enc(b: B)(using encoder: Encoder[A, B]): B = encoder.encode(a, b)

object Encoder:

  given countryEncoder: Encoder[Country, Row] with
    override def encode(country: Country, row: Row): Row = row.copy(country = country.name)

  given brandEncoder (using countryEncoder: Encoder[Country, Row]): Encoder[Brand, Row] with
    override def encode(brand: Brand, row: Row): Row =
      val brandRow = row.copy(brand = brand.name)
      countryEncoder.encode(brand.country, brandRow)

  given sizeEncoder: Encoder[Size, Row] with
    override def encode(size: Size, row: Row): Row = row.copy(size = size.value)

  given productEncoder (using sizeEncoder: Encoder[Size, Row], brandEncoder: Encoder[Brand, Row]): Encoder[Product, Row] with
    override def encode(product: Product, row: Row): Row =
      val sizeRow = sizeEncoder.encode(product.size, row)
      val brandRow = brandEncoder.encode(product.brand, sizeRow)
      brandRow.copy(product = product.name)

@main
def main(): Unit =
  import Encoder.*
  val p = Product("кеды", Size("XXL"), Brand("Adidas", Country("USA")))
  val r = Encoder.productEncoder.encode(p, Row.empty)
  println(r) // Row(кеды,Adidas,XXL,USA)