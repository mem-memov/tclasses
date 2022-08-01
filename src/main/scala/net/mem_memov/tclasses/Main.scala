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

object Encoder:

  def apply[A, B](using instance: Encoder[A, B]): Encoder[A, B] = instance // summon

  extension [A, B](a: A)
    def enc(b: B)(using encoder: Encoder[A, B]): B = encoder.encode(a, b)

  given Encoder[Country, Row] with
    override def encode(country: Country, row: Row): Row = row.copy(country = country.name)

  given Encoder[Brand, Row] with
    override def encode(brand: Brand, row: Row): Row =
      val brandRow = row.copy(brand = brand.name)
      Encoder[Country, Row].encode(brand.country, brandRow)

  given Encoder[Size, Row] with
    override def encode(size: Size, row: Row): Row = row.copy(size = size.value)

  given Encoder[Product, Row] with
    override def encode(product: Product, row: Row): Row =
      val sizeRow = Encoder[Size, Row].encode(product.size, row)
      val brandRow = Encoder[Brand, Row].encode(product.brand, sizeRow)
      brandRow.copy(product = product.name)

@main
def main(): Unit =
  import Encoder.*
  val p = Product("кеды", Size("XXL"), Brand("Adidas", Country("USA")))
  val r = p.enc(Row.empty)
  println(r) // Row(кеды,Adidas,XXL,USA)