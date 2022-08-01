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

trait Decoder[A, B]:

  def decode(b: B): A

object Decoder:

  def apply[A, B](using instance: Decoder[A, B]): Decoder[A, B] = instance // summon

  extension [B](b: B)
    def dec[A](using Decoder: Decoder[A, B]): A = Decoder.decode(b)

  given Decoder[Country, Row] with
    override def decode(row: Row): Country = Country(row.country)

  given Decoder[Brand, Row] with
    override def decode(row: Row): Brand =
      Brand(row.brand, Decoder[Country, Row].decode(row))

  given Decoder[Size, Row] with
    override def decode(row: Row): Size = Size(row.size)

  given Decoder[Product, Row] with
    override def decode(row: Row): Product =
      Product(
        row.product,
        Decoder[Size, Row].decode(row),
        Decoder[Brand, Row].decode(row)
      )

@main
def main(): Unit =
  import Encoder.*
  val product = Product("кеды", Size("XXL"), Brand("Adidas", Country("USA")))
  val row = product.enc(Row.empty)
  println(row) // Row(кеды,Adidas,XXL,USA)

  import Decoder.*
  val restoredProduct = row.dec[Product]
  println(restoredProduct) // Product(кеды,Size(XXL),Brand(Adidas,Country(USA)))