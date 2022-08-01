package net.mem_memov.tclasses

import Domain.*
import Row.*

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
