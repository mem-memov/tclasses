package net.mem_memov.tclasses

import Domain.*
import Row.*

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
