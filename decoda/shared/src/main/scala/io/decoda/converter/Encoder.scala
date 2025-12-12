package io.decoda.converter

import OptOmit.NoOmit
import base.*
import io.decoda.*

import java.util.Date
import scala.collection.mutable

case class EncodeOptions(
    opt: OptOmit = NoOmit,
    pattern: String = "",
    df: Option[DateFormatter] = None
)

trait DataEncoder[T]:
  def encode(v: T): Any

case class EncoderMedaTada[T, S](
    name: String,
    opts: Option[EncodeOptions] = None,
    encoder: DataEncoder[S],
    f: T => S
):
  def fnApply(obj: T): Any =
    encoder.encode(f(obj))

class Encoder[T](using jsonCreator: JsonCreator) extends DataEncoder[T]:
  private val fns = mutable.Buffer.empty[EncoderMedaTada[T, ?]]

  def add(fn: EncoderMedaTada[T, ?]): Encoder[T] =
    fns.addOne(fn)
    this

  override def encode(obj: T): Any =
    fns.foldLeft(jsonCreator.mkObject) { (json, fd) =>
      fd.fnApply(obj) match
        case null =>
          if fd.opts.exists(_.opt == NoOmit)
          then json.setByName(fd.name, null)
        case v =>
          val df = fd.opts.flatMap(_.df)
          val pattern = fd.opts.map(_.pattern)
          if df.isEmpty || pattern.isEmpty
          then json.setByName(fd.name, v)
          else json.setByName(fd.name, df.map(_.format(v.asInstanceOf[Date], pattern.get)).get)
      json
    }

  def encodeObject(obj: T): String =
    encode(obj).asInstanceOf[JsonObject].stringify()

  def encodeArray(items: Seq[T]): String =
    val encodedItems =
      items.map(encode).map(_.asInstanceOf[JsonObject])
    val arr = jsonCreator.mkArray
    arr.addAll(encodedItems)
    arr.stringify()

object Encoder:

  type EncoderField[T, S] = DataEncoder[S] ?=> Encoder[T]
  type EncoderCreator[T] = JsonCreator ?=> Encoder[T]

  given DataEncoder[String]:
    override def encode(v: String): Any = v

  given DataEncoder[Short]:
    override def encode(v: Short): Any = v

  given DataEncoder[Int]:
    override def encode(v: Int): Any = v

  given DataEncoder[Long]:
    override def encode(v: Long): Any = v

  given DataEncoder[Float]:
    override def encode(v: Float): Any = v

  given DataEncoder[Double]:
    override def encode(v: Double): Any = v

  given DataEncoder[Boolean]:
    override def encode(v: Boolean): Any = v

  given DataEncoder[Date]:
    override def encode(v: Date): Any = v

  given OptionEncoderCodec: [T: DataEncoder as encoder] => DataEncoder[Option[T]]:
    override def encode(v: Option[T]): Any =
      v.map(encoder.encode).orNull

  given SeqEncoderCodec: [T: DataEncoder as encoder] => DataEncoder[Seq[T]]:
    override def encode(vs: Seq[T]): Any =
      vs.map(encoder.encode)

  /*
  given ListEncoderCodec[T](using encoder: DataEncoder[T]): DataEncoder[List[T]] with
    override def encode(vs: List[T]): Any =
      vs.map(encoder.encode)
   */

  given SetEncoderCodec: [T: DataEncoder as encoder] => DataEncoder[Set[T]]:
    override def encode(vs: Set[T]): Any =
      vs.map(encoder.encode)

  inline def typ[T]: EncoderCreator[T] =
    new Encoder

  inline def field[T, S](name: String, f: T => S)(encoder: Encoder[T]): EncoderField[T, S] =
    encoder.add(EncoderMedaTada(name, None, summon[DataEncoder[S]], f))

  inline def field[T, S](name: String, f: T => S, opts: EncodeOptions)(encoder: Encoder[T]): EncoderField[T, S] =
    encoder.add(EncoderMedaTada(name, Some(opts), summon[DataEncoder[S]], f))
