package io.decoda.converter.auto

import io.decoda.converter.base
import io.decoda.converter.base.{Json, JsonArray, JsonCreator, JsonObject, JsonValue}
import io.decoda.{JsonCodecException, defs}
import io.decoda.infra.JsonFields
import io.decoda.infra.macros.{getFieldsDefaults, getJsonFields}

import java.util.Date
import scala.annotation.implicitNotFound
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag

/** Typeclass for converting between Scala.js and native JavaScript.
  *
  * @tparam A
  *   the type to convert
  */
@implicitNotFound("Could not find an implicit NativeConverter[${A}]")
trait JsonConverter[A]:
  extension (a: A)
    /** Convert type A to a JSON string */
    def toJson: String

    def toJsonValue: Json

  /** Convert a Json String to type A. Returns either A, or a String error.
    */
  def fromJson(json: Any): A

object JsonConverter:

  inline def apply[A](using nc: JsonConverter[A]): JsonConverter[A] = nc

  private type ImplicitlyAny =
    String | Boolean | Float | Double | Short | Int | Long | Null | Date

  given intConv: JsonConverter[Int] = new JsonConverter[Int] {
    extension (a: Int)
      override def toJsonValue: Json = JsonValue(a)
      override def toJson: String = a.toString

    override def fromJson(value: Any): Int =
      value match
        case _: Int   => value.asInstanceOf[Int]
        case _: Long  => value.asInstanceOf[Long].toInt
        case _: Short => value.asInstanceOf[Short].toInt
        case _        => throw new JsonCodecException(s"can't parse ${value} to Int")
  }

  given longConv: JsonConverter[Long] = new JsonConverter[Long] {
    extension (a: Long)
      override def toJsonValue: Json = JsonValue(a)
      override def toJson: String = a.toString

    override def fromJson(value: Any): Long =
      value match
        case _: Int   => value.asInstanceOf[Int].toLong
        case _: Long  => value.asInstanceOf[Long]
        case _: Short => value.asInstanceOf[Short].toLong
        case _        => throw new JsonCodecException(s"can't parse ${value} to Int")
  }

  given stringConv: JsonConverter[String] = new JsonConverter[String] {
    extension (a: String)
      override def toJsonValue: Json = JsonValue(a)
      override def toJson: String = a

    override def fromJson(value: Any): String =
      try value.asInstanceOf[String]
      catch
        case _ =>
          throw new JsonCodecException(s"can't parse ${value} to String")
  }

  given BooleanConv: JsonConverter[Boolean] = new JsonConverter[Boolean] {

    extension (a: Boolean)
      override def toJsonValue: Json = JsonValue(a)
      override def toJson: String = a.toString

    override def fromJson(value: Any): Boolean =
      try value.asInstanceOf[Boolean]
      catch
        case _ =>
          throw new JsonCodecException(s"can't parse ${value} to Boolean")
  }

  given shortConv: JsonConverter[Short] = new JsonConverter[Short] {
    extension (a: Short)
      override def toJsonValue: Json = JsonValue(a)
      override def toJson: String = a.toString

    override def fromJson(value: Any): Short =
      value match
        case _: Int   => value.asInstanceOf[Int].toShort
        case _: Long  => value.asInstanceOf[Long].toShort
        case _: Short => value.asInstanceOf[Short]
        case _        => throw new JsonCodecException(s"can't parse ${value} to Short")
  }

  given doubleConv: JsonConverter[Double] = new JsonConverter[Double] {
    extension (a: Double)
      override def toJsonValue: Json = JsonValue(a)
      override def toJson: String = a.toString

    override def fromJson(value: Any): Double =
      value match
        case _: Double => value.asInstanceOf[Double]
        case _: Float  => value.asInstanceOf[Float].toDouble
        case _ =>
          throw new JsonCodecException(s"can't parse ${value} to Double")
  }

  given floatConv: JsonConverter[Float] = new JsonConverter[Float] {
    extension (a: Float)
      override def toJsonValue: Json = JsonValue(a)
      override def toJson: String = a.toString

    override def fromJson(value: Any): Float =
      value match
        case _: Double => value.asInstanceOf[Double].toFloat
        case _: Float  => value.asInstanceOf[Float]
        case _ =>
          throw new JsonCodecException(s"can't parse ${value} to Float")
  }

  given nullConv: JsonConverter[Null] = new JsonConverter[Null] {
    extension (a: Null)
      override def toJsonValue: Json = JsonValue(a)
      override def toJson: String = "null"

    override def fromJson(value: Any): Null =
      try value.asInstanceOf[Null]
      catch
        case _ =>
          throw new JsonCodecException(s"can't parse ${value} to Null")
  }

  inline given dateConv: JsonConverter[Date] = new JsonConverter[Date] {
    extension (a: Date)
      override def toJsonValue: Json = JsonValue(a.toString)
      override def toJson: String = a.toString

    override def fromJson(value: Any): Date =
      value match
        case _: Date => value.asInstanceOf[Date]
        case null    => null.asInstanceOf[Date]
        case _ =>
          throw new JsonCodecException(s"can't parse ${value} to Date")
  }

  given optionCodec: [A: JsonConverter] => JsonConverter[Option[A]] = new JsonConverter[Option[A]] {
    extension (o: Option[A])
      override def toJsonValue: Json =
        o.map(s => JsonValue(s)).orNull

      def toJson: String = o.map(_.toJson).orNull

    def fromJson(value: Any): Option[A] =
      value match
        case null => None
        case _    => Some(JsonConverter[A].fromJson(value))
  }

  given mapConv: [A: JsonConverter] => JsonCreator => JsonConverter[Map[String, A]] {
    extension (m: Map[String, A])
      override def toJsonValue: Json =
        summon[JsonCreator].fromMap(m)

      def toJson: String =
        summon[JsonCreator].fromMap(m).stringify()

    def fromJson(value: Any): Map[String, A] =
      value match
        case obj: JsonObject =>
          jsonObjectToMap(obj)
        case _ => Map.empty
  }

  given immutableMapConv: [A: JsonConverter] => JsonCreator => JsonConverter[immutable.Map[String, A]] =
    new JsonConverter[immutable.Map[String, A]] {
      extension (m: immutable.Map[String, A])
        def toJsonValue: Json =
          summon[JsonCreator].fromMap(m)

        def toJson: String =
          summon[JsonCreator].fromMap(m).stringify()

      def fromJson(value: Any): Map[String, A] =
        value match
          case obj: JsonObject =>
            immutable.Map.from(jsonObjectToMap(obj))
          case _ => immutable.Map.empty
    }

  given arrayConv: [A: {ClassTag, JsonConverter}] => JsonCreator => JsonConverter[Array[A]] =
    new JsonConverter[Array[A]] {
      extension (a: Array[A])
        def toJsonValue: Json =
          summon[JsonCreator].fromIterable(a)

        def toJson: String =
          summon[JsonCreator].fromIterable(a).stringify()

      def fromJson(value: Any): Array[A] =
        value match
          case arr: JsonArray =>
            jsonArrayToCollection(arr, Array.newBuilder)
          case _ => Array.empty
    }

  given iterableConv: [A: JsonConverter] => JsonCreator => JsonConverter[Iterable[A]] = new JsonConverter[Iterable[A]] {
    extension (a: Iterable[A])
      def toJsonValue: Json =
        summon[JsonCreator].fromIterable(a)

      def toJson: String =
        summon[JsonCreator].fromIterable(a).stringify()

    def fromJson(value: Any): Iterable[A] =
      value match
        case arr: JsonArray =>
          jsonArrayToCollection(arr, ArrayBuffer.newBuilder)
        case _ => ArrayBuffer.empty
  }

  given seqConv: [A: JsonConverter] => JsonCreator => JsonConverter[Seq[A]] = new JsonConverter[Seq[A]] {
    extension (a: Seq[A])
      def toJsonValue: Json =
        summon[JsonCreator].fromIterable(a)

      def toJson: String =
        summon[JsonCreator].fromIterable(a).stringify()

    def fromJson(value: Any): Seq[A] =
      value match
        case arr: JsonArray =>
          jsonArrayToCollection(arr, Seq.newBuilder)
        case _ => Seq.empty
  }

  given immutableSeqConv: [A: JsonConverter] => JsonCreator => JsonConverter[immutable.Seq[A]] =
    new JsonConverter[immutable.Seq[A]] {
      extension (a: immutable.Seq[A])
        def toJsonValue: Json =
          summon[JsonCreator].fromIterable(a)

        def toJson: String =
          summon[JsonCreator].fromIterable(a).stringify()

      def fromJson(value: Any): immutable.Seq[A] =
        value match
          case arr: JsonArray =>
            jsonArrayToCollection(arr, immutable.Seq.newBuilder)
          case _ => Seq.empty
    }

  given setCodec: [A: JsonConverter] => JsonCreator => JsonConverter[mutable.Set[A]] =
    new JsonConverter[mutable.Set[A]] {
      extension (a: mutable.Set[A])
        def toJsonValue: Json =
          summon[JsonCreator].fromIterable(a)

        def toJson: String =
          summon[JsonCreator].fromIterable(a).stringify()

      def fromJson(value: Any): mutable.Set[A] =
        value match
          case arr: JsonArray =>
            jsonArrayToCollection(arr, mutable.HashSet.newBuilder)
          case _ => mutable.HashSet.empty
    }

  given immutableSetCodec: [A: JsonConverter] => JsonCreator => JsonConverter[Set[A]] = new JsonConverter[Set[A]] {
    extension (a: Set[A])
      def toJsonValue: Json =
        summon[JsonCreator].fromIterable(a)

      def toJson: String =
        summon[JsonCreator].fromIterable(a).stringify()

    def fromJson(value: Any): Set[A] =
      value match
        case arr: JsonArray =>
          jsonArrayToCollection(arr, immutable.HashSet.newBuilder)
        case _ => immutable.HashSet.empty
  }

  given listConv: [A: JsonConverter] => JsonCreator => JsonConverter[List[A]] = new JsonConverter[List[A]] {
    extension (a: List[A])
      def toJsonValue: Json =
        summon[JsonCreator].fromIterable(a)

      def toJson: String =
        summon[JsonCreator].fromIterable(a).stringify()

    def fromJson(value: Any): List[A] =
      value match
        case arr: JsonArray =>
          jsonArrayToCollection(arr, List.newBuilder)
        case _ => List.empty
  }

  given vectorConv: [A: JsonConverter] => JsonCreator => JsonConverter[Vector[A]] = new JsonConverter[Vector[A]] {
    extension (a: Vector[A])
      def toJsonValue: Json =
        summon[JsonCreator].fromIterable(a)

      def toJson: String =
        summon[JsonCreator].fromIterable(a).stringify()

    def fromJson(value: Any): Vector[A] =
      value match
        case arr: JsonArray =>
          jsonArrayToCollection(arr, Vector.newBuilder)
        case _ => Vector.empty
  }

  given bufferConv: [A: JsonConverter] => JsonCreator => JsonConverter[mutable.Buffer[A]] =
    new JsonConverter[mutable.Buffer[A]] {
      extension (a: mutable.Buffer[A])
        def toJsonValue: Json =
          summon[JsonCreator].fromIterable(a)

        def toJson: String =
          summon[JsonCreator].fromIterable(a).stringify()

      def fromJson(value: Any): mutable.Buffer[A] =
        value match
          case arr: JsonArray =>
            jsonArrayToCollection(arr, mutable.Buffer.newBuilder)
          case _ => mutable.Buffer.empty
    }

  private def jsonObjectToMap[A](obj: JsonObject)(using
      nc: JsonConverter[A]
  ): Map[String, A] =
    obj.toMap.map { (k, v) =>
      k -> nc.fromJson(v)
    }

  private def jsonArrayToCollection[A, C[_]](
      arr: JsonArray,
      builder: mutable.Builder[A, C[A]]
  )(using
      nc: JsonConverter[A]
  ): C[A] =
    builder.sizeHint(arr.size)
    for it <- arr.toSeq do builder += nc.fromJson(it)
    builder.result()

  /** Derive a NativeConverter for type T. This method is called by the compiler automatically when adding `derives
    * NativeConverter` on a class. You can also use it to derive given instances anywhere, which is useful if
    * Cross-Building a Scala.js project: <br> `given NativeConverter[User] \= NativeConverter.derived` <br> Only Sum and
    * Product types are supported
    */
  inline given derived: [A: Mirror.Of as m] => JsonConverter[A] =
    type Mets = m.MirroredElemTypes
    type Mels = m.MirroredElemLabels
    type Label = m.MirroredLabel

    val fields = getJsonFields[A]
    val defaults = getFieldsDefaults[A]

    inline m match
      case p: Mirror.ProductOf[A] =>
        new JsonConverter[A]:
          extension (a: A)
            def toJsonObject: JsonObject =
              productToJson[A, Mets, Mels](
                a.asInstanceOf[Product],
                fields = fields,
                jsonObj = defs.JsonObject()
              )

            override def toJsonValue: Json =
              toJsonObject

            override def toJson: String =
              toJsonObject.stringify()

          override def fromJson(json: Any): A =

            val resArr = Array.ofDim[Any](constValue[Tuple.Size[Mets]])
            json match
              case s: String =>
                defs.Json.parse(s) match
                  case obj: JsonObject =>
                    jsonToProduct[A, Mets, Mels](
                      p,
                      resArr,
                      obj,
                      fields = fields,
                      defaults = defaults
                    )
                  case arr: JsonArray =>
                    throw new JsonCodecException("array value not supported")
                  case _ =>
                    throw new JsonCodecException("ast value not supported")
              case null =>
                null.asInstanceOf[A]
              case obj: JsonObject =>
                jsonToProduct[A, Mets, Mels](
                  p,
                  resArr,
                  obj,
                  fields = fields,
                  defaults = defaults
                )
              case _ =>
                throw new JsonCodecException(s"value ${json} not supported")

      case s: Mirror.SumOf[A] =>
        throw new JsonCodecException("ADT not supported")

  private inline def productToJson[A, Mets, Mels](
      p: Product,
      i: Int = 0,
      jsonObj: JsonObject,
      fields: JsonFields[A]
  ): JsonObject =
    inline (erasedValue[Mets], erasedValue[Mels]) match
      // base case
      case _: (EmptyTuple, EmptyTuple) => jsonObj

      case _: (ImplicitlyAny *: metsTail, mel *: melsTail) =>
        val fieldName = constValue[mel & String]

        fields
          .findJsonName(fieldName)
          .foreach: k =>
            val elem = p.productElement(i)
            val value = fields.toNative(fieldName, elem).getOrElse(elem)
            val fd = fields.findField(fieldName)
            if value == null && fd.nonEmpty
            then
              if !fd.get.omitNull
              then jsonObj.setByName(k, value)
            else jsonObj.setByName(k, value)

        productToJson[A, metsTail, melsTail](p, i + 1, jsonObj, fields)

      case _: (met *: metsTail, mel *: melsTail) =>
        val fieldName = constValue[mel & String]
        val nc = summonInline[JsonConverter[met]]
        val nativeElem = p.productElement(i)
        val omitNull = fields.findField(fieldName).exists(_.omitNull)

        fields
          .findJsonName(fieldName)
          .foreach: k =>
            if nativeElem == null
            then
              if !omitNull then jsonObj.setByName(k, null)
            else
              nc.toJsonValue(nativeElem.asInstanceOf[met]) match
                case obj: JsonObject =>
                  jsonObj.setByName(k, obj)
                case arr: JsonArray =>
                  throw new JsonCodecException("array value not supported")
                case null =>
                  if !omitNull then jsonObj.setByName(k, null)
                case v =>
                  jsonObj.setByName(k, v)

        productToJson[A, metsTail, melsTail](p, i + 1, jsonObj, fields)

  private inline def jsonToProduct[A, Mets, Mels](
      mirror: Mirror.ProductOf[A],
      values: Array[Any],
      jsonObject: JsonObject,
      i: Int = 0,
      fields: JsonFields[A],
      defaults: Map[String, Any]
  ): A =
    inline (erasedValue[Mets], erasedValue[Mels]) match
      case _: (EmptyTuple, EmptyTuple) =>
        mirror.fromProduct(ArrayProduct(values))

      case _: (met *: metsTail, mel *: melsTail) =>
        val nc = summonInline[JsonConverter[met]]
        val key = constValue[mel & String]

        val value =
          fields.findJsonName(key) match
            case None => getDefaultValue(defaults, key)
            case Some(fieldName) =>
              jsonObject.toMap.find(_._1 == fieldName) match
                case None => getDefaultValue(defaults, key)
                case Some((_, v)) =>
                  v match
                    case obj: JsonObject => nc.fromJson(obj)
                    case arr: JsonArray  =>
                    case value =>
                      val v = fields.fromNative(key, value).getOrElse(value)
                      nc.fromJson(v)

        values(i) = value

        jsonToProduct[A, metsTail, melsTail](
          mirror,
          values,
          jsonObject,
          i + 1,
          fields,
          defaults
        )

  private def getDefaultValue(defaults: Map[String, Any], key: String): Any =
    defaults.find(_._1 == key) match // get default field value
      case Some(v) => v._2
      case None =>
        throw new JsonCodecException(
          s"can't get default value to field: ${key}"
        )
