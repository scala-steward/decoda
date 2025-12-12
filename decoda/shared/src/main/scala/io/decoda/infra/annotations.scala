package io.decoda.infra

import scala.annotation.StaticAnnotation

object annotations:

  enum JsonType:
    case IntStr
    case NumStr
    case BoolStr
    case DateStr[T](pattern: String)(using
        val dateConverter: JsonDateConverter
    )
    case Auto

  case class JsonValue(
      name: String = "",
      ignore: Boolean = false,
      omitNull: Boolean = false,
      // force type conversion
      typ: JsonType = JsonType.Auto
  ) extends StaticAnnotation

  trait JsonDateConverter:
    def parse(date: String, pattern: String): Any

    def format(date: Any, pattern: String): String
