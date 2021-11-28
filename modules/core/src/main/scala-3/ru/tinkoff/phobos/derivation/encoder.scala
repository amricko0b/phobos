package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.syntax.*
import ru.tinkoff.phobos.encoding.*
import ru.tinkoff.phobos.derivation.common.*

import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.*
import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}

object encoder {

  inline def deriveElementEncoder[T](
    inline config: ElementCodecConfig
  ): ElementEncoder[T] =
    ${deriveElementEncoderImpl('{config})}

  inline def deriveXmlEncoder[T](
    inline localName: String,
    inline namespace: Option[String],
    inline config: ElementCodecConfig
  ): XmlEncoder[T] =
    ${deriveXmlEncoderImpl('{localName}, '{namespace}, '{config})}

  def deriveElementEncoderImpl[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementEncoder[T]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol
    if (typeSymbol.flags.is(Flags.Case)) {
      deriveProduct(config)
    } else if (typeSymbol.flags.is(Flags.Sealed)) {
      deriveSum(config)
    } else {
      report.throwError(s"${typeSymbol} is not a sum type or product type")
    }
  }

  def deriveXmlEncoderImpl[T: Type](
    localName: Expr[String],
    namespace: Expr[Option[String]],
    config: Expr[ElementCodecConfig],
  )(using Quotes): Expr[XmlEncoder[T]] =
    '{XmlEncoder.fromElementEncoder[T]($localName, $namespace)(${deriveElementEncoderImpl(config)})}

  // PRODUCT

  private def encodeAttributes[T: Type](using Quotes)(
    fields: List[ProductTypeField],
    sw: Expr[PhobosStreamWriter],
    a: Expr[T]
  ): Expr[List[Unit]] = {
    import quotes.reflect.*
    val classTypeRepr = TypeRepr.of[T]
    val classSymbol = classTypeRepr.typeSymbol
    Expr.ofList(fields.map{ field =>
      field.typeRepr.asType match {
        case '[t] => '{
          summonInline[AttributeEncoder[t]].encodeAsAttribute(
            ${Select(a.asTerm, classSymbol.declaredField(field.localName)).asExprOf[t]},
            $sw,
            ${field.xmlName},
            ${field.namespaceUri}
          )
        }
      }
    })
  }

  private def encodeText[T: Type](using Quotes)(
    fields: List[ProductTypeField],
    sw: Expr[PhobosStreamWriter],
    a: Expr[T]
  ): Expr[List[Unit]] = {
    import quotes.reflect.*
    val classTypeRepr = TypeRepr.of[T]
    val classSymbol = classTypeRepr.typeSymbol

    Expr.ofList(fields.map { field =>
      field.typeRepr.asType match {
        case '[t] => '{
          summonInline[TextEncoder[t]]
            .encodeAsText(${Select(a.asTerm, classSymbol.declaredField(field.localName)).asExprOf[t]}, $sw)
        }
      }
    })
  }

  private def encodeElements[T: Type](using Quotes)(
    fields: List[ProductTypeField],
    sw: Expr[PhobosStreamWriter],
    a: Expr[T]
  ): Expr[List[Unit]] = {
    import quotes.reflect.*
    val classTypeRepr = TypeRepr.of[T]
    val classSymbol = classTypeRepr.typeSymbol

    Expr.ofList(fields.map { field =>
      field.typeRepr.asType match {
        case '[t] => '{
          summonInline[ElementEncoder[t]].encodeAsElement(
            ${Select(a.asTerm, classSymbol.declaredField(field.localName)).asExprOf[t]},
            $sw,
            ${field.xmlName},
            ${field.namespaceUri},
          )
        }
      }
    })
  }


  private def deriveProduct[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementEncoder[T]] = {
    import quotes.reflect.*
    val classTypeRepr = TypeRepr.of[T]
    val classSymbol = classTypeRepr.typeSymbol
    val fields = extractProductTypeFields[T](config)

    val groups = fields.groupBy(_.category)

    '{new ElementEncoder[T]{
      def encodeAsElement(a: T, sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String]): Unit = {
        namespaceUri.fold(sw.writeStartElement(localName))(ns => sw.writeStartElement(ns, localName))
        $config.defineNamespaces.foreach { uri =>
          if (sw.getNamespaceContext.getPrefix(uri) == null) sw.writeNamespace(uri)
        }

        ${encodeAttributes[T](groups.getOrElse(FieldCategory.attribute, Nil), 'sw, 'a)}
        ${encodeText[T](groups.getOrElse(FieldCategory.text, Nil), 'sw, 'a)}
        ${encodeElements[T]((groups.getOrElse(FieldCategory.element, Nil) ::: groups.getOrElse(FieldCategory.default, Nil)), 'sw, 'a)}

        sw.writeEndElement()
      }
    }}
  }

  // SUM

  private def encodeChild[T: Type](using Quotes)(
    config: Expr[ElementCodecConfig],
    child: SumTypeChild,
    childValue: Expr[T],
    sw: Expr[PhobosStreamWriter],
    localName: Expr[String],
    namespaceUri: Expr[Option[String]],
  ): Expr[Unit] = {
    import quotes.reflect.*

    '{
      val instance = summonInline[ElementEncoder[T]]
      if ($config.useElementNameAsDiscriminator) {
        instance.encodeAsElement(${childValue}, $sw, ${child.xmlName}, None)
      } else {
        $sw.memorizeDiscriminator($config.discriminatorNamespace, $config.discriminatorLocalName, ${child.xmlName})
        instance.encodeAsElement(${childValue}, $sw, $localName, $namespaceUri)
      }
    }
  }

  private def deriveSum[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementEncoder[T]] = {
    import quotes.reflect.*

    '{
      new ElementEncoder[T] {
        def encodeAsElement(a: T, sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String]): Unit = {
          ${
            Match(
              '{a}.asTerm,
              extractSumTypeChildren[T](config).map { child =>
                child.typeRepr.asType match {
                  case '[t] =>
                    val childValueSymbol = Symbol.newBind(Symbol.spliceOwner, "child", Flags.EmptyFlags, TypeRepr.of[t])
                    val encode = encodeChild(config, child, Ref(childValueSymbol).asExprOf[t], 'sw, 'localName, 'namespaceUri)
                    CaseDef(Bind(childValueSymbol, Typed(Ref(childValueSymbol), TypeTree.of[t])), None, encode.asTerm)
                }
              }
            ).asExprOf[Unit]
          }
        }
      }
    }
  }
}
