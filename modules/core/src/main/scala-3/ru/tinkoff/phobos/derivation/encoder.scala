package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.syntax.*
import ru.tinkoff.phobos.encoding._

import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.*
import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}

case class TypeInfo(owner: String, short: String, typeParams: Iterable[TypeInfo]):
  def full: String = s"$owner.$short"

object encoder {

  sealed trait ParamCategory

  object ParamCategory {
    case object element   extends ParamCategory
    case object attribute extends ParamCategory
    case object text      extends ParamCategory
    case object default   extends ParamCategory
  }

  final class CaseClassParam(using val quotes: Quotes)(
                                   val localName: String,
                                   val xmlName: Expr[String],
                                   val namespaceUri: Expr[Option[String]],
                                   val paramType: quotes.reflect.TypeRepr,
                                   val category: ParamCategory,
                                 )

  final case class SealedTraitSubtype(
                                       constructorName: String,
                                       subtypeType: String,
                                     )

  def paramAnnsI[T: Type](config: Expr[ElementCodecConfig])(using Quotes): List[CaseClassParam] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]

    def filterAnnotation(a: Term): Boolean =
      a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
        a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"

    tpe.typeSymbol.primaryConstructor.paramSymss.flatten.map { field =>
      val anns = field.annotations.filter(filterAnnotation).map(_.asExpr)
      val namespace: Expr[Option[String]]  =
        anns.collect {
          case '{xmlns($a: b)} =>
          '{Some(summonInline[Namespace[b]].getNamespace)}
        } match {
          case Nil => '{None}
          case List(ns) => ns
          case _ => throw new IllegalArgumentException("OOPPS")
        }
      val cat: ParamCategory  = anns
        .collect {
          case '{attr()} => ParamCategory.attribute
          case '{text()} => ParamCategory.text
          case '{default()} => ParamCategory.default
        } match {
        case List(category) => category
        case Nil            => ParamCategory.element
        case categories => throw new IllegalArgumentException("BRRR")
      }
      val xmlName = cat match {
        case ParamCategory.element => '{${config}.transformElementNames(${Expr(field.name)})}
        case ParamCategory.attribute => '{${config}.transformAttributeNames(${Expr(field.name)})}
        case ParamCategory.text =>    Expr(field.name)
        case ParamCategory.default => Expr(field.name)
      }
      CaseClassParam()(field.name, xmlName, namespace, tpe.memberType(field), cat)
    }
  }

  inline def peka[T](inline config: ElementCodecConfig): ElementEncoder[T] = ${pekaImpl('{config})}

  def pekaImpl[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementEncoder[T]] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val params = paramAnnsI[T](config)

    val groups = params.groupBy(_.category)

    val x = '{new ElementEncoder[T]{
      def encodeAsElement(a: T, sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String]): Unit = {
        namespaceUri.fold(sw.writeStartElement(localName))(ns => sw.writeStartElement(ns, localName))
        $config.defineNamespaces.foreach { uri =>
          if (sw.getNamespaceContext.getPrefix(uri) == null) sw.writeNamespace(uri)
        }

        ${
          Expr.ofList(groups.getOrElse(ParamCategory.attribute, Nil).map{ param =>
            param.paramType.asType match {
              case '[t] =>
                '{summonInline[AttributeEncoder[t]].encodeAsAttribute(
                  ${Select('{a}.asTerm, tpe.typeSymbol.declaredField(param.localName)).asExprOf[t]},
                  sw,
                  ${param.xmlName},
                  ${param.namespaceUri})
                }
            }
          })
        }

        ${
          Expr.ofList(groups.getOrElse(ParamCategory.text, Nil).map { param =>
            param.paramType.asType match {
              case '[t] =>
                '{summonInline[TextEncoder[t]]
                  .encodeAsText(${Select('{a}.asTerm, tpe.typeSymbol.declaredField(param.localName)).asExprOf[t]}, sw)
                }
            }
          })
        }

        ${
          Expr.ofList(groups.getOrElse(ParamCategory.element, Nil).map { param =>
            param.paramType.asType match {
              case '[t] =>
                '{summonInline[ElementEncoder[t]].encodeAsElement(
                    ${Select('{a}.asTerm, tpe.typeSymbol.declaredField(param.localName)).asExprOf[t]},
                    sw,
                    ${param.xmlName},
                    ${param.namespaceUri},
                  )
                }
            }
          })
        }

        sw.writeEndElement()
      }
    }}
    println(x.asTerm.show(using Printer.TreeAnsiCode))
    x
  }

  inline def xml[T](inline localName: String, inline config: ElementCodecConfig): XmlEncoder[T] = ${xmlImpl('{localName}, '{config})}

  def xmlImpl[T: Type](localName: Expr[String], config: Expr[ElementCodecConfig])(using Quotes): Expr[XmlEncoder[T]] =
    '{XmlEncoder.fromElementEncoder[T]($localName)(${pekaImpl[T](config)})}
}
