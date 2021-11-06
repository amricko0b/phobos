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

  def paramAnns[T: Type](config: Expr[ElementCodecConfig])(using Quotes): List[CaseClassParam] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]

    def filterAnnotation(a: Term): Boolean =
      a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
        a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"

    tpe.typeSymbol.primaryConstructor.paramSymss.flatten.map { field =>
      val anns = field.annotations.filter(filterAnnotation).map(_.asExpr)

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

      val xmlName = (anns.collect {case '{renamed($a)} => a } match {
        case Nil => None
        case List(name) => Some(name)
        case _ => throw new IllegalArgumentException("SHEESH")
      }).getOrElse(cat match {
          case ParamCategory.element => '{${config}.transformElementNames(${Expr(field.name)})}
          case ParamCategory.attribute => '{${config}.transformAttributeNames(${Expr(field.name)})}
          case _ =>    Expr(field.name)
        })

      val namespace: Expr[Option[String]]  =
        anns.collect {
          case '{xmlns($a: b)} =>
          '{Some(summonInline[Namespace[b]].getNamespace)}
        } match {
          case Nil =>  cat match {
            case ParamCategory.element => '{${config}.elementsDefaultNamespace}
            case ParamCategory.attribute => '{${config}.attributesDefaultNamespace}
            case _ => '{None}
          }
          case List(ns) => ns
          case _ => throw new IllegalArgumentException("OOPPS")
        }
      CaseClassParam()(field.name, xmlName, namespace, tpe.memberType(field), cat)
    }
  }

  inline def deriveProduct[T](inline config: ElementCodecConfig): ElementEncoder[T] = ${deriveProductImpl('{config})}

  def deriveProductImpl[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementEncoder[T]] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val params = paramAnns[T](config)

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
//    println(x.asTerm.show(using Printer.TreeAnsiCode))
    x
  }

  def deriveSumImpl[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementEncoder[T]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol

    println(typeSymbol.children)

    val alternatives = typeSymbol.children.map { child =>
      tpe.select(child).asType match {
        case '[t] =>
          println('{??? match {
            case x if x.isInstanceOf[t] => 123
            case _ => 345
          }}.asTerm.show(using Printer.TreeStructure))
//          CaseDef(???, None, '{summonInline[ElementEncoder[t]]})
      }
    }

//    println('{1 match {
//      case 2 => 0
//      case _: Int => 1
//    }}.asTerm.show(using Printer.TreeStructure))

    '{
      new ElementEncoder[T] {
        def encodeAsElement(a: T, sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String]): Unit = {
          ${Match('{a}.asTerm, Nil).asExprOf[Unit]}
        }
      }
    }
  }

  inline def deriveEncoder[T](inline config: ElementCodecConfig): ElementEncoder[T] =
    ${deriveEncoderImpl('{config})}

  def deriveEncoderImpl[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementEncoder[T]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol
    if (typeSymbol.flags.is(Flags.Case)) {
      deriveProductImpl(config)
    } else if (typeSymbol.flags.is(Flags.Sealed)) {
      deriveSumImpl(config)
    } else {
      println(typeSymbol.flags.show)
      throw new IllegalArgumentException("А это кто")
    }
  }

  inline def xml[T](
    inline localName: String,
    inline namespace: Option[String],
    inline config: ElementCodecConfig
  ): XmlEncoder[T] =
    ${xmlImpl('{localName}, '{namespace}, '{config})}

  def xmlImpl[T: Type](
    localName: Expr[String],
    namespace: Expr[Option[String]],
    config: Expr[ElementCodecConfig],
  )(using Quotes): Expr[XmlEncoder[T]] =
    '{XmlEncoder.fromElementEncoder[T]($localName, $namespace)(${deriveEncoderImpl(config)})}
}
