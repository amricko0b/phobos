package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.syntax.*
import ru.tinkoff.phobos.encoding.*

import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.*
import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}

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

  def deriveProductImpl[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementEncoder[T]] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val params = paramAnns[T](config)

    val groups = params.groupBy(_.category)

    '{new ElementEncoder[T]{
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
          Expr.ofList((groups.getOrElse(ParamCategory.element, Nil) ::: groups.getOrElse(ParamCategory.default, Nil)).map { param =>
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
  }

  def deriveSumImpl[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementEncoder[T]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol

    '{
      new ElementEncoder[T] {
        def encodeAsElement(a: T, sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String]): Unit = {
          ${
            Match(
              '{a}.asTerm,
              typeSymbol.children.map { child =>
                TypeIdent(child).tpe.asType match {
                  case '[t] =>
                      val xmlName: Expr[String] = child.annotations.map(_.asExpr).collect {
                        case '{discriminator($a)} => a
                      } match {
                        case Nil => '{$config.transformConstructorNames(${Expr(child.name)})}
                        case List(value) => value
                        case _ => throw new IllegalArgumentException("that's too much")
                      }

                      val sub = Symbol.newBind(Symbol.spliceOwner, "sub", Flags.EmptyFlags, TypeRepr.of[t])
                      val encodeSub: Expr[Unit] = '{
                        val instance = summonInline[ElementEncoder[t]]
                        if ($config.useElementNameAsDiscriminator) {
                          instance.encodeAsElement(
                            ${Ref(sub).asExprOf[t]},
                            sw,
                            $xmlName,
                            None,
                          )
                        } else {
                          sw.memorizeDiscriminator(
                            $config.discriminatorNamespace,
                            $config.discriminatorLocalName,
                            $xmlName,
                          )
                          instance.encodeAsElement(${Ref(sub).asExprOf[t]}, sw, localName, namespaceUri)
                        }
                      }
                      CaseDef(Bind(sub, Typed(Ref(sub), TypeTree.of[t])), None, encodeSub.asTerm)
                }
              }
            ).asExprOf[Unit]
          }
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
