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

  sealed trait FieldCategory

  object FieldCategory {
    case object element   extends FieldCategory
    case object attribute extends FieldCategory
    case object text      extends FieldCategory
    case object default   extends FieldCategory
  }

  final class CaseClassField(using val quotes: Quotes)(
    val localName: String,
    val xmlName: Expr[String], // Name of element or attribute
    val namespaceUri: Expr[Option[String]],
    val paramType: quotes.reflect.TypeRepr,
    val category: FieldCategory,
  )

  final class SealedTraitChild(using val quotes: Quotes)(
     val xmlName: Expr[String], // Value of discriminator
     val subtypeType: quotes.reflect.TypeRepr,
  )

  // PRODUCT

  private def extractFieldCategory(using Quotes)(
    classSymbol: quotes.reflect.Symbol,
    fieldSymbol: quotes.reflect.Symbol,
    fieldAnnotations: List[Expr[Any]]
  ): FieldCategory = {
    import quotes.reflect.*
    fieldAnnotations
      .collect {
        case '{attr()}    => FieldCategory.attribute
        case '{text()}    => FieldCategory.text
        case '{default()} => FieldCategory.default
      } match {
      case Nil            => FieldCategory.element
      case List(category) => category
      case categories =>
        val categoryAnnotations =
          categories.collect {
            case FieldCategory.attribute => "@attr"
            case FieldCategory.text => "@text"
            case FieldCategory.default => "@default"
          }.mkString(", ")

        report.throwError(
          s"""
             |Case class field cannot have more than one category annotation (@attr, @text or @default).
             |Field '${fieldSymbol.name}' in case class '${classSymbol.name}' has ${categories.size}: $categoryAnnotations
             |""".stripMargin
        )
    }
  }

  private def extractFieldXmlName(using Quotes)(
    config: Expr[ElementCodecConfig],
    classSymbol: quotes.reflect.Symbol,
    fieldSymbol: quotes.reflect.Symbol,
    fieldAnnotations: List[Expr[Any]],
    fieldCategory: FieldCategory,
  ): Expr[String] = {
    import quotes.reflect.*
    (fieldAnnotations.collect {case '{renamed($a)} => a } match {
      case Nil        => None
      case List(name) => Some(name)
      case names =>
        val renamedAnnotations = names.map(name => s"@renamed(${name.asTerm.show})").mkString(", ")
        report.throwError(
          s"""
             |Case class field cannot have more than one @renamed annotation.
             |Field '${fieldSymbol.name}' in case class '${classSymbol.name}' has ${names.size}: $renamedAnnotations
             |""".stripMargin
        )
    }).getOrElse(fieldCategory match {
      case FieldCategory.element   => '{${config}.transformElementNames(${Expr(fieldSymbol.name)})}
      case FieldCategory.attribute => '{${config}.transformAttributeNames(${Expr(fieldSymbol.name)})}
      case _                       => Expr(fieldSymbol.name)
    })
  }

  private def extractFeildNamespace(using Quotes)(
    config: Expr[ElementCodecConfig],
    classSymbol: quotes.reflect.Symbol,
    fieldSymbol: quotes.reflect.Symbol,
    fieldAnnotations: List[Expr[Any]],
    fieldCategory: FieldCategory,
  ): Expr[Option[String]] = {
    import quotes.reflect.*
    fieldAnnotations.collect {
      case '{xmlns($namespace: b)} => '{Some(summonInline[Namespace[b]].getNamespace)}
    } match {
      case Nil => fieldCategory match {
        case FieldCategory.element   => '{${config}.elementsDefaultNamespace}
        case FieldCategory.attribute => '{${config}.attributesDefaultNamespace}
        case _ => '{None}
      }
      case List(namespace) => namespace
      case namespaces =>
        val xmlnsAnnotations =
          fieldAnnotations
            .collect {
              case '{xmlns($namespace)} => s"@xmlns(${namespace.asTerm.show})"
            }
            .mkString(", ")
        report.throwError(
          s"""
             |Case class field cannot have more than on @xmlns annotation.
             |Field '${fieldSymbol.name}' in case class '${classSymbol.name}' has ${namespaces.size}: $xmlnsAnnotations
             |""".stripMargin
        )
    }
  }


  private def extractCaseClassFields[T: Type](config: Expr[ElementCodecConfig])(using Quotes): List[CaseClassField] = {
    import quotes.reflect.*

    val classTypeRepr = TypeRepr.of[T]
    val classSymbol = classTypeRepr.typeSymbol

    classSymbol.primaryConstructor.paramSymss.flatten.map { fieldSymbol =>
      val fieldAnnotations = fieldSymbol.annotations.map(_.asExpr)
      val fieldCategory    = extractFieldCategory(classSymbol, fieldSymbol, fieldAnnotations)
      val fieldXmlName     = extractFieldXmlName(config, classSymbol, fieldSymbol, fieldAnnotations, fieldCategory)
      val fieldNamespace   = extractFeildNamespace(config, classSymbol, fieldSymbol, fieldAnnotations, fieldCategory)
      CaseClassField()(fieldSymbol.name, fieldXmlName, fieldNamespace, classTypeRepr.memberType(fieldSymbol), fieldCategory)
    }
  }

  private def encodeAttributes[T: Type](using Quotes)(
    fields: List[CaseClassField],
    sw: Expr[PhobosStreamWriter],
    a: Expr[T]
  ): Expr[List[Unit]] = {
    import quotes.reflect.*
    val classTypeRepr = TypeRepr.of[T]
    val classSymbol = classTypeRepr.typeSymbol
    Expr.ofList(fields.map{ field =>
      field.paramType.asType match {
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
    fields: List[CaseClassField],
    sw: Expr[PhobosStreamWriter],
    a: Expr[T]
  ): Expr[List[Unit]] = {
    import quotes.reflect.*
    val classTypeRepr = TypeRepr.of[T]
    val classSymbol = classTypeRepr.typeSymbol

    Expr.ofList(fields.map { field =>
      field.paramType.asType match {
        case '[t] => '{
          summonInline[TextEncoder[t]]
            .encodeAsText(${Select(a.asTerm, classSymbol.declaredField(field.localName)).asExprOf[t]}, $sw)
        }
      }
    })
  }

  private def encodeElements[T: Type](using Quotes)(
    fields: List[CaseClassField],
    sw: Expr[PhobosStreamWriter],
    a: Expr[T]
  ): Expr[List[Unit]] = {
    import quotes.reflect.*
    val classTypeRepr = TypeRepr.of[T]
    val classSymbol = classTypeRepr.typeSymbol

    Expr.ofList(fields.map { field =>
      field.paramType.asType match {
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


  private def deriveProductImpl[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementEncoder[T]] = {
    import quotes.reflect.*
    val classTypeRepr = TypeRepr.of[T]
    val classSymbol = classTypeRepr.typeSymbol
    val fields = extractCaseClassFields[T](config)

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

  private def extractChildXmlName(using Quotes)(
    config: Expr[ElementCodecConfig],
    traitSymbol: quotes.reflect.Symbol,
    childSymbol: quotes.reflect.Symbol,
  ): Expr[String] = {
    import quotes.reflect.*
    childSymbol.annotations.map(_.asExpr).collect { case '{discriminator($a)} => a } match {
      case Nil        => '{$config.transformConstructorNames(${Expr(childSymbol.name)})}
      case List(name) => name
      case names =>
        val discriminatorAnnotations =
          names.map(name => s"@discriminator(${name.show})")
        report.throwError(
          s"""
             |Sealed trait child cannot have more than one @discriminator annotation.
             |Field '${childSymbol.name}' in sealed trait '${traitSymbol.name}' has ${names.size}: $discriminatorAnnotations
             |""".stripMargin
        )
    }
  }

  private def extractSealedTraitChildren[T: Type](config: Expr[ElementCodecConfig])(using Quotes): List[SealedTraitChild] = {
    import quotes.reflect.*
    val traitTypeRepr = TypeRepr.of[T]
    val traitSymbol   = traitTypeRepr.typeSymbol

    traitSymbol.children.map { childSymbol =>
      val xmlName = extractChildXmlName(config, traitSymbol, childSymbol)
      SealedTraitChild()(xmlName, TypeIdent(childSymbol).tpe)
    }
  }

  private def encodeChild[T: Type](using Quotes)(
    config: Expr[ElementCodecConfig],
    child: SealedTraitChild,
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

  private def deriveSumImpl[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementEncoder[T]] = {
    import quotes.reflect.*

    '{
      new ElementEncoder[T] {
        def encodeAsElement(a: T, sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String]): Unit = {
          ${
            Match(
              '{a}.asTerm,
              extractSealedTraitChildren[T](config).map { child =>
                child.subtypeType.asType match {
                  case '[t] =>
                    val sub = Symbol.newBind(Symbol.spliceOwner, "sub", Flags.EmptyFlags, TypeRepr.of[t])
                    val encode = encodeChild(config, child, Ref(sub).asExprOf[t], 'sw, 'localName, 'namespaceUri)
                    CaseDef(Bind(sub, Typed(Ref(sub), TypeTree.of[t])), None, encode.asTerm)
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
