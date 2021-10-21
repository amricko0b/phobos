package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.syntax.*
import ru.tinkoff.phobos.encoding.ElementEncoder

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

  trait CaseClassParam {
    type P
    val localName: String
    val xmlName: String
    val namespaceUri: String
    val category: ParamCategory

    override def toString: String = s"CaseClassParam($localName, $xmlName, $namespaceUri, $category)"
  }

  final case class SealedTraitSubtype(
                                       constructorName: String,
                                       subtypeType: String,
                                     )

  inline def paramAnns[T]: List[CaseClassParam] = ${paramAnnsI[T]}

  def paramAnnsI[T: Type](using Quotes): Expr[List[CaseClassParam]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]

    def filterAnnotation(a: Term): Boolean =
      a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
        a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"

    Expr.ofList {
      val constructorAnnotations = tpe.typeSymbol.primaryConstructor.paramSymss.flatten.map { field =>
        field.name -> field.annotations.filter(filterAnnotation).map(_.asExpr.asInstanceOf[Expr[Any]])
      }
      val fieldAnnotations = tpe.typeSymbol.caseFields.collect {
        case field: Symbol if field.tree.isInstanceOf[ValDef] =>
          field.name -> field.annotations.filter(filterAnnotation).map(_.asExpr.asInstanceOf[Expr[Any]])
      }
      (constructorAnnotations ++ fieldAnnotations)
        .filter(_._2.nonEmpty)
        .groupBy(_._1)
        .toList
        .map {
          case (name, l) => Expr(name) -> l.flatMap(_._2)
        }
        .map { (name, anns) =>
          val namespace  =
            anns.collect {
              case '{xmlns($a: b)} =>
              println("KAVO")
              summonInline[b]
              '{Some(summonInline[Namespace[b]].getNamespace)}
            } match {
              case Nil => '{None}
              case List(ns) => ns
              case _ =>
                println("PEKA")
                ???
            }
          val cat = '{anns
            .collect {
              case '{attr()} => ParamCategory.attribute
              case '{text()} => ParamCategory.text
              case '{default()} => ParamCategory.default
            } match {
              case List(category) => category
              case Nil            => ParamCategory.element
              case categories => ParamCategory.element
            }}
          '{
            new CaseClassParam {
            type P = T
//            val localName: String = ${name}
//            val xmlName: String = ${name}
//            val namespaceUri: Option[String] = ${namespace}
            val category: ParamCategory = ${cat}
          }}
        }
    }
  }

//  inline def getParams[T, Labels <: Tuple, Params <: Tuple](
//                                                             annotations: Map[String, List[Any]],
//                                                             typeAnnotations: Map[String, List[Any]],
//                                                             repeated: Map[String, Boolean],
//                                                             idx: Int = 0
//                                                           ): List[CaseClass.Param[Typeclass, T]] =
//
//    inline erasedValue[(Labels, Params)] match {
//      case _: (EmptyTuple, EmptyTuple) =>
//        Nil
//      case _: ((l *: ltail), (p *: ptail)) =>
//        val label = constValue[l].asInstanceOf[String]
//        val typeclass = CallByNeed(summonInline[ElementEncoder[p]])
//
//        CaseClass.Param[ElementEncoder, T, p](label, idx, repeated.getOrElse(label, false), typeclass,
//          CallByNeed(None), IArray.from(annotations.getOrElse(label, List())),
//          IArray.from(typeAnnotations.getOrElse(label, List()))) ::
//          getParams[T, ltail, ptail](annotations, typeAnnotations, repeated, idx + 1)
//    }
//  inline def fetchCategory(annotations: List[Any]): ParamCategory = {
//    val categories = annotations
//      .collect {
//        case _: attr => ParamCategory.attribute
//        case _: text => ParamCategory.text
//        case _: default => ParamCategory.default
//      }
//
//    categories match {
//      case List(category) => category
//      case Nil            => ParamCategory.element
//      case categories => ParamCategory.element
//    }
//  }
//
//  inline def kek[Labels <: Tuple, Params <: Tuple](annots: Map[String, List[Any]] ): List[CaseClassParam] =
//    inline erasedValue[(Labels, Params)] match {
//      case _: (EmptyTuple, EmptyTuple) => Nil
//      case _: ((l *: ltail), (p *: ptail)) =>
//        val label = constValue[l].asInstanceOf[String]
//        val typeclass = summonInline[ElementEncoder[p]]
//        val anns = annots.getOrElse(label, Nil)
//        val namespace =
//          anns.collect{
//            case a: xmlns[?] => println(a)
//          }
//        new CaseClassParam {
//          type T = p
//          val localName = label
//          val xmlName = label
//          val namespaceUri = ""
//          val category = fetchCategory(anns)
//        } :: kek[ltail, ptail](annots)
//    }

  inline def deriveElementEncoderMirror[T](using mirror: Mirror.Of[T]): ElementEncoder[T] =
    inline mirror match {
      case sum: Mirror.SumOf[T] =>
        println(sum)
        ???
      //derivedMirrorSum[T](sum)
      case product: Mirror.ProductOf[T] =>
//        derivedMirrorProduct[T](product)
        val annots = paramAnns[T]
        println(annots)
        ???
    }
}
