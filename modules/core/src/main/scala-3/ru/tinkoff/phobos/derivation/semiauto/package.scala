package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.decoding.{ElementDecoder, XmlDecoder}
import ru.tinkoff.phobos.encoding.{ElementEncoder, XmlEncoder}

import scala.deriving.Mirror

package object semiauto {
  inline def deriveElementEncoder[T]: ElementEncoder[T] =
    encoder.deriveEncoder[T](ElementCodecConfig.default)
  inline def deriveElementEncoderConfigured[T](config: ElementCodecConfig): ElementEncoder[T] =
    encoder.deriveEncoder[T](config)
  inline def deriveXmlEncoder[T](localName: String): XmlEncoder[T] =
    encoder.xml[T](localName, None, ElementCodecConfig.default)
  inline def deriveXmlEncoderConfigured[T](localName: String, config: ElementCodecConfig): XmlEncoder[T] =
    encoder.xml[T](localName, None, config)
  inline def deriveXmlEncoder[T, NS: Namespace](localName: String, ns: NS): XmlEncoder[T] =
    encoder.xml[T](localName, Some(Namespace[NS].getNamespace), ElementCodecConfig.default)
  inline def deriveXmlEncoderConfigured[T, NS: Namespace](localName: String, ns: NS, config: ElementCodecConfig): XmlEncoder[T] =
    encoder.xml[T](localName, Some(Namespace[NS].getNamespace), config)
  def deriveElementDecoder[T]: ElementDecoder[T]                                                              = ???
  def deriveElementDecoderConfigured[T](config: ElementCodecConfig): ElementDecoder[T]                        = ???
  def deriveXmlDecoder[T](localName: String): XmlDecoder[T]                                                   = ???
  def deriveXmlDecoderConfigured[T](localName: String, config: ElementCodecConfig): XmlDecoder[T]             = ???
  def deriveXmlDecoder[T, NS](localName: String, ns: NS): XmlDecoder[T]                                       = ???
  def deriveXmlDecoderConfigured[T, NS](localName: String, ns: NS, config: ElementCodecConfig): XmlDecoder[T] = ???
}
