package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.decoding.{ElementDecoder, XmlDecoder}
import ru.tinkoff.phobos.encoding.{ElementEncoder, XmlEncoder}

import scala.deriving.Mirror

package object semiauto {
  inline def deriveElementEncoder[T]: ElementEncoder[T]                                                       = encoder.peka[T](ElementCodecConfig.default)
  inline def deriveElementEncoderConfigured[T](config: ElementCodecConfig): ElementEncoder[T]                 = encoder.peka[T](config)
  inline def deriveXmlEncoder[T](localName: String): XmlEncoder[T]                                            = encoder.xml[T](localName, ElementCodecConfig.default)
  inline def deriveXmlEncoderConfigured[T](localName: String, config: ElementCodecConfig): XmlEncoder[T]      = encoder.xml[T](localName, config)
  def deriveXmlEncoder[T, NS](localName: String, ns: NS): XmlEncoder[T]                                       = ???
  def deriveXmlEncoderConfigured[T, NS](localName: String, ns: NS, config: ElementCodecConfig): XmlEncoder[T] = ???
  def deriveElementDecoder[T]: ElementDecoder[T]                                                              = ???
  def deriveElementDecoderConfigured[T](config: ElementCodecConfig): ElementDecoder[T]                        = ???
  def deriveXmlDecoder[T](localName: String): XmlDecoder[T]                                                   = ???
  def deriveXmlDecoderConfigured[T](localName: String, config: ElementCodecConfig): XmlDecoder[T]             = ???
  def deriveXmlDecoder[T, NS](localName: String, ns: NS): XmlDecoder[T]                                       = ???
  def deriveXmlDecoderConfigured[T, NS](localName: String, ns: NS, config: ElementCodecConfig): XmlDecoder[T] = ???
}
