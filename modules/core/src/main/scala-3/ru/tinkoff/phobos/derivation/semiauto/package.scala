package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.decoding.{ElementDecoder, XmlDecoder}
import ru.tinkoff.phobos.encoding.{ElementEncoder, XmlEncoder}

package object semiauto {
  def deriveElementEncoder[T]: ElementEncoder[T]                                                              = ???
  def deriveElementEncoderConfigured[T](config: ElementCodecConfig): ElementEncoder[T]                        = ???
  def deriveXmlEncoder[T](localName: String): XmlEncoder[T]                                                   = ???
  def deriveXmlEncoderConfigured[T](localName: String, config: ElementCodecConfig): XmlEncoder[T]             = ???
  def deriveXmlEncoder[T, NS](localName: String, ns: NS): XmlEncoder[T]                                       = ???
  def deriveXmlEncoderConfigured[T, NS](localName: String, ns: NS, config: ElementCodecConfig): XmlEncoder[T] = ???
  def deriveElementDecoder[T]: ElementDecoder[T]                                                              = ???
  def deriveElementDecoderConfigured[T](config: ElementCodecConfig): ElementDecoder[T]                        = ???
  def deriveXmlDecoder[T](localName: String): XmlDecoder[T]                                                   = ???
  def deriveXmlDecoderConfigured[T](localName: String, config: ElementCodecConfig): XmlDecoder[T]             = ???
  def deriveXmlDecoder[T, NS](localName: String, ns: NS): XmlDecoder[T]                                       = ???
  def deriveXmlDecoderConfigured[T, NS](localName: String, ns: NS, config: ElementCodecConfig): XmlDecoder[T] = ???
}
