package org.barabu.emv

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

import EmvUtils._

/**
Meterials:
  http://luca.ntop.org/Teaching/Appunti/asn1.html

  https://www.openscdp.org/scsh3/tlv.html
  https://www.openscdp.org/scsh3/tlvlist.html
  https://www.openscdp.org/scsh3/card.html
  https://www.openscdp.org/scsh3/asn1.html

  https://www.emvlab.org/emvtags/all/ - все tag-коды EMV !!!
*/

object TLV {

  val MASK_CLASS:         Byte = 0xC0
  val MASK_STRUCT:        Byte = 0x20
  val MASK_TAG:           Byte = 0x1F
  val MASK_LENGTH_127:    Byte = 0x7F
  val MASK_LENGTH_255:    Byte = 0x81
  val MASK_LENGTH_MAX:    Byte = 0x82

  val CLASS_UNIVERSAL:    Byte = 0x00
  val CLASS_APPLICATION:  Byte = 0x40
  val CLASS_CONTEXT:      Byte = 0x80
  val CLASS_PRIVATE:      Byte = 0xC0

  val classes = scala.collection.mutable.Map(
    CLASS_UNIVERSAL -> "UNIVERSAL",
    CLASS_APPLICATION -> "APPLICATION",
    CLASS_CONTEXT -> "CONTEXT",
    CLASS_PRIVATE -> "PRIVATE").withDefaultValue("UNKNOWN")

  /**
    *
    * @param tlv
    * @param deep
    */
  def printTlvHierarchy(tlv: TLV, deep: Int = 0): Unit = {

    def tab(count: Int, padding: String = "  "): String = Array.fill(count)(padding).mkString

    logIt(s"${tab(deep)}${bytes2hex(tlv.tlv)}")

    for(i <- 0 until tlv.count) {
      tlv.item(i) match {
        case Success(t) => printTlvHierarchy(t, deep + 1)
        case Failure(e) => logIt(e.getMessage)
      }
    }
  }
}

import TLV._
/**
  *
  * @param raw
  */
case class TLV(raw: Array[Byte]) {

  /**
    * Размеры каждого поля: T, L, V и всего TLV
    */
  val sizeT = (raw(0) & MASK_TAG) match {
    case MASK_TAG => 2
    case _ => 1
  }
  val sizeL = {
    raw(sizeT) match {
      case MASK_LENGTH_255 => 2
      case MASK_LENGTH_MAX => 3
      case _ => 1
    }
  }
  val sizeV = {
    raw(sizeT) match {
      case MASK_LENGTH_255 => raw(sizeT + 1)
      case MASK_LENGTH_MAX => raw(sizeT + 1) << 8 + raw(sizeT + 2)
      case _ => raw(sizeT)
    }
  }
  val size_tlv = sizeT + sizeL + sizeV

  /**
    * Инстанс должен содержать в поле Value только свои данные.
    * Все последующие байты обрезаем.
    */
  val tlv = raw.slice(0, size_tlv)

  // Class by name
  def classSz: String = classes(tlv(0) & MASK_CLASS)

  // Class by ID
  def classId: Int = (tlv(0) & MASK_CLASS) >> 6

  // Tag value
  def tag: Int = tlv(0) & MASK_TAG

  // TLV Data
  def data: Array[Byte] = {
    tlv.slice(sizeT + sizeL, size_tlv)
  }

  // Structured or primitive
  def constructed: Boolean = (tlv(0) & MASK_STRUCT) != 0

  // Количество вложенных элементов первого уровня
  def count: Int = {
    var l = sizeT + sizeL
    var count = 0

    if(constructed) {
      do {
        val tlvNested = TLV(tlv.drop(l))
        l += tlvNested.size_tlv
        count += 1
      }
      while (l < size_tlv)
    }
    count
  }

  def toList: List[TLV] = {
    count match {
      case 0 => Nil
      case _ => {
        val lb = new ListBuffer[TLV]()
        var l = sizeT + sizeL
        for (i <- 0 until count) {
          val t = TLV(tlv.drop(l))
          l += t.size_tlv
          lb += t
        }
        lb.toList
      }
    }
  }

  def item(index: Int): Try[TLV]= {

    Try {
      if( !constructed || index >= count ) throw new RuntimeException("\n\nInvalid index or primitive type\n\n")

      var result = None: Option[TLV]
      var l = sizeT + sizeL

      for (i <- 0 to index) {
        result = Some(TLV(tlv.drop(l)))
        l += result.head.size_tlv
      }

      result.head
    }
  }

  def info: String = {
    val sz = ArrayBuffer[String]()
    sz += s"\n\nTLV: ${bytes2hex(tlv)}\n"
    sz += s"T field size: ${sizeT}\n"
    sz += s"L field size: ${sizeL}\n"
    sz += s"V field size: ${sizeV}\n"

    sz += s"Tag class: ${classSz}\n"
    sz += s"Constructed: ${if(constructed) "'Y'" else "'N'"}\n"
    sz += s"Nested items: ${count}\n"
    sz += s"Value: ${bytes2hex(data)}"
    sz.toArray.mkString

  }

}
