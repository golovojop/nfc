package org.barabu.emv

import scala.util.{Failure, Success, Try}

import android.util.Log

object EmvUtils {

  implicit def intToByte(i: Int): Byte = i.toByte

  val LOG_ID = "MyLog"
  var VERSION_DEBUG = true

  /**
    *
    * @param v
    * @tparam T
    * @return
    */
  def notNull[T](v: T): Option[T] = {
    if(v == null) None else Some(v)
  }

  /**
    *
    * @param msg
    * @return
    */
  def logIt(msg: String) = {
    if(VERSION_DEBUG) Log.d(LOG_ID, msg)
  }


  /**
    * TODO: Конвертируем байтовый массив в строку
    * From here: https://gist.github.com/tmyymmt/3727124
    */
  def bytes2hex(bytes: Array[Byte], sep: Option[String] = None): String = {
    sep match {
      case None => bytes.map("%02x".format(_)).mkString
      case _ => bytes.map("%02x".format(_)).mkString(sep.get)
    }
  }

  /**
    * TODO: Конвертируем строки в байтовый массив
    * From here: https://gist.github.com/tmyymmt/3727124
    */
  def hex2bytes(hex: String): Array[Byte] = {
    hex.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }

}
