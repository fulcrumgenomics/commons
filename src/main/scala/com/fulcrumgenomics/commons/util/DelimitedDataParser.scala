/*
 * The MIT License
 *
 * Copyright (c) 2016 Fulcrum Genomics LLC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package com.fulcrumgenomics.commons.util

import com.fulcrumgenomics.commons.CommonsDef._
import com.fulcrumgenomics.commons.io.Io
import com.fulcrumgenomics.commons.reflect.ReflectionUtil

import scala.collection.compat._
import scala.reflect.runtime.{universe => ru}

/**
  * Represents a row of parsed data.  Provides methods for accessing values in a type-safe
  * way either via apply() methods for non-optional fields or via get for optional fields.
  */
class Row private[util] (private val headerIndices: Map[String,Int], private val fields: Array[String], val trim: Boolean) {
  /* Internal method to pull a value out of the field array by index and trim it if requested. */
  private def value(index: Int): String = {
    if (index > fields.length-1) throw new IndexOutOfBoundsException(s"Invalid column index supplied: ${index}")
    val string = fields(index)
    if (trim) string.trim else string
  }

  /** Fetches a value of the desired type by the 0-based column index. */
  def apply[A](index: Int)(implicit tag: ru.TypeTag[A]): A = {
    val c = ReflectionUtil.typeTagToClass(tag)
    ReflectionUtil.constructFromString(c, c, value(index)).get.asInstanceOf[A]
  }

  /** Fetches a value of the desired type by column name. */
  def apply[A](column: String)(implicit tag: ru.TypeTag[A]): A = apply(headerIndices(column))

  /** Non-type-parameterized apply method that returns the column value as a String. */
  def string(column: String): String = value(headerIndices(column))

  /** Non-type-parameterized apply method that returns the column value as a String. */
  def string(index: Int): String = value(index)

  /** Gets a value from the column with the specified index. If the value is null or empty returns None. */
  def get[A](index: Int)(implicit tag: ru.TypeTag[A]): Option[A] = {
    val string = value(index)
    if (string == null || string.isEmpty) {
      None
    }
    else {
      val c = ReflectionUtil.typeTagToClass(tag)
      Some(ReflectionUtil.constructFromString(c, c, string).get.asInstanceOf[A])
    }
  }

  /** Gets a value from the specified column. If the value is null or empty returns None.  If
    * `allowMissingColumn` is true, then None is returned if the given column name is not present. */
  def get[A](column: String, allowMissingColumn: Boolean = false)(implicit tag: ru.TypeTag[A]): Option[A] =
    (headerIndices.get(column), allowMissingColumn) match {
      case (Some(idx), _) => get[A](idx)      
      case (None, true)   => None
      case (None, false)  => throw new NoSuchElementException(column)
    }

  /** Gets the header keys (column names) for this row */
  def header: Iterable[String] = this.headerIndices.keys
}


object DelimitedDataParser {
  val DefaultBlankPolicy: Boolean = true
  val DefaultTrim :       Boolean = true

  /** Constructs a DelimitedDataParser for a path. */
  def apply(path: FilePath, delimiter: Char): DelimitedDataParser =
    apply(Io.toSource(path).getLines(), delimiter=delimiter, header=Seq.empty)

  /** Constructs a DelimitedDataParser for a path. */
  def apply(path: FilePath, delimiter: Char, header: Seq[String]): DelimitedDataParser =
    apply(Io.toSource(path).getLines(), delimiter=delimiter, header=header)

  /** Constructs a DelimitedDataParser for a sequence of lines. */
  def apply(lines: IterableOnce[String], delimiter: Char): DelimitedDataParser =
    apply(lines, delimiter=delimiter, header=Seq.empty)

  /** Constructs a DelimitedDataParser for a sequence of lines. */
  def apply(lines: IterableOnce[String], delimiter: Char, header: Seq[String]): DelimitedDataParser =
    new DelimitedDataParser(lines, delimiter=delimiter, header=header)
}

/**
  * A parser for files of text columns delimited by some character (e.g. tab-delimited or csv).
  *
  * @param lines the lines from the file
  * @param delimiter the delimiter between columns
  * @param ignoreBlankLines whether blank lines should be ignored
  * @param trimFields whether individual fields should have their String values trimmed
  * @param header the header names for the columns of delimited data. If empty, the first line should contain the header names.
  */
class DelimitedDataParser(lines: IterableOnce[String],
                          val delimiter: Char,
                          val ignoreBlankLines: Boolean = DelimitedDataParser.DefaultBlankPolicy,
                          val trimFields: Boolean = DelimitedDataParser.DefaultTrim,
                          val header: Seq[String] = Seq.empty) extends Iterator[Row] with LazyLogging {

  private val _lines = if (ignoreBlankLines) lines.iterator.filter(_.nonEmpty) else lines.iterator

  // An array of the headers
  private val _headers = {
    if (header.nonEmpty) header.toIndexedSeq
    else if (this._lines.hasNext) this._lines.next().split(delimiter).map(h => if (trimFields) h.trim else h).toIndexedSeq
    else IndexedSeq.empty
  }

  // A temporary array used for parsing each line into fields
  private val tmp = new Array[String](headers.size * 2)

  // A map of header name to column index
  private val headerIndices: Map[String,Int] = _headers.zipWithIndex.toMap

  /** Returns the headers encountered in the file, in order. */
  def headers: Seq[String] = this._headers

  /** True if there is another line available for parsing. */
  override def hasNext: Boolean = this._lines.hasNext

  /** Retrieves the next row. */
  override def next(): Row = {
    val line   = this._lines.next()
    val fields = split(line)
    if (fields.length != _headers.length) {
      logger.error(s"Line has incorrect number of fields. Header length is ${_headers.length}, row length is ${fields.length}")
      logger.error(s"Headers: ${_headers.mkString(delimiter.toString)}")
      logger.error(s"Line: ${line}")
      throw new IllegalStateException(s"Incorrect number of values in line.")
    }

    new Row(headerIndices, fields, trim=trimFields)
  }

  /** Splits the line into it's constituent fields. */
  protected[util] def split(line: String): Array[String] = {
    val count  = StringUtil.split(line=line, delimiter=this.delimiter, arr=this.tmp)
    val fields = new Array[String](count)
    System.arraycopy(this.tmp, 0, fields, 0, count)
    fields
  }
}
