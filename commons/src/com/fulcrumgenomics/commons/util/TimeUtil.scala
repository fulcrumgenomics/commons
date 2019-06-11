/*
 * The MIT License
 *
 * Copyright (c) 2015-2016 Fulcrum Genomics LLC
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

import java.text.{DecimalFormat, NumberFormat}
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.time.temporal.Temporal

object TimeUtil {
  /**
    * Formats a number of seconds into days:hours:minutes:seconds.
    */
  def formatElapsedTime(seconds: Long): String = {
    val timeFmt: NumberFormat = new DecimalFormat("00")
    val s: Long = seconds % 60
    val allMinutes: Long = seconds / 60
    val m: Long = allMinutes % 60
    val h: Long = allMinutes / 60
    List(h, m, s).map(timeFmt.format).mkString(":")
  }

  /** Gets the time stamp as a string (without Nanoseconds), or NA if it is None */
  def timestampStringOrNA(timestamp: Option[Temporal], fmt: DateTimeFormatter = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss").withZone(ZoneId.systemDefault())): String = {
    timestamp match {
      case Some(ts) => fmt.format(ts)
      case None => "NA"
    }
  }
}
