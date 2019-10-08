/*
 * The MIT License
 *
 * Copyright (c) 2019 Fulcrum Genomics
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
 *
 */

package com.fulcrumgenomics.commons.util
import scala.util.matching.Regex


/** System utility defaults and methods. Many methods mock functions in `org.apache.commons:commons-lang3`. */
object SystemUtil {

  /** The operating system name prefixes for Linux */
  private val LinuxNamePrefixes: Seq[String] = Seq("Linux", "LINUX")

  /** The operating system name prefixes for Mac */
  private val MacNamePrefixes: Seq[String] = Seq("Mac")

  /** The current operating system name. */
  lazy val OsName: Option[String] = getSystemProperty(property = "os.name")

  /** The current operating system architecture */
  lazy val OsArch: Option[String] = getSystemProperty(property = "os.arch")

  /** The current operating system version */
  lazy val OsVersion: Option[String] = getSystemProperty(property = "os.version")

  /** Gets a system property.  Returns None if not found or we are not allowed to retrieve the property. */
  def getSystemProperty(property: String): Option[String] = {
    try { Option(System.getProperty(property)) }
    catch { case _: SecurityException => None } // not allowed to look at this property
  }

  /** True if this operating system is Linux, false otherwise. */
  lazy val IsOsLinux: Boolean = LinuxNamePrefixes.exists(prefix => OsName.exists(_.startsWith(prefix)))

  /** True if this operating system is Mac, false otherwise. */
  lazy val IsOsMac: Boolean = MacNamePrefixes.exists(prefix => OsName.exists(_.startsWith(prefix)))

  /** Returns true if the architecture is the given name, false otherwise. */
  def isOsArch(name: String): Boolean = OsArch.contains(name)

  /** Returns true if the architecture matches the given regular expression, false otherwise. */
  def isOsArch(regex: Regex): Boolean = OsArch.exists(regex.matches)

  /** Returns true if the operating system version is the given name, false otherwise. */
  def isOsVersion(name: String): Boolean = OsVersion.contains(name)

  /** Returns true if the operating system version matches the given regular expression, false otherwise. */
  def isOsVersion(regex: Regex): Boolean = OsVersion.exists(regex.matches)

  /** True if the current system could support the Intel Inflater and Deflater, false otherwise. */
  lazy val IntelCompressionLibrarySupported: Boolean = {
    if (!SystemUtil.IsOsLinux && !SystemUtil.IsOsMac) false
    else if (SystemUtil.isOsArch(name = "ppc64le")) false
    else if (SystemUtil.IsOsMac && SystemUtil.isOsVersion("10\\.14\\.".r)) false // FIXME: https://github.com/Intel-HLS/GKL/issues/101
    else true
  }
}
