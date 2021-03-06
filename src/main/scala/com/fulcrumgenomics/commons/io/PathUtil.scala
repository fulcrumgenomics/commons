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

package com.fulcrumgenomics.commons.io

import java.nio.file.{Path, Paths}

/** Provides utility methods for creating and manipulating Path objects and path-like Strings. */
object PathUtil {
  @deprecated("Use `IllegalCharacters`")
  def illegalCharacters: String = IllegalCharacters

  val MaxFileNameSize: Int = 254
  val IllegalCharacters: String = "[!\"#$%&'()*/:;<=>?@\\^`{|}~] "

  /** Resolves a path from a String, and then makes the path absolute. Prefer this to PathUtil.pathTo elsewhere. */
  def pathTo(first: String, more: String*): Path = Paths.get(first, more:_*).toAbsolutePath.normalize

  /** Replaces a set of illegal characters within a String that is to be used as a filename. This will also
    * truncate the file name to be, at maximum, 255 characters.
    *
    * @param fileName the string that is to be used as a filename
    * @param illegalCharacters the set of characters to be replaced if found, defaults to [[IllegalCharacters]]
    * @param replacement an optional replacement character, defaulting to '_'; if None characters are just removed
    * @return the filename without illegal characters
    */
  def sanitizeFileName(fileName: String,
                       illegalCharacters: String = PathUtil.IllegalCharacters,
                       replacement: Option[Char] = Some('_'),
                       maxFileNameSize: Option[Int] = Some(MaxFileNameSize)): String = {
    val sanitizedFileName = replacement match {
      case None    => fileName.filter(c => !illegalCharacters.contains(c))
      case Some(r) => fileName.map(c => if (illegalCharacters.contains(c)) r else c)
    }
    sanitizedFileName.substring(0, Math.min(sanitizedFileName.length, MaxFileNameSize))
  }

  /** Replaces the extension on an existing path. */
  def replaceExtension(path: Path, ext:String) : Path = {
    val name = path.getFileName.toString
    val index = name.lastIndexOf('.')
    val newName = (if (index > 0) name.substring(0, index) else name) + ext
    path.resolveSibling(newName)
  }

  /** Remove the extension from a filename if present (the last . to the end of the string). */
  def removeExtension(path: Path) : Path = replaceExtension(path, "")

  /** Remove the extension from a filename if present (the last . to the end of the string). */
  def removeExtension(pathname: String) : String = {
    // Use Paths.get here so as not to turn the name into an absolute path, since we just toString it again
    removeExtension(Paths.get(pathname)).toString
  }

  /** Returns the extension of the filename (including the period) within the path,
    * or None if there is no period in the name.
    */
  def extensionOf(path: Path) : Option[String] = {
    val name  = path.getFileName.toString
    val index = name.lastIndexOf('.')
    if (index < 0) None
    else Some(name.substring(index))
  }

  /** Works similarly to the unix command basename, by optionally removing an extension, and all leading path elements. */
  def basename(name: Path, trimExt: Boolean = true) : String = {
    val x = if (trimExt) removeExtension(name) else name
    x.getFileName.toString
  }

  /** Works similarly to the unix command basename, by optionally removing an extension, and all leading path elements. */
  def basename(name: String, trimExt: Boolean) : String = {
   basename(pathTo(name), trimExt)
  }
}
