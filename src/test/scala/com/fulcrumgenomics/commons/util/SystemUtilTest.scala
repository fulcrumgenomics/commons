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

import org.scalatest.OptionValues

import scala.util.matching.Regex

class SystemUtilTest extends UnitSpec with OptionValues {

  "SystemUtil" should "determine the OS name" in {
    (SystemUtil.IsOsLinux, SystemUtil.IsOsMac) match {
      case (true, false)  => ()
      case (false, true)  => ()
      case (true,  true)  => throw new IllegalStateException(s"System cannot both be Linux and Mac: ${SystemUtil.OsName}")
      case (false, false) => throw new IllegalStateException(s"Cannot determine the OS: ${SystemUtil.OsName}")
    }
  }

  private def matches(original: String, exact: String => Boolean, regex: Regex => Boolean): Unit = {
    original.length should be > 1

    val prefix = original.dropRight(1)
    val suffix = original.drop(1)

    exact(original) shouldBe true
    exact(prefix) shouldBe false
    exact(suffix) shouldBe false

    regex(s"$original*".r) shouldBe true
    regex(s"$original.*".r) shouldBe true
    regex(s"$original.+".r) shouldBe false

    regex(s"$prefix.*".r) shouldBe true
    regex(s"$prefix.".r) shouldBe true
    regex(prefix.r) shouldBe false

    regex(s".*$suffix".r) shouldBe true
    regex(s".$suffix".r) shouldBe true
    regex(suffix.r) shouldBe false
  }

  it should "match the OS version exactly and with a regex" in {
    matches(SystemUtil.OsVersion.value, s => SystemUtil.isOsVersion(s), r => SystemUtil.isOsVersion(r))
  }

  it should "match the OS architecture exactly and with a regex" in {
    matches(SystemUtil.OsArch.value, s => SystemUtil.isOsArch(s), r => SystemUtil.isOsArch(r))
  }
}
