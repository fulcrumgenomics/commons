/*
 * The MIT License
 *
 * Copyright (c) 2017 Fulcrum Genomics LLC
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

import java.nio.file.{Files, Path}
import java.time.Duration

import com.fulcrumgenomics.commons.io.{Io, PathUtil}
import com.fulcrumgenomics.commons.reflect.ReflectionUtil
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.OptionValues

class ConfigurationLikeTest extends UnitSpec with CaptureSystemStreams with OptionValues {
  private val _config = ConfigFactory.parseString(
    s"""
      |a-string = hello
      |a-boolean = true
      |a-byte   = 42
      |a-short  = 123
      |an-int   = 12345
      |a-long   = 1234567890
      |a-float  = 12345.67
      |a-double = 12345.6789
      |a-bigint = 999999999999999999999999999999999999999999999999999999999999999
      |a-bigdec = 999999999999999999999999999999999999999999999999999999999999999.1
      |a-path   = /foo/bar/splat.txt
      |some-time   = 60s
      |a-path   = /foo/bar/splat.txt
      |some-string-list = ["a", list, "of", strings]
      |some-int-list = [1, 2, 3, 4]
      |empty-string-list = []
      |some-string-set = ["A", "B", "C"]
      |some-option-string = "Some"
      |option-empty-string = ""
      |none-option-string = "${ReflectionUtil.SpecialEmptyOrNoneToken}"
      |some-option-int = 1
      |none-option-int = "${ReflectionUtil.SpecialEmptyOrNoneToken}"
      |a-foo = "do not use"
    """.stripMargin)

  private class TestConfiguration extends Configuration(_config.resolve()) with LazyLogging {
    override protected def handle(message: => String, throwable: Option[Throwable]): Nothing = {
      logger.warning(message)
      super.handle(message, throwable)
    }
  }

  private val conf = new TestConfiguration()

  "ConfigurationLike" should "lookup basic types with keys that exist" in {
    conf[String]("a-string") shouldBe "hello"
    conf[Boolean]("a-boolean") shouldBe true
    conf[Byte]("a-byte") shouldBe 42.toByte
    conf[Short]("a-short") shouldBe 123
    conf[Int]("an-int") shouldBe 12345
    conf[Long]("a-long") shouldBe 1234567890
    conf[Float]("a-float") shouldBe 12345.67f
    conf[Double]("a-double") shouldBe 12345.6789
    conf[BigInt]("a-bigint") shouldBe BigInt("999999999999999999999999999999999999999999999999999999999999999")
    conf[BigDecimal]("a-bigdec") shouldBe BigDecimal("999999999999999999999999999999999999999999999999999999999999999.1")
    conf[Path]("a-path") shouldBe PathUtil.pathTo("/foo/bar/splat.txt")
    conf[Duration]("some-time") shouldBe Duration.ofSeconds(60)
    conf[List[String]]("some-string-list") should contain theSameElementsInOrderAs List("a", "list", "of", "strings")
    conf[List[Int]]("some-int-list") should contain theSameElementsInOrderAs List(1, 2, 3, 4)
    conf[Seq[Double]]("empty-string-list") shouldBe empty
    conf[Set[String]]("some-string-set") should contain theSameElementsAs Set("A", "B", "C")
    conf[Option[String]]("some-option-string").value shouldBe "Some"
    conf[Option[String]]("option-empty-string").value shouldBe ""
    conf[Option[String]]("none-option-string") shouldBe 'empty
    conf[Option[Int]]("some-option-int").value shouldBe 1
    conf[Option[Int]]("none-option-int") shouldBe 'empty
  }

  it should "support optional configuration" in {
    conf.get[String]("a-string") shouldBe Some("hello")
    // The following should all behave the same, but check a few just in case
    conf.get[Long]("non-existent-key") shouldBe None
    conf.get[Path]("non-existent-key") shouldBe None
    conf.get[Duration]("non-existent-key") shouldBe None
  }

  it should "support default values" in {
    // All these exist, defaults should be ignored
    conf.getOrElse[String]("a-string", "wont-get-this") shouldBe "hello"
    conf.getOrElse[Boolean]("a-boolean", false) shouldBe true
    conf.getOrElse[Long]("a-long", 999) shouldBe 1234567890
    conf.getOrElse[Float]("a-float", 999f) shouldBe 12345.67f
    conf.getOrElse[Path]("a-path", PathUtil.pathTo("/path/to/nowhere")) shouldBe PathUtil.pathTo("/foo/bar/splat.txt")
    conf.getOrElse[Path]("a-path", PathUtil.pathTo("/path/to/nowhere")) shouldBe PathUtil.pathTo("/foo/bar/splat.txt")

    // And these should all return their defaults
    conf.getOrElse[String]("not-a-string", "wont-get-this") shouldBe "wont-get-this"
    conf.getOrElse[Boolean]("not.a-boolean", false) shouldBe false
    conf.getOrElse[Long]("foo.bar.splat.a-long", 999) shouldBe 999
    conf.getOrElse[Float]("no-float", 999f) shouldBe 999f
    conf.getOrElse[Path]("xxx", PathUtil.pathTo("/path/to/nowhere")) shouldBe PathUtil.pathTo("/path/to/nowhere")
    conf.getOrElse[Path]("xxx", PathUtil.pathTo("/path/to/nowhere")) shouldBe PathUtil.pathTo("/path/to/nowhere")
  }

  private case class Foo(a: Int, bar: String)

  it should "throw an exception for unsupported types" in {

    // test configure (no default)
    {
      val log = captureLogger(() => {
        an[IllegalArgumentException] should be thrownBy conf[Foo]("a-foo")
      })
      log should include("Don't know how to configure")
    }

    // optionallyConfigure
    {
      val log = captureLogger(() => {
        an[IllegalArgumentException] should be thrownBy conf.get[Foo]("a-foo")
      })
      log should include("Don't know how to configure")
    }

    // test configure (with default)
    {
      val log = captureLogger(() => {
        an[IllegalArgumentException] should be thrownBy conf.getOrElse[Foo]("a-foo", Foo(1, "2"))
      })
      log should include("Don't know how to configure")
    }
  }

  it should "load a config from a file" in {
    val configPath = Files.createTempFile("config", ".txt")
    configPath.toFile.deleteOnExit()

    Io.writeLines(configPath,
      s"""config-name = a-name
         |config-status = false
       """.stripMargin.split("\n")
    )

    val conf = Configuration(configPath)
    conf[String]("config-name") shouldBe "a-name"
    conf[Boolean]("config-status") shouldBe false
    conf.get[String]("config-does-not-exist") shouldBe 'empty

    conf.requestedKeys should contain theSameElementsInOrderAs Seq("config-does-not-exist", "config-name", "config-status")
  }
}
