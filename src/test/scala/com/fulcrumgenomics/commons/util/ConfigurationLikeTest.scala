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

  private class TestConfiguration extends ConfigurationLike with LazyLogging {
    protected val config : Config = _config.resolve()
    override val _logger: Option[Logger] = Some(logger)
  }

  private val conf = new TestConfiguration()

  "ConfigurationLike" should "lookup basic types with keys that exist" in {
    conf.configure[String]("a-string") shouldBe "hello"
    conf.configure[Boolean]("a-boolean") shouldBe true
    conf.configure[Byte]("a-byte") shouldBe 42.toByte
    conf.configure[Short]("a-short") shouldBe 123
    conf.configure[Int]("an-int") shouldBe 12345
    conf.configure[Long]("a-long") shouldBe 1234567890
    conf.configure[Float]("a-float") shouldBe 12345.67f
    conf.configure[Double]("a-double") shouldBe 12345.6789
    conf.configure[BigInt]("a-bigint") shouldBe BigInt("999999999999999999999999999999999999999999999999999999999999999")
    conf.configure[BigDecimal]("a-bigdec") shouldBe BigDecimal("999999999999999999999999999999999999999999999999999999999999999.1")
    conf.configure[Path]("a-path") shouldBe PathUtil.pathTo("/foo/bar/splat.txt")
    conf.configure[Duration]("some-time") shouldBe Duration.ofSeconds(60)
    conf.configure[List[String]]("some-string-list") should contain theSameElementsInOrderAs List("a", "list", "of", "strings")
    conf.configure[List[Int]]("some-int-list") should contain theSameElementsInOrderAs List(1, 2, 3, 4)
    conf.configure[Seq[Double]]("empty-string-list") shouldBe empty
    conf.configure[Set[String]]("some-string-set") should contain theSameElementsAs Set("A", "B", "C")
    conf.configure[Option[String]]("some-option-string").value shouldBe "Some"
    conf.configure[Option[String]]("option-empty-string").value shouldBe ""
    conf.configure[Option[String]]("none-option-string") shouldBe 'empty
    conf.configure[Option[Int]]("some-option-int").value shouldBe 1
    conf.configure[Option[Int]]("none-option-int") shouldBe 'empty
  }

  it should "support optional configuration" in {
    conf.optionallyConfigure[String]("a-string") shouldBe Some("hello")
    // The following should all behave the same, but check a few just in case
    conf.optionallyConfigure[Long]("non-existent-key") shouldBe None
    conf.optionallyConfigure[Path]("non-existent-key") shouldBe None
    conf.optionallyConfigure[Duration]("non-existent-key") shouldBe None
  }

  it should "support default values" in {
    // All these exist, defaults should be ignored
    conf.configure[String]("a-string", "wont-get-this") shouldBe "hello"
    conf.configure[Boolean]("a-boolean", false) shouldBe true
    conf.configure[Long]("a-long", 999) shouldBe 1234567890
    conf.configure[Float]("a-float", 999f) shouldBe 12345.67f
    conf.configure[Path]("a-path", PathUtil.pathTo("/path/to/nowhere")) shouldBe PathUtil.pathTo("/foo/bar/splat.txt")
    conf.configure[Path]("a-path", PathUtil.pathTo("/path/to/nowhere")) shouldBe PathUtil.pathTo("/foo/bar/splat.txt")

    // And these should all return their defaults
    conf.configure[String]("not-a-string", "wont-get-this") shouldBe "wont-get-this"
    conf.configure[Boolean]("not.a-boolean", false) shouldBe false
    conf.configure[Long]("foo.bar.splat.a-long", 999) shouldBe 999
    conf.configure[Float]("no-float", 999f) shouldBe 999f
    conf.configure[Path]("xxx", PathUtil.pathTo("/path/to/nowhere")) shouldBe PathUtil.pathTo("/path/to/nowhere")
    conf.configure[Path]("xxx", PathUtil.pathTo("/path/to/nowhere")) shouldBe PathUtil.pathTo("/path/to/nowhere")
  }

  private case class Foo(a: Int, bar: String)

  it should "throw an exception for unsupported types" in {

    // test configure (no default)
    {
      val log = captureLogger(() => {
        an[IllegalArgumentException] should be thrownBy conf.configure[Foo]("a-foo")
      })
      log should include("IllegalArgumentException")
    }

    // optionallyConfigure
    {
      val log = captureLogger(() => {
        an[IllegalArgumentException] should be thrownBy conf.optionallyConfigure[Foo]("a-foo")
      })
      log should include("IllegalArgumentException")
    }

    // test configure (with default)
    {
      val log = captureLogger(() => {
        an[IllegalArgumentException] should be thrownBy conf.configure[Foo]("a-foo", Foo(1, "2"))
      })
      log should include("IllegalArgumentException")
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

    val conf = new ConfigurationLike {  override val config : Config = ConfigFactory.parseFile(configPath.toFile) }
    conf.configure[String]("config-name") shouldBe "a-name"
    conf.configure[Boolean]("config-status") shouldBe false
    conf.optionallyConfigure[String]("config-does-not-exist") shouldBe 'empty
  }

  "Configuration" should "load config from a file" in {
    val conf = new Configuration {}

    // create a config file with some custom key/value pairs
    val configPath = Files.createTempFile("config", ".txt")
    configPath.toFile.deleteOnExit()
    Io.writeLines(configPath,
      s"""config-name = a-name
         |config-status = false
       """.stripMargin.split("\n")
    )

    // none of these should exists
    conf.optionallyConfigure[String]("config-name") shouldBe 'empty
    conf.optionallyConfigure[Boolean]("config-status") shouldBe 'empty
    conf.optionallyConfigure[String]("config-does-not-exist") shouldBe 'empty

    // initialize with the config file
    conf.initialize(path=Some(configPath))

    // all but the last should exists
    conf.configure[String]("config-name") shouldBe "a-name"
    conf.configure[Boolean]("config-status") shouldBe false
    conf.optionallyConfigure[String]("config-does-not-exist") shouldBe 'empty

  }
}
