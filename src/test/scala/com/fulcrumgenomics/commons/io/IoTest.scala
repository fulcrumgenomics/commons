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

import java.io.{BufferedOutputStream, BufferedReader, FileInputStream, FileOutputStream, InputStreamReader, PrintStream}
import java.nio.file.{Files, FileSystems, Path, Paths}
import java.util.concurrent.TimeUnit
import java.util.zip.GZIPInputStream

import com.fulcrumgenomics.commons.util.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import java.io.BufferedInputStream

/**
 * Tests for various methods in the Io class
 */
class IoTest extends UnitSpec {
  /** Creates a random tmp file that is deleted on exit. */
  def tmpfile(readable: Boolean = true, writable: Boolean = true, executable: Boolean = true) : Path = {
    permission(Files.createTempFile("foo", "bar"), readable, writable, executable)
  }

  /** Creates a random tmp directory and sets it to be deleted on exit. */
  def tmpdir(readable: Boolean = true, writable: Boolean = true, executable: Boolean = true) : Path = {
    permission(Files.createTempDirectory("foo-dir"), readable, writable, executable)
  }

  /** Sets permissions on a path. */
  def permission(path: Path, readable: Boolean = true, writable: Boolean = true, executable: Boolean = true) : Path = {
    val file = path.toFile
    file.setReadable(readable, false)
    file.setWritable(writable, false)
    file.setExecutable(executable, false)
    file.deleteOnExit()
    path
  }

  private val hasMkfifo: Boolean = FileSystems.getDefault().supportedFileAttributeViews().contains("posix")

  def pipe(path: Path, lines: Seq[String]): Unit = {
    val pipeProcess = new ProcessBuilder("mkfifo", s"${path.toAbsolutePath}").start()
    pipeProcess.waitFor(5, TimeUnit.SECONDS) shouldBe true
    pipeProcess.exitValue() shouldBe 0
  }

  "Io.assertReadable" should "not throw an exception for extent files" in {
    val f1 = tmpfile(); val f2 = tmpfile(); val f3 = tmpfile()
    Io.assertReadable(f1)
    Io.assertReadable(List(f1, f2, f3))
  }

  it should "not throw an exception for special files" in {
    Io.assertReadable(Io.StdIn)
  }

  it should "not throw an exception for Unix fifos" in { 
    assume(hasMkfifo, "Canceling Posix specific test.")
    val lines    = Seq("foo", "bar")
    val pipeDir  = tmpdir()
    val pipePath = pipeDir.resolve("pipe1")
    pipe(pipePath, lines)
    Io.assertReadable(pipePath)
    Files.delete(pipePath)
  }

  it should "throw an exception for when file isn't readable" in {
    val nullpath: Path = null
    an[IllegalArgumentException] should be thrownBy { Io.assertReadable(nullpath) }
    an[IllegalArgumentException] should be thrownBy { Io.assertReadable(List(nullpath)) }
    an[IllegalArgumentException] should be thrownBy { Io.assertReadable(Some(nullpath)) }
    an[AssertionError] should be thrownBy {Io.assertReadable(tmpdir())}
    an[AssertionError] should be thrownBy {Io.assertReadable(tmpfile(readable=false))}
  }

  "Io.assertListable" should "not throw an exception for extent dirs" in {
    val f1 = tmpdir(); val f2 = tmpdir(); val f3 = tmpdir()
    Io.assertListable(f1)
    Io.assertListable(List(f1, f2, f3))
  }

  it should "not throw an exception when a directory isn't listable" in {
    val nullpath: Path = null
    an[IllegalArgumentException] should be thrownBy { Io.assertListable(nullpath) }
    an[IllegalArgumentException] should be thrownBy { Io.assertListable(List(nullpath)) }
    an[IllegalArgumentException] should be thrownBy { Io.assertListable(Some(nullpath)) }
    an[AssertionError] should be thrownBy {Io.assertListable(tmpfile())}
    an[AssertionError] should be thrownBy {Io.assertListable(tmpdir(readable=false))}
    an[AssertionError] should be thrownBy {Io.assertListable(tmpdir(executable=false))}
    an[AssertionError] should be thrownBy {Io.assertListable(tmpdir(readable=false, executable=false))}
  }

  "Io.assertCanWriteFile" should "throw an exception because the parent directory does not exist" in {
    an[AssertionError] should be thrownBy Io.assertCanWriteFile(PathUtil.pathTo("/path/to/nowhere"))
  }

  it should "throw an exception because the parent exits and is not a directory" in {
    val f = tmpfile()
    an[AssertionError] should be thrownBy Io.assertCanWriteFile(PathUtil.pathTo(f.toAbsolutePath.toString, "/parent_is_file"))
  }

  it should "throw an exception because the parent directory is not writable" in {
    val dir = tmpdir(writable=false)
    an[AssertionError] should be thrownBy Io.assertCanWriteFile(dir.resolve("somewhere"))
  }

  it should "throw an exception because it is a directory" in {
    val dir = tmpdir(writable=true)
    an[AssertionError] should be thrownBy Io.assertCanWriteFile(dir)
  }

  it should "not throw an exception when given a writable file" in {
    val file = tmpfile(writable=true)
    Io.assertCanWriteFile(file)
  }

  "Io.assertCanWriteFiles" should "not throw an exception when given a writable file" in {
    val file = tmpfile(writable=true)
    Io.assertCanWriteFiles(List(file, file))
  }

  "Io.assertWritableDirectory" should "succeed in assessing writability of a directory" in {
    val dir = tmpdir()
    Io.assertWritableDirectory(List(dir))
  }

  it should "throw an exception because the parent does not exist" in {
    an[AssertionError] should be thrownBy { Io.assertWritableDirectory(PathUtil.pathTo("/path/to/nowhere")) }
  }

  it should "throw an exception because the parent directory is not writable" in {
    val dir = tmpdir(writable=false)
    an[AssertionError] should be thrownBy Io.assertWritableDirectory(dir)
  }

  it should "throw an exception because the path was null" in {
    val nullpath: Path = null
    an[IllegalArgumentException] should be thrownBy Io.assertWritableDirectory(nullpath)
  }

  "Io.mkdirs" should "be able to create a directory" in {
    val dir = tmpdir()
    Files.delete(dir)
    Io.mkdirs(dir) shouldBe true
    Files.delete(dir)
  }

  "Io.findFirstExtentParent" should "find the first extant parent" in {
    val dir = tmpdir()
    val child = PathUtil.pathTo(dir.toAbsolutePath.toString, "child")
    Io.findFirstExtentParent(child).get.toAbsolutePath.toString shouldBe dir.toAbsolutePath.toString
    val grandchild = PathUtil.pathTo(dir.toAbsolutePath.toString, "grand/child")
    Io.findFirstExtentParent(grandchild).get.toAbsolutePath.toString shouldBe dir.toAbsolutePath.toString
  }

  it should "return None if no parent was found" in {
    Io.findFirstExtentParent(PathUtil.pathTo("/")).isEmpty shouldBe true
  }

  "Io.writeLines" should "write lines to a file that can be read back." in {
    val lines = List("hello world", "what was that?", "oh noes!!!!!", "goodbye cruel world.")
    val path = Files.createTempFile("test", "file")
    Io.writeLines(path, lines)
    val roundtripped = Io.readLines(path).toList
    Files.delete(path)

    roundtripped shouldBe lines
  }

  // The expected lines from the file loaded as a resource
  private val ResourceLines = Io.readLines(Paths.get("src/test/resources/com/fulcrumgenomics/commons/io/to-lines-from-resource-test.txt")).toList

  "Io.readLinesFromResource" should "read lines from a resource" in {
    val actual = Io.readLinesFromResource("/com/fulcrumgenomics/commons/io/to-lines-from-resource-test.txt").toList
    actual shouldBe ResourceLines
  }

  it should "work without a leading slash" in {
    val actual = Io.readLinesFromResource("com/fulcrumgenomics/commons/io/to-lines-from-resource-test.txt").toList
    actual shouldBe ResourceLines
  }

  it should "work with a relative path" in {
    // This works because the resource is in the same package as this test class
    val actual = Io.readLinesFromResource("to-lines-from-resource-test.txt").toList
    actual shouldBe ResourceLines
  }

  it should "fail when the resource does not exist" in {
    an[IllegalArgumentException] should be thrownBy Io.readLinesFromResource("/path/does/not/exist.json")
  }

  "Io.readLines" should "read Unix fifos" in { 
    assume(hasMkfifo, "Canceling Posix specific test.")
    val lines    = Seq("foo", "bar")
    val pipeDir  = tmpdir()
    val pipePath = pipeDir.resolve("pipe2")
    Files.deleteIfExists(pipePath) // in case of a failed previous test that needs clean up
    pipe(pipePath, lines)
    val writeFuture = Future { Io.writeLines(pipePath, lines); true }
    Io.readLines(pipePath).toList should contain theSameElementsInOrderAs lines
    Await.result(writeFuture, Duration(3, TimeUnit.SECONDS)) shouldBe true
    Files.delete(pipePath)
  }

  "Io.readBytesFromResource" should "correctly read binary data from a resource" in {
    val expected = Range.inclusive(Byte.MinValue, Byte.MaxValue).map(_.toByte).toArray
    val actual = Io.readBytesFromResource("/com/fulcrumgenomics/commons/io/to-bytes-from-resource-test.bin")
    actual shouldBe expected
  }

  "Io.toInputStream" should "open a file for gzip writing if it ends with .gz" in {
    val text = "This is a stupid little text fragment for compression. Yay compression!"
    val in   = Seq(text, text, text)
    val f    = Io.makeTempFile("test", ".gz")
    Io.writeLines(f, in)

    // Manually read it back as gzipped data
    val reader = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(f.toFile))))
    val out = Seq(reader.readLine(), reader.readLine(), reader.readLine())
    out shouldBe in
    reader.close()
  }

  it should "round trip data to a gzipped file if it ends with .gz" in {
    val text = "This is a stupid little text fragment for compression. Yay compression!"
    val in   = Seq(text, text, text)
    val f    = Io.makeTempFile("test", ".gz")
    Io.writeLines(f, in)
    val out = Io.readLines(f).toSeq
    out shouldBe in
  }

  it should "return BufferedInputStream for both regular and symlinked files" in {
    val tempDir = tmpdir()
    val realFile = tmpfile(readable=true)
    val link = Paths.get(tempDir.toString, "symbolic_file.txt")
    val symFile = Files.createSymbolicLink(link, realFile)
    Io.toInputStream(realFile) shouldBe an[BufferedInputStream]
    Io.toInputStream(symFile) shouldBe an[BufferedInputStream]
  }
  
  /** Impl of IoUtil to test that compressionLevel can be overridden and set */
  class FakeIo(var compressionLevel: Int = 5, override val bufferSize: Int = 128*1024) extends IoUtil {}
  object FakeIo extends FakeIo(compressionLevel=5, bufferSize=128*1024)
    
  "IoUtil.compressionLevel" should "be settable" in {
    FakeIo.compressionLevel = 6
    FakeIo.compressionLevel shouldBe 6
  }
}
