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

package com.fulcrumgenomics.commons.io

import java.nio.file.{Files, Path}

import com.fulcrumgenomics.commons.util.LazyLogging

import scala.io.Source

import scala.util.{Success, Try}

trait CommandLineTool extends LazyLogging {
  /** The name of the executable such as Rscript or gs. */
  val Executable: String

  /** Command used for the above executable. */
  val TestCommand: Seq[String]

  /** Exception class that holds onto an executable's exit/status code. */
  case class ToolException(status: Int) extends RuntimeException {
    override def getMessage: String = s"$Executable failed with exit code $status."
  }


  /** Returns a tuple of (Boolean, String): first element is true when command can execute
    *  with no error; second element is a string of command stdout */
  def execArgs(command: String*):(Boolean, String) = {
    try {
      // Create temp file for storing stdout result
      val output = Files.createTempFile("output.", ".txt")
      output.toFile.deleteOnExit()

      // Redirect stdout to the temp file & start process
      val process = new ProcessBuilder(command: _*)
        .redirectErrorStream(true)
        .redirectOutput(output.toFile)
        .start()

      // True if the command can execute with no error
      val canExecute: Boolean = process.waitFor() == 0

      // Read redirected stdout to a string
      val bufferedSource      = Source.fromFile(output.toString)
      val lines: String       = bufferedSource.getLines().mkString
      bufferedSource.close()

      (canExecute, lines)
    }
    catch { case e: Exception => (false, "") }
  }

  /** Returns true if the tool is available and false otherwise. */
  lazy val Available: Boolean = {
    execArgs(Executable +: TestCommand:_*)._1
  }
}

trait CanRunScript {
  self: CommandLineTool =>
  /** Suffix for scripts that can be run by this tool. */
  val Suffix: String

  /** Executes a script from the classpath if the tested executable is available. */
  def execIfAvailable(scriptResource: String, args: String*): Try[Unit] =
    if (Available) exec(scriptResource, args:_*) else Success(Unit)

  /** Executes from a script stored at a Path if the tested executable is available. */
  def execIfAvailable(script: Path, args: String*): Try[Unit] =
    if (Available) exec(script, args:_*) else Success(Unit)

  /** Executes a script from the classpath. */
  def exec(scriptResource: String, args: String*): Try[Unit] =
    Try { writeResourceToTempFile(scriptResource) }.map(path => exec(path, args:_*))

  /** Executes from a script stored at a Path. */
  def exec(script: Path, args: String*): Try[Unit] = Try {
    val command = Executable +: script.toAbsolutePath.toString +: args
    val process = new ProcessBuilder(command:_*).redirectErrorStream(false).start()
    val pipe1   = Io.pipeStream(process.getErrorStream, logger.info)
    val pipe2   = Io.pipeStream(process.getInputStream, logger.debug)
    val retval  = process.waitFor()
    pipe1.close()
    pipe2.close()

    if (retval != 0) throw ToolException(retval)
  }

  /** Extracts a resource from the classpath and writes it to a temp file on disk. */
  private def writeResourceToTempFile(resource: String): Path = {
    val lines = Io.readLinesFromResource(resource).toSeq
    val path = Io.makeTempFile("script.", suffix = Suffix)
    path.toFile.deleteOnExit()
    Io.writeLines(path, lines)
    path
  }
}

trait Versioned {
  self: CommandLineTool =>
  val VersionFlag: String      = "--version"
  val TestCommand: Seq[String] = Seq(VersionFlag)

  /** Returns version of the tool */
  lazy val Version: String     = execArgs(Executable +: TestCommand:_*)._2
}

trait Modular {
  self: CommandLineTool =>

  /** Command used to test if a module is included with the tested executable. */
  def TestModuleCommand(module: String): Seq[String]

  /** Command used to test if multiple modules are included with the tested executable. */
  def TestModuleCommand(modules: Seq[String]): Seq[Seq[String]] = modules.map(TestModuleCommand)

  /** Returns true if the tested module exist with the tested executable. */
  def IsModuleAvailable(module: String): Boolean                = execArgs(TestModuleCommand(module): _*)._1

  /** Returns true if all tested modules exist with the tested executable. */
  def IsModuleAvailable(modules: Seq[String]): Boolean          =  modules.map(IsModuleAvailable).forall(x => x == true)
}

object Rscript extends CommandLineTool with Versioned with Modular with CanRunScript {
  val Executable: String      = "Rscript"
  val Suffix: String          = ".R"
  def TestModuleCommand(module: String): Seq[String] = Seq(Executable, "-e", s"stopifnot(require('$module'))")

  /** Only returns true if R exists and ggplot2 is installed */
  override lazy val Available: Boolean = {
    val ToolAvailable: Boolean    = execArgs(Executable +: Seq(VersionFlag):_*)._1
    val ModuleAvailable : Boolean = IsModuleAvailable(module = "ggplot2")
    Seq(ToolAvailable, ModuleAvailable).forall( _ == true)
  }
}

object GhostScript extends CommandLineTool with Versioned {
  val Executable: String = "gs"
}

object Python3 extends CommandLineTool with Versioned with Modular with CanRunScript {
  val Executable: String                             = "python3"
  val Suffix: String                                 = ".py"
  def TestModuleCommand(module: String): Seq[String] = Seq(Executable, "-c", s"'import $module'")
}