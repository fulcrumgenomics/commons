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

import java.nio.file.Path

import com.fulcrumgenomics.commons.util.LazyLogging

import scala.collection.mutable.ListBuffer
import scala.util.Try

trait CommandLineTool extends LazyLogging {

  /** The name of the executable such as Rscript or gs. */
  val Executable: String

  /** Command used for the above executable.
    *
    * For example, to run `Rscript --version`:
    * {{{
    * scala> import com.fulcrumgenomics.commons.io._
    * scala> val TestArgs = Seq("--version")
    * scala> val Executable = "Rscript"
    * scala> Rscript.execArgs(Executable +: TestArgs:_*)
    * }}}
    * */
  val TestArgs: Seq[String]

  /** Exception class that holds onto an executable's exit/status code. */
  case class ToolException(status: Int) extends RuntimeException {
    override def getMessage: String = s"$Executable failed with exit code $status."
  }

  /** Executes a command and returns the stdout.
    *
    * @param command the command to be executed
    */
  def execArgs(command: String*): Try[ListBuffer[String]] = {
    Try {
      // Read redirected stdout to a string
      val process  = new ProcessBuilder(command: _*).redirectErrorStream(true).start()
      val stdout   = ListBuffer[String]()
      val sink     = new AsyncStreamSink(process.getInputStream, s => stdout.append(s))
      val exitCode = process.waitFor()

      sink.close()

      require(exitCode == 0, s"Command did not execute successfully. Exit code: $exitCode")

      stdout
    }
  }

  /** Returns true if the tool is available and false otherwise. */
  lazy val Available: Boolean = execArgs(Executable +: TestArgs: _*).isSuccess
}

/** Trait that indicates the executable can run scripts.*/
trait ScriptRunner {
  self: CommandLineTool =>

  /** Suffix for scripts that can be run by this tool. */
  val Suffix: String

  /** Executes a script from the classpath if this executable is available.
    *
    * @param scriptResource the name of the script resource on the classpath
    * @param args a variable list of arguments to pass to the script
    * @throws Exception when we are unable to execute to this script on the classpath with the given arguments.
    */
  def execIfAvailable(scriptResource: String, args: String*): Unit =
    try { if (Available) exec(scriptResource, args:_*) }
    catch { case e: Exception => throw new Exception(s"Cannot execute script from $scriptResource with args:$args") }

  /** Executes a script stored at a Path if the this executable is available.
    *
    * @param script Path of the script to be run
    * @param args a variable list of arguments to pass to the script
    * @throws Exception when we are unable to execute to this script on the classpath with the given arguments.
    */
  def execIfAvailable(script: Path, args: String*): Unit = execIfAvailable(script.toString, args: _*)

  /** Executes a script from the classpath if this executable is available, raise an exception otherwise.
    *
    * @throws ToolException when the exit code from the called process is not zero.
    * @throws Exception when we are unable to execute to this script on the classpath with the given arguments.
    * @param scriptResource the name of the script resource on the classpath
    * @param args a variable list of arguments to pass to the script
    * @return Unit as this code is side-effecting
    */
  def exec(scriptResource: String, args: String*): Unit = exec(writeResourceToTempFile(scriptResource), args:_*)

  /** Execute from a script stored at a Path if this executable is available, raise an exception otherwise.
    *
    * @throws ToolException when the exit code from the called process is not zero.
    * @throws Exception when we are unable to execute to this script with the given arguments.
    * @param script Path of the script to be run
    * @param args a variable list of arguments to pass to the script
    * @return Unit as this code is side-effecting
    */
  def exec(script: Path, args: String*): Unit = try {
    val command = Executable +: script.toAbsolutePath.toString +: args
    val process = new ProcessBuilder(command:_*).redirectErrorStream(false).start()
    val pipe1   = Io.pipeStream(process.getErrorStream, logger.info)
    val pipe2   = Io.pipeStream(process.getInputStream, logger.debug)
    val retval  = process.waitFor()
    pipe1.close()
    pipe2.close()

    if (retval != 0) throw ToolException(retval)
  } catch {case e: Exception => throw new Exception(s"Cannot execute script from path $script with args: $args") }

  /** Extract a resource from the classpath and writes it to a temp file on disk.
    *
    * @param resource a given name on the classpath
    * @return path to the temporary file
    * */
  private def writeResourceToTempFile(resource: String): Path = {
    val lines = Io.readLinesFromResource(resource).toSeq
    val path  = Io.makeTempFile("script.", suffix = Suffix)
    path.toFile.deleteOnExit()
    Io.writeLines(path, lines)
    path
  }
}

trait Versioned {
  self: CommandLineTool =>

  /** The default version flag. */
  val VersionFlag: String   = "--version"

  /** Argument used to test version of the executable. */
  val TestArgs: Seq[String] = Seq(VersionFlag)

  /** Return version of this tool */
  lazy val Version: Try[ListBuffer[String]] = execArgs(Executable +: TestArgs:_*)
}

trait Modular {
  self: CommandLineTool =>

  /** Command used to test if a module is included with the tested executable. */
  def TestModuleCommand(module: String): Seq[String]

  /** Command used to test if multiple modules are included with the tested executable. */
  def TestModuleCommand(modules: String*): Seq[Seq[String]] = modules.map(TestModuleCommand)

  /** Returns true if the tested module exist with the tested executable. */
  def ModuleAvailable(module: String): Boolean = execArgs(TestModuleCommand(module): _*).isSuccess

  /** Returns true if all tested modules exist with the tested executable.
    *
    * For example:
    * {{
    * scala> import com.fulcrumgenomics.commons.io._
    * scala> Rscript.IsModuleAvailable(Seq("ggplot2", "dplyr"))
    * }}
    */
  def ModuleAvailable(modules: Seq[String]): Boolean =  modules.map(ModuleAvailable).forall(_ == true)
}

object Rscript extends CommandLineTool with Versioned with Modular with ScriptRunner {
  val Executable: String = "Rscript"
  val Suffix: String     = ".R"
  def TestModuleCommand(module: String): Seq[String] = Seq(Executable, "-e", s"stopifnot(require('$module'))")

  /** Only returns true if R exists and ggplot2 is installed */
  override lazy val Available: Boolean = {
    val ToolAvailable: Boolean   = execArgs(Executable +: Seq(VersionFlag):_*).isSuccess
    Seq(ToolAvailable, ModuleAvailable("ggplot2")).forall( _ == true)
  }
}

trait Python extends CommandLineTool with Versioned with Modular with ScriptRunner {
  val Executable: String
  val Suffix: String = ".py"
  def TestModuleCommand(module: String): Seq[String] = Seq(Executable, "-c", s"import $module")
}

object Python extends Python  { val Executable: String = "python" }
object Python2 extends Python { val Executable: String = "python2" }
object Python3 extends Python { val Executable: String = "python3" }