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

import java.io.{ByteArrayOutputStream, PrintStream}

/** Methods to help capture stdin and stderr */
trait CaptureSystemStreams {
  type StdOutString = String
  type StdErrString = String
  type LoggerString = String

  /**
    * captures [[System.out]] while runnable is executing
    * @param runnable a code block to execute
    * @return everything written to [[System.out]] by runnable
    */
  def captureStdout(runnable: () => Unit): StdOutString = this.synchronized {
    captureSystemStream(runnable, System.out, (out: PrintStream) => System.setOut(out))
  }

  /**
    * captures [[System.err]] while runnable is executing
    * @param runnable a code block to execute
    * @return everything written to [[System.err]] by runnable
    */
  def captureStderr(runnable: () => Unit): StdErrString = this.synchronized {
    captureSystemStream(runnable, System.err, (out: PrintStream) => System.setErr(out))
  }

  /**
    * captures [[Logger.out]] while runnable is executing
    * @param runnable a code block to execute
    * @return everything written to [[Logger.out]] by runnable
    */
  def captureLogger(runnable: () => Unit): LoggerString = this.synchronized {
    val out: ByteArrayOutputStream = new ByteArrayOutputStream
    val previousOut = Logger.out
    Logger.out = new PrintStream(out)
    try {
      runnable()
    } finally {
      Logger.out = previousOut
    }
    out.toString
  }

  /**
    * captures both [[System.err]] and [[System.out]] while runnable is executing
    * @param runnable a code block to execute
    * @return everything written to [[System.err]] and [[System.out]] by runnable
    */
  def captureStreams(runnable: () => Unit): (StdErrString, StdOutString) = this.synchronized {
    var stdout: String = ""
    val stderr: String = captureStderr(() => {
      stdout = captureStdout(() => {
        runnable()
      })
    })
    (stderr, stdout)
  }

  /**
    * captures [[System.err]], [[System.out]], and [[Logger.out]] while runnable is executing
    * @param runnable a code block to execute
    * @return everything written to [[System.err]], [[System.out]], and [[Logger.out]]  by runnable
    */
  def captureItAll(runnable: () => Unit): (StdErrString, StdOutString, LoggerString) = this.synchronized {
    var stdout: String = ""
    var stderr: String = ""
    val log = captureLogger(() => {
      val (e, o) = captureStreams(() => {
        runnable()
      })
      stderr = e
      stdout = o
    })
    (stderr, stdout, log)
  }


  private def captureSystemStream(runnable: () => Unit, stream: PrintStream, setterMethod: (PrintStream) => Unit): String = {
    val out: ByteArrayOutputStream = new ByteArrayOutputStream
    setterMethod(new PrintStream(out))
    try {
      runnable()
    } finally {
      setterMethod(stream)
    }
    out.toString
  }
}

