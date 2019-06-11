package com.fulcrumgenomics.commons.util

import java.io.OutputStream
import java.nio.file.Files

import com.fulcrumgenomics.commons.io.PathUtil

/** ANSI Strings for cursor movements.
  *
  * See: http://www.tldp.org/HOWTO/Bash-Prompt-HOWTO/x361.html
  */
private object CursorMovement {
  def startOfLine(): String = "\r"
  def eraseToEOL(): String = "\u001b" // \033[K"
  def eraseLine(): String = s"${startOfLine()}${eraseToEOL()}"
}

class CaptureSystemStreamsTest extends UnitSpec with CaptureSystemStreams with LazyLogging {
  import CursorMovement._

  private val consoleSink: OutputStream = Files.newOutputStream(PathUtil.pathTo("/dev/null"))

  private val knrm: String = "\u001B[0m"

  "CaptureSystemStreams" should "capture stderr and stdout separately" in {
    var err = ""
    var out = ""

    Console.withOut(consoleSink) {
      out = captureStdout(() => {
        err = captureStderr(() => {
          println("ABC")
          System.out.println("DEF")
          System.err.println("GHI")
        })
        System.out.println("JKL")
        System.err.print(s"MNO${eraseLine()}") // not captured, sorry :/
      })
    }
    out shouldBe "DEF\nJKL\n"
    err shouldBe "GHI\n"

    Console.withOut(consoleSink) {
      val (e, o) = captureStreams(() => {
        println("ABC")
        System.out.println("DEF")
        System.err.println("GHI")
      })
      System.out.print(s"JKL${eraseLine()}") // not captured, sorry :/
      System.err.print(s"MNO${eraseLine()}") // not captured, sorry :/
      err = e
      out = o
    }
    out shouldBe "DEF\n"
    err shouldBe "GHI\n"
  }

  it should "capture the logger" in {
    var log = ""
    Console.withOut(consoleSink) {
      log = captureLogger(() => {
        println("ABC")
        System.out.print(s"DEF${eraseLine()}") // not captured, sorry :/
        System.err.print(s"GHI${eraseLine()}") // not captured, sorry :/
        logger.info("JKL")
      })
    }
    log.endsWith("JKL\n") shouldBe true
  }

  it should "capture it all!" in {
    var err = ""
    var out = ""
    var log = ""
    Console.withOut(consoleSink) {
      val (e, o, l) = captureItAll(() => {
        println("ABC")
        System.out.print("DEF\n")
        System.err.print("GHI\n")
        logger.info("JKL")
      })
      err = e
      out = o
      log = l

    }
    out shouldBe "DEF\n"
    err shouldBe "GHI\n"
    log.endsWith("JKL\n") shouldBe true
  }
}
