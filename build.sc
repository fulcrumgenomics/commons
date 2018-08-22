import mill.Cross
import mill.scalalib.{SbtModule, PublishModule, Dep, CrossSbtModule, DepSyntax}
import mill.scalalib.publish.{PomSettings, License, Developer, SCM}
import scala.sys.process.Process

object commons extends Cross[CommonsModule]("2.11.11", "2.12.2")

class CommonsModule(val crossScalaVersion: String) extends CrossSbtModule with PublishModule {
  def millSourcePath = super.millSourcePath / ammonite.ops.up
  def artifactName = "commons"
  def gitHash = Process("git rev-parse --short HEAD").lineStream.head
  def publishVersion = s"0.6.0-${gitHash}-SNAPSHOT"

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.fulcrumgenomics",
    url = "https://github.com/fulcrumgenomics/commons",
    licenses = Seq(License("MIT license", "http://www.opensource.org/licenses/mit-license.php")),
    scm = SCM(
      "git://github.com/fulcrumgenomics/commons.git",
      "scm:git://github.com/fulcrumgenomics/commons.git"
    ),
    developers = Seq(
      Developer("nh13", "Nils Homer", "https://github.com/nh13"),
      Developer("tfenne", "Tim Fennell", "https://github.com/tfenne")
    )
  )

  def ivyDeps = Agg(
    ivy"org.scala-lang:scala-compiler:${scalaVersion()}",
    ivy"com.typesafe:config:1.3.2",
  )
  
  object test extends Tests {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.0.1"
    )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}
