import $ivy.`com.lihaoyi::mill-contrib-buildinfo:0.4.0`
import mill.contrib.scoverage.ScoverageModule

import mill.Cross
import mill.scalalib.{SbtModule, PublishModule, Dep, CrossSbtModule, DepSyntax}
import mill.scalalib.publish.{PomSettings, License, Developer, SCM}
import ammonite.ops._

import scala.sys.process.Process

object commons extends SbtModule with ScoverageModule with PublishModule { outer =>
  def millSourcePath = super.millSourcePath / ammonite.ops.up
  def artifactName = "commons"
  def gitHash = Process("git rev-parse --short HEAD").lineStream.head
  def publishVersion = s"0.6.0-${gitHash}-SNAPSHOT"
  def scalaVersion = "2.12.8"
  def scoverageVersion = "1.3.1"
  def scalacOptions = Seq("-target:jvm-1.8", "-deprecation", "-unchecked")

  // TODO: start year (2015)
  // TODO: organization homepage ("http://www.fulcrumgenomics.com")
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

  // TODO: ignore long running tests with "-l LongRunningTest"
  object test extends Tests {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.5")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }

  object coverage extends ScoverageTests {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.5")
    def testFrameworks = Seq("org.scalatest.tools.Framework")

    // See: https://github.com/lihaoyi/mill/blob/master/scalalib/src/MiscModule.scala#L66-L77
    override def millSourcePath = outer.millSourcePath
    override def intellijModulePath = outer.millSourcePath / 'src / 'test
    override def sources = T.sources(
      millSourcePath / 'src / 'test / 'scala,
      millSourcePath / 'src / 'test / 'java
    )
    override def resources = T.sources{ millSourcePath / 'src / 'test / 'resources }

  }

  private def deployJar(assembly: PathRef, jarName:String) = {
    mkdir(pwd / 'jars)
    println(s"Copying artifact ${assembly.path} to jars / $jarName")
    cp.over(assembly.path, pwd / 'jars / jarName)
  }

  def assemblyJar = T { deployJar(assembly(), s"commons-${publishVersion()}.jar") }
}
