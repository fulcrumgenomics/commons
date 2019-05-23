import $ivy.`com.lihaoyi::mill-contrib-buildinfo:0.4.0`
import mill.contrib.scoverage.ScoverageModule

import mill.Cross
import mill.scalalib.{SbtModule, PublishModule, Dep, CrossSbtModule, DepSyntax}
import mill.scalalib.publish.{PomSettings, License, Developer, SCM}
import ammonite.ops._

import scala.sys.process.Process

object commons extends SbtModule with ScoverageModule with PublishModule {
  def millSourcePath = super.millSourcePath / ammonite.ops.up
  def artifactName = "commons"
  def gitHash = Process("git rev-parse --short HEAD").lineStream.head
  def publishVersion = s"0.6.0-${gitHash}-SNAPSHOT"
  def scalaVersion = "2.12.8"
  def scoverageVersion = "1.3.1"

  // Build options
  // TODO: Seq("-target:jvm-1.8", "-deprecation", "-unchecked"),
  // TODO: start year 2015
  // TODO: organizationHomepage := Some(url("http://www.fulcrumgenomics.com")),
  // TODO: testOptions in Test  += Tests.Argument(TestFrameworks.ScalaTest, "-h", Option(System.getenv("TEST_HTML_REPORTS")).getOrElse(htmlReportsDirectory)),
  // TODO: testOptions in Test  += Tests.Argument("-l", "LongRunningTest"), // ignores long running tests
  // TODO
  // - sonatype: http://www.lihaoyi.com/mill/page/common-project-layouts.html#publishing
  // - name the output assembly JAR

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

  object test extends ScoverageTests {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.5")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }

  private def deployJar(assembly: PathRef, jarName:String) = {
    mkdir(pwd / 'jars)
    println(s"Copying artifact ${assembly.path} to jars / $jarName")
    cp.over(assembly.path, pwd / 'jars / jarName)
  }

  def assemblyJar = T { deployJar(assembly(), "commons.jar") }
}
