// IMPORTANT: the version of the contrib modules must be 0.4.0-9-1db2ee or later.
// IMPORTANT: if you change this version to a release, please update the .travis.yml
// FIXME: change to a release version after 0.4.0-9-1db2ee
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:0.4.0-9-1db2ee`
import mill.contrib.scoverage.ScoverageModule

import mill.scalalib.{PublishModule, Dep, DepSyntax, ScalaModule}
import mill.scalalib.publish.{PomSettings, License, Developer, SCM}
import ammonite.ops._

import scala.sys.process.Process

object commons extends ScalaModule with ScoverageModule with PublishModule {
  def artifactName = "commons"
  def gitHash = os.proc("git", "rev-parse", "--short", "HEAD").call().out.trim
  def publishVersion = s"0.8.0-${gitHash}-SNAPSHOT"
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
    ivy"com.typesafe:config:1.3.2"
  )

  // TODO: ignore long running tests with "-l LongRunningTest"
  object test extends ScoverageTests {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.5")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }

  private def deployJar(assembly: PathRef, jarName:String) = {
    mkdir(pwd / 'jars)
    println(s"Copying artifact ${assembly.path} to jars / $jarName")
    cp.over(assembly.path, pwd / 'jars / jarName)
  }

  def assemblyJar = T { deployJar(assembly(), s"commons-${publishVersion()}.jar") }
}
