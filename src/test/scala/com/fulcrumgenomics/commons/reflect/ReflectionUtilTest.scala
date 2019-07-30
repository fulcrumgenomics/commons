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
package com.fulcrumgenomics.commons.reflect

import java.util
import java.util.concurrent.TimeUnit

import com.fulcrumgenomics.commons.CommonsDef._
import com.fulcrumgenomics.commons.io.PathUtil
import com.fulcrumgenomics.commons.reflect.ReflectionUtilTest.SecondaryConstructor
import com.fulcrumgenomics.commons.util.{LogLevel, UnitSpec}

import scala.reflect.runtime.{universe => ru}
import scala.util.Success

object ReflectionUtilTest {
  case class IntNoDefault(var v: Int)
  case class TwoParamNoDefault(var v: Int = 2, var w: Int)
  case class IntDefault(var v: Int = 2)
  case class NoneDefaultWithOption(var w: Option[Int] = None)
  case class SomeDefaultWithOption(var o: Option[String] = Some("default"))
  case class NoParams()
  case class StringDefault(var s: String = "null")
  case class ComplexDefault(var v: StringDefault = new StringDefault())
  case class ComplexNoDefault(var v: StringDefault)
  // Primary constructor cannot be built from a string, so we have to use the secondary constructor
  object SecondaryConstructor {
    def apply(ints: String): SecondaryConstructor = new SecondaryConstructor(ints.split(",").map(_.toInt))
  }
  class SecondaryConstructor(val ints: Seq[Int]) extends scala.collection.immutable.Seq[Int] {
    def this(ints: String) = this(SecondaryConstructor.apply(ints).ints)
    def length: Int = ints.length
    def iterator: Iterator[Int] = ints.iterator
    def apply(idx: Int): Int = ints(idx)
  }
}

object UsingCompanion { def apply(s: String): UsingCompanion = new UsingCompanion(s.toInt)}
case class UsingCompanion(x: Int)

/** Sealed-trait hierarchy with no companion */
sealed trait NoCompanion
case object ANoCompanion extends NoCompanion
case object BNoCompanion extends NoCompanion
case object CNoCompanion extends NoCompanion

/** Sealed-trait hierarchy with a companion but neither a `values` nor a `findValues` method */
sealed trait EmptyCompanion
case object AEmptyCompanion extends EmptyCompanion
case object BEmptyCompanion extends EmptyCompanion
case object CEmptyCompanion extends EmptyCompanion
object NoCompanion

/** Sealed-trait hierarchy where `values` returns the values in the order the case objects they were declared */
sealed trait CompanionWithValues
case object ACompanionWithValues extends CompanionWithValues
case object BCompanionWithValues extends CompanionWithValues
case object CCompanionWithValues extends CompanionWithValues
object CompanionWithValues {
  def values: Seq[CompanionWithValues] = Seq(ACompanionWithValues, BCompanionWithValues, CCompanionWithValues)
}

/** Sealed-trait hierarchy where `findValues` returns the values in a different order than the case objects they were declared */
sealed trait CompanionWithFindValues
case object ACompanionWithFindValues extends CompanionWithFindValues
case object BCompanionWithFindValues extends CompanionWithFindValues
case object CCompanionWithFindValues extends CompanionWithFindValues
object CompanionWithFindValues {
  def findValues: Seq[CompanionWithFindValues] = Seq(BCompanionWithFindValues, ACompanionWithFindValues, CCompanionWithFindValues)
}

sealed trait CaseClasses
case class ACaseClass(s: String) extends CaseClasses
case class BCaseClass(s: String) extends CaseClasses
case class CCaseClass(s: String) extends CaseClasses

sealed trait CompanionWithNestedObjects
object CompanionWithNestedObjects {
  case object ACompanionWithNestedObjects extends CompanionWithNestedObjects
  case object BCompanionWithNestedObjects extends CompanionWithNestedObjects
  case object CCompanionWithNestedObjects extends CompanionWithNestedObjects
}

sealed trait BaseTrait
sealed trait LeftTrait extends BaseTrait
sealed trait RightTrait extends BaseTrait
case object LeftObject extends LeftTrait
case object RightObject extends RightTrait

/** Tests for many of the methods in ReflectionUtil.  More tests in other classes below! */
class ReflectionUtilTest extends UnitSpec {
  val mirror: ru.Mirror = ru.runtimeMirror(getClass.getClassLoader)

  def newJavaCollectionInstanceTest[T <: java.util.Collection[_]](clazz: Class[T]): Unit = {
    val c: util.Collection[_] = ReflectionUtil.newJavaCollectionInstance(clazz, Seq[AnyRef]("1", "2", "3"))
    c should have size 3
    Seq("1", "2", "3").foreach { v => c should contain(v) }
  }

  "ReflectionUtil.newJavaCollectionInstance" should "create a java.util.Collection[_]" in {
    newJavaCollectionInstanceTest(clazz = classOf[java.util.Collection[_]])
  }

  it should "create a java.util.List[_]" in {
    newJavaCollectionInstanceTest(clazz = classOf[java.util.List[_]])
  }

  it should "create a java.util.Set[_]" in {
    newJavaCollectionInstanceTest(clazz = classOf[java.util.Set[_]])
  }

  it should "create a java.util.SortedSet[_]" in {
    newJavaCollectionInstanceTest(clazz = classOf[java.util.SortedSet[_]])
  }

  it should "create a java.util.NavigableSet[_]" in {
    newJavaCollectionInstanceTest(clazz = classOf[java.util.NavigableSet[_]])
  }

  "ReflectionUtil.newScalaCollection" should "instantiate scala.collection.mutable.Seq[Int]" in {
    ReflectionUtil.newScalaCollection(classOf[scala.collection.mutable.Seq[Int]], Seq(1, 2, 3)) should be(List(1, 2, 3))
    ReflectionUtil.newScalaCollection(classOf[scala.collection.mutable.Seq[Int]], Nil) should have size 0
  }

  it should "instantiate scala.collection.mutable.Seq[_]" in {
    ReflectionUtil.newScalaCollection(classOf[scala.collection.mutable.Seq[_]], List(1, 2, 3)) should be(List(1, 2, 3))
    ReflectionUtil.newScalaCollection(classOf[scala.collection.mutable.Seq[_]], Nil) should have size 0
  }

  it should "instantiate scala.collection.immutable.Seq[Int]" in {
    ReflectionUtil.newScalaCollection(classOf[scala.collection.immutable.Seq[Int]], List(1, 2, 3)) should be(List(1, 2, 3))
    ReflectionUtil.newScalaCollection(classOf[scala.collection.immutable.Seq[Int]], Nil) should have size 0
  }

  it should "instantiate scala.collection.immutable.Seq[_]" in {
    ReflectionUtil.newScalaCollection(classOf[scala.collection.immutable.Seq[_]], List(1, 2, 3)) should be(List(1, 2, 3))
    ReflectionUtil.newScalaCollection(classOf[scala.collection.immutable.Seq[_]], Nil) should have size 0
  }

  it should "instantiate scala.collection.Seq[Int]" in {
    ReflectionUtil.newScalaCollection(classOf[scala.collection.Seq[Int]], List(1, 2, 3)) should be(List(1, 2, 3))
    ReflectionUtil.newScalaCollection(classOf[scala.collection.Seq[Int]], Nil) should have size 0
  }

  it should "instantiate scala.collection.Seq[_]" in {
    ReflectionUtil.newScalaCollection(classOf[scala.collection.Seq[_]], List(1, 2, 3)) should be(List(1, 2, 3))
    ReflectionUtil.newScalaCollection(classOf[scala.collection.Seq[_]], Nil) should have size 0
  }

  "ReflectionUtil.ifPrimitiveThenWrapper" should "get type for java primitive types" in {
    ReflectionUtil.ifPrimitiveThenWrapper(java.lang.Boolean.TYPE) shouldBe classOf[java.lang.Boolean]
    ReflectionUtil.ifPrimitiveThenWrapper(java.lang.Integer.TYPE) shouldBe classOf[java.lang.Integer]
    ReflectionUtil.ifPrimitiveThenWrapper(classOf[LogLevel]) shouldBe classOf[LogLevel]
  }

  {
    "ReflectionUtil" should "identify various sub-classes of Seq[_] as collection fields" in {
      val classes = List(
        classOf[scala.collection.Seq[_]],
        classOf[scala.collection.IndexedSeq[_]],
        classOf[scala.collection.immutable.Seq[_]],
        classOf[scala.collection.immutable.IndexedSeq[_]],
        classOf[scala.collection.mutable.Seq[_]],
        classOf[scala.collection.mutable.IndexedSeq[_]],
        classOf[scala.collection.mutable.ListBuffer[_]],
        classOf[scala.collection.mutable.ListBuffer[Any]],
        classOf[scala.collection.mutable.ListBuffer[Int]]
      )

      classes.foreach { clazz => ReflectionUtil.isSeqClass(clazz) shouldBe true }
    }

    it should "identify various classes that are not sub-classes of Seq[_] not as seq fields" in {
      val classes = List(
        classOf[scala.collection.Map[_, _]],
        classOf[scala.collection.immutable.Map[_, _]],
        classOf[scala.collection.immutable.ListMap[_, _]],
        classOf[scala.collection.mutable.Map[_, _]],
        classOf[scala.collection.mutable.HashMap[_, _]],
        classOf[scala.collection.mutable.HashSet[_]],
        classOf[Integer],
        classOf[java.util.Collection[_]]
      )

      classes.foreach { clazz => ReflectionUtil.isSeqClass(clazz) shouldBe false }
    }

    it should "identify various sub-classes of Seq[_] or java.util.Collection[_] as collection fields" in {
      val classes = List(
        classOf[scala.collection.Seq[_]],
        classOf[scala.collection.IndexedSeq[_]],
        classOf[scala.collection.immutable.Seq[_]],
        classOf[scala.collection.immutable.IndexedSeq[_]],
        classOf[scala.collection.mutable.Seq[_]],
        classOf[scala.collection.mutable.IndexedSeq[_]],
        classOf[scala.collection.mutable.ListBuffer[_]],
        classOf[scala.collection.mutable.ListBuffer[Any]],
        classOf[scala.collection.mutable.ListBuffer[Int]],
        classOf[java.util.Collection[_]]
      )

      classes.foreach { clazz => ReflectionUtil.isCollectionClass(clazz) shouldBe true }
    }

    it should "identify various classes that are not sub-classes of either Seq[_] or java.util.Collection[_] not as collection fields" in {
      val classes = List(
        classOf[scala.collection.Map[_, _]],
        classOf[scala.collection.immutable.Map[_, _]],
        classOf[scala.collection.immutable.ListMap[_, _]],
        classOf[scala.collection.mutable.Map[_, _]],
        classOf[scala.collection.mutable.HashMap[_, _]],
        classOf[Integer]
      )

      classes.foreach { clazz => ReflectionUtil.isCollectionClass(clazz) shouldBe false }
    }
  }

  "ReflectionUtil.isEmptyCollection" should "throw an IllegalArgumentException if the type was not a collection" in {
    an[IllegalArgumentException] should be thrownBy ReflectionUtil.isEmptyCollection(value=2)
  }

  "ReflectionUtil.constructFromString" should "construct an Int from a string for an Int field" in {
    ReflectionUtil.constructFromString(classOf[Int], classOf[Int], "1").get shouldBe 1
  }

  it should "construct an String from a string for a String field" in {
    ReflectionUtil.constructFromString(classOf[String], classOf[String], "str").get shouldBe "str"
  }

  it should "construct an Option[_] from a string for an Option[_] field" in {
    ReflectionUtil.constructFromString(classOf[Option[_]], classOf[String], "1").get shouldBe Some("1")
  }

  it should "construct an Option[Int] from a string for an Option[Int] field" in {
    ReflectionUtil.constructFromString(classOf[Option[Int]], classOf[Int], "1").get shouldBe Some(1)
  }

  it should "construct an Any from a string for a List[_] field" in {
    ReflectionUtil.constructFromString(classOf[List[_]], classOf[String], "1", "2").get shouldBe List("1", "2")
  }

  it should "construct an Int from a string for a List[Int] field" in {
    ReflectionUtil.constructFromString(classOf[List[Int]], classOf[Int], "1", "2").get shouldBe List(1, 2)
  }

  it should "construct an Any from a string for a java.util.Collection[_] field" in {
    val collection: java.util.Collection[_] = ReflectionUtil.constructFromString(classOf[java.util.Collection[_]], classOf[String], "1", "2").get.asInstanceOf[java.util.Collection[_]]
    collection should have size 2
    collection should contain ("1")
    collection should contain ("2")
  }

  it should "construct an Any from a string for a java.util.Set[_] field" in {
    val collection: java.util.Collection[_] = ReflectionUtil.constructFromString(classOf[java.util.Set[_]], classOf[String], "1", "2").get.asInstanceOf[java.util.Collection[_]]
    collection should have size 2
    collection should contain ("1")
    collection should contain ("2")
  }

  it should "construct using a secondary string constructor" in {
    val thing = ReflectionUtil.constructFromString(classOf[SecondaryConstructor], classOf[SecondaryConstructor], "1,2,3").get.asInstanceOf[SecondaryConstructor]
    thing.ints should contain theSameElementsInOrderAs Seq(1, 2, 3)
  }

  it should "construct an Path from a string for a PathToBam field" in {
    type PathToSomething = java.nio.file.Path
    ReflectionUtil.constructFromString(classOf[PathToSomething], classOf[PathToSomething], PathUtil.pathTo("b", "c").toString).get shouldBe PathUtil.pathTo("b", "c")
  }

  it should "construct an URI from a string" in {
    import java.net.URI
    ReflectionUtil.constructFromString(classOf[URI], classOf[URI], "http://A/B").get shouldBe new URI("http://A/B")
  }

  it should "construct an String from a string for a String field in a child class" in {
    ReflectionUtil.constructFromString(classOf[String], classOf[String], "str").get shouldBe "str"
  }

  it should "construct an Enum value from a string for a Enum field" in {
    ReflectionUtil.constructFromString(classOf[LogLevel], classOf[LogLevel], "Debug").get shouldBe LogLevel.Debug
  }

  it should "not construct an Option[Option[String]] from a string" in {
    class OptionOptionString(var v: Option[Option[String]])
    an[Exception] should be thrownBy ReflectionUtil.constructFromString( classOf[Option[Option[String]]], classOf[Option[String]], "str").get
  }

  it should "not construct an Map[String,String] from a string" in {
    class MapString(var v: Map[String, String])
    an[Exception] should be thrownBy ReflectionUtil.constructFromString(classOf[Map[String, String]], classOf[String], "str").get
  }

  it should "construct a Set and value from a string for a Set[_] field" in {
    ReflectionUtil.constructFromString(classOf[Set[_]], classOf[String], "value").get shouldBe Set("value")
  }

  it should "construct a Seq and value from a string for a Seq[_] field" in {
    ReflectionUtil.constructFromString(classOf[Seq[_]], classOf[String], "value").get shouldBe Seq("value")
  }

  it should "not treat the argument value 'null' as anything special" in {
    ReflectionUtil.constructFromString(classOf[Seq[_]], classOf[String], "prefix", "null", "suffix").get shouldBe Seq("prefix", "null", "suffix")
  }

  it should "treat the None token as special for options" in {
    ReflectionUtil.constructFromString(classOf[Option[Int]], classOf[Int], ReflectionUtil.SpecialEmptyOrNoneToken).get shouldBe None
    ReflectionUtil.constructFromString(classOf[Option[String]], classOf[String], ReflectionUtil.SpecialEmptyOrNoneToken).get shouldBe None

  }

  it should "treat the Empty token as special for collections" in {
    ReflectionUtil.constructFromString(classOf[Seq[_]], classOf[String], ReflectionUtil.SpecialEmptyOrNoneToken).get shouldBe List.empty[String]
    ReflectionUtil.constructFromString(classOf[Seq[_]], classOf[Int], ReflectionUtil.SpecialEmptyOrNoneToken).get shouldBe List.empty[Int]
  }

  it should "should not ignore values prior to the Empty token for collections" in {
    ReflectionUtil.constructFromString(classOf[Seq[_]], classOf[String], "A", "B", ReflectionUtil.SpecialEmptyOrNoneToken).get shouldBe List("A", "B", ReflectionUtil.SpecialEmptyOrNoneToken)
    ReflectionUtil.constructFromString(classOf[Seq[_]], classOf[String], "A", "B", ReflectionUtil.SpecialEmptyOrNoneToken, "C").get shouldBe List("A", "B", ReflectionUtil.SpecialEmptyOrNoneToken, "C")
  }

  it should "construct a Char value from a non-empty String" in {
    ReflectionUtil.constructFromString(classOf[Char], classOf[Char], "X").get shouldBe 'X'
    ReflectionUtil.constructFromString(java.lang.Character.TYPE, java.lang.Character.TYPE, "X").get shouldBe 'X'
  }

  class NoStringCtor(v: Int)
  it should "not be able to construct from string for a class with no string ctor" in {
    an[Exception] should be thrownBy ReflectionUtil.constructFromString(classOf[NoStringCtor], classOf[NoStringCtor], "v").get
  }

  abstract class AbstractClass(v: String)
  it should "not be able to construct from string for an abstract class" in {
    an[Exception] should be thrownBy ReflectionUtil.constructFromString(classOf[AbstractClass], classOf[AbstractClass], "v").get
  }

  private class PrivateClass(v: String)
  it should "not be able to construct from string for an private class" in {
    an[Exception] should be thrownBy ReflectionUtil.constructFromString(classOf[PrivateClass], classOf[PrivateClass], "v").get
  }

  class PrivateCtorClass private(v: String)
  it should "not be able to construct from string for a class with a private constructor" in {
    an[Exception] should be thrownBy ReflectionUtil.constructFromString(classOf[PrivateCtorClass], classOf[PrivateCtorClass], "v").get
  }

  it should "not be able to construct a Seq[Int] when a bad value for the Int(s) are given" in {
    an[Exception] should be thrownBy ReflectionUtil.constructFromString(classOf[Seq[Int]], classOf[Int], "prefix", "null", "suffix").get
  }

  it should "construct a case class from string using it's companion object when necessary" in {
    ReflectionUtil.constructFromString(classOf[UsingCompanion], classOf[UsingCompanion], "123").get shouldBe UsingCompanion(123)
  }

  "ReflectionUtil.typedValueFromString" should "not be able to construct a java collection with a bad value for its type" in {
    an[Exception] should be thrownBy ReflectionUtil.typedValueFromString(classOf[java.util.Set[Int]], classOf[Int], "blah balh").get
  }

  it should "not be able to construct a non-collection if given multiple values" in {
    an[Exception] should be thrownBy ReflectionUtil.typedValueFromString(classOf[Int], classOf[Int], "1", "2", "3").get
  }

  Seq("Y", "y", "Yes", "YES", "yes", "true", "tRUE", "TRUE", "t", "T").foreach { truth =>
      it should s"parse $truth as true for a boolean value" in {
        ReflectionUtil.constructFromString(classOf[Boolean], classOf[Boolean], truth) shouldBe Success(true)
      }
  }

  Seq("yup", "nope", "no", "false", "truthy", "True Dat").foreach { falsehood  =>
      it should s"parse $falsehood as false for a boolean value" in {
        ReflectionUtil.constructFromString(classOf[Boolean], classOf[Boolean], falsehood) shouldBe Success(false)
      }
  }

  "ReflectionUtil.enumOptions" should "return the options for an enum" in {
    val options = ReflectionUtil.enumOptions(classOf[GoodEnum])
    options.isSuccess shouldBe true
    options.get should contain (GoodEnum.GOOD)
    options.get should contain (GoodEnum.BOY)
  }

  it should "fail when there are no options for an enum" in {
    ReflectionUtil.enumOptions(classOf[BadEnum]).isFailure shouldBe true
  }

  "ReflectionUtil.sealedTraitOptions" should "find the possible values in a sealed trait/case object hierarchy with a values method" in {
    ReflectionUtil.sealedTraitOptions(classOf[NoCompanion]).get should contain theSameElementsInOrderAs Seq(ANoCompanion, BNoCompanion, CNoCompanion)
    ReflectionUtil.sealedTraitOptions(classOf[EmptyCompanion]).get should contain theSameElementsInOrderAs Seq(AEmptyCompanion, BEmptyCompanion, CEmptyCompanion)
    ReflectionUtil.sealedTraitOptions(classOf[CompanionWithValues]).get should contain theSameElementsInOrderAs CompanionWithValues.values
    ReflectionUtil.sealedTraitOptions(classOf[CompanionWithFindValues]).get should contain theSameElementsInOrderAs CompanionWithFindValues.findValues
    ReflectionUtil.sealedTraitOptions(classOf[CompanionWithNestedObjects]).get should contain theSameElementsInOrderAs
      Seq(CompanionWithNestedObjects.ACompanionWithNestedObjects, CompanionWithNestedObjects.BCompanionWithNestedObjects, CompanionWithNestedObjects.CCompanionWithNestedObjects)
    ReflectionUtil.sealedTraitOptions(classOf[BaseTrait]).get should contain theSameElementsInOrderAs Seq(LeftObject, RightObject)
  }

  it should "fail when the class is not a sealed trait" in {
    ReflectionUtil.sealedTraitOptions(classOf[GoodEnum]).isFailure shouldBe true
  }

  it should "fail when the sub-classes are not case objects" in {
    ReflectionUtil.sealedTraitOptions(classOf[CaseClasses]).isFailure shouldBe true
  }

  "ReflectionUtil.buildUnitFromString" should "throw an Exception when an unknown enum value is given" in {
    val exception = intercept[Exception] { ReflectionUtil.buildUnitFromString(classOf[GoodEnum], value="DOG").get }
    exception.getMessage should not include "WrappedArray"
    GoodEnum.values().foreach { value => exception.getMessage should include (value.name()) }
  }
}


/** Annotation that is used in testing the findAnnotation method (has to be outer class). */
case class Ann(s:String = "foo", i:Int = 42, b:Boolean=false) extends ConstantAnnotation
@Ann class NoValues
@Ann(s="Hello") class Hello
@Ann(s="Hello", i=101, b=true) class Hello101True
@Ann(s="foo" + "bar", i=2+2, b=false || true) class Expressions
@Ann(s=java.util.jar.JarFile.MANIFEST_NAME, i=Integer.MAX_VALUE) class WithConstRefs

/* Purposefully not a case class, to test both case and non-case class annotations. */
class EnumAnn(val e:TimeUnit= TimeUnit.MINUTES) extends ConstantAnnotation {
  override def equals(that: scala.Any): Boolean = this.e == that.asInstanceOf[EnumAnn].e
}
@EnumAnn class EnumAnnotated
@EnumAnn(e=TimeUnit.DAYS) class EnumAnnotatedWaiting

case class InnerEnumAnn(state: Thread.State = Thread.State.NEW) extends ConstantAnnotation
@InnerEnumAnn(state=Thread.State.BLOCKED) class InnerEnumAnnotated

case class ArrayAnn(ss: Array[String] = Array()) extends ConstantAnnotation {
  override def equals(that: scala.Any): Boolean = this.ss.toSeq == that.asInstanceOf[ArrayAnn].ss.toSeq
  override def toString: String = "@ArrayAnn(ss=" + ss.mkString("[", ", ", "]") + ")"
}
@ArrayAnn class ArrWithDefault
@ArrayAnn(ss=Array("foo", "bar", "splat")) class ArrWithElems

case class ClassAnn(c: Class[_] = classOf[Any]) extends ConstantAnnotation
@ClassAnn(c=classOf[Thread]) class ClassAnnotated
@Ann object SomeEnum extends Enumeration {
  val Mon,Tue,Wed,Thu,Fri = Value
}
@TestJavaAnnotation(placeholder = "value") class ClassJavaAnnotation

/** Tests for the various annotation finding/extracting methods. Separated out here so that all
  * the annotations and annotated classes can be directly above without being inner classes of
  * the preceding test class.
  */
class ReflectUtilAnnotationTest extends UnitSpec {
  "ReflectionUtil.findAnnotation" should "find an annotation with all default values" in {
    ReflectionUtil.findScalaAnnotation[Ann,NoValues] shouldBe Some(Ann())
  }

  it should "find an annotation with a string literal parameter" in {
    ReflectionUtil.findScalaAnnotation[Ann,Hello] shouldBe Some(Ann(s="Hello"))
  }

  it should "find an annotation with multiple literal parameters" in {
    ReflectionUtil.findScalaAnnotation[Ann,Hello101True] shouldBe Some(Ann(s="Hello", i=101, b=true))
  }

  it should "find an annotation with a constant string concat expression" in {
    ReflectionUtil.findScalaAnnotation[Ann,Expressions] shouldBe Some(Ann(s="foobar", i=4, b=true))
  }

  it should "find an annotation with a constant reference values" in {
    ReflectionUtil.findScalaAnnotation[Ann,WithConstRefs] shouldBe Some(Ann(s=java.util.jar.JarFile.MANIFEST_NAME, i=Integer.MAX_VALUE))
  }

  it should "work with Java enums" in {
    ReflectionUtil.findScalaAnnotation[EnumAnn,EnumAnnotated] shouldBe Some(new EnumAnn())
    ReflectionUtil.findScalaAnnotation[EnumAnn,EnumAnnotatedWaiting] shouldBe Some(new EnumAnn(TimeUnit.DAYS))
    ReflectionUtil.findScalaAnnotation[InnerEnumAnn,InnerEnumAnnotated] shouldBe Some(InnerEnumAnn(Thread.State.BLOCKED))
  }

  it should "work with an array parameter with default value" in {
    ReflectionUtil.findScalaAnnotation[ArrayAnn,ArrWithDefault] shouldBe Some(ArrayAnn())
  }

  it should "work with an array parameter with one value" in {
    ReflectionUtil.findScalaAnnotation[ArrayAnn,ArrWithElems] shouldBe Some(ArrayAnn(ss=Array("foo", "bar", "splat")))
  }

  it should "work with Class values in annotations" in {
    ReflectionUtil.findScalaAnnotation[ClassAnn,ClassAnnotated] shouldBe Some(ClassAnn(c=classOf[Thread]))
  }

  it should "work with Scala Enumerations" in {
    ReflectionUtil.findScalaAnnotation[Ann,SomeEnum.type].map(_.getClass) shouldBe Some(classOf[Ann])
  }

  "ReflectionUtil.hasScalaAnnotation" should "return if the symbol is annotated with a scala annotation" in {
    ReflectionUtil.hasScalaAnnotation[ClassAnn,ClassAnnotated] shouldBe true
    ReflectionUtil.hasScalaAnnotation[EnumAnn,ClassAnnotated] shouldBe false
  }

  "ReflectionUtil.findJavaAnnotation" should "return the annotation on a class" in {
    val value = ReflectionUtil.findJavaAnnotation(clazz=classOf[ClassJavaAnnotation], annotationClazz=classOf[TestJavaAnnotation])
    value.isDefined shouldBe true
    //value.get.getClass shouldBe classOf[TestJavaAnnotation]
    value.map(_.placeholder()) shouldBe Some("value")
  }

  "ReflectionUtil.typeTagToClass" should "return the class from a TagType" in {
    ReflectionUtil.typeTagToClass[Int] shouldBe classOf[Int]
    ReflectionUtil.typeTagToClass[Class[Int]] shouldBe classOf[Class[Int]]
  }
}
