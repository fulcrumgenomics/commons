/*
 * The MIT License
 *
 * Copyright (c) 2017-2018 Fulcrum Genomics LLC
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

import java.nio.file.Path
import java.time.Duration

import com.fulcrumgenomics.commons.CommonsDef._
import com.fulcrumgenomics.commons.io.PathUtil.pathTo
import com.fulcrumgenomics.commons.reflect.ReflectionUtil
import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions}

import scala.collection.{SortedSet, mutable}
import scala.reflect.runtime.universe.{TypeTag, typeOf}
import scala.util.{Failure, Success}

/**
  * Trait that provides useful methods for resolving all kinds of things in configuration.  Uses
  * [[Config]] to retrieve configuration values in a type-safe way.
  *
  * The `config` method must be implemented by a subclass, giving the [[Config]] from which to retrieve configuration
  * values.
  *
  * Overriding the `keyRequested` method allows a call to be received each time a configuration path is requested.
  *
  * Overriding the `handle` method allows a subclass to determine how errors are handled.
  *
  * Extending the `asType` method allows a subclass to support additional (possibly custom) types.
  *
  * The `fetch` method is used to retrieve configuration values.
  *
  * @example Retrieve a value at a given path:
  * {{{
  *   scala> import com.fulcrumgenomics.commons.util.ConfigurationLike
  *   scala> import com.typesafe.config.{Config, ConfigFactory}
  *   scala> import scala.reflect.runtime.universe.TypeTag
  *   scala> val conf = new ConfigurationLike {
  *     val config: Config = ConfigFactory.parseString("""a.str = "string", a.int = 2, a.set = [1,2,3]""")
  *     def get[T : TypeTag](path: String): Option[T] = fetch[T](path)
  *   }
  *   scala> conf.get[String]("a.str")
  *   res0: Option[String] = Some(string)
  *   scala> conf.get[Int]("a.int")
  *   res1: Option[Int] = Some(2)
  *   scala> conf.get[Set[Int]]("a.set")
  *   res2: Option[Set[Int]] = Some(Set(1, 2, 3))
  *   scala> conf.get[Set[Int]]("a.does-not-exist")
  *   res3: Option[Set[Int]] = None
  *   scala> conf.get[Int]("a.set")
  *   java.lang.IllegalArgumentException: Exception retrieving configuration key 'a.set'
  *   ...
  * }}}
  */
trait ConfigurationLike {
  /** The configuration to query. */
  protected def config : Config

  /** Method that can be overridden to receive a call each time a configuration path is requested. */
  protected def keyRequested(path: String): Unit = ()

  /**
    * Method to allow subclasses to override how errors are handled, e.g. by logging or throwing
    * different exceptions.
    */
  protected def handle(message: => String, throwable: Option[Throwable] = None): Nothing = throwable match {
    case Some(thr) => throw new IllegalArgumentException(message, thr)
    case None      => throw new IllegalArgumentException(message)
  }

  /**
    * Method to fetch a configuration key that may or may not be present.
    */
  protected def fetch[T : TypeTag](path: String): Option[T] = {
    keyRequested(path)
    try {
      if (config.hasPath(path)) Some(asType[T](path)) else None
    } catch {
      case ex: Exception => handle(s"Exception retrieving configuration key '$path'", Some(ex))
    }
  }

  /** Converts the configuration path to the given type. If the requested type is not supported, an
    * exception is thrown.  Override this method to support custom types.
    */
  protected def asType[T : TypeTag](path: String) : T = {
    typeOf[T] match {
      case t if t =:= typeOf[String]       => config.getString(path).asInstanceOf[T]
      case t if t =:= typeOf[Boolean]      => config.getBoolean(path).asInstanceOf[T]
      case t if t =:= typeOf[Byte]         => config.getInt(path).asInstanceOf[T]
      case t if t =:= typeOf[Char]         => config.getInt(path).asInstanceOf[T]
      case t if t =:= typeOf[Short]        => config.getInt(path).toShort.asInstanceOf[T]
      case t if t =:= typeOf[Int]          => config.getInt(path).asInstanceOf[T]
      case t if t =:= typeOf[Long]         => config.getLong(path).asInstanceOf[T]
      case t if t =:= typeOf[Float]        => config.getDouble(path).toFloat.asInstanceOf[T]
      case t if t =:= typeOf[Double]       => config.getDouble(path).asInstanceOf[T]
      case t if t =:= typeOf[BigInt]       => BigInt(config.getString(path)).asInstanceOf[T]
      case t if t =:= typeOf[BigDecimal]   => BigDecimal(config.getString(path)).asInstanceOf[T]
      case t if t =:= typeOf[Path]         => pathTo(config.getString(path)).asInstanceOf[T]
      case t if t =:= typeOf[Duration]     => config.getDuration(path).asInstanceOf[T]
      case paramType =>
        val paramClass     = ReflectionUtil.typeToClass(paramType)
        val paramUnitType  = paramType.typeArgs.headOption getOrElse paramType
        val paramUnitClass = ReflectionUtil.typeToClass(paramUnitType)

        val values = if (ReflectionUtil.isCollectionClass(paramClass)) {
          config.getStringList(path).toList
        } else {
          Seq(config.getString(path))
        }

        ReflectionUtil.constructFromString(paramClass, paramUnitClass, values:_*) match {
          case Success(obj) => obj.asInstanceOf[T]
          case Failure(thr) => handle("Don't know how to configure a " + typeOf[T] + ". ", Some(thr))
        }
    }
  }
}

/** Companion object to [[Configuration]]. */
object Configuration {
  /** Generates a default Configuration object. */
  def apply(): Configuration = new Configuration(ConfigFactory.load())

  /** Generates a default Configuration object.  Allows the for the initialize of the configuration from a specified
    * path, with fallbacks to system properties, application.conf and reference.conf files.
    * */
  def apply(path: Path): Configuration = {
      // setAllowMissing(false) refers to allowing the file(!) to be missing, not values within the file
      val options = ConfigParseOptions.defaults().setAllowMissing(false)
      val localConfig = ConfigFactory.parseFile(path.toFile, options)

      // This mimics the behaviour of ConfigFactory.load() but with the localConfig sandwiched in
      val config = ConfigFactory.defaultOverrides()
        .withFallback(localConfig)
        .withFallback(ConfigFactory.defaultApplication())
        .withFallback(ConfigFactory.defaultReference())
        .resolve()

    new Configuration(config)
  }
}

/**
  * Class that provides useful methods for resolving all kinds of things in configuration.  Uses
  * [[Config]] to retrieve configuration values in a type-safe way.
  *
  * The `requestedKeys` method returns all the keys requested.
  *
  * The public methods to retrieve values are the `apply`, `get`, and `getOrElse` methods.
  *
  * @example A common pattern is to create an `object` that extends this class, and reference that object when retrieving
  *          values from a path.
  * {{{
  *   scala> import com.fulcrumgenomics.commons.util.Configuration
  *   scala> import com.typesafe.config.{Config, ConfigFactory}
  *   scala> object CustomConfiguration extends Configuration(ConfigFactory.load())
  *   scala> CustomConfiguration.get[Long]("path.does-not-exist")
  *   res0: Option[Long] = None
  * }}}
  *
  * @example A common pattern is to create an `object` that extends this class, and reference that object when retrieving
  *          values from a path.  An additional trait is created that references the custom configuration object, allowing
  *          other classes to mix in the additional trait to get access the the configuration methods that use the
  *          custom configuration object.
  * {{{
  *   scala> import com.fulcrumgenomics.commons.util.{Configuration, ConfigurationLike}
  *   scala> import com.typesafe.config.{Config, ConfigFactory}
  *   scala> // create an object that stores the config that will be referenced everywhere
  *   scala> object CustomConfiguration extends Configuration(ConfigFactory.load())
  *   scala> // create the trait that can be mixed into other classes to get access to the configuration methods that use the custom configuration object
  *   scala> trait CustomConfiguration extends ConfigurationLike { protected def config: Config = CustomConfiguration.config }
  *   scala> // create a class that mixes in the custom configuration trait
  *   scala> class Foo extends CustomConfiguration { val maybeLong: Option[Long] = this.get[Long]("path.does-not-exist") }
  *   scala> val foo = new Foo()
  *   scala> foo.maybeLong
  *   res0: Option[Long] = None
  *   scala> foo.get("path.does-not-exist")
  *   res1: Option[Long] = None
  *   scala> foo[Long]("path.does-exists-and-is-1")
  *   res2: Long = 1
  *   scala> foo.getOrElse[Long]("path.does-not-exist", 2)
  *   res3: Long = 2
  * }}}
  */
class Configuration(override protected val config: Config) extends ConfigurationLike {
  /** The keys requested, in order. */
  private val _requestedKeys = mutable.Set[String]()

  /** Method that can be overridden to receive a call each time a configuration path is requested. */
  override protected def keyRequested(path: String): Unit = this._requestedKeys.add(path)

  /** Returns a sorted set of all keys that have been requested up to this point in time. */
  def requestedKeys: SortedSet[String] = collection.immutable.TreeSet[String](this._requestedKeys.toSeq:_*)

  /**
    * Looks up a single value of a specific type in configuration. If the configuration key
    * does not exist, an exception is thrown. If the requested type is not supported, an
    * exception is thrown.
    */
  def apply[T : TypeTag](path: String) : T = fetch[T](path).getOrElse(handle(s"No such configuration key: $path."))

  /**
    * Optionally accesses a configuration value. If the value is not present in the configuration
    * a None will be returned, else a Some(T) of the appropriate type.
    */
  def get[T : TypeTag](path: String) : Option[T] = fetch[T](path)

  /**
    * Optionally accesses a configuration value. If the value is not present in the configuration
    * a the default value will be returned.
    */
  def getOrElse[T : TypeTag](path: String, default: => T) : T = get(path).getOrElse(default)
}
