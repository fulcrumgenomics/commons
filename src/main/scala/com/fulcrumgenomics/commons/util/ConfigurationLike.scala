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

package com.fulcrumgenomics.commons.util

import java.nio.file.Path
import java.time.Duration

import com.fulcrumgenomics.commons.CommonsDef._
import com.fulcrumgenomics.commons.io.PathUtil.pathTo
import com.fulcrumgenomics.commons.reflect.ReflectionUtil
import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions}

import scala.collection.SortedSet
import scala.reflect.runtime.universe.{TypeTag, typeOf}
import scala.util.{Failure, Success}

/**
  * Trait that provides useful methods for resolving all kinds of things in configuration.  Uses
  * [[Config]] to retrieve configuration values in a type-safe way.
  *
  * Keeps track of all configuration keys that have been requested so that they can be reported later if desired.
  *
  * @example Retrieve a value at a given path, where the path must exist:
  * {{{
  *   scala> import com.fulcrumgenomics.commons.util.ConfigurationLike
  *   scala> import com.typesafe.config.{Config, ConfigFactory}
  *   scala> val conf = new ConfigurationLike {
  *     val config: Config = ConfigFactory.parseString("""a.str = "string", a.int = 2, a.set = [1,2,3]""")
  *   }
  *   scala> conf[String]("a.str")
  *   res0: String = string
  *   scala> conf[Int]("a.int")
  *   res1: Int = 2
  *   scala> conf[Set[Int]]("a.set")
  *   res2: Set[Int] = Set(1, 2, 3)
  *   scala> conf[Set[Int]]("a.does-not-exist")
  *   java.lang.IllegalArgumentException: Exception retrieving configuration key 'a.does-not-exist': No configuration setting found for key 'a.does-not-exist'
  *   ...
  * }}}
  *
  * @example Retrieve a value at a given path, where the path may not exist
  * {{{
  *   scala> import com.fulcrumgenomics.commons.util.ConfigurationLike
  *   scala> import com.typesafe.config.{Config, ConfigFactory}
  *   scala> val conf = new ConfigurationLike {
  *     val config: Config = ConfigFactory.parseString("""a.str = "string", a.int = 2, a.set = [1,2,3]""")
  *   }
  *   scala> conf.get[String]("a.str")
  *   res0: Option[String] = Some(string)
  *   scala> conf.get[Int]("a.int")
  *   res1: Option[Int] = Some(2)
  *   scala> conf.get[Set[Int]]("a.set")
  *   res2: Option[Set[Int]] = Some(Set(1, 2, 3))
  *   scala> conf.get[Set[Int]]("a.does-not-exist")
  *   res3: Option[Set[Int]] = None
  * }}}
  *
  * @example Retrieve a value at a given path if it exists, otherwise return a default:
  * {{{
  *   scala> import com.fulcrumgenomics.commons.util.ConfigurationLike
  *   scala> import com.typesafe.config.{Config, ConfigFactory}
  *   scala> val conf = new ConfigurationLike {
  *     val config: Config = ConfigFactory.parseString("""a.str = "string", a.int = 2, a.set = [1,2,3]""")
  *   }
  *   scala> conf.getOrElse[String]("a.does-not-exist", "default")
  *   res0: String = default
  *   scala> conf.getOrElse[Int]("a.does-not-exist", 0)
  *   res1: Int = 0
  *   scala> conf.getOrElse[Set[Int]]("a.does-not-exist", Set.empty)
  *   res2: Set[Int] = Set()
  * }}}
  */
trait ConfigurationLike {
  /** The configuration to query. */
  protected def config : Config

  /** A logger to which to report exceptions. */
  protected def _logger: Option[Logger] = None

  /** The keys requested, in order. */
  private val _requestedKeys = new java.util.concurrent.ConcurrentSkipListSet[String]()

  /**
    * Looks up a single value of a specific type in configuration. If the configuration key
    * does not exist, an exception is thrown. If the requested type is not supported, an
    * exception is thrown.
    */
  def apply[T : TypeTag](path: String) : T = {
    this._requestedKeys.add(path)
    try {
      asType[T](path)
    }
    catch {
      case ex : Exception =>
        this._logger.foreach(_.exception(ex))
        throw new IllegalArgumentException(s"Exception retrieving configuration key '$path': ${ex.getMessage}", ex)
    }
  }

  /**
    * Optionally accesses a configuration value. If the value is not present in the configuration
    * a None will be returned, else a Some(T) of the appropriate type.
    */
  def get[T : TypeTag](path: String) : Option[T] = {
    this._requestedKeys.add(path)
    if (config.hasPath(path)) Some(apply[T](path))
    else None
  }

  /**
    * Optionally accesses a configuration value. If the value is not present in the configuration
    * a None will be returned, else a Some(T) of the appropriate type.
    */
  def getOrElse[T : TypeTag](path: String, default: => T) : T = this.get(path).getOrElse(default)

  /** Returns a sorted set of all keys that have been requested up to this point in time. */
  def requestedKeys: SortedSet[String] = collection.immutable.TreeSet[String](requestedKeys.toSeq:_*)

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
          case Failure(thr) =>
            this._logger.foreach(_.exception(thr))
            throw new IllegalArgumentException("Don't know how to configure a " + typeOf[T] + ". " + thr.getMessage)
        }
    }
  }
}

/**
  * Trait that provides useful methods for resolving all kinds of things in configuration.  Uses
  * [[Config]] to retrieve configuration values in a type-safe way.  Adds an additional method to
  *
  * Allows the for the initialize of the configuration from a specified path, with fallbacks to system properties,
  * application.conf and reference.conf files.
  *
  * @example A common pattern is to create an `object` that extends this trait, and reference that object when retrieving
  *          values from a path.
  * {{{
  *   scala> import com.fulcrumgenomics.commons.util.Configuration
  *   scala> import com.typesafe.config.{Config, ConfigFactory}
  *   scala> object CustomConfiguration extends Configuration
  *   scala> CustomConfiguration.get[Long]("path.does-not-exist")
  *   res0: Option[Long] = None
  * }}}
  *
  * @example A common pattern is to create an `object` that extends this trait, and reference that object when retrieving
  *          values from a path.  An additional trait is created that references the custom configuration object, allowing
  *          other classes to mix in the additional trait to get access the the configuration methods that use the
  *          custom configuration object.
  * {{{
  *   scala> import com.fulcrumgenomics.commons.util.{Configuration, ConfigurationLike}
  *   scala> import com.typesafe.config.{Config, ConfigFactory}
  *   scala> // create an object that stores the config that will be referenced everywhere
  *   scala> object CustomConfiguration extends Configuration
  *   scala> // create the trait that can be mixed into other classes to get access to the configuration methods that use the custom configuration object
  *   scala> trait CustomConfiguration extends ConfigurationLike { protected def config: Config = CustomConfiguration.config }
  *   scala> // create a class that mixes in the custom configuration trait
  *   scala> class Foo extends CustomConfiguration { val maybeLong: Option[Long] = this.get[Long]("path.does-not-exist") }
  *   scala> val foo = new Foo()
  *   scala> foo.maybeLong
  *   res0: Option[Long] = None
  *   scala> foo.get("path.does-not-exist")
  *   res1: Option[Long] = None
  * }}}
  */
trait Configuration extends ConfigurationLike {
  /** The global configuration instance */
  private var _config: Config = ConfigFactory.load()

  /** The configuration to query. */
  protected def config: Config = _config

  /**
    * Initialize the configuration by loading configuration from the supplied path, and combining it with
    * configuration information from the system properties (higher priority), application.conf and
    * reference.conf files (lower priority).
    */
  def initialize(path: Option[Path]): Unit = path match {
    case None    =>
      this._config = ConfigFactory.load()
    case Some(p) =>
      // setAllowMissing(false) refers to allowing the file(!) to be missing, not values within the file
      val options = ConfigParseOptions.defaults().setAllowMissing(false)
      val localConfig = ConfigFactory.parseFile(p.toFile, options)

      // This mimics the behaviour of ConfigFactory.load() but with the localConfig sandwiched in
      this._config = ConfigFactory.defaultOverrides()
        .withFallback(localConfig)
        .withFallback(ConfigFactory.defaultApplication())
        .withFallback(ConfigFactory.defaultReference())
        .resolve()
  }

  /** Allows initialization with a custom configuration. */
  protected def initialize(customConfig: Config): Unit = this._config = customConfig
}
