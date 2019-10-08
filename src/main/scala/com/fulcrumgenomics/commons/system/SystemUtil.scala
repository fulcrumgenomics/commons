package com.fulcrumgenomics.commons.system

/** System utility defaults and methods. Many methods mock functions in `org.apache.commons:commons-lang3`. */
object SystemUtil {

  /** The operating system name prefixes for Linux */
  private val LinuxNamePrefixes: Seq[String] = Seq("Linux", "LINUX")

  /** The operating system name prefixes for Mac */
  private val MacNamePrefixes: Seq[String] = Seq("Mac")

  /** The current operating system name. */
  private lazy val OsName: Option[String] = getSystemProperty(property = "os.name")

  /** The current operating system architecture */
  private lazy val OsArch: Option[String] = getSystemProperty(property = "os.arch")

  /** The current operating system version */
  private lazy val OsVersion: Option[String] = getSystemProperty(property = "os.version")

  /** Gets a system property.  Returns None if not found or we are not allowed to retrieve the property. */
  private def getSystemProperty(property: String): Option[String] = {
    try { Option(System.getProperty(property)) }
    catch { case _: SecurityException => None } // not allowed to look at this property
  }

  /** True if this operating system is Linux, false otherwise. */
  private lazy val IsOsLinux: Boolean = LinuxNamePrefixes.exists(prefix => OsName.exists(_.startsWith(prefix)))

  /** True if this operating system is Mac, false otherwise. */
  private lazy val IsOsMac: Boolean = MacNamePrefixes.exists(prefix => OsName.exists(_.startsWith(prefix)))

  /** Returns true if the architecture is the given name, false otherwise. */
  private def isOsArch(name: String): Boolean = OsArch.contains(name)

  /** Returns true if the operating system version starts with the given version string, false otherwise. */
  private def isOsVersion(prefix: String): Boolean = OsVersion.exists(_.startsWith(prefix))

  /** True if the current system could support the Intel Inflater and Deflater, false otherwise. */
  lazy val IntelCompressionLibrarySupported: Boolean = {
    if (!SystemUtil.IsOsLinux && !SystemUtil.IsOsMac) false
    else if (SystemUtil.isOsArch(name = "ppc64le")) false
    else if (SystemUtil.IsOsMac && SystemUtil.isOsVersion(prefix = "10.14.")) false // FIXME: https://github.com/Intel-HLS/GKL/issues/101
    else true
  }
}
