/*
 * The MIT License
 *
 * Copyright (c) 2021 Fulcrum Genomics
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
 *
 */

package com.fulcrumgenomics.commons.collection

/** A cache that stores up to a maximum number of entries, that when reached,
  * evicts the least recently used (by get or put) key. */
class LeastRecentlyUsedCache[Key, Value](maxEntries: Int) extends Iterable[(Key, Value)] {

  private val map = scala.collection.mutable.LinkedHashMap[Key, Value]()

  /** Gets value for the given key, or None if it doesn't exist.  Will update the key to
    * be the most recently used. */
  def get(key: Key): Option[Value] = map.synchronized {
    val result = map.get(key)
    // remove and add the key/value if found, to move it to the most recently used
    result.foreach { value =>
      map.remove(key)
      map.put(key, value)
    }
    result
  }

  /** Adds the value for the given key.  Will update the key to be the most recently used. Returns
    * the value that was removed, or None if the key was not in the cache. */
  def put(key: Key, value: Value): Option[Value] = map.synchronized {
    // remove the key if present
    val previous = map.remove(key)
    // remove the least recently used element if needed
    // IMPORTANT: must check the size **after** removing the key, since if the key is present, we're overwriting it and
    // so the # of entries wont change.
    if (map.size == maxEntries) { // this implies the key was not present in the cache
      map.headOption.foreach { case (k, _) => map.remove(k) }
    }
    // add the key/value as the most recently used
    map.put(key, value)
    // return the removed value
    previous
  }

  /** Removes the value with the given key from the cache. */
  def remove(key: Key): Option[Value] = map.remove(key)

  /** An iterator over key-value pairs from least to most recently used. */
  override def iterator: Iterator[(Key, Value)] = map.iterator

  /** The number elements in the cache. */
  override def size: Int = map.size
}
