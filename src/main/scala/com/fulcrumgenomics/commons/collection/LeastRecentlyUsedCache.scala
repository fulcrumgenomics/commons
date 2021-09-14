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

  require(maxEntries > 0, s"The maximum number of entries must be greater than one, found: $maxEntries")

  private val map = scala.collection.mutable.LinkedHashMap[Key, Value]()

  /** Gets value for the given key, or None if it doesn't exist.  Will update the key to
    * be the most recently used. */
  def get(key: Key): Option[Value] = map.synchronized {
    val result = map.remove(key)
    // re-insert if found, to move it to the most recently used
    result.foreach { value => map.put(key, value) }
    result
  }

  /** Adds the value for the given key.  Will update the key to be the most recently used. Returns
    * the value that was removed, or None if the key was not in the cache. */
  def put(key: Key, value: Value): Option[Value] = map.synchronized {
    // If the key exists in the underyling map, it needs to be re-inserted to update the insertion order used in
    // LinkedHashMap.  If the key is not present, previous is None.
    val previous = map.remove(key)
    // Remove the least recently used element if needed
    // IMPORTANT: must check the size **after** removing the key, since if the key is present, we're overwriting it and
    // so the # of entries wont change.
    if (map.size == maxEntries) { // this implies the key was not present in the cache
      map.headOption.foreach { case (k, _) => map.remove(k) }
    }
    // Add the key/value as the most recently used
    map.put(key, value)
    // Return the removed value
    previous
  }

  /** Removes the value with the given key from the cache. */
  def remove(key: Key): Option[Value] = map.synchronized { map.remove(key) }

  /** An iterator over key-value pairs from least to most recently used.
    *
    * The key-value pairs will be reflect the key-value pairs at the time this methods is
    * called, and store them separately in memory from this LRU cache.  I.e. additional
    * modifications to this cache will not be reflected in the iterator returned herein.
    * */
  override def iterator: Iterator[(Key, Value)] = map.synchronized { map.toSeq.iterator }

  /** The number elements in the cache. */
  override def size: Int = map.synchronized { map.size }
}
