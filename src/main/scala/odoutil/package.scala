import scala.collection.mutable

/**
 * User: Oleg
 * Date: 08-May-14
 * Time: 03:23
 */
package object odoutil {
  implicit class UpdatingDefaultMap[K, V](val map: mutable.Map[K, V]) extends AnyVal {
    def updateDefault(default: K => V) = map withDefault (k => {
      val v = default(k)
      map(k) = v
      v
    })
  }
}
