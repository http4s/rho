package scala.collection.compat

import scala.collection.{IterableOps, View}

package object view {
  type IterableView[A, _] = IterableOps[A, View, View[A]]
}
