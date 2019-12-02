package scala.collection.compat

import scala.collection.{IterableView => SCIterableView}

package object view extends MapViewExtensions {
  type IterableView[A, B] = SCIterableView[A, B]
}
