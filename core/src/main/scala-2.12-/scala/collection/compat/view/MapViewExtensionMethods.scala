package scala.collection.compat.view

import scala.collection.generic.CanBuildFrom

class MapViewExtensionMethods[K, V, C <: scala.collection.Map[K, V]](
    private val self: IterableView[(K, V), C])
    extends AnyVal {
  def mapValues[W, That](f: V => W)(implicit
      bf: CanBuildFrom[IterableView[(K, V), C], (K, W), That]): That =
    self.map[(K, W), That] { case (k, v) => (k, f(v)) }
}

trait MapViewExtensions {
  implicit def toMapViewExtensionMethods[K, V, C <: scala.collection.Map[K, V]](
      self: IterableView[(K, V), C]): MapViewExtensionMethods[K, V, C] =
    new MapViewExtensionMethods[K, V, C](self)
}
