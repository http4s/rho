/*
 * Copyright 2014 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scala.collection.compat.view

import scala.collection.generic.CanBuildFrom

class MapViewExtensionMethods[K, V, C <: scala.collection.Map[K, V]](private val self: IterableView[(K, V), C]) extends AnyVal {
  def mapValues[W, That](f: V => W)(implicit bf: CanBuildFrom[IterableView[(K, V), C], (K, W), That]): That =
    self.map[(K, W), That] { case (k, v) => (k, f(v)) }
}

trait MapViewExtensions {
  implicit def toMapViewExtensionMethods[K, V, C <: scala.collection.Map[K, V]](self: IterableView[(K, V), C]): MapViewExtensionMethods[K, V, C] =
    new MapViewExtensionMethods[K, V, C](self)
}