package doodle

/**
  * A 2D point.
  */
final case class Point(x: Double, y: Double) {
  def *(s: Double): Point =
    Point(s * x, s * y)

  def +(p: Point): Point =
    Point(x + p.x, y + p.y)

  def -(p: Point): Point =
    Point(x - p.x, y - p.y)

  def dot(p: Point): Double =
    (x * p.x) + (y * p.y)

  def magnitude: Double =
    Math.sqrt(x * x + y * y)
}

object Point {
  def zero: Point = Point(0, 0)
}
