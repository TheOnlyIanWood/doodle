package doodle

/**
  * A PathElement is an atomic element for drawing on the
  * screen. Currently we support two types of path elements: straight
  * lines and cubic Bezier curves. The canvas API supports more, but
  * we either don't need them (move and quadratic Bezier curves) or
  * they are considered too complex to support at present (arcs).
  *
  * Path elements do not specify a start point -- that is implicitly
  * the origin in the current coordinate system. They do, however,
  * have an end point.
  */
sealed trait PathElement {
  def end: Point
  /**
    * Given a vector (which we represent as a Point), computes the
    * distance along that vector that one can travel to place a
    * perpendicular vector that just touches the PathElement. An
    * extension of a bounding box to all directions.
    * 
    * This idea taken from the Haskell Diagrams package. See
    * http://projects.haskell.org/diagrams/haddock/Diagrams-Envelope.html
    */
  def envelope(v: Point): Double
}
final case class LineTo(end: Point) extends PathElement {
  def envelope(v: Point): Double =
    // One of the two endpoints of the line will always define the
    // envelope. The question is, which end point? We can take the dot
    // product of each end point with v, which projects the end point
    // onto v and thus measures how far along v the end point is. Then
    // we simply normalise and return the maximum. 
    (Point.zero dot v) max (end dot v) / (v.magnitude)
}
final case class BezierCurveTo(c1: Point, c2: Point, end: Point) extends PathElement {
  def envelope(v: Point): Double = {
    // The envelope is either specified by an endpoint or by finding a
    // point on the curve that is furthest along
    //
    // See
    // https://github.com/diagrams/diagrams-lib/blob/master/src/Diagrams/Segment.hs
    // for the derivation of this
    val pt0 = Point.zero dot v
    val pt1 = end dot v
    val ptsInside =
      quadraticSolution(
        ((c1 * 3) - (c2 * 3) + end) * 3 dot v,
        ((c1 * -2) + c2) * 6 dot v,
        (c1 * 3) dot v
      ).filter(x => x > 0 && x < 1)
    val pts = ptsInside ++ List(pt0, pt1)
    pts.max / v.magnitude
  }

  private def quadraticSolution(a: Double, b: Double, c: Double): List[Double] = {
    val d = (b * b) - (4 * a * c)
    val q = -1/2*(b + Math.signum(b) * Math.sqrt(d))

    (a, b, c) match {
      case (0, 0, 0)   => List(0)
      case (0, 0, _)   => List()
      case (0, _, _)   => List(-c / b)
      case _ if d < 0  => List()
      case (_, 0, _)   => List(Math.sqrt(-c / a), - Math.sqrt(-c / a))
      case _ if d == 0 => List(-b / (2 * a))
      case _ => List(q/a, c/q)
    }
  }
}
