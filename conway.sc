#!/usr/bin/env amm
/** Scala 2.12 KISS implementation of Conway's game of life.
  *
  * Run using Ammonite https://ammonite.io
  *
  * Inspired by Stop Writing Classes by Jack Diederich,
  * https://www.youtube.com/watch?v=o9pEzgHorH0
  *
  * Ported from https://gist.github.com/lourkeur/95799b35e2d3aac54cdd0e4a7c8d2037 (Python)
  * Patterns from http://conwaylife.com/wiki"
  */

type Point = (Int, Int)
def neighbours(c: Point): Seq[Point] = {
  val (x, y) = c
  Seq(
    (x-1,y-1),(x  ,y-1),(x+1,y-1),
    (x-1,y  ),          (x+1,y  ),
    (x-1,y+1),(x  ,y+1),(x+1,y+1),
    )
}

import scala.collection._

type Grid = immutable.Set[Point]

def tick(grid: Grid): Grid = {
  val ncount = mutable.Map.empty[Point, Int].withDefaultValue(0)
  for (c <- grid; n <- neighbours(c))
    ncount(n) += 1
  (for {
     (c, x) <- ncount.toSeq
     if (x == 2 && grid.contains(c) || x == 3)
       } yield c).toSet
}

/** Simple RLE parser.
  *
  * Not accounted for: the usual ! terminator for RLE-encoded patterns on the internet.
  */
object rle {
  private val EltRx = """(\d*)(b|o)(\$?)""".r
  def parse(data: String): Grid = {
    var x, y = 0
    val out = immutable.Set.newBuilder[Point]
    for (elt <- EltRx.findAllMatchIn(data)) {
      val Seq(n, tp, eol) = elt.subgroups
      val rep = if (n == null) 1 else n.toInt
      if (tp == "o")
        for (i <- 0 until rep)
          out += ((x + i, y))
      if (eol == null)
        x += rep
      else {
        x = 0
        y += 1
      }
    }
    out.result
  }
}
