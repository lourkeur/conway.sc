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
object Grid {
  def fromSeq(cs: Seq[Point]): Grid = immutable.Set(cs: _*)
  def apply(cs: Point*): Grid = fromSeq(cs)
  def newBuilder: mutable.Builder[Point, Grid] = immutable.Set.newBuilder
}

def tick(grid: Grid): Grid = {
  val ncount = mutable.Map.empty[Point, Int].withDefaultValue(0)
  for (c <- grid; n <- neighbours(c))
    ncount(n) += 1
  Grid.fromSeq(
    for {
      (c, x) <- ncount.toSeq
      if (x == 2 && grid.contains(c) || x == 3)
        } yield c)
}

/** Simple RLE parser.
  *
  * Not accounted for: the usual ! terminator for RLE-encoded patterns on the internet.
  */
object rle {
  private val EltRx = """(\d*)(b|o)(\$?)""".r
  def parse(data: String): Grid = {
    var x, y = 0
    val out = Grid.newBuilder
    for (elt <- EltRx.findAllMatchIn(data)) {
      val Seq(n, tp, eol) = elt.subgroups
      val rep = if (n == "") 1 else n.toInt
      if (tp == "o")
        for (i <- 0 until rep)
          out += ((x + i, y))
      if (eol == "")
        x += rep
      else {
        x = 0
        y += 1
      }
    }
    out.result
  }
}

val Acorn = rle.parse("bo5b$3bo3b$2o2b3o")
val Diehard = rle.parse("6bob$2o6b$bo3b3o")
val Glider = rle.parse("bo$2bo$3o")
val GosperGliderGun = rle.parse("24bo$22bobo$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o$2o8bo3bob2o4bobo$10bo5bo7bo$11bo3bo$12b2o")
val Herschel = rle.parse("o$3o$obo$2bo")
//val Lay = rle.parse("obo$2bo$4bo$4bobo$4bob2o$6bo")
val Lay = rle.parse("2o2bo$o2bo$o2b2o$2bo$ob3o")
val Line = rle.parse("8ob5o3b3o6b7ob5o")
val Lwss = rle.parse("bo2bo$o4b$o3bo$4o")
val Mess = rle.parse("3bo$3bobo$3b2o$o$b2o$2o")
val RPentomino = rle.parse("b2o$2ob$bo")
val Toad = rle.parse("b3o$3o")


import $ivy.`com.googlecode.lanterna:lanterna:3.0.1`
import com.googlecode.lanterna._
import scala.util.control.Exception._


val DrawCharmap = immutable.Map(
  (false, false) -> TextCharacter.DEFAULT_CHARACTER,
  (true,  false) -> new TextCharacter('\u2580'),
  (false, true)  -> new TextCharacter('\u2584'),
  (true,  true)  -> new TextCharacter('\u2588'),
  )

def draw(scr: screen.Screen, grid: Grid) = {
  scr.clear()
  val sz = scr.getTerminalSize
  val Y = sz.getRows
  val X = sz.getColumns
  val halfY = Y / 2
  val halfX = X / 2
  for (i <- 0 until Y; j <- 0 until X) {
    val y = (i - halfY) * 2
    val x = j - halfX
    scr.setCharacter(j, i, DrawCharmap((grid contains (x,y), grid contains (x,y+1))))
  }
}

def now() = System.currentTimeMillis

@main
def simulate() = {
  val tf = new terminal.DefaultTerminalFactory
  val term = tf.createTerminal
  val scr = new screen.TerminalScreen(term)
  val steps = Iterator.iterate(Acorn)(tick) zip Iterator.iterate(now())(_ + 200)
  scr.startScreen()
  ultimately { scr.stopScreen() } {
    for ((grid, inst) <- steps) {
      draw(scr, grid)
      Thread.sleep(math.max(0, inst - now()))
      scr.refresh()
    }
  }
}
