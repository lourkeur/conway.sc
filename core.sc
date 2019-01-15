import scala.collection._

type Point = (Int, Int)
def neighbours(c: Point): Seq[Point] = {
  val (x, y) = c
  Seq(
    (x-1,y-1),(x  ,y-1),(x+1,y-1),
    (x-1,y  ),          (x+1,y  ),
    (x-1,y+1),(x  ,y+1),(x+1,y+1),
    )
}

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