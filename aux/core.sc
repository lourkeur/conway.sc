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
  private val EltRx = """(\d*)([bo$])""".r
  def parse(data: String): Grid = {
    var x, y = 0
    val out = Grid.newBuilder
    for (elt <- EltRx.findAllMatchIn(data)) {
      val Seq(n, tp) = elt.subgroups
      val rep = if (n == "") 1 else n.toInt
      if (tp == "o")
        for (i <- 0 until rep)
          out += ((x + i, y))
      if (tp == "$") {
        x = 0
        y += rep
      } else
        x += rep
    }
    out.result
  }
}

object Patterns extends Enumeration {
  protected case class Val(data: String) extends super.Val {
    lazy val grid: Grid = rle.parse(data)
  }

  implicit def valueToPatternsVal(x: Value) = x.asInstanceOf[Val]

  val Acorn = Val("bo5b$3bo3b$2o2b3o")
  val Diehard = Val("6bob$2o6b$bo3b3o")
  val Glider = Val("bo$2bo$3o")
  val GosperGliderGun = Val("24bo$22bobo$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o$2o8bo3bob2o4bobo$10bo5bo7bo$11bo3bo$12b2o")
  val Herschel = Val("o$3o$obo$2bo")
    //val Lay = Val("obo$2bo$4bo$4bobo$4bob2o$6bo")
  val Lay = Val("2o2bo$o2bo$o2b2o$2bo$ob3o")
  val Line = Val("8ob5o3b3o6b7ob5o")
  val Lwss = Val("bo2bo$o4b$o3bo$4o")
  val Mess = Val("3bo$3bobo$3b2o$o$b2o$2o")
  val RPentomino = Val("b2o$2ob$bo")
  val Toad = Val("b3o$3o")
  val Copperhead = Val("b2o2b2o$3b2o$3b2o$obo2bobo$o6bo2$o6bo$b2o2b2o$2b4o2$3b2o$3b2o")
}
