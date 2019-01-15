import $ivy.`org.scalacheck::scalacheck:1.14.0`
import org.scalacheck._

import $file.conway
import conway._

/** some cheap basic non-exhaustive correctness tests */
object Examples extends Properties("examples") {
  import Prop._

  property("tick block") = {
    val block = Grid((0,0), (0,1), (1,0), (1,1))
    tick(block) ?= block
  }

  property("tick blinker") = {
    val blinker1 = Grid((1,0), (1,1), (1,2))
    val blinker2 = Grid((0,1), (1,1), (2,1))
    (tick(blinker1) ?= blinker2) &&
      (tick(blinker2) ?= blinker1)
  }

  property("tick glider") = {
    val glider1 = Grid((0,0), (1,1), (1,2), (2,1), (2,0))
    val glider3 = Grid((1,0), (2,1), (2,2), (3,1), (1,2))
    tick(tick(glider1)) ?= glider3
  }
}

@main
def test(args: String*) =
  Examples.main(args.toArray)
