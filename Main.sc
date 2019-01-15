import $ivy.`com.googlecode.lanterna:lanterna:3.0.1`
import com.googlecode.lanterna._

import scala.util.control.Exception._
import scala.collection._

import $file.core
import core._

val DrawCharmap = immutable.Map(
  (false, false) -> TextCharacter.DEFAULT_CHARACTER,
  (true,  false) -> new TextCharacter('\u2580'),
  (false, true)  -> new TextCharacter('\u2584'),
  (true,  true)  -> new TextCharacter('\u2588'),
  )

def draw(scr: screen.Screen, grid: Grid): Unit = {
  scr.doResizeIfNecessary()
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

def run() = {
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
