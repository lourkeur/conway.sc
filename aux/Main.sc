import $ivy.`com.googlecode.lanterna:lanterna:3.0.1`
import com.googlecode.lanterna._

import scala.util._
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

case class Config(
  forceTextTerminal: Boolean = false,
  preferTerminalEmulator: Boolean = false,
  initialGrid: Either[Patterns.Value, Grid] = Left(Patterns.Acorn),
  )
object OptionParser extends scopt.OptionParser[Try[Config]]("conway") {
  opt[Unit]('t', "force-text-terminal")
    .action((_, c) => c map { _.copy(forceTextTerminal = true) })

  opt[Unit]('g', "prefer-terminal-emulator")
    .action((_, c) => c map { _.copy(preferTerminalEmulator = true) })

  opt[String]('p', "initial-grid-pattern")
    .action((x, c) =>
      for { c <- c
            pat <- Try { Patterns.withName(x) }
      } yield c.copy(initialGrid = Left(pat)))

  arg[String]("initial-grid")
    .optional
    .action((x, c) =>
      for { c <- c
            g <- Try { rle.parse(x) }
      } yield c.copy(initialGrid = Right(g)))

  checkConfig({ case Success(_) => success
                case Failure(e) => failure(f"$e")
              })
}

def run(args: Seq[String]): Unit = {
  val c = OptionParser.parse(args, Success(Config())) match {
    case None => return
    case Some(Success(c)) => c
  }
  val tf = new terminal.DefaultTerminalFactory
  tf setTerminalEmulatorTitle "Conway's Game of Life"
  tf setForceTextTerminal c.forceTextTerminal
  tf setPreferTerminalEmulator c.preferTerminalEmulator
  val scr = tf.createScreen()
  val init = c.initialGrid match {
    case Left(pat) => pat.grid
    case Right(g) => g
  }
  val steps = Iterator.iterate(init)(tick) zip Iterator.iterate(now())(_ + 200)
  scr.startScreen()
  ultimately { scr.stopScreen() } {
    for ((grid, inst) <- steps) {
      draw(scr, grid)
      Thread.sleep(math.max(0, inst - now()))
      scr.refresh()
    }
  }
}
