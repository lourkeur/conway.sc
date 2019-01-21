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

def draw(scr: screen.Screen, grid: Grid, center: (Int, Int)): Unit = {
  scr.doResizeIfNecessary()
  scr.clear()
  val sz = scr.getTerminalSize
  val W = sz.getColumns
  val H = sz.getRows
  val halfW = W / 2
  val halfH = H / 2
  for (i <- 0 until H; j <- 0 until W) {
    val x = j - halfW - center._1
    val y = (i - halfH) * 2 - center._2
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
  scr setCursorPosition null  // hide cursor
  val init = c.initialGrid match {
    case Left(pat) => pat.grid
    case Right(g) => g
  }
  val steps = Iterator.iterate(init)(tick) zip Iterator.iterate(now())(_ + 200)
  var x, y = 0  // center of the view
  scr.startScreen()
  def isQuitKey(ks: input.KeyStroke) =
    (ks.getCharacter: Char).toLower == 'q' ||
      ks.getCharacter == 'c' && ks.isCtrlDown ||
      ks.getKeyType == input.KeyType.EOF  // happens when terminal gets disconnected
  ultimately { scr.stopScreen() } {
    for ((grid, inst) <- steps) {
      for (ks <- Iterator.continually(scr.pollInput()).takeWhile(_ != null)) {
        if (isQuitKey(ks)) {
          return
        }
        ks.getKeyType match {
          case input.KeyType.ArrowDown =>
            y -= 1
          case input.KeyType.ArrowLeft =>
            x += 1
          case input.KeyType.ArrowRight =>
            x -= 1
          case input.KeyType.ArrowUp =>
            y += 1
          case _ =>
        }
      }
      draw(scr, grid, (x, y))
      Thread.sleep(math.max(0, inst - now()))
      scr.refresh()
    }
  }
}
