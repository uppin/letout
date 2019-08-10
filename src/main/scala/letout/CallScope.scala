package letout

import java.io.PrintStream

case class CallScope(private val stdIO: StdIO) extends PrintStream(stdIO.out) {
  def err: PrintStream = stdIO.err
}
