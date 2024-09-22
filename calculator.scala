import scala.util.boundary, boundary.break

@main def calculator(commands: String*): Unit = {
  def parseInt(s: String): Int = s.toInt

  var acc: Int = 0
  var a: Int = 0
  var b: Int = 0
  var blink: Boolean = false

  boundary:
    for (c <- commands) {
      c match
      case "+" =>
        acc = a + b
        blink = false
      case "-" =>
        acc = a - b
        blink = false
      case "*" | "x" =>
        acc = a * b
        blink = false
      case "/" =>
        if (b != 0) then 
          acc = a / b
        else 
          acc = 0
          a = 0
          b = 0
        blink = false
      case "swap" =>
        val k = a
        a = b
        b = k
      case "blink" => blink = !blink
      case "acc" =>
        if (blink) then b = acc else a = acc
        blink = !blink
      case "break" => break()
      case _ =>
        if (blink) then b = parseInt(c) else a = parseInt(c)
        blink = !blink
    }

  println(acc)
}
