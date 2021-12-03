import scala.io.Source
import scala.util.control.Exception._

def makeInt(s: String): Option[Int] = allCatch.opt(s.toInt)

enum Direction:
  case Forward, Down, Up

case class Command(
    direction: Direction,
    steps: Int
)

def parseCommand(s: String): Command =
  val splitted_command = s.split(" ")
  val direction = splitted_command(0) match {
    case "forward" => Direction.Forward
    case "down"    => Direction.Down
    case "up"      => Direction.Up
  }
  val steps = makeInt(splitted_command(1)).getOrElse(0)
  Command(direction, steps)

def processCommand(a: Command): (Int, Int) = a.direction match {
  case Direction.Forward => (a.steps, 0)
  case Direction.Down    => (0, a.steps)
  case Direction.Up      => (0, -a.steps)
}

@main def hello: Unit =
// part 1
  val part_one_lines = Source
    .fromFile("/home/ed/Documents/AoF2021/day2/src/part1")
    .getLines
    .toList
    .map(parseCommand)

  val reduced = part_one_lines
    .map(processCommand)
    .reduce((a, b) => (a._1 + b._1, a._2 + b._2))

  val part_one_answer = reduced._1 * reduced._2

// part2
  val part_two_lines = Source
    .fromFile("/home/ed/Documents/AoF2021/day2/src/part2")
    .getLines
    .toList
    .map(parseCommand)

  val processed = part_two_lines
    .map(processCommand)
    .map((a, b) => /*pair: horizontal pos, aim, depth*/ (a, b, 0))
    .reduceLeft((a, b) =>
      (a._1 + b._1, a._2 + b._2, a._3 + (b._1 * (a._2 + b._2)))
    )

  val part_two_answer = processed._1 * processed._3

  println(s"part_one: $part_one_answer")
  println(s"part two: $part_two_answer")
