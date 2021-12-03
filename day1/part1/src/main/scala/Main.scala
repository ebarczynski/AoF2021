import scala.io.Source
import scala.collection.View.Empty
import scala.util.control.Exception._
import math.Fractional.Implicits.infixFractionalOps
import math.Integral.Implicits.infixIntegralOps
import math.Numeric.Implicits.infixNumericOps

def makeInt(s: String): Option[Int] = allCatch.opt(s.toInt)

@main def hello: Unit =
  println("Hello world!")
  val part_one_lines = Source
    .fromFile("/home/ed/Documents/AoF2021/day1/part1/src/part1")
    .getLines
    .toList
    .map(makeInt(_).getOrElse(0))
  val part_two_lines = Source
    .fromFile("/home/ed/Documents/AoF2021/day1/part1/src/part2")
    .getLines
    .toList
    .map(makeInt(_).getOrElse(0))

  val part_one_answer = part_one_lines
    .sliding(2, 1)
    .collect { case Seq(a, b) => b - a }
    .toList
    .filter(_ > 0)
    .size

  val part_two_answer = part_two_lines
    .sliding(3, 1)
    .collect { case Seq(a, b, c) => a + b + c }
    .toList
    .sliding(2, 1)
    .collect { case Seq(a, b) => b - a }
    .toList
    .filter(_ > 0)
    .size

  println(s"part_one: $part_one_answer")
  println(s"part two: $part_two_answer")
