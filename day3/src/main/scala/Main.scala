import scala.io.Source
import scala.collection.View.Empty
import scala.util.control.Exception._
import scala.runtime.BoxesRunTime

def makeInt(s: String): Option[Int] = allCatch.opt(s.toInt)
// def makeBoolean(s: String): Option[Boolean] = allCatch.opt(s.toBooleanOption)

def parseInput(s: String): List[Int] =
  s.map(_.toString).map(makeInt(_).getOrElse(0)).toList

def produceBinaryString(seq: List[Boolean]): String =
  seq.map(_.compareTo(false).toBinaryString).reduceLeft((a, b) => a + b)

@main def hello: Unit =
// part 1
  val part_one_lines = Source
    .fromFile("/home/ed/Documents/AoF2021/day3/src/part1")
    .getLines
    .toList
    .map(parseInput)
  val size = part_one_lines.size

  val gamma_rate_raw =
    part_one_lines.transpose
      .map(_.sum)
      .map(a => a > (size / 2))

  val gamma_rate = Integer.parseInt(
    produceBinaryString(gamma_rate_raw),
    2
  )

  val epsilon_rate_raw = gamma_rate_raw
    .map(_.unary_!)

  val epsilon_rate = Integer.parseInt(
    produceBinaryString(epsilon_rate_raw),
    2
  )
  val part_one_answer = epsilon_rate * gamma_rate
  println(s"part_one: $part_one_answer")
// println(s"part two: $part_two_answer")
