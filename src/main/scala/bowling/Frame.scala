package bowling

object Frame {

  /**
    * Make a score list
    * @param frames Characters of this game
    * @return list of the result by the frame
    */
  def frames(frames: List[String]): List[Frame] = frames.map {
    case s if isStrike(s) => Strike()
    case s if isSpare(s)  => Spare(firstThrowPoint(s))
    case s                => Normal(firstThrowPoint(s), secondThrowPoint(s))
  }

  /**
    * Get first throwing point
    * @param result the field to get the point
    * @return the first point
    */
  def firstThrowPoint(result: String): Int =
    stringToPoint(result, FIRST_THROW)

  /**
    * Get second throwing point
    * @param result the field to get the point
    * @return the second point
    */
  def secondThrowPoint(result: String): Int =
    stringToPoint(result, SECOND_THROW)

  /**
    * Convert character thet means throwing result to number
    * @param result the field to get the point
    * @param time throwing time. the first throw is 1, the second throw is 2
    * @return point the point at the throw
    */
  def stringToPoint(result: String, time: Int): Int = {

    val scoreChar: String = getChar(result, time)

    if (isGutter(scoreChar)) ZERO_POINT
    else scoreChar.toInt
  }

}

sealed trait Frame
case class Strike() extends Frame
case class Spare(one: Int) extends Frame
case class Normal(one: Int, two: Int) extends Frame
