package bowling

object BonusBall {

  /**
    * Check the right of the throwing at the bonus game
    * @param throwingResult the field to check the right
    * @param time throwing time
    * @return true if can throw bonus game, false if not
    */
  def canThrowBonusBall(throwingResult: String, time: Int): Boolean =
    throwingResult.length >= time

  /**
    * Bonus point
    */
  def bonusBallResult(normalAndBonusGames: Array[String]): Option[BonusBall] =
    if (normalAndBonusGames.length == 1) None // if can not get the bonus games
    else
    // if can get the bonus games
      normalAndBonusGames(1) match {
        case s if canThrowBonusBall(s, SECOND_THROW) =>
          Some(StrikeBonusBall(s))
        case s if canThrowBonusBall(s, FIRST_THROW) =>
          Some(SpareBonusBall(s))
      }
}

sealed trait BonusBall {

  val firstPoint: Int

  def getFirstPoint: Int
}

case class StrikeBonusBall(result: String) extends BonusBall {

  val firstPoint: Int = getFirstPoint

  val secondPoint: Int = getSecondPoint

  /**
    * Get the first throwing point at the bonus game
    * @return the first throwing point
    */
  def getFirstPoint: Int = {

    val scoreChar: String = getChar(result, FIRST_THROW)

    if (isGutter(scoreChar)) ZERO_POINT
    else if (isStrike(scoreChar)) MAX_POINT
    else scoreChar.toInt
  }

  /**
    * Get the second throwing point at the bonus game
    * @return the second throwing point
    */
  def getSecondPoint: Int = {

    val scoreChar: String = getChar(result, SECOND_THROW)

    if (isGutter(scoreChar)) ZERO_POINT
    else if (isStrike(scoreChar)) MAX_POINT
    else if (isSpare(scoreChar)) MAX_POINT - firstPoint
    else scoreChar.toInt
  }
}

case class SpareBonusBall(result: String) extends BonusBall {

  val firstPoint: Int = getFirstPoint

  /**
    * Get the throwing point at the bonus game
    * @return the throwing point
    */
  def getFirstPoint: Int = {

    val scoreChar: String = getChar(result, FIRST_THROW)

    if (isGutter(scoreChar)) ZERO_POINT
    else if (isStrike(scoreChar)) MAX_POINT
    else scoreChar.toInt
  }
}
