import scala.annotation.tailrec

case class BowlingScoreSheet(str: String) {

  /**
    * Frame separator
    */
  val SCORE_SEP: String = "\\|"

  /**
    * Strike the all pins at the first throw of the frame
    */
  val STRIKE_SIGN: String = "X"

  /**
    * Strike the all pins at the second throw of the frame
    */
  val SPARE_SIGN: String = "/"

  /**
    * Strike no pins at the throw of the frame
    */
  val GUTTER_SIGN: String = "-"

  /**
    * Max point that get at one frame
    */
  val MAX_POINT: Int = 10

  /**
    * No point
    */
  val ZERO_POINT: Int = 0

  /**
    * First throw at the frame
    */
  val FIRST_THROW: Int = 1

  /**
    * Second throw at the frame
    */
  val SECOND_THROW: Int = 2

  /**
    *  Split game to NormalGame and BonusGame
    */
  val normalAndBonusGames: Array[String] = str.split(SCORE_SEP + SCORE_SEP)

  /**
    * Split NormalGame By frames
    */
  val normalFrames: List[String] =
    normalAndBonusGames.head.split(SCORE_SEP).toList

  /**
    * Make a score list
    */
  val frames: List[Frame] = normalFrames.map {
    case s if isStrike(s) => Strike()
    case s if isSpare(s)  => Spare(firstThrowPoint(s))
    case s                => Normal(firstThrowPoint(s), secondThrowPoint(s))
  }

  /**
    * Bonus point
    */
  val bonusBallResult: Option[BonusBall] =
    if (normalAndBonusGames.length == 1) None // if can not get the bonus games
    else
      // if can get the bonus games
      normalAndBonusGames(1) match {
        case s if canThrowBonusBall(s, SECOND_THROW) =>
          val one = getFirstPointWhenTwiceBonusBall(s)
          val two = getSecondPointWhenTwiceBonusBall(s, one)
          Some(StrikeBonusBall(one, two))
        case s if canThrowBonusBall(s, FIRST_THROW) =>
          val one = getPointWhenOnceBonusBall(s)
          Some(SpareBonusBall(one))
      }

  /**
    * Get Character
    * @param throwingResult
    * @param time
    * @return
    */
  def getChar(throwingResult: String, time: Int): String =
    throwingResult.charAt(time - 1).toString

  /**
    * Check the frame is Strike
    * @param throwingResult the field to check, return true when the result is Strike
    * @return true if the result is Strike, false if not
    */
  def isStrike(throwingResult: String): Boolean =
    throwingResult == STRIKE_SIGN

  /**
    * Check the frame is Spare
    * @param throwingResult the field to check, return true when the result is Spare
    * @return true if the result is Spare, false if not
    */
  def isSpare(throwingResult: String): Boolean =
    throwingResult.length >= 2 && getChar(throwingResult, SECOND_THROW) == SPARE_SIGN

  /**
    * Check the frame is Gutter
    * @param throwingResult the field to check, return true when the result is Gutter
    * @return true if the result is Gutter, false if not
    */
  def isGutter(throwingResult: String): Boolean =
    throwingResult == GUTTER_SIGN

  /**
    * Get first throwing point
    * @param throwingResult the field to get the point
    * @return the first point
    */
  def firstThrowPoint(throwingResult: String): Int =
    stringToPoint(throwingResult, FIRST_THROW)

  /**
    * Get second throwing point
    * @param throwingResult the field to get the point
    * @return the second point
    */
  def secondThrowPoint(throwingResult: String): Int =
    stringToPoint(throwingResult, SECOND_THROW)

  /**
    * Convert character thet means throwing result to number
    * @param throwingResult the field to get the point
    * @param time throwing time. the first throw is 1, the second throw is 2
    * @return point the point at the throw
    */
  def stringToPoint(throwingResult: String, time: Int): Int = {

    val scoreChar: String = getChar(throwingResult, time)

    if (isGutter(scoreChar)) ZERO_POINT
    else scoreChar.toInt
  }

  /**
    * Check the right of the throwing at the bonus game
    * @param throwingResult the field to check the right
    * @param time throwing time
    * @return true if can throw bonus game, false if not
    */
  def canThrowBonusBall(throwingResult: String, time: Int): Boolean =
    throwingResult.length >= time

  /**
    * Get the first throwing point at the bonus game
    * @param throwingResult the field to get the point
    * @return the first throwing point
    */
  def getFirstPointWhenTwiceBonusBall(throwingResult: String): Int = {

    val scoreChar: String = getChar(throwingResult, FIRST_THROW)

    if (isGutter(scoreChar)) ZERO_POINT
    else if (isStrike(scoreChar)) MAX_POINT
    else scoreChar.toInt
  }

  /**
    * Get the second throwing point at the bonus game
    * @param throwingResult the field to get the point
    * @return the second throwing point
    */
  def getSecondPointWhenTwiceBonusBall(throwingResult: String,
                                       firstPoint: Int): Int = {

    val scoreChar: String = getChar(throwingResult, SECOND_THROW)

    if (isGutter(scoreChar)) ZERO_POINT
    else if (isStrike(scoreChar)) MAX_POINT
    else if (isSpare(scoreChar)) MAX_POINT - firstPoint
    else scoreChar.toInt
  }

  /**
    * Get the throwing point at the bonus game
    * @param throwingResult the field to get the point
    * @return the throwing point
    */
  def getPointWhenOnceBonusBall(throwingResult: String): Int = {

    val scoreChar: String = getChar(throwingResult, FIRST_THROW)

    if (isGutter(scoreChar)) ZERO_POINT
    else if (isStrike(scoreChar)) MAX_POINT
    else scoreChar.toInt
  }

  /**
    * calculate th bowling score
    * @return score point
    */
  def calcScore(): Int = {

    def plusAdditionalPoint(l: List[AdditionBonus],
                            p: Int): (List[AdditionBonus], Int) =
      l.foldLeft((Nil: List[AdditionBonus], ZERO_POINT)) {
        (t: (List[AdditionBonus], Int), addition: AdditionBonus) =>
          addition match {
            case AddOnce  => (t._1, t._2 + p)
            case AddTwice => (t._1 :+ AddOnce, t._2 + p)
          }
      }

    @tailrec
    def go(frames: List[Frame],
           score: Int,
           rightList: List[AdditionBonus]): (List[AdditionBonus], Int) =
      frames match {
        case Nil => plusAdditionalPoint(rightList, ZERO_POINT)
        case x :: Nil =>
          x match {
            // this is the last frame
            case Strike() => {
              val (additionalRights, additionalPoint) =
                plusAdditionalPoint(rightList, MAX_POINT)
              (additionalRights, score + MAX_POINT + additionalPoint)
            }
            case Spare(_) => {
              val (additionalRights, additionalPoint) =
                plusAdditionalPoint(rightList, MAX_POINT)
              (Nil, score + MAX_POINT + additionalPoint)
            }
            case Normal(m, n) => {
              // the first throw
              val (firstAdditionalRights, firstAdditionalPoint) =
                plusAdditionalPoint(rightList, m)
              // the second throw
              val (secondAdditionalRights, secondAdditionalPoint) =
                plusAdditionalPoint(firstAdditionalRights, n)
              (secondAdditionalRights,
               score + m + n + firstAdditionalPoint + secondAdditionalPoint)
            }
          }
        case x :: xs =>
          x match {
            case Strike() =>
              val (additionalRights, additionalPoint) =
                plusAdditionalPoint(rightList, MAX_POINT)
              // calculate next frame
              go(xs,
                 score + MAX_POINT + additionalPoint,
                 additionalRights :+ AddTwice)

            case Spare(m) =>
              // the first throw
              val (firstAdditionalRights, firstAdditionalPoint) =
                plusAdditionalPoint(rightList, m)
              // the second throw
              val (_, secondAdditionalPoint) =
                plusAdditionalPoint(firstAdditionalRights, MAX_POINT - m)
              // calculate next frame
              go(
                xs,
                score + MAX_POINT + firstAdditionalPoint + secondAdditionalPoint,
                AddOnce :: Nil)

            case Normal(m, n) =>
              // the first throw
              val (firstAdditionalRights, firstAdditionalPoint) =
                plusAdditionalPoint(rightList, m)
              // the second throw
              val (secondAdditionalRights, secondAdditionalPoint) =
                plusAdditionalPoint(firstAdditionalRights, n)
              // calculate next frame
              go(xs,
                 score + m + n + firstAdditionalPoint + secondAdditionalPoint,
                 secondAdditionalRights)
          }
      }

    // make the blank list to contain the additional right
    val additionalRightList: List[AdditionBonus] = Nil

    // get the score!!!
    val (restAdditionRights, totalPoint) =
      go(frames, ZERO_POINT, additionalRightList)

    val bonusResultPoint: Int = bonusBallResult match {
      case Some(StrikeBonusBall(one, two)) => {
        // the first throw
        val (_, firstAdditionalPoint) =
          plusAdditionalPoint(restAdditionRights, one)
        // the second throw
        val (_, secondAdditionalPoint) =
          plusAdditionalPoint(Nil, two)
        one + two + firstAdditionalPoint + secondAdditionalPoint
      }
      case Some(SpareBonusBall(one)) => {
        val (_, firstAdditionalPoint) =
          plusAdditionalPoint(restAdditionRights, one)
        one + firstAdditionalPoint
      }
      case _ => 0
    }

    totalPoint + bonusResultPoint
  }
}
