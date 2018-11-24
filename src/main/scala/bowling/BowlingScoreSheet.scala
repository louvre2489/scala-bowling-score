package bowling

import scala.annotation.tailrec

case class BowlingScoreSheet(str: String) {

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
  val frames: List[Frame] = Frame.frames(normalFrames)

  /**
    * Bonus point
    */
  val bonusBallResult: Option[BonusBall] =
    BonusBall.bonusBallResult(normalAndBonusGames)

  /**
    * calculate th bowling score
    * @return score point
    */
  def calcScore(): Int = {

    /**
      * calculate the bonus point
      * @param addList rest of the addition right
      * @return bonus point
      */
    def calculateBonusPoint(addList: List[AdditionBonus]): Int =
      bonusBallResult match {
        case Some(bonusBall) =>
          bonusBall match {
            case strikeBonus: StrikeBonusBall =>
              // the first throw
              val firstPoint = strikeBonus.firstPoint
              val (_, firstAdditionalPoint) =
                plusAdditionalPoint(addList, firstPoint)

              // the second throw
              val secondPoint = strikeBonus.secondPoint
              val (_, secondAdditionalPoint) =
                plusAdditionalPoint(Nil, secondPoint)

              firstPoint + secondPoint + firstAdditionalPoint + secondAdditionalPoint

            case spareBonus: SpareBonusBall =>
              val firstPoint = spareBonus.firstPoint
              val (_, firstAdditionalPoint) =
                plusAdditionalPoint(addList, firstPoint)

              MAX_POINT + firstAdditionalPoint
          }
        case _ => 0
      }

    /**
      * calculate additional point
      * In case of Strike, we can add point at next two times
      * In case of Spare, we can add at next time
      * @param addList rest of the addition right
      * @param point point
      * @return the result point of the calculation
      */
    def plusAdditionalPoint(addList: List[AdditionBonus],
                            point: Int): (List[AdditionBonus], Int) =
      addList.foldLeft((Nil: List[AdditionBonus], ZERO_POINT)) {
        (t: (List[AdditionBonus], Int), addition: AdditionBonus) =>
          addition match {
            case AddOnce  => (t._1, t._2 + point)
            case AddTwice => (t._1 :+ AddOnce, t._2 + point)
          }
      }

    /**
      * calculate point
      * @param frames result of thie game
      * @param point point
      * @param rightList rest of the addition right
      * @return the result point of the calculation
      */
    @tailrec
    def go(frames: List[Frame],
           point: Int,
           rightList: List[AdditionBonus]): (List[AdditionBonus], Int) =
      frames match {
        case Nil => plusAdditionalPoint(rightList, ZERO_POINT)

        case x :: Nil =>
          x match {
            // this is the last frame
            case Strike() =>
              val (additionalRights, additionalPoint) =
                plusAdditionalPoint(rightList, MAX_POINT)
              (additionalRights, point + MAX_POINT + additionalPoint)

            case Spare(_) =>
              val (_, additionalPoint) =
                plusAdditionalPoint(rightList, MAX_POINT)
              (Nil, point + MAX_POINT + additionalPoint)

            case Normal(m, n) =>
              // the first throw
              val (firstAdditionalRights, firstAdditionalPoint) =
                plusAdditionalPoint(rightList, m)
              // the second throw
              val (secondAdditionalRights, secondAdditionalPoint) =
                plusAdditionalPoint(firstAdditionalRights, n)
              (secondAdditionalRights,
               point + m + n + firstAdditionalPoint + secondAdditionalPoint)
          }

        case x :: xs =>
          x match {
            case Strike() =>
              val (additionalRights, additionalPoint) =
                plusAdditionalPoint(rightList, MAX_POINT)
              // calculate next frame
              go(xs,
                 point + MAX_POINT + additionalPoint,
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
                point + MAX_POINT + firstAdditionalPoint + secondAdditionalPoint,
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
                 point + m + n + firstAdditionalPoint + secondAdditionalPoint,
                 secondAdditionalRights)
          }
      }

    // make the blank list to contain the additional right
    val additionalRightList: List[AdditionBonus] = Nil

    // the score except the bonus
    val (restAdditionRights, totalPoint) =
      go(frames, ZERO_POINT, additionalRightList)

    // calculate total score
    totalPoint + calculateBonusPoint(restAdditionRights)
  }
}
