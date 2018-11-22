package object bowling {

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
    * Get Character
    * @param throwingResult result of th throwing
    * @param time throwing time
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
}
