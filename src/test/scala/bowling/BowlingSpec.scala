package bowling

import org.scalatest.FlatSpec

/**
  * スコアの検算は以下で可能
  * <a href="http://www.n-arts.com/bowling/score.cgi">ボーリングスコア計算CGI</a>
  */
class BowlingSpec extends FlatSpec {

  "Bowling Score Sheet" should "全部のフレームがストライクだと300点" in {
    val input = "X|X|X|X|X|X|X|X|X|X||XX"
    val output = 300

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "全部のフレームが9本だと90点" in {
    val input = "9-|9-|9-|9-|9-|9-|9-|9-|9-|9-||"
    val output = 90

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }


  /**
    * ガーターの検証
    */
  "ガーター" should "全部のフレームでガーターだと0点" in {
    val input = "--|--|--|--|--|--|--|--|--|--||"
    val output = 0

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "１投目ガーターだと２投目の点数のみが加算される" in {
    val input = "-3|--|--|--|--|--|--|--|--|--||"
    val output = 3

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "２投目ガーターだと１投目の点数のみが加算される" in {
    val input = "4-|--|--|--|--|--|--|--|--|--||"
    val output = 4

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  /**
    * ボーナスゲームの検証
    */
  "ボーナスゲーム" should "１〜９フレーム目まではガーター、１０フレーム目とボーナスゲームが全てストライクだと30点" in {
    val input = "--|--|--|--|--|--|--|--|--|X||XX"
    val output = 30

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "１〜９フレーム目まではストライク、ボーナスボールは全てガーターだと240点" in {
    val input = "X|X|X|X|X|X|X|X|X|--||"
    val output = 240

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  /**
    * ストライクの検証
    */
  "ストライク" should "次の得点とさらにその次の得点が加算される" in {
    val input = "X|45|1-|--|--|--|--|--|--|--||"
    val output = 29

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "次の得点がガーターなら、さらにその次の得点のみが加算される" in {
    val input = "X|-5|1-|--|--|--|--|--|--|--||"
    val output = 21

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "翌２投目がガーターの場合は直後の得点のみのが加算される" in {
    val input = "X|4-|1-|--|--|--|--|--|--|--||"
    val output = 19

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "次もその次もガーターの場合は加算はない" in {
    val input = "X|--|1-|--|--|--|--|--|--|--||"
    val output = 11

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  /**
    * スペアの検証
    */
  "スペア" should "次の得点が加算される" in {
    val input = "-/|45|1-|--|--|--|--|--|--|--||"
    val output = 24

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "次がガーターの場合は加算はない" in {
    val input = "-/|-5|1-|--|--|--|--|--|--|--||"
    val output = 16

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  "仕様書内容の検証" should "１フレーム目完了時：10点" in {
    val input = "X|--|--|--|--|--|--|--|--|--||"
    val output = 10

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "２フレーム目完了時：30点" in {
    val input = "X|7/|--|--|--|--|--|--|--|--||"
    val output = 30

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "３フレーム目完了時：48点" in {
    val input = "X|7/|9-|--|--|--|--|--|--|--||"
    val output = 48

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "４フレーム目完了時：58点" in {
    val input = "X|7/|9-|X|--|--|--|--|--|--||"
    val output = 58

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "５フレーム目完了時：74点" in {
    val input = "X|7/|9-|X|-8|--|--|--|--|--||"
    val output = 74

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "６フレーム目完了時：84点" in {
    val input = "X|7/|9-|X|-8|8/|--|--|--|--||"
    val output = 84

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "７フレーム目完了時：90点" in {
    val input = "X|7/|9-|X|-8|8/|-6|--|--|--||"
    val output = 90

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "８フレーム目完了時：100点" in {
    val input = "X|7/|9-|X|-8|8/|-6|X|--|--||"
    val output = 100

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "９フレーム目完了時：120点" in {
    val input = "X|7/|9-|X|-8|8/|-6|X|X|--||"
    val output = 120

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "10フレーム目完了時：150点" in {
    val input = "X|7/|9-|X|-8|8/|-6|X|X|X||"
    val output = 150

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }

  it should "ボーナスゲーム完了時：167点" in {
    val input = "X|7/|9-|X|-8|8/|-6|X|X|X||81"
    val output = 167

    val bowlingScoreSheet = BowlingScoreSheet(input)
    val score: Int = bowlingScoreSheet.calcScore()
    assert(score === output)
  }
}
