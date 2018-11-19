sealed trait Frame

case class Strike() extends Frame
case class Spare(one: Int) extends Frame
case class Normal(one: Int, two: Int) extends Frame
