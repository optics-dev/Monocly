package monooptics

import scala.annotation.alpha

trait Optional[From, To] { self =>
  def getOption(from: From): Option[To]
  def replace(to: To)(from: From): From

  @alpha("andThen")
  def >>>[Next](other: Optional[To, Next]): Optional[From, Next] =
    new Optional[From, Next] {
      def getOption(from: From): Option[Next] =
        self.getOption(from).flatMap(other.getOption)

      def replace(newValue: Next)(from: From): From =
        self.getOption(from).fold(from) { to =>
          val newNext = other.replace(newValue)(to)
          self.replace(newNext)(from)
        }
    }
}

object Optional {
  def index[Key, Value](key: Key): Optional[Map[Key, Value], Value] =
    new Optional[Map[Key, Value], Value] {
      def getOption(from: Map[Key, Value]): Option[Value] = from.get(key)
      def replace(to: Value)(from: Map[Key, Value]): Map[Key, Value] =
        if(from.contains(key)) from + (key -> to)
        else from
    }
}

object Test extends App {
  val config = Map(
    "kafka" -> Map(
      "topics" -> Map(
        "event1" -> Map(
          "partitions" -> 4
        ),
        "event2" -> Map(
          "partitions" -> 1
        )
      )
    )
  )

  import Optional.index

  def partition(topic: String): Optional[Map[String, Map[String, Map[String, Map[String, Int]]]], Int] =
    index("kafka") >>> index("topics") >>> index(topic) >>> index("partitions")

  val event1 = partition("event1")
  val foo    = partition("foo")

  println(partition("event1").getOption(config))
  println(partition("event1").replace(6)(config))

  println(partition("foo").getOption(config))
  println(partition("foo").replace(6)(config))

  def nel: ::[Int] = ???

}
