package monocly.internal

case class NonEmptyList[+A](head: A, tail: List[A]):
  def toList: List[A] = 
    head :: tail

  def ++[B >: A](other: NonEmptyList[B]): NonEmptyList[B] = 
    NonEmptyList(head, tail ++ other.toList)

  def map[B](f: A => B): NonEmptyList[B] = 
    NonEmptyList(f(head), tail.map(f))

  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] = 
    val headList = f(head)
    NonEmptyList(headList.head, headList.tail ++ tail.flatMap(a => f(a).toList))

end NonEmptyList