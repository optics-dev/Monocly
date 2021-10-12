package monocle.internal

case class NonEmptyList[+A](head: A, tail: List[A]):
  def toList: List[A] = 
    head :: tail

  def ::[A1 >: A](a: A1): NonEmptyList[A1] = 
    NonEmptyList(a, toList)

  def ++[B >: A](other: NonEmptyList[B]): NonEmptyList[B] = 
    NonEmptyList(head, tail ++ other.toList)

  def map[B](f: A => B): NonEmptyList[B] = 
    NonEmptyList(f(head), tail.map(f))

  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] = 
    val headList = f(head)
    NonEmptyList(headList.head, headList.tail ++ tail.flatMap(a => f(a).toList))

  def foldLeft[B](b: B)(f: (B, A) => B): B =
      tail.foldLeft(f(b, head))(f)

  def foldRight[B](b: B)(f: (A, B) => B): B =
      tail.foldRight(f(head, b))(f)

  def reduceRight[B >: A](f: (A, B) => B): B =
      (head :: tail).reduceRight[B](f)
      
  def iterator: Iterator[A] =
    toList.iterator  

end NonEmptyList

object NonEmptyList {
  def of[A](head: A, tail: A*): NonEmptyList[A] =
    NonEmptyList(head, tail.toList)
}