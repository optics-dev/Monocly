package monooptics

import scala.annotation.alpha



trait EOptional[+Error, From, To] { self =>
  def getOrError(from: From): Either[Error, To]
  def replace(to: To)(from: From): From

  def modify(f: To => To)(from: From): From =
    getOrError(from).fold(_ => from, replace(_)(from))

  def getOption(from: From): Option[To] =
    getOrError(from).toOption

  @alpha("andThen")
  def >>>[NewError >: Error, Next](other: EOptional[NewError, To, Next]): EOptional[NewError, From, Next] =
    new EOptional[NewError, From, Next] {
      def getOrError(from: From): Either[NewError, Next] =
        self.getOrError(from).flatMap(other.getOrError)

      def replace(to: Next)(from: From): From =
        self.modify(other.replace(to))(from)
    }
}

object EOptional {
  def apply[Error, From, To](_getOrError: From => Either[Error, To])(_replace: To => From => From): EOptional[Error, From, To] =
    new EOptional[Error, From, To] {
      def getOrError(from: From): Either[Error, To] = _getOrError(from)
      def replace(to: To)(from: From): From = _replace(to)(from)
    }
}
