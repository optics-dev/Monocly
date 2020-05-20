```scala
enum User {
  def id: Long
  def name: String

  case InactiveUser(id: Long, name: String)
  case ActiveUser(id: Long, name: String, address: Address, paymentMethods: Map[String, PaymentMethod])
}

case class Address(streetNumber: Int, postCode: String)
enum PaymentMethod {
  case DebitCard(cardNumber: String, expirationDate: YearMonth, securityCode: Int)
  case PayPal(email: String)
}

val eda: InactiveUser = InactiveUser(45, "Eda Smith")

val john: ActiveUser = ActiveUser(
  23,
  "John Doe",
  Address(12, "E16 4SR"),
  Map(
    "Personal" -> PayPal("john@gmail.com"),
    "Business" -> DebitCard("4568 8980 2376 5431", YearMonth.of(2021, 7), 995)
  )
)
```

### Access fields at any level

```scala
john.focus(_.name).get
// res: String = John Doe

john.focus(_.address.streetNumber).get
// res: Int = 12

john.focus(_.address.streetNumber).replace(34)
// res: User = ...
```

### Access common fields in an enumeration

```scala
val user1: User = john // upcast
val user2: User = eda  // upcast

user1.focus(_.id).get
// res: Long = 23

user2.focus(_.id).replace(10)
// res: User = InactiveUser(10, "Eda Smith")
```

### Access a value in a Map

```scala
john.focus(_.paymentMethods.index("Personal")).getOption
// res: Option[PaymentMethod] = Some(PayPal("john@gmail.com"))

john.focus(_.paymentMethods.index("Personal")).replace(DebitCard(...))
// res: User = ...

With useful error message (error type?)

john.focus(_.paymentMethods.index("Job")).getOrError
// res: Either[String, PaymentMethod] = Left("No value at key 'Job' in paymentMethods Map")
```


### Chose a branch within an enumeration

```scala
john.focus(_.paymentMethods.index("Personal").as[DebitCard].securityCode).getOption
// res: Option[Int] = Some(995)
```

With useful error message (error type?)

```scala
john.focus(_.paymentMethods.index("Personal").as[PayPal].email).getOrError
// res: Either[String, String] = Left("Payment method at key 'Personal' is not a PayPal, but a DebitCard")

john.focus(_.paymentMethods.index("Job").as[PayPal].email).getOrError
// res: Either[String, String] = Left("No value at key 'Job' in paymentMethods Map")
```

### Access all values in a Map

```scala
john.focus(_.paymentMethods.*.as[DebitCard].expirationDate).getAll
// res: List[YearMonth] = List(YearMonth.of(2021, 7))
```
