# Focus 

`Focus` generates an optic given a path. This is similar to XPath, JsonPath or JQuery but for all immutable data. 

## 1. Fields of a case class

`Focus` can zoom into the field of a case class several level deep using the syntax `.field1.field2`.

```scala
case class User(name: String, address: Address)
case class Address(streetNumber: Int, postcode: String)

val user = User("Elise", Address(12, "high street"))

user.focus(_.name).replace("Bob")
// res: User = User("Bob", Address(12, "high street"))

user.focus(_.address.postcode).modify(_.upperCase)
// res: User = User("Elise", Address(12, "HIGH STREET"))
```

### Technical details

Type of optics generated: `Lens`

If `Focus` targets the field of a case class with a type parameter, it will generate a mononmorphic optic,
meaning that `Focus` doesn't let us change the type of the data. Future, version of `Focus` will most likely
lift this restriction.

```scala
case class Id[A](id: Long, value: A)

Id(5, "hello").focus(_.value).replace("world")
// res: Id[String] = Id(5, "world")

Id(5, "hello").focus(_.value).replace(true)
// error: focus does not yet support to change the type of a parametric class
```

## 2. Options

`Focus` offer two modes to zoom into an `Option`:
1. `some` if the `Option` is a `Some` zoom into it, otherwise, it is a no-operation.
2. `withDefault(defaultValue)` same as `some` but falls back to `defaultValue` if the `Option` is a `None`.

```scala
case class User(name: String, address: Option[Address])
case class Address(streetNumber: Int, postcode: String)

val elise = User("Elise", Some(Address(12, "high street")))
val bob   = User("bob"  , None)

elise.focus(_.address.some.streetNumber).replace(24)
// res: User = User("Elise", Some(Address(24, "high street")))

// no-op address is None
bob.focus(_.address.some.streetNumber).replace(24) 
// res: User = User("bob", None) 

// same as some
elise.focus(_.address.witdDefault(Address(1, "")).streetNumber).replace(24)
// res: User = User("Elise", Some(Address(24, "high street")))

// same as some
bob.focus(_.address.witdDefault(Address(1, "")).streetNumber).replace(24)
// res: User = User("bob", Some(Address(24, ""))) 
```

### Technical details

Type of optics generated: `Prism` for `some`, `Iso` for `withDefault`.

If `Focus` targets an `Option[A]` where `A` is generic, then `some` and `withDefault` will generate a mononmorphic optic,
meaning that `Focus` doesn't let us change the type of the data. Future, version of `Focus` will most likely
lift this restriction for `some`.

```scala
case class Id[A](id: Long, value: Option[A])

Id(5, Some("hello")).focus(_.some.value).replace("world")
// res: Id[String] = Id(5, Some("world"))

Id(5, "hello").focus(_.some.value).replace(true)
// error: focus does not yet support to change the type of a parametric class
```