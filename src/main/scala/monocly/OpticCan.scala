package monocly

//        GetMany
//          /\
//         /  \
// GetOption  GetOneOrMore
//         \  /
//          \/
//        GetOne
sealed trait GetteCan
trait GetMany extends GetteCan
trait GetOption extends GetMany
trait GetOneOrMore extends GetMany
trait GetOne extends GetOption with GetOneOrMore

//    Modify
//      ^
//      |
//  ReverseGet
sealed trait SetterCan
trait Modify extends SetterCan
trait ReverseGet extends Modify
