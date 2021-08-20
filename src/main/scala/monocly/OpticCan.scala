package monocly

sealed trait OpticCan

//        GetMany
//          /\
//         /  \
// GetOption  GetOneOrMore
//         \  /
//          \/
//        GetOne
trait GetMany extends OpticCan
trait GetOption extends GetMany
trait GetOneOrMore extends GetMany
trait GetOne extends GetOption with GetOneOrMore

//    Modify
//      ^
//      |
//  ReverseGet
trait Modify extends OpticCan
trait ReverseGet extends Modify
