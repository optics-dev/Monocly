package monocly

//        GetMany
//          /\
//         /  \
// GetOption  GetOneOrMore
//         \  /
//          \/
//        GetOne
trait GetMany
trait GetOption extends GetMany
trait GetOneOrMore extends GetMany
trait GetOne extends GetOption with GetOneOrMore

//    Modify
//      ^
//      |
//  ReverseGet
trait Modify
trait ReverseGet extends Modify
