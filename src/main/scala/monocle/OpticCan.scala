package monocle

//        GetMany
//          /\
//         /  \
// GetOption  GetOneOrMore
//         \  /
//          \/
//          Get
trait GetMany
trait GetOption    extends GetMany
trait GetOneOrMore extends GetMany
trait Get          extends GetOption with GetOneOrMore

//    Modify
//      ^
//      |
//  ReverseGet
trait Modify
trait ReverseGet extends Modify


type Edit = Get & Modify
type EditOption = GetOption & Modify
type EditOneOrMore = GetOneOrMore & Modify
type EditMany = GetMany & Modify
type ConvertBetween = Get & ReverseGet
type SelectBranch = GetOption & ReverseGet