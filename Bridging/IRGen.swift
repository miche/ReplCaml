import Foundation

indirect enum Typ {
    public typealias T = Typ

    case UNIT
    case BOOL
    case INT
    case FLOAT
    case VAR(T)
    case FUN([T], T)
}

indirect enum ClosureT {
    typealias T = ClosureT
    typealias I = String
    case INT(Int)
    case ADD(I, I)
    case LET(I, T, T)
    case MakeCls(I, I, [I], T)
    //    case MakeCls(IdentX, L, [I], T) //(IdentX, ClosureX(Id.l, [Id.t]), T)
    //    case AppCls(I, [I]) // closure
    //    case AppDir(L, [I]) // direct call(top level)
}

