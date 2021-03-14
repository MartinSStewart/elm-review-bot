module Version exposing (..)


type alias MajorVersion =
    Int


type alias Version =
    { major : MajorVersion
    , minor : Int
    , patch : Int
    }


toString : Version -> String
toString { major, minor, patch } =
    String.fromInt major ++ "." ++ String.fromInt minor ++ "." ++ String.fromInt patch


compare : Version -> Version -> Order
compare a b =
    case Basics.compare a.major b.major of
        EQ ->
            case Basics.compare a.minor b.minor of
                EQ ->
                    case Basics.compare a.patch b.patch of
                        EQ ->
                            EQ

                        order ->
                            order

                order ->
                    order

        order ->
            order
