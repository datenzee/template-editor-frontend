module Rdf exposing (..)

import Dict exposing (Dict)


type alias Prefix =
    { prefix : String
    , fullUrl : String
    }


createPrefix : String -> String -> Prefix
createPrefix =
    Prefix


prefixToString : Prefix -> String
prefixToString { prefix, fullUrl } =
    "@prefix " ++ prefix ++ ": <" ++ fullUrl ++ "> ."


usePrefix : Prefix -> String -> String
usePrefix { prefix } obj =
    prefix ++ ":" ++ obj


type alias Node =
    { subject : String
    , predicates : Dict String (List Object)
    }


type Object
    = Ref String
    | Literal String
    | IRI String
    | Boolean Bool


createNode : String -> Node
createNode subject =
    { subject = subject
    , predicates = Dict.empty
    }


addPredicate : String -> String -> Node -> Node
addPredicate predicate object =
    addPredicateObject predicate (Ref object)


addPredicateIRI : String -> String -> Node -> Node
addPredicateIRI predicate object =
    addPredicateObject predicate (IRI object)


addPredicateLiteral : String -> String -> Node -> Node
addPredicateLiteral predicate object =
    addPredicateObject predicate (Literal object)


addPredicateBoolean : String -> Bool -> Node -> Node
addPredicateBoolean predicate object =
    addPredicateObject predicate (Boolean object)


addPredicateObject : String -> Object -> Node -> Node
addPredicateObject predicate object node =
    let
        newObjectList =
            case Dict.get predicate node.predicates of
                Just objectList ->
                    objectList ++ [ object ]

                Nothing ->
                    [ object ]
    in
    { node | predicates = Dict.insert predicate newObjectList node.predicates }


nodeToString : Node -> String
nodeToString node =
    let
        predicates =
            Dict.toList node.predicates
                |> List.map predicateToString
                |> String.join " ;\n    "
    in
    node.subject ++ "\n    " ++ predicates ++ " .\n\n"


predicateToString : ( String, List Object ) -> String
predicateToString ( predicate, objectList ) =
    let
        objects =
            List.map objectToString objectList
                |> String.join " , "
    in
    predicate ++ " " ++ objects


objectToString : Object -> String
objectToString object =
    case object of
        Ref ref ->
            ref

        Literal literal ->
            "\"" ++ literal ++ "\"^^xsd:string"

        IRI iri ->
            "<" ++ iri ++ ">"

        Boolean value ->
            if value then
                "true"

            else
                "false"
