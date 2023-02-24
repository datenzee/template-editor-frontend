module TemplateEditor.Data.ViewOntology.Prefixes exposing (..)

import Rdf


voPrefix : Rdf.Prefix
voPrefix =
    Rdf.createPrefix "vo" "http://purl.org/datenzee/view-ontology#"


vo : String -> String
vo =
    Rdf.usePrefix voPrefix


owlPrefix : Rdf.Prefix
owlPrefix =
    Rdf.createPrefix "owl" "http://www.w3.org/2002/07/owl#"


owl : String -> String
owl =
    Rdf.usePrefix owlPrefix


rdfPrefix : Rdf.Prefix
rdfPrefix =
    Rdf.createPrefix "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"


rdf : String -> String
rdf =
    Rdf.usePrefix rdfPrefix


xmlPrefix : Rdf.Prefix
xmlPrefix =
    Rdf.createPrefix "xml" "http://www.w3.org/XML/1998/namespace"


xsdPrefix : Rdf.Prefix
xsdPrefix =
    Rdf.createPrefix "xsd" "http://www.w3.org/2001/XMLSchema#"


rdfsPrefix : Rdf.Prefix
rdfsPrefix =
    Rdf.createPrefix "rdfs" "http://www.w3.org/2000/01/rdf-schema#"


rdfs : String -> String
rdfs =
    Rdf.usePrefix rdfsPrefix


dctPrefix : Rdf.Prefix
dctPrefix =
    Rdf.createPrefix "dct" "http://purl.org/dc/terms/"


dct : String -> String
dct =
    Rdf.usePrefix dctPrefix


foafPrefix : Rdf.Prefix
foafPrefix =
    Rdf.createPrefix "foaf" "http://xmlns.com/foaf/0.1/"


foaf : String -> String
foaf =
    Rdf.usePrefix foafPrefix


basePrefix : Rdf.Prefix
basePrefix =
    Rdf.createPrefix "" "http://example.com#"


base : String -> String
base =
    Rdf.usePrefix basePrefix
