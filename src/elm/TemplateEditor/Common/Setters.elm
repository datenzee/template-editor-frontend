module TemplateEditor.Common.Setters exposing
    ( setComponentType
    , setContent
    , setDashboard
    , setLogin
    , setPredicate
    , setTemplateEditor
    , setTemplateEditors
    , setUrlLabel
    , setValue
    )


setContent : v -> { a | content : v } -> { a | content : v }
setContent value data =
    { data | content = value }


setComponentType : v -> { a | componentType : v } -> { a | componentType : v }
setComponentType value data =
    { data | componentType = value }


setDashboard : v -> { a | dashboard : v } -> { a | dashboard : v }
setDashboard value data =
    { data | dashboard = value }


setLogin : v -> { a | login : v } -> { a | login : v }
setLogin value data =
    { data | login = value }


setPredicate : v -> { a | predicate : v } -> { a | predicate : v }
setPredicate value data =
    { data | predicate = value }


setTemplateEditor : v -> { a | templateEditor : v } -> { a | templateEditor : v }
setTemplateEditor value data =
    { data | templateEditor = value }


setTemplateEditors : v -> { a | templateEditors : v } -> { a | templateEditors : v }
setTemplateEditors value data =
    { data | templateEditors = value }


setUrlLabel : v -> { a | urlLabel : v } -> { a | urlLabel : v }
setUrlLabel value data =
    { data | urlLabel = value }


setValue : v -> { a | value : v } -> { a | value : v }
setValue value data =
    { data | value = value }
