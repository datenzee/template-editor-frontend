module TemplateEditor.Common.Setters exposing (setDashboard, setLogin, setTemplateEditor, setTemplateEditors)


setDashboard : v -> { a | dashboard : v } -> { a | dashboard : v }
setDashboard value data =
    { data | dashboard = value }


setLogin : v -> { a | login : v } -> { a | login : v }
setLogin value data =
    { data | login = value }


setTemplateEditor : v -> { a | templateEditor : v } -> { a | templateEditor : v }
setTemplateEditor value data =
    { data | templateEditor = value }


setTemplateEditors : v -> { a | templateEditors : v } -> { a | templateEditors : v }
setTemplateEditors value data =
    { data | templateEditors = value }
