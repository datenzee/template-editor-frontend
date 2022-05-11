const program = require('./elm/TemplateEditor.elm')


function getApiUrl() {
    if (window.registry && window.registry['apiUrl']) return window.registry['apiUrl']
    return 'http://localhost:3000'
}


function loadApp() {
    const app = program.Elm.TemplateEditor.init({
        flags: {
            apiUrl: getApiUrl(),
            session: JSON.parse(localStorage.getItem('session')),
        }
    })

    app.ports?.storeSession?.subscribe(function (session) {
        localStorage.setItem('session', JSON.stringify(session))
    })

    app.ports?.clearSession?.subscribe(function () {
        localStorage.removeItem('session')
    })
}


window.onload = function () {
    loadApp()
}
