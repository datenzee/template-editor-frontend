const program = require('./elm/TemplateEditor.elm')


function getDSWApiUrl() {
    if (window.templateEditor && window.templateEditor['dswApiUrl']) return window.templateEditor['dswApiUrl']
    return 'http://localhost:3000'
}
function getTEApiUrl() {
    if (window.templateEditor && window.templateEditor['teApiUrl']) return window.templateEditor['teApiUrl']
    return 'http://localhost:3000'
}


function loadApp() {
    const app = program.Elm.TemplateEditor.init({
        flags: {
            dswApiUrl: getDSWApiUrl(),
            teApiUrl: getTEApiUrl(),
            session: JSON.parse(localStorage.getItem('session')),
            seed: Math.floor(Math.random() * 0xFFFFFFFF)
        }
    })

    app.ports?.storeSession?.subscribe(function (session) {
        localStorage.setItem('session', JSON.stringify(session))
    })

    app.ports?.clearSession?.subscribe(function () {
        localStorage.removeItem('session')
    })

    app.ports?.clearSessionAndReload?.subscribe(function () {
        localStorage.removeItem('session')
        location.reload()
    })
}


window.onload = function () {
    loadApp()
}
