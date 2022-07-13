module.exports = function (app) {
    app.ports.copyToClipboard.subscribe(copyToClipboard)

    function copyToClipboard(string) {
        var textarea = document.createElement('textarea')
        textarea.style.cssText = 'position: absolute; left: -99999em';
        document.body.appendChild(textarea)
        textarea.value = string
        textarea.select()
        document.execCommand('copy')
        textarea.remove()
    }
}
