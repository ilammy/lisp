function addClass(element, className)
{
    element.className = element.className + ' ' + className
}

function removeClass(element, className)
{
    var r = new RegExp('\\s+\\b' + className + '\\b', 'g')

    element.className = element.className.replace(r, '')
}

function findInParents(element, nodeName, nodeClass)
{
    while (element && (element.nodeName != nodeName || element.className != nodeClass))
    {
        element = element.parentNode
    }

    return element
}

function findInNextSiblings(element, nodeName, nodeClass)
{
    while (element && (element.nodeName != nodeName || element.className != nodeClass))
    {
        element = element.nextSibling
    }

    return element
}

function findInChildren(element, nodeName, nodeClass)
{
    if (element)
    {
        if (element.nodeName == nodeName && element.className == nodeClass)
        {
            return element
        }

        for (var i = 0; i < element.children.length; i++)
        {
            return findInChildren(element.children[i], nodeName, nodeClass)
        }
    }

    return null
}

function addEventHandler(element, event, thunk)
{
    if (element.addEventListener) element.addEventListener(event, thunk, false)
    else if (element.attachEvent) element.attachEvent('on' + event, thunk)
    else                          element['on' + event] = thunk
}

function showFootnote(event)
{
    event = event || window.event
    event.preventDefault()

    var footref, footwrap

    footref = findInParents(event.target, 'A', 'footref')
    footwrap = findInNextSiblings(footref, 'DIV', 'foot-wrap')

    addClass(footwrap, 'visible')
}

function hideFootnote(event)
{
    event = event || window.event
    event.preventDefault()

    var footwrap = findInParents(event.target, 'DIV', 'foot-wrap visible')

    removeClass(footwrap, 'visible')
}

function setupFootnotes()
{
    var footrefs = document.getElementsByClassName('footref')

    for (var i = 0; i < footrefs.length; i++)
    {
        if (footrefs[i].nodeName != 'A') continue

        addEventHandler(footrefs[i], 'click', showFootnote)

        var footwrap = findInNextSiblings(footrefs[i], 'DIV', 'foot-wrap'),
            footcontent = findInChildren(footwrap, 'DIV', 'foot-content')

        var hideButton = document.createElement('a')
        hideButton.className = 'close'
        hideButton.href = '#'
        hideButton.textContent = '✕'
        addEventHandler(hideButton, 'click', hideFootnote)

        footcontent.insertBefore(hideButton, footcontent.firstChild)
    }
}

if (document.addEventListener)
{
    document.addEventListener("DOMContentLoaded", setupFootnotes, false)
}
else if (document.attachEvent)
{
    document.attachEvent("onreadystatechange", setupFootnotes)
}
else
{
    document.onload = setupFootnotes
}
