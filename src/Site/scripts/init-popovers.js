var popoverList

function waitForElem(selector) {
  return new Promise((resolve) => {
    if (document.getElementById(selector)) {
      return resolve(document.getElementById(selector))
    }

    const observer = new MutationObserver((mutations) => {
      if (document.getElementById(selector)) {
        resolve(document.getElementById(selector))
        observer.disconnect()
      }
    })

    observer.observe(document.body, {
      childList: true,
      subtree: true,
    })
  })
}

waitForElem('phi-editor').then((elem) => {
  let popoverTriggerList = [].slice.call(
    document.querySelectorAll('[data-bs-toggle="popover"]')
  )

  popoverList = popoverTriggerList.map(function (popoverTriggerEl) {
    return new bootstrap.Popover(popoverTriggerEl, {
      container: 'body',
      html: true,
    })
  })
  // console.log('Element is ready');
  // console.log(elem.textContent);
})
