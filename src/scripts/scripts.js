let view = app.view;
const params = new URLSearchParams(window.location.search);

// use default snippet if one is not set in the query parameters 
snippet =
      `[
  price -> ^1.int_2,
  book -> [
    title -> ^2.title,
    price -> ?
  ],
  manga -> [
    manga_title -> ^2.str_publisher.add(str -> ^0.title),
    @ -> ^1.book
  ](price -> ^0.price),
  title -> ^0.manga.manga_title
].manga.price`

if (params.has("snippet")) {
    snippet = decodeURIComponent(params.get("snippet"));
};

// change editor content
view.dispatch({
    changes: { from: 0, to: view.state.doc.length, insert: snippet }
});

var popoverList = []

let target = document.getElementById('ghcjs');

let config = {
    childList: true,
    subtree: true
};

const callback = function (mutationsList, observer) {
    let t = document.getElementById("app_div");
    if (t) {
    let popoverTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle="popover"]'))
    popoverList = popoverTriggerList.map(function (popoverTriggerEl) {
        return new bootstrap.Popover(popoverTriggerEl, { container: 'body', html: true })
    });
    observer.disconnect();
    return;
    }
};

const app_observer = new MutationObserver(callback);

app_observer.observe(target, config);