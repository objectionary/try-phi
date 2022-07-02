function pollDOM() {
  if (typeof app !== 'undefined') {
    return app.view.state.doc.toString()
  }
  else {
    let p = 
`[
  price -> 2,
  book -> [
    title -> ^2.title,
    price -> ?
  ],
  manga -> [
    manga_title -> ^2.str_publisher.add(str -> ^0.title),
    @ -> ^1.book
  ](price -> ^0.price),
  title -> ^0.manga.manga_title
].manga.price`;
    return p
  }
}

pollDOM()

// var _TIMEOUT = 50; // waitfor test rate [msec]
// waitfor(() => (typeof app !== 'undefined'), true, _TIMEOUT, 0, 'play->busy false', function() {
//   return app.view.state.doc.toString()
// })

// (async() => {
//   console.log("waiting for variable");
//   while(!window.hasOwnProperty("app")) // define the condition as you like
//       await new Promise(resolve => setTimeout(resolve, 100));
//   return app.view.state.doc.toString()
// })();
// console.log("above code doesn't block main function stack");