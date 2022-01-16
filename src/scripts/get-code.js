function pollDOM() {
  if (typeof app !== 'undefined') {
    return app.view.state.doc.toString()
  }
}

pollDOM()