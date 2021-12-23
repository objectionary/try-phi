import fs from 'fs'
import {NodeProp} from 'lezer'
import {parser} from './src/parser'

export class NodeInfo {
  constructor(type, start, end) {
    this.children = []
    if (type == null) {
      return
    }
    const { name, id } = type
    const error = type.prop(NodeProp.error)

    Object.assign(this, {
      name,
      id,
      start,
      end,
      error,
    })
  }

  repr() {
    if (!('name' in this)) return `[Node <Unknown>]`
    return `<${this.name}: ${this.start}~${this.end}>`
  }
}


function parse(src) {
  const tree = parser.parse(src)
  let hasError = false

  const stack = [new NodeInfo()]

  tree.iterate({
    // from: 0,
    // to: tree.length,
    enter(...args) {
      const node = new NodeInfo(...args)
      stack.push(node)
      if (node.error) {
        hasError = true
      }
      // console.log('enter', nodeTypeRepr(node.type), start, end)
    },
    leave() {
      const node = stack.pop()
      const parent = stack[stack.length - 1]
      parent.children.push(node)
      // console.log('leave', nodeTypeRepr(node.type), start, end)
    },
  })

  function getExpr(node, level=0) {
    const arr = []
    let excerpt = src.substring(node.start, node.end).replace(/\n/g, '↵')

    if (excerpt.length > 100) {
      excerpt = excerpt.slice(0, 30) + ' ... ' + excerpt.substr(excerpt.length - 30)
    }

    const repr = `${node.repr()}: "${excerpt}"`
    const indent = ' '.repeat(level * 2)

    if (node.error) {
      // id == 0 can be treated as an error?
      arr.push(indent + '\x1b[31m! [ERROR] ' + repr + '\x1b[0m')
    } else {
      arr.push(indent + '* ' + repr)
    }

    // is it terminal?

    node.children.forEach(child => {
      getExpr(child, level + 1).forEach(token => {
          arr.push(token)
      })
    })
    return arr
  }

  const treeRoot = stack[0].children[0]
  getExpr(treeRoot).forEach(ln => console.log(ln))

  // returns if the syntax is "correct"
  return !hasError
}

// parse('(8 + 9) * 17 + 20 * 20 - ')

function mainLoop() {
  const inp = fs.readFileSync('/dev/stdin', 'utf-8')
  parse(inp)
}

mainLoop()
