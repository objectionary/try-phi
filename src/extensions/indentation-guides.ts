import { EditorView, WidgetType, Decoration } from '@codemirror/view';
import { StateField, StateEffect, Transaction } from '@codemirror/state';
const characterWidthEffect = StateEffect.define({});
export const extraCycleCharacterWidth = StateField.define({
  create() {
    return null;
  },
  update(value, tr) {
    for (const effect of tr.effects) {
      if (effect.is(characterWidthEffect)) return effect.value;
    }
    return value;
  },
});

export const characterWidthListener = EditorView.updateListener.of(
  (viewupdate) => {
    const width = viewupdate.view.defaultCharacterWidth;
    let currentWidth = viewupdate.view.state.field(
      extraCycleCharacterWidth,
      false
    ) as any;
    if (currentWidth !== width) {
      currentWidth = width;
      viewupdate.view.dispatch({
        effects: [characterWidthEffect.of(currentWidth)],
      });
    }
  }
);

class CheckboxWidget extends WidgetType {
  left?: number;
  className?: string;
  constructor(className?: string, left?: number) {
    super();
    this.left = left;
    this.className = className;
  }
  toDOM() {
    const wrap = document.createElement('span');
    wrap.className = this.className || 'cm-tab';
    wrap.style.position = 'absolute';
    wrap.innerText = '    ';
    if (this.left !== undefined) {
      wrap.style.left = this.left + 'px';
    }
    return wrap;
  }
  ignoreEvent() {
    return false;
  }
}

let beforeCharWidth: number;
let beforeActiveIndentStartLineNumber: number;
let beforeActiveIndentEndLineNumber: number;

const indentGuidesDecoration = StateField.define({
  create() {
    return Decoration.none;
  },
  update(deco, view) {
    const tabSize = view.state.tabSize;
    const charWidth = view.state.field(
      extraCycleCharacterWidth,
      false
    ) as number;
    let activeIndentLevel = 0;
    let activeIndentStartLineNumber = 0;
    let activeIndentEndLineNumber = 0;
    if (charWidth == null) return Decoration.none;
    const primaryLineNumber = view.state.doc.lineAt(
      view.state.selection.ranges[0].from
    ).number;
    const activeIndentInfo = getActiveIndentGuide(
      primaryLineNumber,
      1,
      view.state.doc.lines,
      view
    );
    activeIndentStartLineNumber = activeIndentInfo.startLineNumber - 1; // 高亮行开始行
    activeIndentEndLineNumber = activeIndentInfo.endLineNumber - 1; // 高亮tab结束行
    activeIndentLevel = activeIndentInfo.indent;
    if (
      !view.docChanged &&
      deco !== Decoration.none &&
      beforeCharWidth === charWidth && // NOTE: 字体大小变化需要重新计算位置
      beforeActiveIndentStartLineNumber === activeIndentStartLineNumber &&
      beforeActiveIndentEndLineNumber === activeIndentEndLineNumber
    )
      return deco;
    const indents = getLinesIndentGuides(1, view.state.doc.lines, view);
    const decorations = [];
    beforeCharWidth = charWidth;

    for (let i = 0; i < view.state.doc.lines; i++) {
      const line = view.state.doc.line(i + 1);
      const containsActiveIndentGuide =
        activeIndentStartLineNumber <= i && i <= activeIndentEndLineNumber;
      for (let k = 0; k < indents[i]; k++) {
        decorations.push(
          Decoration.widget({
            widget: new CheckboxWidget(
              k + 1 === activeIndentLevel &&
              containsActiveIndentGuide &&
              activeIndentLevel !== 0
                ? 'cm-tab-active'
                : 'cm-tab',
              k * (charWidth * tabSize) + 4
            ),
            side: 0,
          }).range(line.from)
        );
      }
    }
    return Decoration.set(decorations, true);
  },
  provide: (f) => EditorView.decorations.from(f),
});

function getLinesIndentGuides(
  startLineNumber: number,
  endLineNumber: number,
  view: Transaction
) {
  const lineCount = view.state.doc.lines;
  const tabSize = view.state.tabSize;
  if (startLineNumber > lineCount || endLineNumber > lineCount) {
    throw new Error('Illegal value for startLineNumber');
  }
  const result = new Array(endLineNumber - startLineNumber + 1);
  let aboveContentLineIndex =
    -2; /* -2 is a marker for not having computed it */
  let aboveContentLineIndent = -1;
  let belowContentLineIndex =
    -2; /* -2 is a marker for not having computed it */
  let belowContentLineIndent = -1;
  for (
    let lineNumber = startLineNumber;
    lineNumber <= endLineNumber;
    lineNumber++
  ) {
    const resultIndex = lineNumber - startLineNumber;
    const currentIndent = computeIndentLevel(
      view.state.doc.line(lineNumber).text,
      view.state.tabSize
    );
    if (currentIndent >= 0) {
      // This line has content (besides whitespace)
      // Use the line's indent
      aboveContentLineIndex = lineNumber - 1;
      aboveContentLineIndent = currentIndent;
      result[resultIndex] = Math.ceil(currentIndent / tabSize);
      continue;
    }
    if (aboveContentLineIndex === -2) {
      aboveContentLineIndex = -1;
      aboveContentLineIndent = -1;
      // must find previous line with content
      for (let lineIndex = lineNumber - 2; lineIndex >= 0; lineIndex--) {
        const indent = computeIndentLevel(
          view.state.doc.line(lineIndex + 1).text,
          view.state.tabSize
        );
        if (indent >= 0) {
          aboveContentLineIndex = lineIndex;
          aboveContentLineIndent = indent;
          break;
        }
      }
    }
    if (
      belowContentLineIndex !== -1 &&
      (belowContentLineIndex === -2 || belowContentLineIndex < lineNumber - 1)
    ) {
      belowContentLineIndex = -1;
      belowContentLineIndent = -1;
      // must find next line with content
      for (let lineIndex = lineNumber; lineIndex < lineCount; lineIndex++) {
        const indent = computeIndentLevel(
          view.state.doc.line(lineIndex + 1).text,
          view.state.tabSize
        );
        if (indent >= 0) {
          belowContentLineIndex = lineIndex;
          belowContentLineIndent = indent;
          break;
        }
      }
    }
    result[resultIndex] = getIndentLevelForWhitespaceLine(
      true,
      aboveContentLineIndent,
      belowContentLineIndent,
      view.state.tabSize
    );
  }
  return result;
}

function computeIndentLevel(line: string, tabSize: number) {
  let indent = 0;
  let i = 0;
  const len = line.length;
  while (i < len) {
    const chCode = line.charCodeAt(i);
    if (chCode === 32 /* Space */) {
      indent++;
    } else if (chCode === 9 /* Tab */) {
      indent = indent - (indent % tabSize) + tabSize;
    } else {
      break;
    }
    i++;
  }
  if (i === len) {
    return -1; // line only consists of whitespace
  }
  return indent;
}

function getIndentLevelForWhitespaceLine(
  offSide: boolean,
  aboveContentLineIndent: number,
  belowContentLineIndent: number,
  tabSize: number
) {
  if (aboveContentLineIndent === -1 || belowContentLineIndent === -1) {
    // At the top or bottom of the file
    return 0;
  } else if (aboveContentLineIndent < belowContentLineIndent) {
    // we are inside the region above
    return 1 + Math.floor(aboveContentLineIndent / tabSize);
  } else if (aboveContentLineIndent === belowContentLineIndent) {
    // we are in between two regions
    return Math.ceil(belowContentLineIndent / tabSize);
  } else {
    if (offSide) {
      // same level as region below
      return Math.ceil(belowContentLineIndent / tabSize);
    } else {
      // we are inside the region that ends below
      return 1 + Math.floor(belowContentLineIndent / tabSize);
    }
  }
}

function getActiveIndentGuide(
  lineNumber: number,
  minLineNumber: number,
  maxLineNumber: number,
  view: Transaction
) {
  const lineCount = view.state.doc.lines;
  const tabSize = view.state.tabSize;
  if (lineNumber > lineCount) {
    throw new Error('Illegal value for lineNumber');
  }
  const offSide = true;
  let upAboveContentLineIndex =
    -2; /* -2 is a marker for not having computed it */
  let upAboveContentLineIndent = -1;
  let upBelowContentLineIndex =
    -2; /* -2 is a marker for not having computed it */
  let upBelowContentLineIndent = -1;
  const upResolveIndents = function (lineNumber1: number) {
    if (
      upAboveContentLineIndex !== -1 &&
      (upAboveContentLineIndex === -2 ||
        upAboveContentLineIndex > lineNumber1 - 1)
    ) {
      upAboveContentLineIndex = -1;
      upAboveContentLineIndent = -1;
      // must find previous line with content
      for (let lineIndex = lineNumber1 - 2; lineIndex >= 0; lineIndex--) {
        const indent1 = computeIndentLevel(
          view.state.doc.line(lineIndex + 1).text,
          tabSize
        );
        if (indent1 >= 0) {
          upAboveContentLineIndex = lineIndex;
          upAboveContentLineIndent = indent1;
          break;
        }
      }
    }
    if (upBelowContentLineIndex === -2) {
      upBelowContentLineIndex = -1;
      upBelowContentLineIndent = -1;
      // must find next line with content
      for (let lineIndex = lineNumber1; lineIndex < lineCount; lineIndex++) {
        const indent2 = computeIndentLevel(
          view.state.doc.line(lineIndex + 1).text,
          tabSize
        );
        if (indent2 >= 0) {
          upBelowContentLineIndex = lineIndex;
          upBelowContentLineIndent = indent2;
          break;
        }
      }
    }
  };
  let downAboveContentLineIndex =
    -2; /* -2 is a marker for not having computed it */
  let downAboveContentLineIndent = -1;
  let downBelowContentLineIndex =
    -2; /* -2 is a marker for not having computed it */
  let downBelowContentLineIndent = -1;
  const downResolveIndents = function (lineNumber2: number) {
    if (downAboveContentLineIndex === -2) {
      downAboveContentLineIndex = -1;
      downAboveContentLineIndent = -1;
      // must find previous line with content
      for (let lineIndex = lineNumber2 - 2; lineIndex >= 0; lineIndex--) {
        const indent3 = computeIndentLevel(
          view.state.doc.line(lineIndex + 1).text,
          tabSize
        );
        if (indent3 >= 0) {
          downAboveContentLineIndex = lineIndex;
          downAboveContentLineIndent = indent3;
          break;
        }
      }
    }
    if (
      downBelowContentLineIndex !== -1 &&
      (downBelowContentLineIndex === -2 ||
        downBelowContentLineIndex < lineNumber2 - 1)
    ) {
      downBelowContentLineIndex = -1;
      downBelowContentLineIndent = -1;
      // must find next line with content
      for (let lineIndex = lineNumber2; lineIndex < lineCount; lineIndex++) {
        const indent4 = computeIndentLevel(
          view.state.doc.line(lineIndex + 1).text,
          tabSize
        );
        if (indent4 >= 0) {
          downBelowContentLineIndex = lineIndex;
          downBelowContentLineIndent = indent4;
          break;
        }
      }
    }
  };
  let startLineNumber = 0;
  let goUp = true;
  let endLineNumber = 0;
  let goDown = true;
  let indent = 0;
  for (let distance = 0; goUp || goDown; distance++) {
    const upLineNumber = lineNumber - distance;
    const downLineNumber = lineNumber + distance;
    if (distance !== 0 && (upLineNumber < 1 || upLineNumber < minLineNumber)) {
      goUp = false;
    }
    if (
      distance !== 0 &&
      (downLineNumber > lineCount || downLineNumber > maxLineNumber)
    ) {
      goDown = false;
    }
    if (distance > 50000) {
      // stop processing
      goUp = false;
      goDown = false;
    }
    if (goUp) {
      // compute indent level going up
      let upLineIndentLevel = void 0 as any;
      const currentIndent = computeIndentLevel(
        view.state.doc.line(upLineNumber).text,
        tabSize
      );
      if (currentIndent >= 0) {
        // This line has content (besides whitespace)
        // Use the line's indent
        upBelowContentLineIndex = upLineNumber - 1;
        upBelowContentLineIndent = currentIndent;
        upLineIndentLevel = Math.ceil(currentIndent / tabSize);
      } else {
        upResolveIndents(upLineNumber);
        upLineIndentLevel = getIndentLevelForWhitespaceLine(
          offSide,
          upAboveContentLineIndent,
          upBelowContentLineIndent,
          tabSize
        );
      }
      if (distance === 0) {
        // This is the initial line number
        startLineNumber = upLineNumber;
        endLineNumber = downLineNumber;
        indent = upLineIndentLevel;
        if (indent === 0) {
          // No need to continue
          return {
            startLineNumber: startLineNumber,
            endLineNumber: endLineNumber,
            indent: indent,
          };
        }
        continue;
      }
      if (upLineIndentLevel >= indent) {
        startLineNumber = upLineNumber;
      } else {
        goUp = false;
      }
    }
    if (goDown) {
      // compute indent level going down
      let downLineIndentLevel = void 0 as any;
      const currentIndent = computeIndentLevel(
        view.state.doc.line(downLineNumber).text,
        tabSize
      );
      if (currentIndent >= 0) {
        // This line has content (besides whitespace)
        // Use the line's indent
        downAboveContentLineIndex = downLineNumber - 1;
        downAboveContentLineIndent = currentIndent;
        downLineIndentLevel = Math.ceil(currentIndent / tabSize);
      } else {
        downResolveIndents(downLineNumber);
        downLineIndentLevel = getIndentLevelForWhitespaceLine(
          offSide,
          downAboveContentLineIndent,
          downBelowContentLineIndent,
          tabSize
        );
      }
      if (downLineIndentLevel >= indent) {
        endLineNumber = downLineNumber;
      } else {
        goDown = false;
      }
    }
  }
  return {
    startLineNumber: startLineNumber,
    endLineNumber: endLineNumber,
    indent: indent,
  };
}

export const indentGuidesPlugin = [
  indentGuidesDecoration,
  extraCycleCharacterWidth,
  characterWidthListener,
];
