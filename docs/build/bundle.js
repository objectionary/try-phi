(function () {
    'use strict';

    // Compressed representation of the Grapheme_Cluster_Break=Extend
    // information from
    // http://www.unicode.org/Public/13.0.0/ucd/auxiliary/GraphemeBreakProperty.txt.
    // Each pair of elements represents a range, as an offet from the
    // previous range and a length. Numbers are in base-36, with the empty
    // string being a shorthand for 1.
    let extend = "lc,34,7n,7,7b,19,,,,2,,2,,,20,b,1c,l,g,,2t,7,2,6,2,2,,4,z,,u,r,2j,b,1m,9,9,,o,4,,9,,3,,5,17,3,3b,f,,w,1j,,,,4,8,4,,3,7,a,2,t,,1m,,,,2,4,8,,9,,a,2,q,,2,2,1l,,4,2,4,2,2,3,3,,u,2,3,,b,2,1l,,4,5,,2,4,,k,2,m,6,,,1m,,,2,,4,8,,7,3,a,2,u,,1n,,,,c,,9,,14,,3,,1l,3,5,3,,4,7,2,b,2,t,,1m,,2,,2,,3,,5,2,7,2,b,2,s,2,1l,2,,,2,4,8,,9,,a,2,t,,20,,4,,2,3,,,8,,29,,2,7,c,8,2q,,2,9,b,6,22,2,r,,,,,,1j,e,,5,,2,5,b,,10,9,,2u,4,,6,,2,2,2,p,2,4,3,g,4,d,,2,2,6,,f,,jj,3,qa,3,t,3,t,2,u,2,1s,2,,7,8,,2,b,9,,19,3,3b,2,y,,3a,3,4,2,9,,6,3,63,2,2,,1m,,,7,,,,,2,8,6,a,2,,1c,h,1r,4,1c,7,,,5,,14,9,c,2,w,4,2,2,,3,1k,,,2,3,,,3,1m,8,2,2,48,3,,d,,7,4,,6,,3,2,5i,1m,,5,ek,,5f,x,2da,3,3x,,2o,w,fe,6,2x,2,n9w,4,,a,w,2,28,2,7k,,3,,4,,p,2,5,,47,2,q,i,d,,12,8,p,b,1a,3,1c,,2,4,2,2,13,,1v,6,2,2,2,2,c,,8,,1b,,1f,,,3,2,2,5,2,,,16,2,8,,6m,,2,,4,,fn4,,kh,g,g,g,a6,2,gt,,6a,,45,5,1ae,3,,2,5,4,14,3,4,,4l,2,fx,4,ar,2,49,b,4w,,1i,f,1k,3,1d,4,2,2,1x,3,10,5,,8,1q,,c,2,1g,9,a,4,2,,2n,3,2,,,2,6,,4g,,3,8,l,2,1l,2,,,,,m,,e,7,3,5,5f,8,2,3,,,n,,29,,2,6,,,2,,,2,,2,6j,,2,4,6,2,,2,r,2,2d,8,2,,,2,2y,,,,2,6,,,2t,3,2,4,,5,77,9,,2,6t,,a,2,,,4,,40,4,2,2,4,,w,a,14,6,2,4,8,,9,6,2,3,1a,d,,2,ba,7,,6,,,2a,m,2,7,,2,,2,3e,6,3,,,2,,7,,,20,2,3,,,,9n,2,f0b,5,1n,7,t4,,1r,4,29,,f5k,2,43q,,,3,4,5,8,8,2,7,u,4,44,3,1iz,1j,4,1e,8,,e,,m,5,,f,11s,7,,h,2,7,,2,,5,79,7,c5,4,15s,7,31,7,240,5,gx7k,2o,3k,6o".split(",").map(s => s ? parseInt(s, 36) : 1);
    // Convert offsets into absolute values
    for (let i = 1; i < extend.length; i++)
        extend[i] += extend[i - 1];
    function isExtendingChar(code) {
        for (let i = 1; i < extend.length; i += 2)
            if (extend[i] > code)
                return extend[i - 1] <= code;
        return false;
    }
    function isRegionalIndicator(code) {
        return code >= 0x1F1E6 && code <= 0x1F1FF;
    }
    const ZWJ = 0x200d;
    /// Returns a grapheme cluster end _after_ (not equal to) `pos`, if
    /// possible. Moves across surrogate pairs, extending characters,
    /// characters joined with zero-width joiners, and flag emoji.
    function nextClusterBreak(str, pos) {
        if (pos == str.length)
            return pos;
        // If pos is in the middle of a surrogate pair, move to its start
        if (pos && surrogateLow(str.charCodeAt(pos)) && surrogateHigh(str.charCodeAt(pos - 1)))
            pos--;
        let prev = codePointAt(str, pos);
        pos += codePointSize(prev);
        while (pos < str.length) {
            let next = codePointAt(str, pos);
            if (prev == ZWJ || next == ZWJ || isExtendingChar(next)) {
                pos += codePointSize(next);
                prev = next;
            }
            else if (isRegionalIndicator(next)) {
                let countBefore = 0, i = pos - 2;
                while (i >= 0 && isRegionalIndicator(codePointAt(str, i))) {
                    countBefore++;
                    i -= 2;
                }
                if (countBefore % 2 == 0)
                    break;
                else
                    pos += 2;
            }
            else {
                break;
            }
        }
        return pos;
    }
    /// Returns a grapheme cluster end _before_ `pos`, if possible.
    function prevClusterBreak(str, pos) {
        while (pos > 0) {
            let found = nextClusterBreak(str, pos - 2);
            if (found < pos)
                return found;
            pos--;
        }
        return 0;
    }
    function surrogateLow(ch) { return ch >= 0xDC00 && ch < 0xE000; }
    function surrogateHigh(ch) { return ch >= 0xD800 && ch < 0xDC00; }
    /// Find the code point at the given position in a string (as in the
    /// [`codePointAt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/codePointAt)
    /// string method).
    function codePointAt(str, pos) {
        let code0 = str.charCodeAt(pos);
        if (!surrogateHigh(code0) || pos + 1 == str.length)
            return code0;
        let code1 = str.charCodeAt(pos + 1);
        if (!surrogateLow(code1))
            return code0;
        return ((code0 - 0xd800) << 10) + (code1 - 0xdc00) + 0x10000;
    }
    /// Given a Unicode codepoint, return the JavaScript string that
    /// respresents it (as in
    /// [`String.fromCodePoint`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/fromCodePoint).
    function fromCodePoint(code) {
        if (code <= 0xffff)
            return String.fromCharCode(code);
        code -= 0x10000;
        return String.fromCharCode((code >> 10) + 0xd800, (code & 1023) + 0xdc00);
    }
    /// The first character that takes up two positions in a JavaScript
    /// string. It is often useful to compare with this after calling
    /// `codePointAt`, to figure out whether your character takes up 1 or
    /// 2 index positions.
    function codePointSize(code) { return code < 0x10000 ? 1 : 2; }

    /// Count the column position at the given offset into the string,
    /// taking extending characters and tab size into account.
    function countColumn(string, n, tabSize) {
        for (let i = 0; i < string.length;) {
            if (string.charCodeAt(i) == 9) {
                n += tabSize - (n % tabSize);
                i++;
            }
            else {
                n++;
                i = nextClusterBreak(string, i);
            }
        }
        return n;
    }
    /// Find the offset that corresponds to the given column position in a
    /// string, taking extending characters and tab size into account.
    function findColumn(string, n, col, tabSize) {
        for (let i = 0; i < string.length;) {
            if (n >= col)
                return { offset: i, leftOver: 0 };
            n += string.charCodeAt(i) == 9 ? tabSize - (n % tabSize) : 1;
            i = nextClusterBreak(string, i);
        }
        return { offset: string.length, leftOver: col - n };
    }

    /// The document tree type.
    class Text {
        /// @internal
        constructor() { }
        /// Get the line description around the given position.
        lineAt(pos) {
            if (pos < 0 || pos > this.length)
                throw new RangeError(`Invalid position ${pos} in document of length ${this.length}`);
            for (let line of lineCache) {
                if (line.doc == this && line.from <= pos && line.to >= pos)
                    return line;
            }
            return cacheLine(this.lineInner(pos, false, 1, 0).finish(this));
        }
        /// Get the description for the given (1-based) line number.
        line(n) {
            if (n < 1 || n > this.lines)
                throw new RangeError(`Invalid line number ${n} in ${this.lines}-line document`);
            for (let line of lineCache) {
                if (line.doc == this && line.number == n)
                    return line;
            }
            return cacheLine(this.lineInner(n, true, 1, 0).finish(this));
        }
        /// Replace a range of the text with the given lines. `text` should
        /// have a length of at least one.
        replace(from, to, text) {
            let parts = [];
            this.decompose(0, from, parts);
            parts.push(text);
            this.decompose(to, this.length, parts);
            return TextNode.from(parts, this.length - (to - from) + text.length);
        }
        /// Append another document to this one.
        append(text) {
            return this.length == 0 ? text : text.length == 0 ? this : TextNode.from([this, text], this.length + text.length);
        }
        /// Retrieve the text between the given points.
        slice(from, to = this.length) {
            let parts = [];
            this.decompose(from, to, parts);
            return TextNode.from(parts, to - from);
        }
        /// Test whether this text is equal to another instance.
        eq(other) { return this == other || eqContent(this, other); }
        /// Iterate over the text. When `dir` is `-1`, iteration happens
        /// from end to start. This will return lines and the breaks between
        /// them as separate strings, and for long lines, might split lines
        /// themselves into multiple chunks as well.
        iter(dir = 1) { return new RawTextCursor(this, dir); }
        /// Iterate over a range of the text. When `from` > `to`, the
        /// iterator will run in reverse.
        iterRange(from, to = this.length) { return new PartialTextCursor(this, from, to); }
        /// Iterate over lines in the text, starting at position (_not_ line
        /// number) `from`. An iterator returned by this combines all text
        /// on a line into a single string (which may be expensive for very
        /// long lines), and skips line breaks (its
        /// [`lineBreak`](#text.TextIterator.lineBreak) property is always
        /// false).
        iterLines(from = 0) { return new LineCursor(this, from); }
        /// @internal
        toString() { return this.sliceString(0); }
        /// Convert the document to an array of lines (which can be
        /// deserialized again via [`Text.of`](#text.Text^of).
        toJSON() {
            let lines = [];
            for (let iter = this.iterLines(); !iter.next().done;)
                lines.push(iter.value);
            return lines;
        }
        /// Create a `Text` instance for the given array of lines.
        static of(text) {
            if (text.length == 0)
                throw new RangeError("A document must have at least one line");
            if (text.length == 1 && !text[0] && Text.empty)
                return Text.empty;
            let length = textLength(text);
            return length < 1024 /* MaxLeaf */ ? new TextLeaf(text, length) : TextNode.from(TextLeaf.split(text, []), length);
        }
    }
    if (typeof Symbol != "undefined")
        Text.prototype[Symbol.iterator] = function () { return this.iter(); };
    let lineCache = [], lineCachePos = -1, lineCacheSize = 10;
    function cacheLine(line) {
        return lineCache[lineCachePos = (lineCachePos + 1) % lineCacheSize] = line;
    }
    // Leaves store an array of strings. There are always line breaks
    // between these strings (though not between adjacent Text nodes).
    // These are limited in length, so that bigger documents are
    // constructed as a tree structure. Long lines will be broken into a
    // number of single-line leaves.
    class TextLeaf extends Text {
        constructor(text, length = textLength(text)) {
            super();
            this.text = text;
            this.length = length;
        }
        get lines() { return this.text.length; }
        get children() { return null; }
        lineInner(target, isLine, line, offset) {
            for (let i = 0;; i++) {
                let string = this.text[i], end = offset + string.length;
                if ((isLine ? line : end) >= target)
                    return new Line(offset, end, line, string);
                offset = end + 1;
                line++;
            }
        }
        decompose(from, to, target) {
            target.push(new TextLeaf(sliceText(this.text, from, to), Math.min(to, this.length) - Math.max(0, from)));
        }
        lastLineLength() { return this.text[this.text.length - 1].length; }
        firstLineLength() { return this.text[0].length; }
        replace(from, to, text) {
            let newLen = this.length + text.length - (to - from);
            if (newLen >= 1024 /* MaxLeaf */ || !(text instanceof TextLeaf))
                return super.replace(from, to, text);
            return new TextLeaf(appendText(this.text, appendText(text.text, sliceText(this.text, 0, from)), to), newLen);
        }
        sliceString(from, to = this.length, lineSep = "\n") {
            let result = "";
            for (let pos = 0, i = 0; pos <= to && i < this.text.length; i++) {
                let line = this.text[i], end = pos + line.length;
                if (pos > from && i)
                    result += lineSep;
                if (from < end && to > pos)
                    result += line.slice(Math.max(0, from - pos), to - pos);
                pos = end + 1;
            }
            return result;
        }
        flatten(target) {
            target[target.length - 1] += this.text[0];
            for (let i = 1; i < this.text.length; i++)
                target.push(this.text[i]);
        }
        static split(text, target) {
            let part = [], length = -1;
            for (let line of text) {
                for (;;) {
                    let newLength = length + line.length + 1;
                    if (newLength < 512 /* BaseLeaf */) {
                        length = newLength;
                        part.push(line);
                        break;
                    }
                    let cut = 512 /* BaseLeaf */ - length - 1, after = line.charCodeAt(cut);
                    if (after >= 0xdc00 && after < 0xe000)
                        cut++;
                    part.push(line.slice(0, cut));
                    target.push(new TextLeaf(part, 512 /* BaseLeaf */));
                    line = line.slice(cut);
                    length = -1;
                    part = [];
                }
            }
            if (length != -1)
                target.push(new TextLeaf(part, length));
            return target;
        }
    }
    // Nodes provide the tree structure of the `Text` type. They store a
    // number of other nodes or leaves, taking care to balance itself on
    // changes.
    class TextNode extends Text {
        constructor(children, length) {
            super();
            this.children = children;
            this.length = length;
            this.lines = 1;
            for (let child of children)
                this.lines += child.lines - 1;
        }
        lineInner(target, isLine, line, offset) {
            for (let i = 0;; i++) {
                let child = this.children[i], end = offset + child.length, endLine = line + child.lines - 1;
                if ((isLine ? endLine : end) >= target) {
                    let inner = child.lineInner(target, isLine, line, offset), add;
                    if (inner.from == offset && (add = this.lineLengthTo(i))) {
                        inner.from -= add;
                        inner.content = null;
                    }
                    if (inner.to == end && (add = this.lineLengthFrom(i + 1))) {
                        inner.to += add;
                        inner.content = null;
                    }
                    return inner;
                }
                offset = end;
                line = endLine;
            }
        }
        decompose(from, to, target) {
            for (let i = 0, pos = 0; pos < to && i < this.children.length; i++) {
                let child = this.children[i], end = pos + child.length;
                if (from < end && to > pos) {
                    if (pos >= from && end <= to)
                        target.push(child);
                    else
                        child.decompose(from - pos, to - pos, target);
                }
                pos = end;
            }
        }
        lineLengthTo(to) {
            let length = 0;
            for (let i = to - 1; i >= 0; i--) {
                let child = this.children[i];
                if (child.lines > 1)
                    return length + child.lastLineLength();
                length += child.length;
            }
            return length;
        }
        lastLineLength() { return this.lineLengthTo(this.children.length); }
        lineLengthFrom(from) {
            let length = 0;
            for (let i = from; i < this.children.length; i++) {
                let child = this.children[i];
                if (child.lines > 1)
                    return length + child.firstLineLength();
                length += child.length;
            }
            return length;
        }
        firstLineLength() { return this.lineLengthFrom(0); }
        replace(from, to, text) {
            // Looks like a small change, try to optimize
            if (text.length < 512 /* BaseLeaf */ && to - from < 512 /* BaseLeaf */) {
                let lengthDiff = text.length - (to - from);
                for (let i = 0, pos = 0; i < this.children.length; i++) {
                    let child = this.children[i], end = pos + child.length;
                    // Fast path: if the change only affects one child and the
                    // child's size remains in the acceptable range, only update
                    // that child
                    if (from >= pos && to <= end &&
                        child.length + lengthDiff < (this.length + lengthDiff) >> (3 /* BranchShift */ - 1) &&
                        child.length + lengthDiff > 0) {
                        let copy = this.children.slice();
                        copy[i] = child.replace(from - pos, to - pos, text);
                        return new TextNode(copy, this.length + lengthDiff);
                    }
                    pos = end;
                }
            }
            return super.replace(from, to, text);
        }
        sliceString(from, to = this.length, lineSep = "\n") {
            let result = "";
            for (let i = 0, pos = 0; pos < to && i < this.children.length; i++) {
                let child = this.children[i], end = pos + child.length;
                if (from < end && to > pos) {
                    let part = child.sliceString(from - pos, to - pos, lineSep);
                    if (from >= pos && to <= end)
                        return part;
                    result += part;
                }
                pos = end;
            }
            return result;
        }
        flatten(target) {
            for (let child of this.children)
                child.flatten(target);
        }
        static from(children, length) {
            if (!children.every(ch => ch instanceof Text))
                throw new Error("NOP");
            if (length < 1024 /* MaxLeaf */) {
                let text = [""];
                for (let child of children)
                    child.flatten(text);
                return new TextLeaf(text, length);
            }
            let chunkLength = Math.max(512 /* BaseLeaf */, length >> 3 /* BranchShift */), maxLength = chunkLength << 1, minLength = chunkLength >> 1;
            let chunked = [], currentLength = 0, currentChunk = [];
            function add(child) {
                let childLength = child.length, last;
                if (!childLength)
                    return;
                if (childLength > maxLength && child instanceof TextNode) {
                    for (let node of child.children)
                        add(node);
                }
                else if (childLength > minLength && (currentLength > minLength || currentLength == 0)) {
                    flush();
                    chunked.push(child);
                }
                else if (child instanceof TextLeaf && currentLength > 0 &&
                    (last = currentChunk[currentChunk.length - 1]) instanceof TextLeaf &&
                    child.length + last.length <= 512 /* BaseLeaf */) {
                    currentLength += childLength;
                    currentChunk[currentChunk.length - 1] = new TextLeaf(appendText(child.text, last.text.slice()), child.length + last.length);
                }
                else {
                    if (currentLength + childLength > chunkLength)
                        flush();
                    currentLength += childLength;
                    currentChunk.push(child);
                }
            }
            function flush() {
                if (currentLength == 0)
                    return;
                chunked.push(currentChunk.length == 1 ? currentChunk[0] : TextNode.from(currentChunk, currentLength));
                currentLength = 0;
                currentChunk.length = 0;
            }
            for (let child of children)
                add(child);
            flush();
            return chunked.length == 1 ? chunked[0] : new TextNode(chunked, length);
        }
    }
    Text.empty = Text.of([""]);
    function textLength(text) {
        let length = -1;
        for (let line of text)
            length += line.length + 1;
        return length;
    }
    function appendText(text, target, from = 0, to = 1e9) {
        for (let pos = 0, i = 0, first = true; i < text.length && pos <= to; i++) {
            let line = text[i], end = pos + line.length;
            if (end >= from) {
                if (end > to)
                    line = line.slice(0, to - pos);
                if (pos < from)
                    line = line.slice(from - pos);
                if (first) {
                    target[target.length - 1] += line;
                    first = false;
                }
                else
                    target.push(line);
            }
            pos = end + 1;
        }
        return target;
    }
    function sliceText(text, from, to) {
        return appendText(text, [""], from, to);
    }
    function eqContent(a, b) {
        if (a.length != b.length || a.lines != b.lines)
            return false;
        let iterA = new RawTextCursor(a), iterB = new RawTextCursor(b);
        for (let offA = 0, offB = 0;;) {
            if (iterA.lineBreak != iterB.lineBreak || iterA.done != iterB.done) {
                return false;
            }
            else if (iterA.done) {
                return true;
            }
            else if (iterA.lineBreak) {
                iterA.next();
                iterB.next();
                offA = offB = 0;
            }
            else {
                let strA = iterA.value.slice(offA), strB = iterB.value.slice(offB);
                if (strA.length == strB.length) {
                    if (strA != strB)
                        return false;
                    iterA.next();
                    iterB.next();
                    offA = offB = 0;
                }
                else if (strA.length > strB.length) {
                    if (strA.slice(0, strB.length) != strB)
                        return false;
                    offA += strB.length;
                    iterB.next();
                    offB = 0;
                }
                else {
                    if (strB.slice(0, strA.length) != strA)
                        return false;
                    offB += strA.length;
                    iterA.next();
                    offA = 0;
                }
            }
        }
    }
    class RawTextCursor {
        constructor(text, dir = 1) {
            this.dir = dir;
            this.done = false;
            this.lineBreak = false;
            this.value = "";
            this.nodes = [text];
            this.offsets = [dir > 0 ? 0 : text instanceof TextLeaf ? text.text.length : text.children.length];
        }
        next(skip = 0) {
            for (;;) {
                let last = this.nodes.length - 1;
                if (last < 0) {
                    this.done = true;
                    this.value = "";
                    this.lineBreak = false;
                    return this;
                }
                let top = this.nodes[last];
                let offset = this.offsets[last];
                if (top instanceof TextLeaf) {
                    // Internal offset with lineBreak == false means we have to
                    // count the line break at this position
                    if (offset != (this.dir > 0 ? 0 : top.text.length) && !this.lineBreak) {
                        this.lineBreak = true;
                        if (skip == 0) {
                            this.value = "\n";
                            return this;
                        }
                        skip--;
                        continue;
                    }
                    // Otherwise, move to the next string
                    let next = top.text[offset - (this.dir < 0 ? 1 : 0)];
                    this.offsets[last] = (offset += this.dir);
                    if (offset == (this.dir > 0 ? top.text.length : 0)) {
                        this.nodes.pop();
                        this.offsets.pop();
                    }
                    this.lineBreak = false;
                    if (next.length > Math.max(0, skip)) {
                        this.value = skip == 0 ? next : this.dir > 0 ? next.slice(skip) : next.slice(0, next.length - skip);
                        return this;
                    }
                    skip -= next.length;
                }
                else if (offset == (this.dir > 0 ? top.children.length : 0)) {
                    this.nodes.pop();
                    this.offsets.pop();
                }
                else {
                    let next = top.children[this.dir > 0 ? offset : offset - 1], len = next.length;
                    this.offsets[last] = offset + this.dir;
                    if (skip > len) {
                        skip -= len;
                    }
                    else {
                        this.nodes.push(next);
                        this.offsets.push(this.dir > 0 ? 0 : next instanceof TextLeaf ? next.text.length : next.children.length);
                    }
                }
            }
        }
    }
    class PartialTextCursor {
        constructor(text, start, end) {
            this.value = "";
            this.cursor = new RawTextCursor(text, start > end ? -1 : 1);
            if (start > end) {
                this.skip = text.length - start;
                this.limit = start - end;
            }
            else {
                this.skip = start;
                this.limit = end - start;
            }
        }
        next() {
            if (this.limit <= 0) {
                this.limit = -1;
            }
            else {
                let { value, lineBreak, done } = this.cursor.next(this.skip);
                this.skip = 0;
                this.value = value;
                let len = lineBreak ? 1 : value.length;
                if (len > this.limit)
                    this.value = this.cursor.dir > 0 ? value.slice(0, this.limit) : value.slice(len - this.limit);
                if (done || this.value.length == 0)
                    this.limit = -1;
                else
                    this.limit -= this.value.length;
            }
            return this;
        }
        get lineBreak() { return this.cursor.lineBreak; }
        get done() { return this.limit < 0; }
    }
    class LineCursor {
        constructor(text, from = 0) {
            this.value = "";
            this.done = false;
            this.cursor = text.iter();
            this.skip = from;
        }
        next(skip = 0) {
            if (this.cursor.done) {
                this.done = true;
                this.value = "";
                return this;
            }
            skip += this.skip;
            this.skip = 0;
            for (this.value = "";;) {
                let { value, lineBreak, done } = this.cursor.next(skip);
                skip = 0;
                if (done || lineBreak)
                    return this;
                this.value += value;
            }
        }
        get lineBreak() { return false; }
    }
    /// This type describes a line in the document. It is created
    /// on-demand when lines are [queried](#text.Text.lineAt).
    class Line {
        /// @internal
        constructor(
        /// The position of the start of the line.
        from, 
        /// The position at the end of the line (_before_ the line break,
        /// if this isn't the last line).
        to, 
        /// This line's line number (1-based).
        number, 
        /// @internal
        content) {
            this.from = from;
            this.to = to;
            this.number = number;
            this.content = content;
        }
        /// The length of the line (not including any line break after it).
        get length() { return this.to - this.from; }
        /// Retrieve a part of the content of this line. This is a method,
        /// rather than, say, a string property, to avoid concatenating long
        /// lines whenever they are accessed. Try to write your code, if it
        /// is going to be doing a lot of line-reading, to read only the
        /// parts it needs.
        slice(from = 0, to = this.length) {
            if (from == to)
                return "";
            if (typeof this.content == "string")
                return this.content.slice(from, to);
            if (!this.content)
                this.content = new LineContent(this.doc, this.from);
            let result = this.content.slice(from, to);
            if (from == 0 && to == this.length)
                this.content = result;
            return result;
        }
        /// @internal
        finish(text) {
            this.doc = text;
            return this;
        }
        /// Find the next (or previous if `forward` is false) grapheme
        /// cluster break from the given start position (as an offset inside
        /// the line, not the document). Will return a position greater than
        /// (or less than if `forward` is false) `start` unless there is no
        /// such index in the string.
        findClusterBreak(start, forward) {
            if (start < 0 || start > this.length)
                throw new RangeError("Invalid position given to Line.findClusterBreak");
            let contextStart, context;
            if (this.content == "string") {
                contextStart = this.from;
                context = this.content;
            }
            else {
                contextStart = Math.max(0, start - 256);
                context = this.slice(contextStart, Math.min(this.length, contextStart + 512));
            }
            return (forward ? nextClusterBreak : prevClusterBreak)(context, start - contextStart) + contextStart;
        }
    }
    class LineContent {
        constructor(doc, start) {
            this.doc = doc;
            this.start = start;
            this.cursor = null;
            this.strings = null;
        }
        // FIXME quadratic complexity (somewhat) when iterating long lines in small pieces
        slice(from, to) {
            if (!this.cursor) {
                this.cursor = this.doc.iter();
                this.strings = [this.cursor.next(this.start).value];
            }
            for (let result = "", pos = 0, i = 0;; i++) {
                if (i == this.strings.length) {
                    let next = this.cursor.next().value;
                    if (!next)
                        return result;
                    this.strings.push(next);
                }
                let string = this.strings[i], start = pos;
                pos += string.length;
                if (pos <= from)
                    continue;
                result += string.slice(Math.max(0, from - start), Math.min(string.length, to - start));
                if (pos >= to)
                    return result;
            }
        }
    }

    const DefaultSplit = /\r\n?|\n/;
    /// Distinguishes different ways in which positions can be mapped.
    var MapMode;
    (function (MapMode) {
        /// Map a position to a valid new position, even when its context
        /// was deleted.
        MapMode[MapMode["Simple"] = 0] = "Simple";
        /// Return null if deletion happens across the position.
        MapMode[MapMode["TrackDel"] = 1] = "TrackDel";
        /// Return null if the character _before_ the position is deleted.
        MapMode[MapMode["TrackBefore"] = 2] = "TrackBefore";
        /// Return null if the character _after_ the position is deleted.
        MapMode[MapMode["TrackAfter"] = 3] = "TrackAfter";
    })(MapMode || (MapMode = {}));
    /// A change description is a variant of [change set](#state.ChangeSet)
    /// that doesn't store the inserted text. As such, it can't be
    /// applied, but is cheaper to store and manipulate.
    class ChangeDesc {
        // Sections are encoded as pairs of integers. The first is the
        // length in the current document, and the second is -1 for
        // unaffected sections, and the length of the replacement content
        // otherwise. So an insertion would be (0, n>0), a deletion (n>0,
        // 0), and a replacement two positive numbers.
        /// @internal
        constructor(
        /// @internal
        sections) {
            this.sections = sections;
        }
        /// The length of the document before the change.
        get length() {
            let result = 0;
            for (let i = 0; i < this.sections.length; i += 2)
                result += this.sections[i];
            return result;
        }
        /// The length of the document after the change.
        get newLength() {
            let result = 0;
            for (let i = 0; i < this.sections.length; i += 2) {
                let ins = this.sections[i + 1];
                result += ins < 0 ? this.sections[i] : ins;
            }
            return result;
        }
        /// False when there are actual changes in this set.
        get empty() { return this.sections.length == 0 || this.sections.length == 2 && this.sections[1] < 0; }
        /// Iterate over the unchanged parts left by these changes.
        iterGaps(f) {
            for (let i = 0, posA = 0, posB = 0; i < this.sections.length;) {
                let len = this.sections[i++], ins = this.sections[i++];
                if (ins < 0) {
                    f(posA, posB, len);
                    posB += len;
                }
                else {
                    posB += ins;
                }
                posA += len;
            }
        }
        /// Iterate over the ranges changed by these changes. (See
        /// [`ChangeSet.iterChanges`](#state.ChangeSet.iterChanges) for a
        /// variant that also provides you with the inserted text.)
        ///
        /// When `individual` is true, adjacent changes (which are kept
        /// separate for [position mapping](#state.ChangeDesc.mapPos)) are
        /// reported separately.
        iterChangedRanges(f, individual = false) {
            iterChanges(this, f, individual);
        }
        /// Get a description of the inverted form of these changes.
        get invertedDesc() {
            let sections = [];
            for (let i = 0; i < this.sections.length;) {
                let len = this.sections[i++], ins = this.sections[i++];
                if (ins < 0)
                    sections.push(len, ins);
                else
                    sections.push(ins, len);
            }
            return new ChangeDesc(sections);
        }
        /// Compute the combined effect of applying another set of changes
        /// after this one. The length of the document after this set should
        /// match the length before `other`.
        composeDesc(other) { return this.empty ? other : other.empty ? this : composeSets(this, other); }
        /// Map this description, which should start with the same document
        /// as `other`, over another set of changes, so that it can be
        /// applied after it.
        mapDesc(other, before = false) { return other.empty ? this : mapSet(this, other, before); }
        mapPos(pos, assoc = -1, mode = MapMode.Simple) {
            let posA = 0, posB = 0;
            for (let i = 0; i < this.sections.length;) {
                let len = this.sections[i++], ins = this.sections[i++], endA = posA + len;
                if (ins < 0) {
                    if (endA > pos)
                        return posB + (pos - posA);
                    posB += len;
                }
                else {
                    if (mode != MapMode.Simple && endA >= pos &&
                        (mode == MapMode.TrackDel && posA < pos && endA > pos ||
                            mode == MapMode.TrackBefore && posA < pos ||
                            mode == MapMode.TrackAfter && endA > pos))
                        return null;
                    if (endA > pos || endA == pos && assoc < 0 && !len)
                        return pos == posA || assoc < 0 ? posB : posB + ins;
                    posB += ins;
                }
                posA = endA;
            }
            if (pos > posA)
                throw new RangeError(`Position ${pos} is out of range for changeset of length ${posA}`);
            return posB;
        }
        /// Check whether these changes touch a given range. When one of the
        /// changes entirely covers the range, the string `"cover"` is
        /// returned.
        touchesRange(from, to = from) {
            for (let i = 0, pos = 0; i < this.sections.length && pos <= to;) {
                let len = this.sections[i++], ins = this.sections[i++], end = pos + len;
                if (ins >= 0 && pos <= to && end >= from)
                    return pos < from && end > to ? "cover" : true;
                pos = end;
            }
            return false;
        }
        /// @internal
        toString() {
            let result = "";
            for (let i = 0; i < this.sections.length;) {
                let len = this.sections[i++], ins = this.sections[i++];
                result += (result ? " " : "") + len + (ins >= 0 ? ":" + ins : "");
            }
            return result;
        }
    }
    /// A change set represents a group of modifications to a document. It
    /// stores the document length, and can only be applied to documents
    /// with exactly that length.
    class ChangeSet extends ChangeDesc {
        /// @internal
        constructor(sections, 
        /// @internal
        inserted) {
            super(sections);
            this.inserted = inserted;
        }
        /// Apply the changes to a document, returning the modified
        /// document.
        apply(doc) {
            if (this.length != doc.length)
                throw new RangeError("Applying change set to a document with the wrong length");
            iterChanges(this, (fromA, toA, fromB, _toB, text) => doc = doc.replace(fromB, fromB + (toA - fromA), text), false);
            return doc;
        }
        /// Map this set, which should start with the same document as
        /// `other`, over another set of changes, so that it can be applied
        /// after it. When `before` is true, map as if the changes in
        /// `other` happened before the ones in `this`.
        mapDesc(other, before = false) { return mapSet(this, other, before, true); }
        /// Given the document as it existed _before_ the changes, return a
        /// change set that represents the inverse of this set, which could
        /// be used to go from the document created by the changes back to
        /// the document as it existed before the changes.
        invert(doc) {
            let sections = this.sections.slice(), inserted = [];
            for (let i = 0, pos = 0; i < sections.length; i += 2) {
                let len = sections[i], ins = sections[i + 1];
                if (ins >= 0) {
                    sections[i] = ins;
                    sections[i + 1] = len;
                    let index = i >> 1;
                    while (inserted.length < index)
                        inserted.push(Text.empty);
                    inserted.push(len ? doc.slice(pos, pos + len) : Text.empty);
                }
                pos += len;
            }
            return new ChangeSet(sections, inserted);
        }
        /// Combine two subsequent change sets into a single set. `other`
        /// must start in the document produced by `this`. If `this` goes
        /// `docA` → `docB` and `other` represents `docB` → `docC`, the
        /// returned value will represent the change `docA` → `docC`.
        compose(other) { return this.empty ? other : other.empty ? this : composeSets(this, other, true); }
        /// Given another change set starting in the same document, maps this
        /// change set over the other, producing a new change set that can be
        /// applied to the document produced by applying `other`. When
        /// `before` is `true`, order changes as if `this` comes before
        /// `other`, otherwise (the default) treat `other` as coming first.
        ///
        /// Given two changes `A` and `B`, `A.compose(B.map(A))` and
        /// `B.compose(A.map(B, true))` will produce the same document. This
        /// provides a basic form of [operational
        /// transformation](https://en.wikipedia.org/wiki/Operational_transformation),
        /// and can be used for collaborative editing.
        map(other, before = false) { return other.empty ? this : mapSet(this, other, before, true); }
        /// Iterate over the changed ranges in the document, calling `f` for
        /// each.
        iterChanges(f, individual = false) {
            iterChanges(this, f, individual);
        }
        /// Get a [change description](#state.ChangeDesc) for this change
        /// set.
        get desc() { return new ChangeDesc(this.sections); }
        /// @internal
        filter(ranges) {
            let resultSections = [], resultInserted = [], filteredSections = [];
            let iter = new SectionIter(this);
            done: for (let i = 0, pos = 0;;) {
                let next = i == ranges.length ? 1e9 : ranges[i++];
                while (pos < next || pos == next && iter.len == 0) {
                    if (iter.done)
                        break done;
                    let len = Math.min(iter.len, next - pos);
                    addSection(filteredSections, len, -1);
                    let ins = iter.ins == -1 ? -1 : iter.off == 0 ? iter.ins : 0;
                    addSection(resultSections, len, ins);
                    if (ins > 0)
                        addInsert(resultInserted, resultSections, iter.text);
                    iter.forward(len);
                    pos += len;
                }
                let end = ranges[i++];
                while (pos < end) {
                    if (iter.done)
                        break done;
                    let len = Math.min(iter.len, end - pos);
                    addSection(resultSections, len, -1);
                    addSection(filteredSections, len, iter.ins == -1 ? -1 : iter.off == 0 ? iter.ins : 0);
                    iter.forward(len);
                    pos += len;
                }
            }
            return { changes: new ChangeSet(resultSections, resultInserted),
                filtered: new ChangeDesc(filteredSections) };
        }
        /// Serialize this change set to a JSON-representable value.
        toJSON() {
            let parts = [];
            for (let i = 0; i < this.sections.length; i += 2) {
                let len = this.sections[i], ins = this.sections[i + 1];
                if (ins < 0)
                    parts.push(len);
                else if (ins == 0)
                    parts.push([len]);
                else
                    parts.push([len, this.inserted[i >> 1].toJSON()]);
            }
            return parts;
        }
        /// Create a change set for the given changes, for a document of the
        /// given length, using `lineSep` as line separator.
        static of(changes, length, lineSep) {
            let sections = [], inserted = [], pos = 0;
            let total = null;
            function flush(force = false) {
                if (!force && !sections.length)
                    return;
                if (pos < length)
                    addSection(sections, length - pos, -1);
                let set = new ChangeSet(sections, inserted);
                total = total ? total.compose(set.map(total)) : set;
                sections = [];
                inserted = [];
                pos = 0;
            }
            function process(spec) {
                if (Array.isArray(spec)) {
                    for (let sub of spec)
                        process(sub);
                }
                else if (spec instanceof ChangeSet) {
                    if (spec.length != length)
                        throw new RangeError(`Mismatched change set length (got ${spec.length}, expected ${length})`);
                    flush();
                    total = total ? total.compose(spec.map(total)) : spec;
                }
                else {
                    let { from, to = from, insert } = spec;
                    if (from > to || from < 0 || to > length)
                        throw new RangeError(`Invalid change range ${from} to ${to} (in doc of length ${length})`);
                    let insText = !insert ? Text.empty : typeof insert == "string" ? Text.of(insert.split(lineSep || DefaultSplit)) : insert;
                    let insLen = insText.length;
                    if (from == to && insLen == 0)
                        return;
                    if (from < pos)
                        flush();
                    if (from > pos)
                        addSection(sections, from - pos, -1);
                    addSection(sections, to - from, insLen);
                    addInsert(inserted, sections, insText);
                    pos = to;
                }
            }
            process(changes);
            flush(!total);
            return total;
        }
        /// Create an empty changeset of the given length.
        static empty(length) {
            return new ChangeSet(length ? [length, -1] : [], []);
        }
        /// Create a changeset from its JSON representation (as produced by
        /// [`toJSON`](#state.ChangeSet.toJSON).
        static fromJSON(json) {
            let sections = [], inserted = [];
            for (let i = 0; i < json.length; i++) {
                let part = json[i];
                if (typeof part == "number") {
                    sections.push(part, -1);
                }
                else if (part.length == 1) {
                    sections.push(part[0], 0);
                }
                else {
                    while (inserted.length < i)
                        inserted.push(Text.empty);
                    inserted[i] = Text.of(part[1]);
                    sections.push(part[0], inserted[i].length);
                }
            }
            return new ChangeSet(sections, inserted);
        }
    }
    function addSection(sections, len, ins, forceJoin = false) {
        if (len == 0 && ins <= 0)
            return;
        let last = sections.length - 2;
        if (last >= 0 && ins <= 0 && ins == sections[last + 1])
            sections[last] += len;
        else if (len == 0 && sections[last] == 0)
            sections[last + 1] += ins;
        else if (forceJoin) {
            sections[last] += len;
            sections[last + 1] += ins;
        }
        else
            sections.push(len, ins);
    }
    function addInsert(values, sections, value) {
        if (value.length == 0)
            return;
        let index = (sections.length - 2) >> 1;
        if (index < values.length) {
            values[values.length - 1] = values[values.length - 1].append(value);
        }
        else {
            while (values.length < index)
                values.push(Text.empty);
            values.push(value);
        }
    }
    function iterChanges(desc, f, individual) {
        let inserted = desc.inserted;
        for (let posA = 0, posB = 0, i = 0; i < desc.sections.length;) {
            let len = desc.sections[i++], ins = desc.sections[i++];
            if (ins < 0) {
                posA += len;
                posB += len;
            }
            else {
                let endA = posA, endB = posB, text = Text.empty;
                for (;;) {
                    endA += len;
                    endB += ins;
                    if (ins && inserted)
                        text = text.append(inserted[(i - 2) >> 1]);
                    if (individual || i == desc.sections.length || desc.sections[i + 1] < 0)
                        break;
                    len = desc.sections[i++];
                    ins = desc.sections[i++];
                }
                f(posA, endA, posB, endB, text);
                posA = endA;
                posB = endB;
            }
        }
    }
    function mapSet(setA, setB, before, mkSet = false) {
        let sections = [], insert = mkSet ? [] : null;
        let a = new SectionIter(setA), b = new SectionIter(setB);
        for (let posA = 0, posB = 0;;) {
            if (a.ins == -1) {
                posA += a.len;
                a.next();
            }
            else if (b.ins == -1 && posB < posA) {
                let skip = Math.min(b.len, posA - posB);
                b.forward(skip);
                addSection(sections, skip, -1);
                posB += skip;
            }
            else if (b.ins >= 0 && (a.done || posB < posA || posB == posA && (b.len < a.len || b.len == a.len && !before))) {
                addSection(sections, b.ins, -1);
                while (posA > posB && !a.done && posA + a.len < posB + b.len) {
                    posA += a.len;
                    a.next();
                }
                posB += b.len;
                b.next();
            }
            else if (a.ins >= 0) {
                let len = 0, end = posA + a.len;
                for (;;) {
                    if (b.ins >= 0 && posB > posA && posB + b.len < end) {
                        len += b.ins;
                        posB += b.len;
                        b.next();
                    }
                    else if (b.ins == -1 && posB < end) {
                        let skip = Math.min(b.len, end - posB);
                        len += skip;
                        b.forward(skip);
                        posB += skip;
                    }
                    else {
                        break;
                    }
                }
                addSection(sections, len, a.ins);
                if (insert)
                    addInsert(insert, sections, a.text);
                posA = end;
                a.next();
            }
            else if (a.done && b.done) {
                return insert ? new ChangeSet(sections, insert) : new ChangeDesc(sections);
            }
            else {
                throw new Error("Mismatched change set lengths");
            }
        }
    }
    function composeSets(setA, setB, mkSet = false) {
        let sections = [];
        let insert = mkSet ? [] : null;
        let a = new SectionIter(setA), b = new SectionIter(setB);
        for (let open = false;;) {
            if (a.done && b.done) {
                return insert ? new ChangeSet(sections, insert) : new ChangeDesc(sections);
            }
            else if (a.ins == 0) { // Deletion in A
                addSection(sections, a.len, 0, open);
                a.next();
            }
            else if (b.len == 0 && !b.done) { // Insertion in B
                addSection(sections, 0, b.ins, open);
                if (insert)
                    addInsert(insert, sections, b.text);
                b.next();
            }
            else if (a.done || b.done) {
                throw new Error("Mismatched change set lengths");
            }
            else {
                let len = Math.min(a.len2, b.len), sectionLen = sections.length;
                if (a.ins == -1) {
                    let insB = b.ins == -1 ? -1 : b.off ? 0 : b.ins;
                    addSection(sections, len, insB, open);
                    if (insert && insB)
                        addInsert(insert, sections, b.text);
                }
                else if (b.ins == -1) {
                    addSection(sections, a.off ? 0 : a.len, len, open);
                    if (insert)
                        addInsert(insert, sections, a.textBit(len));
                }
                else {
                    addSection(sections, a.off ? 0 : a.len, b.off ? 0 : b.ins, open);
                    if (insert && !b.off)
                        addInsert(insert, sections, b.text);
                }
                open = (a.ins > len || b.ins >= 0 && b.len > len) && (open || sections.length > sectionLen);
                a.forward2(len);
                b.forward(len);
            }
        }
    }
    class SectionIter {
        constructor(set) {
            this.set = set;
            this.i = 0;
            this.next();
        }
        next() {
            let { sections } = this.set;
            if (this.i < sections.length) {
                this.len = sections[this.i++];
                this.ins = sections[this.i++];
            }
            else {
                this.len = 0;
                this.ins = -2;
            }
            this.off = 0;
        }
        get done() { return this.ins == -2; }
        get len2() { return this.ins < 0 ? this.len : this.ins; }
        get text() {
            let { inserted } = this.set, index = (this.i - 2) >> 1;
            return index >= inserted.length ? Text.empty : inserted[index];
        }
        textBit(len) {
            let { inserted } = this.set, index = (this.i - 2) >> 1;
            return index >= inserted.length && !len ? Text.empty
                : inserted[index].slice(this.off, len == null ? undefined : this.off + len);
        }
        forward(len) {
            if (len == this.len)
                this.next();
            else {
                this.len -= len;
                this.off += len;
            }
        }
        forward2(len) {
            if (this.ins == -1)
                this.forward(len);
            else if (len == this.ins)
                this.next();
            else {
                this.ins -= len;
                this.off += len;
            }
        }
    }

    /// A single selection range. When
    /// [`allowMultipleSelections`](#state.EditorState^allowMultipleSelections)
    /// is enabled, a [selection](#state.EditorSelection) may hold
    /// multiple ranges. By default, selections hold exactly one range.
    class SelectionRange {
        // @internal
        constructor(
        /// The lower side of the range.
        from, 
        /// The upper side of the range.
        to, flags) {
            this.from = from;
            this.to = to;
            this.flags = flags;
        }
        /// The anchor of the range—the side that doesn't move when you
        /// extend it.
        get anchor() { return this.flags & 16 /* Inverted */ ? this.to : this.from; }
        /// The head of the range, which is moved when the range is
        /// [extended](#state.SelectionRange.extend).
        get head() { return this.flags & 16 /* Inverted */ ? this.from : this.to; }
        /// True when `anchor` and `head` are at the same position.
        get empty() { return this.from == this.to; }
        /// If this is a cursor that is explicitly associated with the
        /// character on one of its sides, this returns the side. -1 means
        /// the character before its position, 1 the character after, and 0
        /// means no association.
        get assoc() { return this.flags & 4 /* AssocBefore */ ? -1 : this.flags & 8 /* AssocAfter */ ? 1 : 0; }
        /// The bidirectional text level associated with this cursor.
        get bidiLevel() {
            let level = this.flags & 3 /* BidiLevelMask */;
            return level == 3 ? null : level;
        }
        get goalColumn() {
            let value = this.flags >> 5 /* GoalColumnOffset */;
            return value == 33554431 /* NoGoalColumn */ ? undefined : value;
        }
        /// Map this range through a mapping.
        map(mapping) {
            let from = mapping.mapPos(this.from), to = mapping.mapPos(this.to);
            return from == this.from && to == this.to ? this : new SelectionRange(from, to, this.flags);
        }
        /// Extend this range to cover at least `from` to `to`.
        extend(from, to = from) {
            if (from <= this.anchor && to >= this.anchor)
                return EditorSelection.range(from, to);
            let head = Math.abs(from - this.anchor) > Math.abs(to - this.anchor) ? from : to;
            return EditorSelection.range(this.anchor, head);
        }
        /// Compare this range to another range.
        eq(other) {
            return this.anchor == other.anchor && this.head == other.head;
        }
        /// Return a JSON-serializable object representing the range.
        toJSON() { return { anchor: this.anchor, head: this.head }; }
        /// Convert a JSON representation of a range to a `SelectionRange`
        /// instance.
        static fromJSON(json) {
            if (!json || typeof json.anchor != "number" || typeof json.head != "number")
                throw new RangeError("Invalid JSON representation for SelectionRange");
            return EditorSelection.range(json.anchor, json.head);
        }
    }
    /// An editor selection holds one or more selection ranges.
    class EditorSelection {
        /// @internal
        constructor(
        /// The ranges in the selection, sorted by position. Ranges cannot
        /// overlap (but they may touch, if they aren't empty).
        ranges, 
        /// The index of the _primary_ range in the selection (which is
        /// usually the range that was added last).
        primaryIndex = 0) {
            this.ranges = ranges;
            this.primaryIndex = primaryIndex;
        }
        /// Map a selection through a mapping. Mostly used to adjust the
        /// selection position for changes.
        map(mapping) {
            if (mapping.empty)
                return this;
            return EditorSelection.create(this.ranges.map(r => r.map(mapping)), this.primaryIndex);
        }
        /// Compare this selection to another selection.
        eq(other) {
            if (this.ranges.length != other.ranges.length ||
                this.primaryIndex != other.primaryIndex)
                return false;
            for (let i = 0; i < this.ranges.length; i++)
                if (!this.ranges[i].eq(other.ranges[i]))
                    return false;
            return true;
        }
        /// Get the primary selection range. Usually, you should make sure
        /// your code applies to _all_ ranges, by using methods like
        /// [`changeByRange`](#state.EditorState.changeByRange).
        get primary() { return this.ranges[this.primaryIndex]; }
        /// Make sure the selection only has one range. Returns a selection
        /// holding only the primary range from this selection.
        asSingle() {
            return this.ranges.length == 1 ? this : new EditorSelection([this.primary]);
        }
        /// Extend this selection with an extra range.
        addRange(range, primary = true) {
            return EditorSelection.create([range].concat(this.ranges), primary ? 0 : this.primaryIndex + 1);
        }
        /// Replace a given range with another range, and then normalize the
        /// selection to merge and sort ranges if necessary.
        replaceRange(range, which = this.primaryIndex) {
            let ranges = this.ranges.slice();
            ranges[which] = range;
            return EditorSelection.create(ranges, this.primaryIndex);
        }
        /// Convert this selection to an object that can be serialized to
        /// JSON.
        toJSON() {
            return { ranges: this.ranges.map(r => r.toJSON()), primaryIndex: this.primaryIndex };
        }
        /// Create a selection from a JSON representation.
        static fromJSON(json) {
            if (!json || !Array.isArray(json.ranges) || typeof json.primaryIndex != "number" || json.primaryIndex >= json.ranges.length)
                throw new RangeError("Invalid JSON representation for EditorSelection");
            return new EditorSelection(json.ranges.map((r) => SelectionRange.fromJSON(r)), json.primaryIndex);
        }
        /// Create a selection holding a single range.
        static single(anchor, head = anchor) {
            return new EditorSelection([EditorSelection.range(anchor, head)], 0);
        }
        /// Sort and merge the given set of ranges, creating a valid
        /// selection.
        static create(ranges, primaryIndex = 0) {
            if (ranges.length == 0)
                throw new RangeError("A selection needs at least one range");
            for (let pos = 0, i = 0; i < ranges.length; i++) {
                let range = ranges[i];
                if (range.empty ? range.from <= pos : range.from < pos)
                    return normalized(ranges.slice(), primaryIndex);
                pos = range.to;
            }
            return new EditorSelection(ranges, primaryIndex);
        }
        /// Create a cursor selection range at the given position. You can
        /// probably ignore [association](#state.SelectionRange.assoc) and
        /// [bidi level](#state.SelectionRange.bidiLevel) in most
        /// situations.
        static cursor(pos, assoc = 0, bidiLevel, goalColumn) {
            return new SelectionRange(pos, pos, (assoc == 0 ? 0 : assoc < 0 ? 4 /* AssocBefore */ : 8 /* AssocAfter */) |
                (bidiLevel == null ? 3 : Math.min(2, bidiLevel)) |
                ((goalColumn !== null && goalColumn !== void 0 ? goalColumn : 33554431 /* NoGoalColumn */) << 5 /* GoalColumnOffset */));
        }
        /// Create a selection range.
        static range(anchor, head, goalColumn) {
            let goal = (goalColumn !== null && goalColumn !== void 0 ? goalColumn : 33554431 /* NoGoalColumn */) << 5 /* GoalColumnOffset */;
            return head < anchor ? new SelectionRange(head, anchor, 16 /* Inverted */ | goal) : new SelectionRange(anchor, head, goal);
        }
    }
    function normalized(ranges, primaryIndex = 0) {
        let primary = ranges[primaryIndex];
        ranges.sort((a, b) => a.from - b.from);
        primaryIndex = ranges.indexOf(primary);
        for (let i = 1; i < ranges.length; i++) {
            let range = ranges[i], prev = ranges[i - 1];
            if (range.empty ? range.from <= prev.to : range.from < prev.to) {
                let from = prev.from, to = Math.max(range.to, prev.to);
                if (i <= primaryIndex)
                    primaryIndex--;
                ranges.splice(--i, 2, range.anchor > range.head ? EditorSelection.range(to, from) : EditorSelection.range(from, to));
            }
        }
        return new EditorSelection(ranges, primaryIndex);
    }
    function checkSelection(selection, docLength) {
        for (let range of selection.ranges)
            if (range.to > docLength)
                throw new RangeError("Selection points outside of document");
    }

    let nextID = 0;
    /// A facet is a value that is assicated with a state and can be
    /// influenced by any number of extensions. Extensions can provide
    /// input values for the facet, and the facet combines those into an
    /// output value.
    ///
    /// Examples of facets are the theme styles associated with an editor
    /// (which are all stored) or the tab size (which is reduced to a
    /// single value, using the input with the hightest precedence).
    class Facet {
        constructor(
        /// @internal
        combine, 
        /// @internal
        compareInput, 
        /// @internal
        compare, isStatic) {
            this.combine = combine;
            this.compareInput = compareInput;
            this.compare = compare;
            this.isStatic = isStatic;
            /// @internal
            this.id = nextID++;
            this.default = combine([]);
        }
        /// Define a new facet.
        static define(config = {}) {
            return new Facet(config.combine || ((a) => a), config.compareInput || ((a, b) => a === b), config.compare || (!config.combine ? sameArray : (a, b) => a === b), !!config.static);
        }
        /// Returns an extension that adds the given value for this facet.
        of(value) {
            return new FacetProvider([], this, 0 /* Static */, value);
        }
        /// Create an extension that computes a value for the facet from a
        /// state. You must take care to declare the parts of the state that
        /// this value depends on, since your function is only called again
        /// for a new state when one of those parts changed.
        ///
        /// In most cases, you'll want to use the
        /// [`provide`](#state.StateField^define^config.provide) option when
        /// defining a field instead.
        compute(deps, get) {
            if (this.isStatic)
                throw new Error("Can't compute a static facet");
            return new FacetProvider(deps, this, 1 /* Single */, get);
        }
        /// Create an extension that computes zero or more values for this
        /// facet from a state.
        computeN(deps, get) {
            if (this.isStatic)
                throw new Error("Can't compute a static facet");
            return new FacetProvider(deps, this, 2 /* Multi */, get);
        }
        /// Helper method for registering a facet source with a state field
        /// via its [`provide`](#state.StateField^define^config.provide) option.
        /// Returns a value that can be passed to that option to make the
        /// field automatically provide a value for this facet.
        from(get, prec) {
            return field => maybePrec(prec, this.compute([field], state => get(state.field(field))));
        }
        /// Helper for [providing](#state.StateField^define^config.provide)
        /// a dynamic number of values for this facet from a state field.
        nFrom(get, prec) {
            return field => maybePrec(prec, this.computeN([field], state => get(state.field(field))));
        }
    }
    function sameArray(a, b) {
        return a == b || a.length == b.length && a.every((e, i) => e === b[i]);
    }
    class FacetProvider {
        constructor(dependencies, facet, type, value) {
            this.dependencies = dependencies;
            this.facet = facet;
            this.type = type;
            this.value = value;
            this.id = nextID++;
        }
        dynamicSlot(addresses) {
            var _a;
            let getter = this.value;
            let compare = this.facet.compareInput;
            let idx = addresses[this.id] >> 1, multi = this.type == 2 /* Multi */;
            let depDoc = false, depSel = false, depAddrs = [];
            for (let dep of this.dependencies) {
                if (dep == "doc")
                    depDoc = true;
                else if (dep == "selection")
                    depSel = true;
                else if ((((_a = addresses[dep.id]) !== null && _a !== void 0 ? _a : 1) & 1) == 0)
                    depAddrs.push(addresses[dep.id]);
            }
            return (state, tr) => {
                if (!tr || tr.reconfigure) {
                    state.values[idx] = getter(state);
                    return 1 /* Changed */;
                }
                else {
                    let depChanged = (depDoc && tr.docChanged) || (depSel && (tr.docChanged || tr.selection)) ||
                        depAddrs.some(addr => (ensureAddr(state, addr) & 1 /* Changed */) > 0);
                    if (!depChanged)
                        return 0;
                    let newVal = getter(state), oldVal = tr.startState.values[idx];
                    if (multi ? compareArray(newVal, oldVal, compare) : compare(newVal, oldVal))
                        return 0;
                    state.values[idx] = newVal;
                    return 1 /* Changed */;
                }
            };
        }
    }
    function compareArray(a, b, compare) {
        if (a.length != b.length)
            return false;
        for (let i = 0; i < a.length; i++)
            if (!compare(a[i], b[i]))
                return false;
        return true;
    }
    function dynamicFacetSlot(addresses, facet, providers) {
        let providerAddrs = providers.map(p => addresses[p.id]);
        let providerTypes = providers.map(p => p.type);
        let dynamic = providerAddrs.filter(p => !(p & 1));
        let idx = addresses[facet.id] >> 1;
        return (state, tr) => {
            let oldAddr = !tr ? null : tr.reconfigure ? tr.startState.config.address[facet.id] : idx << 1;
            let changed = oldAddr == null;
            for (let dynAddr of dynamic) {
                if (ensureAddr(state, dynAddr) & 1 /* Changed */)
                    changed = true;
            }
            if (!changed)
                return 0;
            let values = [];
            for (let i = 0; i < providerAddrs.length; i++) {
                let value = getAddr(state, providerAddrs[i]);
                if (providerTypes[i] == 2 /* Multi */)
                    for (let val of value)
                        values.push(val);
                else
                    values.push(value);
            }
            let newVal = facet.combine(values);
            if (oldAddr != null && facet.compare(newVal, getAddr(tr.startState, oldAddr)))
                return 0;
            state.values[idx] = newVal;
            return 1 /* Changed */;
        };
    }
    function maybeIndex(state, id) {
        let found = state.config.address[id];
        return found == null ? null : found >> 1;
    }
    /// Fields can store additional information in an editor state, and
    /// keep it in sync with the rest of the state.
    class StateField {
        constructor(
        /// @internal
        id, createF, updateF, compareF, 
        /// @internal
        facets) {
            this.id = id;
            this.createF = createF;
            this.updateF = updateF;
            this.compareF = compareF;
            this.facets = facets;
        }
        /// Define a state field.
        static define(config) {
            let facets = [];
            let field = new StateField(nextID++, config.create, config.update, config.compare || ((a, b) => a === b), facets);
            if (config.provide)
                for (let p of config.provide) {
                    if (p instanceof Facet)
                        facets.push(p.compute([field], state => state.field(field)));
                    else
                        facets.push(p(field));
                }
            return field;
        }
        /// @internal
        slot(addresses) {
            let idx = addresses[this.id] >> 1;
            return (state, tr) => {
                if (!tr) {
                    state.values[idx] = this.createF(state);
                    return 1 /* Changed */;
                }
                let oldVal, changed = 0;
                if (tr.reconfigure) {
                    let oldIdx = maybeIndex(tr.startState, this.id);
                    oldVal = oldIdx == null ? this.createF(tr.startState) : tr.startState.values[oldIdx];
                    changed = 1 /* Changed */;
                }
                else {
                    oldVal = tr.startState.values[idx];
                }
                let value = this.updateF(oldVal, tr);
                if (!changed && !this.compareF(oldVal, value))
                    changed = 1 /* Changed */;
                if (changed)
                    state.values[idx] = value;
                return changed;
            };
        }
    }
    const Prec = { fallback: 3, default: 2, extend: 1, override: 0 };
    /// By default extensions are registered in the order they are found
    /// the flattened form of nested array that was provided. Individual
    /// extension values can be assigned a precedence to override this.
    /// Extensions that do not have a precedence set get the precedence of
    /// the nearest parent with a precedence, or
    /// [`"default"`](#state.Precedence) if there is no such parent. The
    /// final ordering of extensions is determined by first sorting by
    /// precedence and then by order within each precedence.
    function precedence(extension, value) {
        if (!Prec.hasOwnProperty(value))
            throw new RangeError(`Invalid precedence: ${value}`);
        return new PrecExtension(extension, Prec[value]);
    }
    function maybePrec(prec, ext) {
        return prec ? precedence(ext, prec) : ext;
    }
    class PrecExtension {
        constructor(e, prec) {
            this.e = e;
            this.prec = prec;
        }
    }
    class TaggedExtension {
        constructor(tag, extension) {
            this.tag = tag;
            this.extension = extension;
        }
    }
    class Configuration {
        constructor(source, replacements, dynamicSlots, address, staticValues) {
            this.source = source;
            this.replacements = replacements;
            this.dynamicSlots = dynamicSlots;
            this.address = address;
            this.staticValues = staticValues;
            this.statusTemplate = [];
            while (this.statusTemplate.length < dynamicSlots.length)
                this.statusTemplate.push(0 /* Uninitialized */);
        }
        staticFacet(facet) {
            let addr = this.address[facet.id];
            return addr == null ? facet.default : this.staticValues[addr >> 1];
        }
        static resolve(extension, replacements = Object.create(null), oldState) {
            let fields = [];
            let facets = Object.create(null);
            for (let ext of flatten(extension, replacements)) {
                if (ext instanceof StateField)
                    fields.push(ext);
                else
                    (facets[ext.facet.id] || (facets[ext.facet.id] = [])).push(ext);
            }
            let address = Object.create(null);
            let staticValues = [];
            let dynamicSlots = [];
            for (let field of fields) {
                address[field.id] = dynamicSlots.length << 1;
                dynamicSlots.push(a => field.slot(a));
            }
            for (let id in facets) {
                let providers = facets[id], facet = providers[0].facet;
                if (providers.every(p => p.type == 0 /* Static */)) {
                    address[facet.id] = (staticValues.length << 1) | 1;
                    let value = facet.combine(providers.map(p => p.value));
                    let oldAddr = oldState ? oldState.config.address[facet.id] : null;
                    if (oldAddr != null) {
                        let oldVal = getAddr(oldState, oldAddr);
                        if (facet.compare(value, oldVal))
                            value = oldVal;
                    }
                    staticValues.push(value);
                }
                else {
                    for (let p of providers) {
                        if (p.type == 0 /* Static */) {
                            address[p.id] = (staticValues.length << 1) | 1;
                            staticValues.push(p.value);
                        }
                        else {
                            address[p.id] = dynamicSlots.length << 1;
                            dynamicSlots.push(a => p.dynamicSlot(a));
                        }
                    }
                    address[facet.id] = dynamicSlots.length << 1;
                    dynamicSlots.push(a => dynamicFacetSlot(a, facet, providers));
                }
            }
            return new Configuration(extension, replacements, dynamicSlots.map(f => f(address)), address, staticValues);
        }
    }
    function allKeys(obj) {
        return (Object.getOwnPropertySymbols ? Object.getOwnPropertySymbols(obj) : []).concat(Object.keys(obj));
    }
    function flatten(extension, replacements) {
        let result = [[], [], [], []];
        let seen = new Map();
        let tagsSeen = Object.create(null);
        function inner(ext, prec) {
            let known = seen.get(ext);
            if (known != null) {
                if (known >= prec)
                    return;
                let found = result[known].indexOf(ext);
                if (found > -1)
                    result[known].splice(found, 1);
            }
            seen.set(ext, prec);
            if (Array.isArray(ext)) {
                for (let e of ext)
                    inner(e, prec);
            }
            else if (ext instanceof TaggedExtension) {
                if (ext.tag in tagsSeen)
                    throw new RangeError(`Duplicate use of tag '${String(ext.tag)}' in extensions`);
                tagsSeen[ext.tag] = true;
                inner(replacements[ext.tag] || ext.extension, prec);
            }
            else if (ext.extension) {
                inner(ext.extension, prec);
            }
            else if (ext instanceof PrecExtension) {
                inner(ext.e, ext.prec);
            }
            else {
                result[prec].push(ext);
                if (ext instanceof StateField)
                    inner(ext.facets, prec);
            }
        }
        inner(extension, Prec.default);
        for (let key of allKeys(replacements))
            if (!(key in tagsSeen) && key != "full" && replacements[key]) {
                tagsSeen[key] = true;
                inner(replacements[key], Prec.default);
            }
        return result.reduce((a, b) => a.concat(b));
    }
    function ensureAddr(state, addr) {
        if (addr & 1)
            return 2 /* Computed */;
        let idx = addr >> 1;
        let status = state.status[idx];
        if (status == 4 /* Computing */)
            throw new Error("Cyclic dependency between fields and/or facets");
        if (status & 2 /* Computed */)
            return status;
        state.status[idx] = 4 /* Computing */;
        let changed = state.config.dynamicSlots[idx](state, state.applying);
        return state.status[idx] = 2 /* Computed */ | changed;
    }
    function getAddr(state, addr) {
        return addr & 1 ? state.config.staticValues[addr >> 1] : state.values[addr >> 1];
    }

    const languageData = Facet.define();
    const allowMultipleSelections = Facet.define({
        combine: values => values.some(v => v),
        static: true
    });
    const lineSeparator = Facet.define({
        combine: values => values.length ? values[0] : undefined,
        static: true
    });
    const changeFilter = Facet.define();
    const transactionFilter = Facet.define();
    const transactionExtender = Facet.define();

    /// Annotations are tagged values that are used to add metadata to
    /// transactions in an extensible way. They should be used to model
    /// things that effect the entire transaction (such as its [time
    /// stamp](#state.Transaction^time) or information about its
    /// [origin](#state.Transaction^userEvent)). For effects that happen
    /// _alongside_ the other changes made by the transaction, [state
    /// effects](#state.StateEffect) are more appropriate.
    class Annotation {
        /// @internal
        constructor(type, value) {
            this.type = type;
            this.value = value;
        }
        /// Define a new type of annotation.
        static define() { return new AnnotationType(); }
    }
    /// Marker that identifies a type of [annotation](#state.Annotation).
    class AnnotationType {
        of(value) { return new Annotation(this, value); }
    }
    /// State effects can be used to represent additional effects
    /// associated with a [transaction](#state.Transaction.effects). They
    /// are often useful to model changes to custom [state
    /// fields](#state.StateField), when those changes aren't implicit in
    /// document or selection changes.
    class StateEffect {
        /// @internal
        constructor(
        /// @internal
        type, 
        /// The value of this effect.
        value) {
            this.type = type;
            this.value = value;
        }
        /// Map this effect through a position mapping. Will return
        /// `undefined` when that ends up deleting the effect.
        map(mapping) {
            let mapped = this.type.map(this.value, mapping);
            return mapped === undefined ? undefined : mapped == this.value ? this : new StateEffect(this.type, mapped);
        }
        /// Tells you whether this effect object is of a given
        /// [type](#state.StateEffectType).
        is(type) { return this.type == type; }
        /// Define a new effect type. The type parameter indicates the type
        /// of values that his effect holds.
        static define(spec = {}) {
            return new StateEffectType(spec.map || (v => v));
        }
        /// Map an array of effects through a change set.
        static mapEffects(effects, mapping) {
            if (!effects.length)
                return effects;
            let result = [];
            for (let effect of effects) {
                let mapped = effect.map(mapping);
                if (mapped)
                    result.push(mapped);
            }
            return result;
        }
    }
    /// Representation of a type of state effect. Defined with
    /// [`StateEffect.define`](#state.StateEffect^define).
    class StateEffectType {
        /// @internal
        constructor(
        // The `any` types in these function types are there to work
        // around TypeScript issue #37631, where the type guard on
        // `StateEffect.is` mysteriously stops working when these properly
        // have type `Value`.
        /// @internal
        map) {
            this.map = map;
        }
        /// Create a [state effect](#state.StateEffect) instance of this
        /// type.
        of(value) { return new StateEffect(this, value); }
    }
    /// Changes to the editor state are grouped into transactions.
    /// Typically, a user action creates a single transaction, which may
    /// contain any number of document changes, may change the selection,
    /// or have other effects. Create a transaction by calling
    /// [`EditorState.update`](#state.EditorState.update).
    class Transaction {
        /// @internal
        constructor(
        /// The state from which the transaction starts.
        startState, 
        /// The document changes made by this transaction.
        changes, 
        /// The selection set by this transaction, or undefined if it
        /// doesn't explicitly set a selection.
        selection, 
        /// The effects added to the transaction.
        effects, 
        /// @internal
        annotations, 
        /// Holds an object when this transaction
        /// [reconfigures](#state.ReconfigurationSpec) the state.
        reconfigure, 
        /// Whether the selection should be scrolled into view after this
        /// transaction is dispatched.
        scrollIntoView) {
            this.startState = startState;
            this.changes = changes;
            this.selection = selection;
            this.effects = effects;
            this.annotations = annotations;
            this.reconfigure = reconfigure;
            this.scrollIntoView = scrollIntoView;
            /// @internal
            this._doc = null;
            /// @internal
            this._state = null;
            if (selection)
                checkSelection(selection, changes.newLength);
            if (!annotations.some((a) => a.type == Transaction.time))
                this.annotations = annotations.concat(Transaction.time.of(Date.now()));
        }
        /// The new document produced by the transaction. (Mostly exposed so
        /// that [transaction filters](#state.EditorState^transactionFilter)
        /// can look at the new document without forcing an entire new state
        /// to be computed by accessing
        /// [`.state`](#state.Transaction.state).
        get newDoc() {
            return this._doc || (this._doc = this.changes.apply(this.startState.doc));
        }
        /// The new selection produced by the transaction. If
        /// [`this.selection`](#state.Transaction.selection) is undefined,
        /// this will [map](#state.EditorSelection.map) the start state's
        /// current selection through the changes made by the transaction.
        get newSelection() {
            return this.selection || this.startState.selection.map(this.changes);
        }
        /// The new state created by the transaction.
        get state() {
            if (!this._state)
                this.startState.applyTransaction(this);
            return this._state;
        }
        /// Get the value of the given annotation type, if any.
        annotation(type) {
            for (let ann of this.annotations)
                if (ann.type == type)
                    return ann.value;
            return undefined;
        }
        /// Indicates whether the transaction changed the document.
        get docChanged() { return !this.changes.empty; }
    }
    /// Annotation used to store transaction timestamps.
    Transaction.time = Annotation.define();
    /// Annotation used to associate a transaction with a user interface
    /// event. The view will set this to...
    ///
    ///  - `"input"` when the user types text
    ///  - `"delete"` when the user deletes the selection or text near the selection
    ///  - `"keyboardselection"` when moving the selection via the keyboard
    ///  - `"pointerselection"` when moving the selection through the pointing device
    ///  - `"paste"` when pasting content
    ///  - `"cut"` when cutting
    ///  - `"drop"` when content is inserted via drag-and-drop
    Transaction.userEvent = Annotation.define();
    /// Annotation indicating whether a transaction should be added to
    /// the undo history or not.
    Transaction.addToHistory = Annotation.define();
    function joinRanges(a, b) {
        let result = [];
        for (let iA = 0, iB = 0;;) {
            let from, to;
            if (iA < a.length && (iB == b.length || b[iB] >= a[iA])) {
                from = a[iA++];
                to = a[iA++];
            }
            else if (iB < b.length) {
                from = b[iB++];
                to = b[iB++];
            }
            else
                return result;
            if (!result.length || result[result.length - 1] < from)
                result.push(from, to);
            else if (result[result.length - 1] < to)
                result[result.length - 1] = to;
        }
    }
    function mergeTransaction(a, b, sequential) {
        var _a;
        let mapForA, mapForB, changes;
        if (sequential) {
            mapForA = b.changes;
            mapForB = ChangeSet.empty(b.changes.length);
            changes = a.changes.compose(b.changes);
        }
        else {
            mapForA = b.changes.map(a.changes);
            mapForB = a.changes.mapDesc(b.changes, true);
            changes = a.changes.compose(mapForA);
        }
        return {
            changes,
            selection: b.selection ? b.selection.map(mapForB) : (_a = a.selection) === null || _a === void 0 ? void 0 : _a.map(mapForA),
            effects: StateEffect.mapEffects(a.effects, mapForA).concat(StateEffect.mapEffects(b.effects, mapForB)),
            annotations: a.annotations.length ? a.annotations.concat(b.annotations) : b.annotations,
            scrollIntoView: a.scrollIntoView || b.scrollIntoView,
            reconfigure: !b.reconfigure ? a.reconfigure : b.reconfigure.full || !a.reconfigure ? b.reconfigure
                : Object.assign({}, a.reconfigure, b.reconfigure)
        };
    }
    function resolveTransactionInner(state, spec, docSize) {
        let reconf = spec.reconfigure;
        if (reconf && reconf.append) {
            reconf = Object.assign({}, reconf);
            let tag = typeof Symbol == "undefined" ? "__append" + Math.floor(Math.random() * 0xffffffff) : Symbol("appendConf");
            reconf[tag] = reconf.append;
            reconf.append = undefined;
        }
        let sel = spec.selection;
        return {
            changes: spec.changes instanceof ChangeSet ? spec.changes
                : ChangeSet.of(spec.changes || [], docSize, state.facet(lineSeparator)),
            selection: sel && (sel instanceof EditorSelection ? sel : EditorSelection.single(sel.anchor, sel.head)),
            effects: asArray(spec.effects),
            annotations: asArray(spec.annotations),
            scrollIntoView: !!spec.scrollIntoView,
            reconfigure: reconf
        };
    }
    function resolveTransaction(state, specs, filter) {
        let s = resolveTransactionInner(state, specs.length ? specs[0] : {}, state.doc.length);
        if (specs.length && specs[0].filter === false)
            filter = false;
        for (let i = 1; i < specs.length; i++) {
            if (specs[i].filter === false)
                filter = false;
            let seq = !!specs[i].sequential;
            s = mergeTransaction(s, resolveTransactionInner(state, specs[i], seq ? s.changes.newLength : state.doc.length), seq);
        }
        let tr = new Transaction(state, s.changes, s.selection, s.effects, s.annotations, s.reconfigure, s.scrollIntoView);
        return extendTransaction(filter ? filterTransaction(tr) : tr);
    }
    // Finish a transaction by applying filters if necessary.
    function filterTransaction(tr) {
        let state = tr.startState;
        // Change filters
        let result = true;
        for (let filter of state.facet(changeFilter)) {
            let value = filter(tr);
            if (value === false) {
                result = false;
                break;
            }
            if (Array.isArray(value))
                result = result === true ? value : joinRanges(result, value);
        }
        if (result !== true) {
            let changes, back;
            if (result === false) {
                back = tr.changes.invertedDesc;
                changes = ChangeSet.empty(state.doc.length);
            }
            else {
                let filtered = tr.changes.filter(result);
                changes = filtered.changes;
                back = filtered.filtered.invertedDesc;
            }
            tr = new Transaction(state, changes, tr.selection && tr.selection.map(back), StateEffect.mapEffects(tr.effects, back), tr.annotations, tr.reconfigure, tr.scrollIntoView);
        }
        // Transaction filters
        let filters = state.facet(transactionFilter);
        for (let i = filters.length - 1; i >= 0; i--) {
            let filtered = filters[i](tr);
            if (filtered instanceof Transaction)
                tr = filtered;
            else if (Array.isArray(filtered) && filtered.length == 1 && filtered[0] instanceof Transaction)
                tr = filtered[0];
            else
                tr = resolveTransaction(state, asArray(filtered), false);
        }
        return tr;
    }
    function extendTransaction(tr) {
        let state = tr.startState, extenders = state.facet(transactionExtender), spec = tr;
        for (let i = extenders.length - 1; i >= 0; i--) {
            let extension = extenders[i](tr);
            if (extension && Object.keys(extension).length)
                spec = mergeTransaction(tr, resolveTransactionInner(state, extension, tr.changes.newLength), true);
        }
        return spec == tr ? tr : new Transaction(state, tr.changes, tr.selection, spec.effects, spec.annotations, spec.reconfigure, spec.scrollIntoView);
    }
    const none = [];
    function asArray(value) {
        return value == null ? none : Array.isArray(value) ? value : [value];
    }

    /// This is used to [categorize](#state.EditorState.charCategorizer)
    /// characters into three categories—word characters, whitespace, and
    /// anything else. It is used do things like selecting by word.
    var CharCategory;
    (function (CharCategory) {
        CharCategory[CharCategory["Word"] = 0] = "Word";
        CharCategory[CharCategory["Space"] = 1] = "Space";
        CharCategory[CharCategory["Other"] = 2] = "Other";
    })(CharCategory || (CharCategory = {}));
    const nonASCIISingleCaseWordChar = /[\u00df\u0587\u0590-\u05f4\u0600-\u06ff\u3040-\u309f\u30a0-\u30ff\u3400-\u4db5\u4e00-\u9fcc\uac00-\ud7af]/;
    let wordChar;
    try {
        wordChar = new RegExp("[\\p{Alphabetic}\\p{Number}_]", "u");
    }
    catch (_) { }
    function hasWordChar(str) {
        if (wordChar)
            return wordChar.test(str);
        for (let i = 0; i < str.length; i++) {
            let ch = str[i];
            if (/\w/.test(ch) || ch > "\x80" && (ch.toUpperCase() != ch.toLowerCase() || nonASCIISingleCaseWordChar.test(ch)))
                return true;
        }
        return false;
    }
    function makeCategorizer(wordChars) {
        return (char) => {
            if (!/\S/.test(char))
                return CharCategory.Space;
            if (hasWordChar(char))
                return CharCategory.Word;
            for (let i = 0; i < wordChars.length; i++)
                if (char.indexOf(wordChars[i]) > -1)
                    return CharCategory.Word;
            return CharCategory.Other;
        };
    }

    /// The editor state class is a persistent (immutable) data structure.
    /// To update a state, you [create](#state.EditorState.update) a
    /// [transaction](#state.Transaction), which produces a _new_ state
    /// instance, without modifying the original object.
    ///
    /// As such, _never_ mutate properties of a state directly. That'll
    /// just break things.
    class EditorState {
        /// @internal
        constructor(
        /// @internal
        config, 
        /// The current document.
        doc, 
        /// The current selection.
        selection, tr = null) {
            this.config = config;
            this.doc = doc;
            this.selection = selection;
            /// @internal
            this.applying = null;
            this.status = config.statusTemplate.slice();
            if (tr && !tr.reconfigure) {
                this.values = tr.startState.values.slice();
            }
            else {
                this.values = config.dynamicSlots.map(_ => null);
                // Copy over old values for shared facets/fields if this is a reconfigure
                if (tr)
                    for (let id in config.address) {
                        let cur = config.address[id], prev = tr.startState.config.address[id];
                        if (prev != null && (cur & 1) == 0)
                            this.values[cur >> 1] = getAddr(tr.startState, prev);
                    }
            }
            this.applying = tr;
            // Fill in the computed state immediately, so that further queries
            // for it made during the update return this state
            if (tr)
                tr._state = this;
            for (let i = 0; i < this.config.dynamicSlots.length; i++)
                ensureAddr(this, i << 1);
            this.applying = null;
        }
        field(field, require = true) {
            let addr = this.config.address[field.id];
            if (addr == null) {
                if (require)
                    throw new RangeError("Field is not present in this state");
                return undefined;
            }
            ensureAddr(this, addr);
            return getAddr(this, addr);
        }
        /// Create a [transaction](#state.Transaction) that updates this
        /// state. Any number of [transaction specs](#state.TransactionSpec)
        /// can be passed. The [changes](#state.TransactionSpec.changes) (if
        /// any) of each spec are assumed to start in the _current_ document
        /// (not the document produced by previous specs), and its
        /// [selection](#state.TransactionSpec.selection) and
        /// [effects](#state.TransactionSpec.effects) are assumed to refer
        /// to the document created by its _own_ changes. The resulting
        /// transaction contains the combined effect of all the different
        /// specs. For things like
        /// [selection](#state.TransactionSpec.selection) or
        /// [reconfiguration](#state.TransactionSpec.reconfigure), later
        /// specs take precedence over earlier ones.
        update(...specs) {
            return resolveTransaction(this, specs, true);
        }
        /// @internal
        applyTransaction(tr) {
            let conf = this.config;
            if (tr.reconfigure)
                conf = Configuration.resolve(tr.reconfigure.full || conf.source, Object.assign(conf.replacements, tr.reconfigure, { full: undefined }), this);
            new EditorState(conf, tr.newDoc, tr.newSelection, tr);
        }
        /// Create a [transaction spec](#state.TransactionSpec) that
        /// replaces every selection range with the given content.
        replaceSelection(text) {
            if (typeof text == "string")
                text = this.toText(text);
            return this.changeByRange(range => ({ changes: { from: range.from, to: range.to, insert: text },
                range: EditorSelection.cursor(range.from + text.length) }));
        }
        /// Create a set of changes and a new selection by running the given
        /// function for each range in the active selection. The function
        /// can return an optional set of changes (in the coordinate space
        /// of the start document), plus an updated range (in the coordinate
        /// space of the document produced by the call's own changes). This
        /// method will merge all the changes and ranges into a single
        /// changeset and selection, and return it as a [transaction
        /// spec](#state.TransactionSpec), which can be passed to
        /// [`update`](#state.EditorState.update).
        changeByRange(f) {
            let sel = this.selection;
            let result1 = f(sel.ranges[0]);
            let changes = this.changes(result1.changes), ranges = [result1.range];
            let effects = asArray(result1.effects);
            for (let i = 1; i < sel.ranges.length; i++) {
                let result = f(sel.ranges[i]);
                let newChanges = this.changes(result.changes), newMapped = newChanges.map(changes);
                for (let j = 0; j < i; j++)
                    ranges[j] = ranges[j].map(newMapped);
                let mapBy = changes.mapDesc(newChanges, true);
                ranges.push(result.range.map(mapBy));
                changes = changes.compose(newMapped);
                effects = StateEffect.mapEffects(effects, newMapped).concat(StateEffect.mapEffects(asArray(result.effects), mapBy));
            }
            return {
                changes,
                selection: EditorSelection.create(ranges, sel.primaryIndex),
                effects
            };
        }
        /// Create a [change set](#state.ChangeSet) from the given change
        /// description, taking the state's document length and line
        /// separator into account.
        changes(spec = []) {
            if (spec instanceof ChangeSet)
                return spec;
            return ChangeSet.of(spec, this.doc.length, this.facet(EditorState.lineSeparator));
        }
        /// Using the state's [line
        /// separator](#state.EditorState^lineSeparator), create a
        /// [`Text`](#text.Text) instance from the given string.
        toText(string) {
            return Text.of(string.split(this.facet(EditorState.lineSeparator) || DefaultSplit));
        }
        /// Return the given range of the document as a string.
        sliceDoc(from = 0, to = this.doc.length) {
            return this.doc.sliceString(from, to, this.lineBreak);
        }
        /// Get the value of a state [facet](#state.Facet).
        facet(facet) {
            let addr = this.config.address[facet.id];
            if (addr == null)
                return facet.default;
            ensureAddr(this, addr);
            return getAddr(this, addr);
        }
        /// Convert this state to a JSON-serializable object.
        toJSON() {
            // FIXME plugin state serialization
            return {
                doc: this.sliceDoc(),
                selection: this.selection.toJSON()
            };
        }
        /// Deserialize a state from its JSON representation.
        static fromJSON(json, config = {}) {
            if (!json || typeof json.doc != "string")
                throw new RangeError("Invalid JSON representation for EditorState");
            return EditorState.create({
                doc: json.doc,
                selection: EditorSelection.fromJSON(json.selection),
                extensions: config.extensions
            });
        }
        /// Create a new state. You'll usually only need this when
        /// initializing an editor—updated states are created by applying
        /// transactions.
        static create(config = {}) {
            let configuration = Configuration.resolve(config.extensions || []);
            let doc = config.doc instanceof Text ? config.doc
                : Text.of((config.doc || "").split(configuration.staticFacet(EditorState.lineSeparator) || DefaultSplit));
            let selection = !config.selection ? EditorSelection.single(0)
                : config.selection instanceof EditorSelection ? config.selection
                    : EditorSelection.single(config.selection.anchor, config.selection.head);
            checkSelection(selection, doc.length);
            if (!configuration.staticFacet(allowMultipleSelections))
                selection = selection.asSingle();
            return new EditorState(configuration, doc, selection);
        }
        /// The size (in columns) of a tab in the document, determined by
        /// the [`tabSize`](#state.EditorState^tabSize) facet.
        get tabSize() { return this.facet(EditorState.tabSize); }
        /// Get the proper [line-break](#state.EditorState^lineSeparator)
        /// string for this state.
        get lineBreak() { return this.facet(EditorState.lineSeparator) || "\n"; }
        /// Look up a translation for the given phrase (via the
        /// [`phrases`](#state.EditorState^phrases) facet), or return the
        /// original string if no translation is found.
        phrase(phrase) {
            for (let map of this.facet(EditorState.phrases))
                if (Object.prototype.hasOwnProperty.call(map, phrase))
                    return map[phrase];
            return phrase;
        }
        /// Find the values for a given language data field, provided by the
        /// the [`languageData`](#state.EditorState^languageData) facet.
        languageDataAt(name, pos) {
            let values = [];
            for (let provider of this.facet(languageData)) {
                for (let result of provider(this, pos)) {
                    if (Object.prototype.hasOwnProperty.call(result, name))
                        values.push(result[name]);
                }
            }
            return values;
        }
        /// Return a function that can categorize strings (expected to
        /// represent a single [grapheme cluster](#text.nextClusterBreak))
        /// into one of:
        ///
        ///  - Word (contains an alphanumeric character or a character
        ///    explicitly listed in the local language's `"wordChars"`
        ///    language data, which should be a string)
        ///  - Space (contains only whitespace)
        ///  - Other (anything else)
        charCategorizer(at) {
            return makeCategorizer(this.languageDataAt("wordChars", at).join(""));
        }
    }
    /// A facet that, when enabled, causes the editor to allow multiple
    /// ranges to be selected. Be careful though, because by default the
    /// editor relies on the native DOM selection, which cannot handle
    /// multiple selections. An extension like
    /// [`drawSelection`](#view.drawSelection) can be used to make
    /// secondary selections visible to the user.
    EditorState.allowMultipleSelections = allowMultipleSelections;
    /// Configures the tab size to use in this state. The first
    /// (highest-precedence) value of the facet is used. If no value is
    /// given, this defaults to 4.
    EditorState.tabSize = Facet.define({
        combine: values => values.length ? values[0] : 4
    });
    /// The line separator to use. By default, any of `"\n"`, `"\r\n"`
    /// and `"\r"` is treated as a separator when splitting lines, and
    /// lines are joined with `"\n"`.
    ///
    /// When you configure a value here, only that precise separator
    /// will be used, allowing you to round-trip documents through the
    /// editor without normalizing line separators.
    EditorState.lineSeparator = lineSeparator;
    /// Registers translation phrases. The
    /// [`phrase`](#state.EditorState.phrase) method will look through
    /// all objects registered with this facet to find translations for
    /// its argument.
    EditorState.phrases = Facet.define();
    /// A facet used to register [language
    /// data](#state.EditorState.languageDataAt) providers.
    EditorState.languageData = languageData;
    /// Facet used to register change filters, which are called for each
    /// transaction (unless explicitly
    /// [disabled](#state.TransactionSpec.filter)), and can suppress
    /// part of the transaction's changes.
    ///
    /// Such a function can return `true` to indicate that it doesn't
    /// want to do anything, `false` to completely stop the changes in
    /// the transaction, or a set of ranges in which changes should be
    /// suppressed. Such ranges are represented as an array of numbers,
    /// with each pair of two number indicating the start and end of a
    /// range. So for example `[10, 20, 100, 110]` suppresses changes
    /// between 10 and 20, and between 100 and 110.
    EditorState.changeFilter = changeFilter;
    /// Facet used to register a hook that gets a chance to update or
    /// replace transaction specs before they are applied. This will
    /// only be applied for transactions that don't have
    /// [`filter`](#state.TransactionSpec.filter) set to `false`. You
    /// can either return a single (possibly the input transaction), or
    /// an array of specs (which will be combined in the same way as the
    /// arguments to [`EditorState.update`](#state.EditorState.update)).
    ///
    /// When possible, it is recommended to avoid accessing
    /// [`Transaction.state`](#state.Transaction.state) in a filter,
    /// since it will force creation of a state that will then be
    /// discarded again, if the transaction is actually filtered.
    ///
    /// (This functionality should be used with care. Indiscriminately
    /// modifying transaction is likely to break something or degrade
    /// the user experience.)
    EditorState.transactionFilter = transactionFilter;
    /// This is a more limited form of
    /// [`transactionFilter`](#state.EditorState^transactionFilter),
    /// which can only add
    /// [annotations](#state.TransactionSpec.annotations),
    /// [effects](#state.TransactionSpec.effects), and
    /// [configuration](#state.TransactionSpec.reconfigure) info. _But_,
    /// this type of filter runs even the transaction has disabled
    /// regular [filtering](#state.TransactionSpec.filter), making it
    /// suitable for effects that don't need to touch the changes or
    /// selection, but do want to process every transaction.
    ///
    /// Extenders run _after_ filters, when both are applied.
    EditorState.transactionExtender = transactionExtender;

    /// Utility function for combining behaviors to fill in a config
    /// object from an array of provided configs. Will, by default, error
    /// when a field gets two values that aren't ===-equal, but you can
    /// provide combine functions per field to do something else.
    function combineConfig(configs, defaults, // Should hold only the optional properties of Config, but I haven't managed to express that
    combine = {}) {
        let result = {};
        for (let config of configs)
            for (let key of Object.keys(config)) {
                let value = config[key], current = result[key];
                if (current === undefined)
                    result[key] = value;
                else if (current === value || value === undefined) ; // No conflict
                else if (Object.hasOwnProperty.call(combine, key))
                    result[key] = combine[key](current, value);
                else
                    throw new Error("Config merge conflict for field " + key);
            }
        for (let key in defaults)
            if (result[key] === undefined)
                result[key] = defaults[key];
        return result;
    }

    const C = "\u037c";
    const COUNT = typeof Symbol == "undefined" ? "__" + C : Symbol.for(C);
    const SET = typeof Symbol == "undefined" ? "__styleSet" + Math.floor(Math.random() * 1e8) : Symbol("styleSet");
    const top = typeof globalThis != "undefined" ? globalThis : typeof window != "undefined" ? window : {};

    // :: - Style modules encapsulate a set of CSS rules defined from
    // JavaScript. Their definitions are only available in a given DOM
    // root after it has been _mounted_ there with `StyleModule.mount`.
    //
    // Style modules should be created once and stored somewhere, as
    // opposed to re-creating them every time you need them. The amount of
    // CSS rules generated for a given DOM root is bounded by the amount
    // of style modules that were used. So to avoid leaking rules, don't
    // create these dynamically, but treat them as one-time allocations.
    class StyleModule {
      // :: (Object<Style>, ?{process: (string) → string, extend: (string, string) → string})
      // Create a style module from the given spec.
      //
      // When `process` is given, it is called on regular (non-`@`)
      // selector properties to provide the actual selector. When `extend`
      // is given, it is called when a property containing an `&` is
      // found, and should somehow combine the `&`-template (its first
      // argument) with the selector (its second argument) to produce an
      // extended selector.
      constructor(spec, options) {
        this.rules = [];
        let {process, extend} = options || {};

        function processSelector(selector) {
          if (/^@/.test(selector)) return [selector]
          let selectors = selector.split(",");
          return process ? selectors.map(process) : selectors
        }

        function render(selectors, spec, target) {
          let local = [], isAt = /^@(\w+)\b/.exec(selectors[0]);
          if (isAt && spec == null) return target.push(selectors[0] + ";")
          for (let prop in spec) {
            let value = spec[prop];
            if (/&/.test(prop)) {
              render(selectors.map(s => extend ? extend(prop, s) : prop.replace(/&/, s)), value, target);
            } else if (value && typeof value == "object") {
              if (!isAt) throw new RangeError("The value of a property (" + prop + ") should be a primitive value.")
              render(isAt[1] == "keyframes" ? [prop] : processSelector(prop), value, local);
            } else if (value != null) {
              local.push(prop.replace(/_.*/, "").replace(/[A-Z]/g, l => "-" + l.toLowerCase()) + ": " + value + ";");
            }
          }
          if (local.length || isAt && isAt[1] == "keyframes") target.push(selectors.join(",") + " {" + local.join(" ") + "}");
        }

        for (let prop in spec) render(processSelector(prop), spec[prop], this.rules);
      }

      // :: () → string
      // Returns a string containing the module's CSS rules.
      getRules() { return this.rules.join("\n") }

      // :: () → string
      // Generate a new unique CSS class name.
      static newName() {
        let id = top[COUNT] || 1;
        top[COUNT] = id + 1;
        return C + id.toString(36)
      }

      // :: (union<Document, ShadowRoot>, union<[StyleModule], StyleModule>)
      //
      // Mount the given set of modules in the given DOM root, which ensures
      // that the CSS rules defined by the module are available in that
      // context.
      //
      // Rules are only added to the document once per root.
      //
      // Rule order will follow the order of the modules, so that rules from
      // modules later in the array take precedence of those from earlier
      // modules. If you call this function multiple times for the same root
      // in a way that changes the order of already mounted modules, the old
      // order will be changed.
      static mount(root, modules) {
        (root[SET] || new StyleSet(root)).mount(Array.isArray(modules) ? modules : [modules]);
      }
    }

    let adoptedSet = null;

    class StyleSet {
      constructor(root) {
        if (root.adoptedStyleSheets && typeof CSSStyleSheet != "undefined") {
          if (adoptedSet) {
            root.adoptedStyleSheets = [adoptedSet.sheet].concat(root.adoptedStyleSheets);
            return root[SET] = adoptedSet
          }
          this.sheet = new CSSStyleSheet;
          root.adoptedStyleSheets = [this.sheet].concat(root.adoptedStyleSheets);
          adoptedSet = this;
        } else {
          this.styleTag = (root.ownerDocument || root).createElement("style");
          let target = root.head || root;
          target.insertBefore(this.styleTag, target.firstChild);
        }
        this.modules = [];
        root[SET] = this;
      }

      mount(modules) {
        let sheet = this.sheet;
        let pos = 0 /* Current rule offset */, j = 0; /* Index into this.modules */
        for (let i = 0; i < modules.length; i++) {
          let mod = modules[i], index = this.modules.indexOf(mod);
          if (index < j && index > -1) { // Ordering conflict
            this.modules.splice(index, 1);
            j--;
            index = -1;
          }
          if (index == -1) {
            this.modules.splice(j++, 0, mod);
            if (sheet) for (let k = 0; k < mod.rules.length; k++)
              sheet.insertRule(mod.rules[k], pos++);
          } else {
            while (j < index) pos += this.modules[j++].rules.length;
            pos += mod.rules.length;
            j++;
          }
        }

        if (!sheet) {
          let text = "";
          for (let i = 0; i < this.modules.length; i++)
            text += this.modules[i].getRules() + "\n";
          this.styleTag.textContent = text;
        }
      }
    }

    // Style::Object<union<Style,string>>
    //
    // A style is an object that, in the simple case, maps CSS property
    // names to strings holding their values, as in `{color: "red",
    // fontWeight: "bold"}`. The property names can be given in
    // camel-case—the library will insert a dash before capital letters
    // when converting them to CSS.
    //
    // If you include an underscore in a property name, it and everything
    // after it will be removed from the output, which can be useful when
    // providing a property multiple times, for browser compatibility
    // reasons.
    //
    // A property in a style object can also be a sub-selector, which
    // extends the current context to add a pseudo-selector or a child
    // selector. Such a property should contain a `&` character, which
    // will be replaced by the current selector. For example `{"&:before":
    // {content: '"hi"'}}`. Sub-selectors and regular properties can
    // freely be mixed in a given object. Any property containing a `&` is
    // assumed to be a sub-selector.
    //
    // Finally, a property can specify an @-block to be wrapped around the
    // styles defined inside the object that's the property's value. For
    // example to create a media query you can do `{"@media screen and
    // (min-width: 400px)": {...}}`.

    /// Each range is associated with a value, which must inherit from
    /// this class.
    class RangeValue {
        /// Compare this value with another value. The default
        /// implementation compares by identity.
        eq(other) { return this == other; }
        /// Create a [range](#rangeset.Range) with this value.
        range(from, to = from) { return new Range(from, to, this); }
    }
    RangeValue.prototype.startSide = RangeValue.prototype.endSide = 0;
    RangeValue.prototype.point = false;
    RangeValue.prototype.mapMode = MapMode.TrackDel;
    /// A range associates a value with a range of positions.
    class Range {
        /// @internal
        constructor(
        /// The range's start position.
        from, 
        /// Its end position.
        to, 
        /// The value associated with this range.
        value) {
            this.from = from;
            this.to = to;
            this.value = value;
        }
    }
    function cmpRange(a, b) {
        return a.from - b.from || a.value.startSide - b.value.startSide;
    }
    // The maximum amount of ranges to store in a single chunk
    const ChunkSize = 250, 
    // Chunks with points of this size are never skipped during
    // compare, since moving past those points is likely to speed
    // up, rather than slow down, the comparison.
    BigPointSize = 500, 
    // A large (fixnum) value to use for max/min values.
    Far = 1e9;
    class Chunk {
        constructor(from, to, value, 
        // Chunks are marked with the largest point that occurs
        // in them (or -1 for no points), so that scans that are
        // only interested in points (such as the
        // heightmap-related logic) can skip range-only chunks.
        maxPoint) {
            this.from = from;
            this.to = to;
            this.value = value;
            this.maxPoint = maxPoint;
        }
        get length() { return this.to[this.to.length - 1]; }
        // With side == -1, return the first index where to >= pos. When
        // side == 1, the first index where from > pos.
        findIndex(pos, end, side = end * Far, startAt = 0) {
            if (pos <= 0)
                return startAt;
            let arr = end < 0 ? this.to : this.from;
            for (let lo = startAt, hi = arr.length;;) {
                if (lo == hi)
                    return lo;
                let mid = (lo + hi) >> 1;
                let diff = arr[mid] - pos || (end < 0 ? this.value[mid].startSide : this.value[mid].endSide) - side;
                if (mid == lo)
                    return diff >= 0 ? lo : hi;
                if (diff >= 0)
                    hi = mid;
                else
                    lo = mid + 1;
            }
        }
        between(offset, from, to, f) {
            for (let i = this.findIndex(from, -1), e = this.findIndex(to, 1, undefined, i); i < e; i++)
                if (f(this.from[i] + offset, this.to[i] + offset, this.value[i]) === false)
                    return false;
        }
        map(offset, changes) {
            let value = [], from = [], to = [], newPos = -1, maxPoint = -1;
            for (let i = 0; i < this.value.length; i++) {
                let val = this.value[i], curFrom = this.from[i] + offset, curTo = this.to[i] + offset, newFrom, newTo;
                if (curFrom == curTo) {
                    let mapped = changes.mapPos(curFrom, val.startSide, val.mapMode);
                    if (mapped == null)
                        continue;
                    newFrom = newTo = mapped;
                }
                else {
                    newFrom = changes.mapPos(curFrom, val.startSide);
                    newTo = changes.mapPos(curTo, val.endSide);
                    if (newFrom > newTo || newFrom == newTo && val.startSide > 0 && val.endSide <= 0)
                        continue;
                }
                if ((newTo - newFrom || val.endSide - val.startSide) < 0)
                    continue;
                if (newPos < 0)
                    newPos = newFrom;
                if (val.point)
                    maxPoint = Math.max(maxPoint, newTo - newFrom);
                value.push(val);
                from.push(newFrom - newPos);
                to.push(newTo - newPos);
            }
            return { mapped: value.length ? new Chunk(from, to, value, maxPoint) : null, pos: newPos };
        }
    }
    /// A range set stores a collection of [ranges](#rangeset.Range) in a
    /// way that makes them efficient to [map](#rangeset.RangeSet.map) and
    /// [update](#rangeset.RangeSet.update). This is an immutable data
    /// structure.
    class RangeSet {
        /// @internal
        constructor(
        /// @internal
        chunkPos, 
        /// @internal
        chunk, 
        /// @internal
        nextLayer = RangeSet.empty, 
        /// @internal
        maxPoint) {
            this.chunkPos = chunkPos;
            this.chunk = chunk;
            this.nextLayer = nextLayer;
            this.maxPoint = maxPoint;
        }
        /// @internal
        get length() {
            let last = this.chunk.length - 1;
            return last < 0 ? 0 : Math.max(this.chunkEnd(last), this.nextLayer.length);
        }
        /// @internal
        get size() {
            if (this == RangeSet.empty)
                return 0;
            let size = this.nextLayer.size;
            for (let chunk of this.chunk)
                size += chunk.value.length;
            return size;
        }
        /// @internal
        chunkEnd(index) {
            return this.chunkPos[index] + this.chunk[index].length;
        }
        /// Update the range set, optionally adding new ranges or filtering
        /// out existing ones.
        update(updateSpec) {
            let { add = [], sort = false, filter, filterFrom = 0, filterTo = this.length } = updateSpec;
            if (add.length == 0 && !filter)
                return this;
            if (sort)
                add.slice().sort(cmpRange);
            if (this == RangeSet.empty)
                return add.length ? RangeSet.of(add) : this;
            let cur = new LayerCursor(this, null, -1).goto(0), i = 0, spill = [];
            let builder = new RangeSetBuilder();
            while (cur.value || i < add.length) {
                if (i < add.length && (cur.from - add[i].from || cur.startSide - add[i].value.startSide) >= 0) {
                    let range = add[i++];
                    if (!builder.addInner(range.from, range.to, range.value))
                        spill.push(range);
                }
                else if (cur.rangeIndex == 1 && cur.chunkIndex < this.chunk.length &&
                    (i == add.length || this.chunkEnd(cur.chunkIndex) < add[i].from) &&
                    (!filter || filterFrom > this.chunkEnd(cur.chunkIndex) || filterTo < this.chunkPos[cur.chunkIndex]) &&
                    builder.addChunk(this.chunkPos[cur.chunkIndex], this.chunk[cur.chunkIndex])) {
                    cur.nextChunk();
                }
                else {
                    if (!filter || filterFrom > cur.to || filterTo < cur.from || filter(cur.from, cur.to, cur.value)) {
                        if (!builder.addInner(cur.from, cur.to, cur.value))
                            spill.push(new Range(cur.from, cur.to, cur.value));
                    }
                    cur.next();
                }
            }
            return builder.finishInner(this.nextLayer == RangeSet.empty && !spill.length ? RangeSet.empty
                : this.nextLayer.update({ add: spill, filter, filterFrom, filterTo }));
        }
        /// Map this range set through a set of changes, return the new set.
        map(changes) {
            if (changes.length == 0 || this == RangeSet.empty)
                return this;
            let chunks = [], chunkPos = [], maxPoint = -1;
            for (let i = 0; i < this.chunk.length; i++) {
                let start = this.chunkPos[i], chunk = this.chunk[i];
                let touch = changes.touchesRange(start, start + chunk.length);
                if (touch === false) {
                    maxPoint = Math.max(maxPoint, chunk.maxPoint);
                    chunks.push(chunk);
                    chunkPos.push(changes.mapPos(start));
                }
                else if (touch === true) {
                    let { mapped, pos } = chunk.map(start, changes);
                    if (mapped) {
                        maxPoint = Math.max(maxPoint, mapped.maxPoint);
                        chunks.push(mapped);
                        chunkPos.push(pos);
                    }
                }
            }
            let next = this.nextLayer.map(changes);
            return chunks.length == 0 ? next : new RangeSet(chunkPos, chunks, next, maxPoint);
        }
        /// Iterate over the ranges that touch the region `from` to `to`,
        /// calling `f` for each. There is no guarantee that the ranges will
        /// be reported in any order. When the callback returns `false`,
        /// iteration stops.
        between(from, to, f) {
            if (this == RangeSet.empty)
                return;
            for (let i = 0; i < this.chunk.length; i++) {
                let start = this.chunkPos[i], chunk = this.chunk[i];
                if (to >= start && from <= start + chunk.length &&
                    chunk.between(start, from - start, to - start, f) === false)
                    return;
            }
            this.nextLayer.between(from, to, f);
        }
        /// Iterate over the ranges in this set, in order, including all
        /// ranges that end at or after `from`.
        iter(from = 0) {
            return HeapCursor.from([this]).goto(from);
        }
        /// Iterate over the given sets, starting from `from`.
        static iter(sets, from = 0) {
            return HeapCursor.from(sets).goto(from);
        }
        /// Iterate over two groups of sets, calling methods on `comparator`
        /// to notify it of possible differences. `textDiff` indicates how
        /// the underlying data changed between these ranges, and is needed
        /// to synchronize the iteration. `from` and `to` are coordinates in
        /// the _new_ space, after these changes.
        static compare(oldSets, newSets, textDiff, comparator) {
            var _a;
            let minPoint = (_a = comparator.minPointSize) !== null && _a !== void 0 ? _a : -1;
            let a = oldSets.filter(set => set.maxPoint >= BigPointSize ||
                set != RangeSet.empty && newSets.indexOf(set) < 0 && set.maxPoint >= minPoint);
            let b = newSets.filter(set => set.maxPoint >= BigPointSize ||
                set != RangeSet.empty && oldSets.indexOf(set) < 0 && set.maxPoint >= minPoint);
            let sharedChunks = findSharedChunks(a, b);
            let sideA = new SpanCursor(a, sharedChunks, minPoint);
            let sideB = new SpanCursor(b, sharedChunks, minPoint);
            textDiff.iterGaps((fromA, fromB, length) => compare(sideA, fromA, sideB, fromB, length, comparator));
            if (textDiff.empty && textDiff.length == 0)
                compare(sideA, 0, sideB, 0, 0, comparator);
        }
        /// Iterate over a group of range sets at the same time, notifying
        /// the iterator about the ranges covering every given piece of
        /// content. Returns the open count (see
        /// [`SpanIterator.span`](#rangeset.SpanIterator.span)) at the end
        /// of the iteration.
        static spans(sets, from, to, iterator) {
            var _a;
            let cursor = new SpanCursor(sets, null, (_a = iterator.minPointSize) !== null && _a !== void 0 ? _a : -1).goto(from), pos = from;
            let open = cursor.openStart;
            for (;;) {
                let curTo = Math.min(cursor.to, to);
                if (cursor.point) {
                    iterator.point(pos, curTo, cursor.point, cursor.activeForPoint(cursor.to), open);
                    open = cursor.openEnd(curTo) + (cursor.to > curTo ? 1 : 0);
                }
                else if (curTo > pos) {
                    iterator.span(pos, curTo, cursor.active, open);
                    open = cursor.openEnd(curTo);
                }
                if (cursor.to > to)
                    break;
                pos = cursor.to;
                cursor.next();
            }
            return open;
        }
        /// Create a range set for the given range or array of ranges. By
        /// default, this expects the ranges to be _sorted_ (by start
        /// position and, if two start at the same position,
        /// `value.startSide`). You can pass `true` as second argument to
        /// cause the method to sort them.
        static of(ranges, sort = false) {
            let build = new RangeSetBuilder();
            for (let range of ranges instanceof Range ? [ranges] : sort ? ranges.slice().sort(cmpRange) : ranges)
                build.add(range.from, range.to, range.value);
            return build.finish();
        }
    }
    /// The empty set of ranges.
    RangeSet.empty = new RangeSet([], [], null, -1);
    RangeSet.empty.nextLayer = RangeSet.empty;
    /// A range set builder is a data structure that helps build up a
    /// [range set](#rangeset.RangeSet) directly, without first allocating
    /// an array of [`Range`](#rangeset.Range) objects.
    class RangeSetBuilder {
        /// Create an empty builder.
        constructor() {
            this.chunks = [];
            this.chunkPos = [];
            this.chunkStart = -1;
            this.last = null;
            this.lastFrom = -Far;
            this.lastTo = -Far;
            this.from = [];
            this.to = [];
            this.value = [];
            this.maxPoint = -1;
            this.setMaxPoint = -1;
            this.nextLayer = null;
        }
        finishChunk(newArrays) {
            this.chunks.push(new Chunk(this.from, this.to, this.value, this.maxPoint));
            this.chunkPos.push(this.chunkStart);
            this.chunkStart = -1;
            this.setMaxPoint = Math.max(this.setMaxPoint, this.maxPoint);
            this.maxPoint = -1;
            if (newArrays) {
                this.from = [];
                this.to = [];
                this.value = [];
            }
        }
        /// Add a range. Ranges should be added in sorted (by `from` and
        /// `value.startSide`) order.
        add(from, to, value) {
            if (!this.addInner(from, to, value))
                (this.nextLayer || (this.nextLayer = new RangeSetBuilder)).add(from, to, value);
        }
        /// @internal
        addInner(from, to, value) {
            let diff = from - this.lastTo || value.startSide - this.last.endSide;
            if (diff <= 0 && (from - this.lastFrom || value.startSide - this.last.startSide) < 0)
                throw new Error("Ranges must be added sorted by `from` position and `startSide`");
            if (diff < 0)
                return false;
            if (this.from.length == ChunkSize)
                this.finishChunk(true);
            if (this.chunkStart < 0)
                this.chunkStart = from;
            this.from.push(from - this.chunkStart);
            this.to.push(to - this.chunkStart);
            this.last = value;
            this.lastFrom = from;
            this.lastTo = to;
            this.value.push(value);
            if (value.point)
                this.maxPoint = Math.max(this.maxPoint, to - from);
            return true;
        }
        /// @internal
        addChunk(from, chunk) {
            if ((from - this.lastTo || chunk.value[0].startSide - this.last.endSide) < 0)
                return false;
            if (this.from.length)
                this.finishChunk(true);
            this.setMaxPoint = Math.max(this.setMaxPoint, chunk.maxPoint);
            this.chunks.push(chunk);
            this.chunkPos.push(from);
            let last = chunk.value.length - 1;
            this.last = chunk.value[last];
            this.lastFrom = chunk.from[last] + from;
            this.lastTo = chunk.to[last] + from;
            return true;
        }
        /// Finish the range set. Returns the new set. The builder can't be
        /// used anymore after this has been called.
        finish() { return this.finishInner(RangeSet.empty); }
        /// @internal
        finishInner(next) {
            if (this.from.length)
                this.finishChunk(false);
            if (this.chunks.length == 0)
                return next;
            let result = new RangeSet(this.chunkPos, this.chunks, this.nextLayer ? this.nextLayer.finishInner(next) : next, this.setMaxPoint);
            this.from = null; // Make sure further `add` calls produce errors
            return result;
        }
    }
    function findSharedChunks(a, b) {
        let inA = new Map();
        for (let set of a)
            for (let i = 0; i < set.chunk.length; i++)
                if (set.chunk[i].maxPoint < BigPointSize)
                    inA.set(set.chunk[i], set.chunkPos[i]);
        let shared = new Set();
        for (let set of b)
            for (let i = 0; i < set.chunk.length; i++)
                if (inA.get(set.chunk[i]) == set.chunkPos[i])
                    shared.add(set.chunk[i]);
        return shared;
    }
    class LayerCursor {
        constructor(layer, skip, minPoint, rank = 0) {
            this.layer = layer;
            this.skip = skip;
            this.minPoint = minPoint;
            this.rank = rank;
        }
        get startSide() { return this.value ? this.value.startSide : 0; }
        get endSide() { return this.value ? this.value.endSide : 0; }
        goto(pos, side = -Far) {
            this.chunkIndex = this.rangeIndex = 0;
            this.gotoInner(pos, side, false);
            return this;
        }
        gotoInner(pos, side, forward) {
            while (this.chunkIndex < this.layer.chunk.length) {
                let next = this.layer.chunk[this.chunkIndex];
                if (!(this.skip && this.skip.has(next) ||
                    this.layer.chunkEnd(this.chunkIndex) < pos ||
                    next.maxPoint < this.minPoint))
                    break;
                this.chunkIndex++;
                forward = false;
            }
            let rangeIndex = this.chunkIndex == this.layer.chunk.length ? 0
                : this.layer.chunk[this.chunkIndex].findIndex(pos - this.layer.chunkPos[this.chunkIndex], -1, side);
            if (!forward || this.rangeIndex < rangeIndex)
                this.rangeIndex = rangeIndex;
            this.next();
        }
        forward(pos, side) {
            if ((this.to - pos || this.endSide - side) < 0)
                this.gotoInner(pos, side, true);
        }
        next() {
            for (;;) {
                if (this.chunkIndex == this.layer.chunk.length) {
                    this.from = this.to = Far;
                    this.value = null;
                    break;
                }
                else {
                    let chunkPos = this.layer.chunkPos[this.chunkIndex], chunk = this.layer.chunk[this.chunkIndex];
                    let from = chunkPos + chunk.from[this.rangeIndex];
                    this.from = from;
                    this.to = chunkPos + chunk.to[this.rangeIndex];
                    this.value = chunk.value[this.rangeIndex];
                    if (++this.rangeIndex == chunk.value.length) {
                        this.chunkIndex++;
                        if (this.skip) {
                            while (this.chunkIndex < this.layer.chunk.length && this.skip.has(this.layer.chunk[this.chunkIndex]))
                                this.chunkIndex++;
                        }
                        this.rangeIndex = 0;
                    }
                    if (this.minPoint < 0 || this.value.point && this.to - this.from >= this.minPoint)
                        break;
                }
            }
        }
        nextChunk() {
            this.chunkIndex++;
            this.rangeIndex = 0;
            this.next();
        }
        compare(other) {
            return this.from - other.from || this.startSide - other.startSide || this.to - other.to || this.endSide - other.endSide;
        }
    }
    class HeapCursor {
        constructor(heap) {
            this.heap = heap;
        }
        static from(sets, skip = null, minPoint = -1) {
            let heap = [];
            for (let i = 0; i < sets.length; i++) {
                for (let cur = sets[i]; cur != RangeSet.empty; cur = cur.nextLayer) {
                    if (cur.maxPoint >= minPoint)
                        heap.push(new LayerCursor(cur, skip, minPoint, i));
                }
            }
            return heap.length == 1 ? heap[0] : new HeapCursor(heap);
        }
        get startSide() { return this.value ? this.value.startSide : 0; }
        goto(pos, side = -Far) {
            for (let cur of this.heap)
                cur.goto(pos, side);
            for (let i = this.heap.length >> 1; i >= 0; i--)
                heapBubble(this.heap, i);
            this.next();
            return this;
        }
        forward(pos, side) {
            for (let cur of this.heap)
                cur.forward(pos, side);
            for (let i = this.heap.length >> 1; i >= 0; i--)
                heapBubble(this.heap, i);
            if ((this.to - pos || this.value.endSide - side) < 0)
                this.next();
        }
        next() {
            if (this.heap.length == 0) {
                this.from = this.to = Far;
                this.value = null;
                this.rank = -1;
            }
            else {
                let top = this.heap[0];
                this.from = top.from;
                this.to = top.to;
                this.value = top.value;
                this.rank = top.rank;
                if (top.value)
                    top.next();
                heapBubble(this.heap, 0);
            }
        }
    }
    function heapBubble(heap, index) {
        for (let cur = heap[index];;) {
            let childIndex = (index << 1) + 1;
            if (childIndex >= heap.length)
                break;
            let child = heap[childIndex];
            if (childIndex + 1 < heap.length && child.compare(heap[childIndex + 1]) >= 0) {
                child = heap[childIndex + 1];
                childIndex++;
            }
            if (cur.compare(child) < 0)
                break;
            heap[childIndex] = cur;
            heap[index] = child;
            index = childIndex;
        }
    }
    class SpanCursor {
        constructor(sets, skip, minPoint) {
            this.minPoint = minPoint;
            this.active = [];
            this.activeTo = [];
            this.activeRank = [];
            this.minActive = -1;
            // A currently active point range, if any
            this.point = null;
            this.pointFrom = 0;
            this.pointRank = 0;
            this.to = -Far;
            this.endSide = 0;
            this.openStart = -1;
            this.cursor = HeapCursor.from(sets, skip, minPoint);
        }
        goto(pos, side = -Far) {
            this.cursor.goto(pos, side);
            this.active.length = this.activeTo.length = this.activeRank.length = 0;
            this.minActive = -1;
            this.to = pos;
            this.endSide = side;
            this.openStart = -1;
            this.next();
            return this;
        }
        forward(pos, side) {
            while (this.minActive > -1 && (this.activeTo[this.minActive] - pos || this.active[this.minActive].endSide - side) < 0)
                this.removeActive(this.minActive);
            this.cursor.forward(pos, side);
        }
        removeActive(index) {
            remove(this.active, index);
            remove(this.activeTo, index);
            remove(this.activeRank, index);
            this.minActive = findMinIndex(this.active, this.activeTo);
        }
        addActive(trackOpen) {
            let i = 0, { value, to, rank } = this.cursor;
            while (i < this.activeRank.length && this.activeRank[i] <= rank)
                i++;
            insert(this.active, i, value);
            insert(this.activeTo, i, to);
            insert(this.activeRank, i, rank);
            if (trackOpen)
                insert(trackOpen, i, this.cursor.from);
            this.minActive = findMinIndex(this.active, this.activeTo);
        }
        // After calling this, if `this.point` != null, the next range is a
        // point. Otherwise, it's a regular range, covered by `this.active`.
        next() {
            let from = this.to;
            this.point = null;
            let trackOpen = this.openStart < 0 ? [] : null, trackExtra = 0;
            for (;;) {
                let a = this.minActive;
                if (a > -1 && (this.activeTo[a] - this.cursor.from || this.active[a].endSide - this.cursor.startSide) < 0) {
                    if (this.activeTo[a] > from) {
                        this.to = this.activeTo[a];
                        this.endSide = this.active[a].endSide;
                        break;
                    }
                    this.removeActive(a);
                    if (trackOpen)
                        remove(trackOpen, a);
                }
                else if (!this.cursor.value) {
                    this.to = this.endSide = Far;
                    break;
                }
                else if (this.cursor.from > from) {
                    this.to = this.cursor.from;
                    this.endSide = this.cursor.startSide;
                    break;
                }
                else {
                    let nextVal = this.cursor.value;
                    if (!nextVal.point) { // Opening a range
                        this.addActive(trackOpen);
                        this.cursor.next();
                    }
                    else { // New point
                        this.point = nextVal;
                        this.pointFrom = this.cursor.from;
                        this.pointRank = this.cursor.rank;
                        this.to = this.cursor.to;
                        this.endSide = nextVal.endSide;
                        if (this.cursor.from < from)
                            trackExtra = 1;
                        this.cursor.next();
                        if (this.to > from)
                            this.forward(this.to, this.endSide);
                        break;
                    }
                }
            }
            if (trackOpen) {
                let openStart = 0;
                while (openStart < trackOpen.length && trackOpen[openStart] < from)
                    openStart++;
                this.openStart = openStart + trackExtra;
            }
        }
        activeForPoint(to) {
            if (!this.active.length)
                return this.active;
            let active = [];
            for (let i = 0; i < this.active.length; i++) {
                if (this.activeRank[i] > this.pointRank)
                    break;
                if (this.activeTo[i] > to || this.activeTo[i] == to && this.active[i].endSide > this.point.endSide)
                    active.push(this.active[i]);
            }
            return active;
        }
        openEnd(to) {
            let open = 0;
            while (open < this.activeTo.length && this.activeTo[open] > to)
                open++;
            return open;
        }
    }
    function compare(a, startA, b, startB, length, comparator) {
        a.goto(startA);
        b.goto(startB);
        let endB = startB + length;
        let pos = startB, dPos = startB - startA;
        for (;;) {
            let diff = (a.to + dPos) - b.to || a.endSide - b.endSide;
            let end = diff < 0 ? a.to + dPos : b.to, clipEnd = Math.min(end, endB);
            if (a.point || b.point) {
                if (!(a.point && b.point && (a.point == b.point || a.point.eq(b.point))))
                    comparator.comparePoint(pos, clipEnd, a.point, b.point);
            }
            else {
                if (clipEnd > pos && !sameValues(a.active, b.active))
                    comparator.compareRange(pos, clipEnd, a.active, b.active);
            }
            if (end > endB)
                break;
            pos = end;
            if (diff <= 0)
                a.next();
            if (diff >= 0)
                b.next();
        }
    }
    function sameValues(a, b) {
        if (a.length != b.length)
            return false;
        for (let i = 0; i < a.length; i++)
            if (a[i] != b[i] && !a[i].eq(b[i]))
                return false;
        return true;
    }
    function remove(array, index) {
        for (let i = index, e = array.length - 1; i < e; i++)
            array[i] = array[i + 1];
        array.pop();
    }
    function insert(array, index, value) {
        for (let i = array.length - 1; i >= index; i--)
            array[i + 1] = array[i];
        array[index] = value;
    }
    function findMinIndex(value, array) {
        let found = -1, foundPos = Far;
        for (let i = 0; i < array.length; i++)
            if ((array[i] - foundPos || value[i].endSide - value[found].endSide) < 0) {
                found = i;
                foundPos = array[i];
            }
        return found;
    }

    var base = {
      8: "Backspace",
      9: "Tab",
      10: "Enter",
      12: "NumLock",
      13: "Enter",
      16: "Shift",
      17: "Control",
      18: "Alt",
      20: "CapsLock",
      27: "Escape",
      32: " ",
      33: "PageUp",
      34: "PageDown",
      35: "End",
      36: "Home",
      37: "ArrowLeft",
      38: "ArrowUp",
      39: "ArrowRight",
      40: "ArrowDown",
      44: "PrintScreen",
      45: "Insert",
      46: "Delete",
      59: ";",
      61: "=",
      91: "Meta",
      92: "Meta",
      106: "*",
      107: "+",
      108: ",",
      109: "-",
      110: ".",
      111: "/",
      144: "NumLock",
      145: "ScrollLock",
      160: "Shift",
      161: "Shift",
      162: "Control",
      163: "Control",
      164: "Alt",
      165: "Alt",
      173: "-",
      186: ";",
      187: "=",
      188: ",",
      189: "-",
      190: ".",
      191: "/",
      192: "`",
      219: "[",
      220: "\\",
      221: "]",
      222: "'",
      229: "q"
    };

    var shift = {
      48: ")",
      49: "!",
      50: "@",
      51: "#",
      52: "$",
      53: "%",
      54: "^",
      55: "&",
      56: "*",
      57: "(",
      59: ":",
      61: "+",
      173: "_",
      186: ":",
      187: "+",
      188: "<",
      189: "_",
      190: ">",
      191: "?",
      192: "~",
      219: "{",
      220: "|",
      221: "}",
      222: "\"",
      229: "Q"
    };

    var chrome = typeof navigator != "undefined" && /Chrome\/(\d+)/.exec(navigator.userAgent);
    var safari = typeof navigator != "undefined" && /Apple Computer/.test(navigator.vendor);
    var gecko = typeof navigator != "undefined" && /Gecko\/\d+/.test(navigator.userAgent);
    var mac = typeof navigator != "undefined" && /Mac/.test(navigator.platform);
    var ie = typeof navigator != "undefined" && /MSIE \d|Trident\/(?:[7-9]|\d{2,})\..*rv:(\d+)/.exec(navigator.userAgent);
    var brokenModifierNames = chrome && (mac || +chrome[1] < 57) || gecko && mac;

    // Fill in the digit keys
    for (var i = 0; i < 10; i++) base[48 + i] = base[96 + i] = String(i);

    // The function keys
    for (var i = 1; i <= 24; i++) base[i + 111] = "F" + i;

    // And the alphabetic keys
    for (var i = 65; i <= 90; i++) {
      base[i] = String.fromCharCode(i + 32);
      shift[i] = String.fromCharCode(i);
    }

    // For each code that doesn't have a shift-equivalent, copy the base name
    for (var code in base) if (!shift.hasOwnProperty(code)) shift[code] = base[code];

    function keyName(event) {
      // Don't trust event.key in Chrome when there are modifiers until
      // they fix https://bugs.chromium.org/p/chromium/issues/detail?id=633838
      var ignoreKey = brokenModifierNames && (event.ctrlKey || event.altKey || event.metaKey) ||
        (safari || ie) && event.shiftKey && event.key && event.key.length == 1;
      var name = (!ignoreKey && event.key) ||
        (event.shiftKey ? shift : base)[event.keyCode] ||
        event.key || "Unidentified";
      // Edge sometimes produces wrong names (Issue #3)
      if (name == "Esc") name = "Escape";
      if (name == "Del") name = "Delete";
      // https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/8860571/
      if (name == "Left") name = "ArrowLeft";
      if (name == "Up") name = "ArrowUp";
      if (name == "Right") name = "ArrowRight";
      if (name == "Down") name = "ArrowDown";
      return name
    }

    let [nav, doc] = typeof navigator != "undefined"
        ? [navigator, document]
        : [{ userAgent: "", vendor: "", platform: "" }, { documentElement: { style: {} } }];
    const ie_edge = /Edge\/(\d+)/.exec(nav.userAgent);
    const ie_upto10 = /MSIE \d/.test(nav.userAgent);
    const ie_11up = /Trident\/(?:[7-9]|\d{2,})\..*rv:(\d+)/.exec(nav.userAgent);
    const ie$1 = !!(ie_upto10 || ie_11up || ie_edge);
    const gecko$1 = !ie$1 && /gecko\/(\d+)/i.test(nav.userAgent);
    const chrome$1 = !ie$1 && /Chrome\/(\d+)/.exec(nav.userAgent);
    const webkit = "webkitFontSmoothing" in doc.documentElement.style;
    var browser = {
        mac: /Mac/.test(nav.platform),
        ie: ie$1,
        ie_version: ie_upto10 ? doc.documentMode || 6 : ie_11up ? +ie_11up[1] : ie_edge ? +ie_edge[1] : 0,
        gecko: gecko$1,
        gecko_version: gecko$1 ? +(/Firefox\/(\d+)/.exec(nav.userAgent) || [0, 0])[1] : 0,
        chrome: !!chrome$1,
        chrome_version: chrome$1 ? +chrome$1[1] : 0,
        ios: !ie$1 && /AppleWebKit/.test(nav.userAgent) && /Mobile\/\w+/.test(nav.userAgent),
        android: /Android\b/.test(nav.userAgent),
        webkit,
        safari: /Apple Computer/.test(nav.vendor),
        webkit_version: webkit ? +(/\bAppleWebKit\/(\d+)/.exec(navigator.userAgent) || [0, 0])[1] : 0,
        tabSize: doc.documentElement.style.tabSize != null ? "tab-size" : "-moz-tab-size"
    };

    function getSelection(root) {
        return (root.getSelection ? root.getSelection() : document.getSelection());
    }
    // Work around Chrome issue https://bugs.chromium.org/p/chromium/issues/detail?id=447523
    // (isCollapsed inappropriately returns true in shadow dom)
    function selectionCollapsed(domSel) {
        let collapsed = domSel.isCollapsed;
        if (collapsed && browser.chrome && domSel.rangeCount && !domSel.getRangeAt(0).collapsed)
            collapsed = false;
        return collapsed;
    }
    function hasSelection(dom, selection) {
        if (!selection.anchorNode)
            return false;
        try {
            // Firefox will raise 'permission denied' errors when accessing
            // properties of `sel.anchorNode` when it's in a generated CSS
            // element.
            return dom.contains(selection.anchorNode.nodeType == 3 ? selection.anchorNode.parentNode : selection.anchorNode);
        }
        catch (_) {
            return false;
        }
    }
    function clientRectsFor(dom) {
        if (dom.nodeType == 3) {
            let range = tempRange();
            range.setEnd(dom, dom.nodeValue.length);
            range.setStart(dom, 0);
            return range.getClientRects();
        }
        else if (dom.nodeType == 1) {
            return dom.getClientRects();
        }
        else {
            return [];
        }
    }
    // Scans forward and backward through DOM positions equivalent to the
    // given one to see if the two are in the same place (i.e. after a
    // text node vs at the end of that text node)
    function isEquivalentPosition(node, off, targetNode, targetOff) {
        return targetNode ? (scanFor(node, off, targetNode, targetOff, -1) ||
            scanFor(node, off, targetNode, targetOff, 1)) : false;
    }
    function domIndex(node) {
        for (var index = 0;; index++) {
            node = node.previousSibling;
            if (!node)
                return index;
        }
    }
    function scanFor(node, off, targetNode, targetOff, dir) {
        for (;;) {
            if (node == targetNode && off == targetOff)
                return true;
            if (off == (dir < 0 ? 0 : maxOffset(node))) {
                if (node.nodeName == "DIV")
                    return false;
                let parent = node.parentNode;
                if (!parent || parent.nodeType != 1)
                    return false;
                off = domIndex(node) + (dir < 0 ? 0 : 1);
                node = parent;
            }
            else if (node.nodeType == 1) {
                node = node.childNodes[off + (dir < 0 ? -1 : 0)];
                off = dir < 0 ? maxOffset(node) : 0;
            }
            else {
                return false;
            }
        }
    }
    function maxOffset(node) {
        return node.nodeType == 3 ? node.nodeValue.length : node.childNodes.length;
    }
    function flattenRect(rect, left) {
        let x = left ? rect.left : rect.right;
        return { left: x, right: x, top: rect.top, bottom: rect.bottom };
    }
    function windowRect(win) {
        return { left: 0, right: win.innerWidth,
            top: 0, bottom: win.innerHeight };
    }
    const ScrollSpace = 5;
    function scrollRectIntoView(dom, rect) {
        let doc = dom.ownerDocument, win = doc.defaultView;
        for (let cur = dom.parentNode; cur;) {
            if (cur.nodeType == 1) { // Element
                let bounding, top = cur == document.body;
                if (top) {
                    bounding = windowRect(win);
                }
                else {
                    if (cur.scrollHeight <= cur.clientHeight && cur.scrollWidth <= cur.clientWidth) {
                        cur = cur.parentNode;
                        continue;
                    }
                    let rect = cur.getBoundingClientRect();
                    // Make sure scrollbar width isn't included in the rectangle
                    bounding = { left: rect.left, right: rect.left + cur.clientWidth,
                        top: rect.top, bottom: rect.top + cur.clientHeight };
                }
                let moveX = 0, moveY = 0;
                if (rect.top < bounding.top)
                    moveY = -(bounding.top - rect.top + ScrollSpace);
                else if (rect.bottom > bounding.bottom)
                    moveY = rect.bottom - bounding.bottom + ScrollSpace;
                if (rect.left < bounding.left)
                    moveX = -(bounding.left - rect.left + ScrollSpace);
                else if (rect.right > bounding.right)
                    moveX = rect.right - bounding.right + ScrollSpace;
                if (moveX || moveY) {
                    if (top) {
                        win.scrollBy(moveX, moveY);
                    }
                    else {
                        if (moveY) {
                            let start = cur.scrollTop;
                            cur.scrollTop += moveY;
                            moveY = cur.scrollTop - start;
                        }
                        if (moveX) {
                            let start = cur.scrollLeft;
                            cur.scrollLeft += moveX;
                            moveX = cur.scrollLeft - start;
                        }
                        rect = { left: rect.left - moveX, top: rect.top - moveY,
                            right: rect.right - moveX, bottom: rect.bottom - moveY };
                    }
                }
                if (top)
                    break;
                cur = cur.parentNode;
            }
            else if (cur.nodeType == 11) { // A shadow root
                cur = cur.host;
            }
            else {
                break;
            }
        }
    }
    class DOMSelection {
        constructor() {
            this.anchorNode = null;
            this.anchorOffset = 0;
            this.focusNode = null;
            this.focusOffset = 0;
        }
        eq(domSel) {
            return this.anchorNode == domSel.anchorNode && this.anchorOffset == domSel.anchorOffset &&
                this.focusNode == domSel.focusNode && this.focusOffset == domSel.focusOffset;
        }
        set(domSel) {
            this.anchorNode = domSel.anchorNode;
            this.anchorOffset = domSel.anchorOffset;
            this.focusNode = domSel.focusNode;
            this.focusOffset = domSel.focusOffset;
        }
    }
    let preventScrollSupported = null;
    // Feature-detects support for .focus({preventScroll: true}), and uses
    // a fallback kludge when not supported.
    function focusPreventScroll(dom) {
        if (dom.setActive)
            return dom.setActive(); // in IE
        if (preventScrollSupported)
            return dom.focus(preventScrollSupported);
        let stack = [];
        for (let cur = dom; cur; cur = cur.parentNode) {
            stack.push(cur, cur.scrollTop, cur.scrollLeft);
            if (cur == cur.ownerDocument)
                break;
        }
        dom.focus(preventScrollSupported == null ? {
            get preventScroll() {
                preventScrollSupported = { preventScroll: true };
                return true;
            }
        } : undefined);
        if (!preventScrollSupported) {
            preventScrollSupported = false;
            for (let i = 0; i < stack.length;) {
                let elt = stack[i++], top = stack[i++], left = stack[i++];
                if (elt.scrollTop != top)
                    elt.scrollTop = top;
                if (elt.scrollLeft != left)
                    elt.scrollLeft = left;
            }
        }
    }
    let scratchRange;
    function tempRange() { return scratchRange || (scratchRange = document.createRange()); }

    class DOMPos {
        constructor(node, offset, precise = true) {
            this.node = node;
            this.offset = offset;
            this.precise = precise;
        }
        static before(dom, precise) { return new DOMPos(dom.parentNode, domIndex(dom), precise); }
        static after(dom, precise) { return new DOMPos(dom.parentNode, domIndex(dom) + 1, precise); }
    }
    const none$1 = [];
    class ContentView {
        constructor() {
            this.parent = null;
            this.dom = null;
            this.dirty = 2 /* Node */;
        }
        get editorView() {
            if (!this.parent)
                throw new Error("Accessing view in orphan content view");
            return this.parent.editorView;
        }
        get overrideDOMText() { return null; }
        get posAtStart() {
            return this.parent ? this.parent.posBefore(this) : 0;
        }
        get posAtEnd() {
            return this.posAtStart + this.length;
        }
        posBefore(view) {
            let pos = this.posAtStart;
            for (let child of this.children) {
                if (child == view)
                    return pos;
                pos += child.length + child.breakAfter;
            }
            throw new RangeError("Invalid child in posBefore");
        }
        posAfter(view) {
            return this.posBefore(view) + view.length;
        }
        // Will return a rectangle directly before (when side < 0), after
        // (side > 0) or directly on (when the browser supports it) the
        // given position.
        coordsAt(_pos, _side) { return null; }
        sync(track) {
            if (this.dirty & 2 /* Node */) {
                let parent = this.dom, pos = null;
                for (let child of this.children) {
                    if (child.dirty) {
                        let next = pos ? pos.nextSibling : parent.firstChild;
                        if (next && !child.dom && !ContentView.get(next))
                            child.reuseDOM(next);
                        child.sync(track);
                        child.dirty = 0 /* Not */;
                    }
                    if (track && track.node == parent && pos != child.dom)
                        track.written = true;
                    syncNodeInto(parent, pos, child.dom);
                    pos = child.dom;
                }
                let next = pos ? pos.nextSibling : parent.firstChild;
                if (next && track && track.node == parent)
                    track.written = true;
                while (next)
                    next = rm(next);
            }
            else if (this.dirty & 1 /* Child */) {
                for (let child of this.children)
                    if (child.dirty) {
                        child.sync(track);
                        child.dirty = 0 /* Not */;
                    }
            }
        }
        reuseDOM(_dom) { return false; }
        localPosFromDOM(node, offset) {
            let after;
            if (node == this.dom) {
                after = this.dom.childNodes[offset];
            }
            else {
                let bias = maxOffset(node) == 0 ? 0 : offset == 0 ? -1 : 1;
                for (;;) {
                    let parent = node.parentNode;
                    if (parent == this.dom)
                        break;
                    if (bias == 0 && parent.firstChild != parent.lastChild) {
                        if (node == parent.firstChild)
                            bias = -1;
                        else
                            bias = 1;
                    }
                    node = parent;
                }
                if (bias < 0)
                    after = node;
                else
                    after = node.nextSibling;
            }
            if (after == this.dom.firstChild)
                return 0;
            while (after && !ContentView.get(after))
                after = after.nextSibling;
            if (!after)
                return this.length;
            for (let i = 0, pos = 0;; i++) {
                let child = this.children[i];
                if (child.dom == after)
                    return pos;
                pos += child.length + child.breakAfter;
            }
        }
        domBoundsAround(from, to, offset = 0) {
            let fromI = -1, fromStart = -1, toI = -1, toEnd = -1;
            for (let i = 0, pos = offset; i < this.children.length; i++) {
                let child = this.children[i], end = pos + child.length;
                if (pos < from && end > to)
                    return child.domBoundsAround(from, to, pos);
                if (end >= from && fromI == -1) {
                    fromI = i;
                    fromStart = pos;
                }
                if (end >= to && end != pos && toI == -1) {
                    toI = i;
                    toEnd = end;
                    break;
                }
                pos = end + child.breakAfter;
            }
            return { from: fromStart, to: toEnd < 0 ? offset + this.length : toEnd, startDOM: (fromI ? this.children[fromI - 1].dom.nextSibling : null) || this.dom.firstChild, endDOM: toI < this.children.length - 1 && toI >= 0 ? this.children[toI + 1].dom : null };
        }
        // FIXME track precise dirty ranges, to avoid full DOM sync on every touched node?
        markDirty(andParent = false) {
            if (this.dirty & 2 /* Node */)
                return;
            this.dirty |= 2 /* Node */;
            this.markParentsDirty(andParent);
        }
        markParentsDirty(childList) {
            for (let parent = this.parent; parent; parent = parent.parent) {
                if (childList)
                    parent.dirty |= 2 /* Node */;
                if (parent.dirty & 1 /* Child */)
                    return;
                parent.dirty |= 1 /* Child */;
                childList = false;
            }
        }
        setParent(parent) {
            if (this.parent != parent) {
                this.parent = parent;
                if (this.dirty)
                    this.markParentsDirty(true);
            }
        }
        setDOM(dom) {
            this.dom = dom;
            dom.cmView = this;
        }
        get rootView() {
            for (let v = this;;) {
                let parent = v.parent;
                if (!parent)
                    return v;
                v = parent;
            }
        }
        replaceChildren(from, to, children = none$1) {
            this.markDirty();
            for (let i = from; i < to; i++)
                this.children[i].parent = null;
            this.children.splice(from, to - from, ...children);
            for (let i = 0; i < children.length; i++)
                children[i].setParent(this);
        }
        ignoreMutation(_rec) { return false; }
        ignoreEvent(_event) { return false; }
        childCursor(pos = this.length) {
            return new ChildCursor(this.children, pos, this.children.length);
        }
        childPos(pos, bias = 1) {
            return this.childCursor().findPos(pos, bias);
        }
        toString() {
            let name = this.constructor.name.replace("View", "");
            return name + (this.children.length ? "(" + this.children.join() + ")" :
                this.length ? "[" + (name == "Text" ? this.text : this.length) + "]" : "") +
                (this.breakAfter ? "#" : "");
        }
        static get(node) { return node.cmView; }
    }
    ContentView.prototype.breakAfter = 0;
    // Remove a DOM node and return its next sibling.
    function rm(dom) {
        let next = dom.nextSibling;
        dom.parentNode.removeChild(dom);
        return next;
    }
    function syncNodeInto(parent, after, dom) {
        let next = after ? after.nextSibling : parent.firstChild;
        if (dom.parentNode == parent)
            while (next != dom)
                next = rm(next);
        else
            parent.insertBefore(dom, next);
    }
    class ChildCursor {
        constructor(children, pos, i) {
            this.children = children;
            this.pos = pos;
            this.i = i;
            this.off = 0;
        }
        findPos(pos, bias = 1) {
            for (;;) {
                if (pos > this.pos || pos == this.pos &&
                    (bias > 0 || this.i == 0 || this.children[this.i - 1].breakAfter)) {
                    this.off = pos - this.pos;
                    return this;
                }
                let next = this.children[--this.i];
                this.pos -= next.length + next.breakAfter;
            }
        }
    }

    const none$1$1 = [];
    class InlineView extends ContentView {
        /// Return true when this view is equivalent to `other` and can take
        /// on its role.
        become(_other) { return false; }
        // When this is a zero-length view with a side, this should return a
        // negative number to indicate it is before its position, or a
        // positive number when after its position.
        getSide() { return 0; }
    }
    InlineView.prototype.children = none$1$1;
    const MaxJoinLen = 256;
    class TextView extends InlineView {
        constructor(text) {
            super();
            this.text = text;
        }
        get length() { return this.text.length; }
        createDOM(textDOM) {
            this.setDOM(textDOM || document.createTextNode(this.text));
        }
        sync(track) {
            if (!this.dom)
                this.createDOM();
            if (this.dom.nodeValue != this.text) {
                if (track && track.node == this.dom)
                    track.written = true;
                this.dom.nodeValue = this.text;
            }
        }
        reuseDOM(dom) {
            if (dom.nodeType != 3)
                return false;
            this.createDOM(dom);
            return true;
        }
        merge(from, to, source) {
            if (source && (!(source instanceof TextView) || this.length - (to - from) + source.length > MaxJoinLen))
                return false;
            this.text = this.text.slice(0, from) + (source ? source.text : "") + this.text.slice(to);
            this.markDirty();
            return true;
        }
        slice(from) {
            return new TextView(this.text.slice(from));
        }
        localPosFromDOM(node, offset) {
            return node == this.dom ? offset : offset ? this.text.length : 0;
        }
        domAtPos(pos) { return new DOMPos(this.dom, pos); }
        domBoundsAround(_from, _to, offset) {
            return { from: offset, to: offset + this.length, startDOM: this.dom, endDOM: this.dom.nextSibling };
        }
        coordsAt(pos, side) {
            return textCoords(this.dom, pos, side, this.length);
        }
    }
    class MarkView extends InlineView {
        constructor(mark, children = [], length = 0) {
            super();
            this.mark = mark;
            this.children = children;
            this.length = length;
            for (let ch of children)
                ch.setParent(this);
        }
        createDOM() {
            let dom = document.createElement(this.mark.tagName);
            if (this.mark.class)
                dom.className = this.mark.class;
            if (this.mark.attrs)
                for (let name in this.mark.attrs)
                    dom.setAttribute(name, this.mark.attrs[name]);
            this.setDOM(dom);
        }
        sync(track) {
            if (!this.dom)
                this.createDOM();
            super.sync(track);
        }
        merge(from, to, source, openStart, openEnd) {
            if (source && (!(source instanceof MarkView && source.mark.eq(this.mark)) ||
                (from && openStart <= 0) || (to < this.length && openEnd <= 0)))
                return false;
            mergeInlineChildren(this, from, to, source ? source.children : none$1$1, openStart - 1, openEnd - 1);
            this.markDirty();
            return true;
        }
        slice(from) {
            return new MarkView(this.mark, sliceInlineChildren(this.children, from), this.length - from);
        }
        domAtPos(pos) {
            return inlineDOMAtPos(this.dom, this.children, pos);
        }
        coordsAt(pos, side) {
            return coordsInChildren(this, pos, side);
        }
    }
    function textCoords(text, pos, side, length) {
        let from = pos, to = pos, flatten = 0;
        if (pos == 0 && side < 0 || pos == length && side >= 0) {
            if (!(browser.chrome || browser.gecko)) { // These browsers reliably return valid rectangles for empty ranges
                if (pos) {
                    from--;
                    flatten = 1;
                } // FIXME this is wrong in RTL text
                else {
                    to++;
                    flatten = -1;
                }
            }
        }
        else {
            if (side < 0)
                from--;
            else
                to++;
        }
        let range = tempRange();
        range.setEnd(text, to);
        range.setStart(text, from);
        let rects = range.getClientRects(), rect = rects[(flatten ? flatten < 0 : side >= 0) ? 0 : rects.length - 1];
        if (browser.safari && !flatten && rect.width == 0)
            rect = Array.prototype.find.call(rects, r => r.width) || rect;
        return flatten ? flattenRect(rect, flatten < 0) : rect;
    }
    // Also used for collapsed ranges that don't have a placeholder widget!
    class WidgetView extends InlineView {
        constructor(widget, length, side) {
            super();
            this.widget = widget;
            this.length = length;
            this.side = side;
        }
        static create(widget, length, side) {
            return new (widget.customView || WidgetView)(widget, length, side);
        }
        slice(from) { return WidgetView.create(this.widget, this.length - from, this.side); }
        sync() {
            if (!this.dom || !this.widget.updateDOM(this.dom)) {
                this.setDOM(this.widget.toDOM(this.editorView));
                this.dom.contentEditable = "false";
            }
        }
        getSide() { return this.side; }
        merge(from, to, source, openStart, openEnd) {
            if (source && (!(source instanceof WidgetView) || !this.widget.compare(source.widget) ||
                from > 0 && openStart <= 0 || to < this.length && openEnd <= 0))
                return false;
            this.length = from + (source ? source.length : 0) + (this.length - to);
            return true;
        }
        become(other) {
            if (other.length == this.length && other instanceof WidgetView && other.side == this.side) {
                if (this.widget.constructor == other.widget.constructor) {
                    if (!this.widget.eq(other.widget))
                        this.markDirty(true);
                    this.widget = other.widget;
                    return true;
                }
            }
            return false;
        }
        ignoreMutation() { return true; }
        ignoreEvent(event) { return this.widget.ignoreEvent(event); }
        get overrideDOMText() {
            if (this.length == 0)
                return Text.empty;
            let top = this;
            while (top.parent)
                top = top.parent;
            let view = top.editorView, text = view && view.state.doc, start = this.posAtStart;
            return text ? text.slice(start, start + this.length) : Text.empty;
        }
        domAtPos(pos) {
            return pos == 0 ? DOMPos.before(this.dom) : DOMPos.after(this.dom, pos == this.length);
        }
        domBoundsAround() { return null; }
        coordsAt(pos, side) {
            let rects = this.dom.getClientRects(), rect = null;
            for (let i = pos > 0 ? rects.length - 1 : 0;; i += (pos > 0 ? -1 : 1)) {
                rect = rects[i];
                if (pos > 0 ? i == 0 : i == rects.length - 1 || rect.top < rect.bottom)
                    break;
            }
            return (pos == 0 && side > 0 || pos == this.length && side <= 0) ? rect : flattenRect(rect, pos == 0);
        }
    }
    class CompositionView extends WidgetView {
        domAtPos(pos) { return new DOMPos(this.widget.text, pos); }
        sync() { if (!this.dom)
            this.setDOM(this.widget.toDOM()); }
        localPosFromDOM(node, offset) {
            return !offset ? 0 : node.nodeType == 3 ? Math.min(offset, this.length) : this.length;
        }
        ignoreMutation() { return false; }
        get overrideDOMText() { return null; }
        coordsAt(pos, side) { return textCoords(this.widget.text, pos, side, this.length); }
    }
    function mergeInlineChildren(parent, from, to, elts, openStart, openEnd) {
        let cur = parent.childCursor();
        let { i: toI, off: toOff } = cur.findPos(to, 1);
        let { i: fromI, off: fromOff } = cur.findPos(from, -1);
        let dLen = from - to;
        for (let view of elts)
            dLen += view.length;
        parent.length += dLen;
        let { children } = parent;
        // Both from and to point into the same text view
        if (fromI == toI && fromOff) {
            let start = children[fromI];
            // Maybe just update that view and be done
            if (elts.length == 1 && start.merge(fromOff, toOff, elts[0], openStart, openEnd))
                return;
            if (elts.length == 0) {
                start.merge(fromOff, toOff, null, openStart, openEnd);
                return;
            }
            // Otherwise split it, so that we don't have to worry about aliasing front/end afterwards
            let after = start.slice(toOff);
            if (after.merge(0, 0, elts[elts.length - 1], 0, openEnd))
                elts[elts.length - 1] = after;
            else
                elts.push(after);
            toI++;
            openEnd = toOff = 0;
        }
        // Make sure start and end positions fall on node boundaries
        // (fromOff/toOff are no longer used after this), and that if the
        // start or end of the elts can be merged with adjacent nodes,
        // this is done
        if (toOff) {
            let end = children[toI];
            if (elts.length && end.merge(0, toOff, elts[elts.length - 1], 0, openEnd)) {
                elts.pop();
                openEnd = 0;
            }
            else {
                end.merge(0, toOff, null, 0, 0);
            }
        }
        else if (toI < children.length && elts.length &&
            children[toI].merge(0, 0, elts[elts.length - 1], 0, openEnd)) {
            elts.pop();
            openEnd = 0;
        }
        if (fromOff) {
            let start = children[fromI];
            if (elts.length && start.merge(fromOff, start.length, elts[0], openStart, 0)) {
                elts.shift();
                openStart = 0;
            }
            else {
                start.merge(fromOff, start.length, null, 0, 0);
            }
            fromI++;
        }
        else if (fromI && elts.length) {
            let end = children[fromI - 1];
            if (end.merge(end.length, end.length, elts[0], openStart, 0)) {
                elts.shift();
                openStart = 0;
            }
        }
        // Then try to merge any mergeable nodes at the start and end of
        // the changed range
        while (fromI < toI && elts.length && children[toI - 1].become(elts[elts.length - 1])) {
            elts.pop();
            toI--;
            openEnd = 0;
        }
        while (fromI < toI && elts.length && children[fromI].become(elts[0])) {
            elts.shift();
            fromI++;
            openStart = 0;
        }
        if (!elts.length && fromI && toI < children.length && openStart && openEnd &&
            children[toI].merge(0, 0, children[fromI - 1], openStart, openEnd))
            fromI--;
        // And if anything remains, splice the child array to insert the new elts
        if (elts.length || fromI != toI)
            parent.replaceChildren(fromI, toI, elts);
    }
    function sliceInlineChildren(children, from) {
        let result = [], off = 0;
        for (let elt of children) {
            let end = off + elt.length;
            if (end > from)
                result.push(off < from ? elt.slice(from - off) : elt);
            off = end;
        }
        return result;
    }
    function inlineDOMAtPos(dom, children, pos) {
        let i = 0;
        for (let off = 0; i < children.length; i++) {
            let child = children[i], end = off + child.length;
            if (end == off && child.getSide() <= 0)
                continue;
            if (pos > off && pos < end && child.dom.parentNode == dom)
                return child.domAtPos(pos - off);
            if (pos <= off)
                break;
            off = end;
        }
        for (; i > 0; i--) {
            let before = children[i - 1].dom;
            if (before.parentNode == dom)
                return DOMPos.after(before);
        }
        return new DOMPos(dom, 0);
    }
    // Assumes `view`, if a mark view, has precisely 1 child.
    function joinInlineInto(parent, view, open) {
        let last, { children } = parent;
        if (open > 0 && view instanceof MarkView && children.length &&
            (last = children[children.length - 1]) instanceof MarkView && last.mark.eq(view.mark)) {
            joinInlineInto(last, view.children[0], open - 1);
        }
        else {
            children.push(view);
            view.setParent(parent);
        }
        parent.length += view.length;
    }
    function coordsInChildren(view, pos, side) {
        for (let off = 0, i = 0; i < view.children.length; i++) {
            let child = view.children[i], end = off + child.length;
            if (end == off && child.getSide() <= 0)
                continue;
            if (side <= 0 || end == view.length ? end >= pos : end > pos)
                return child.coordsAt(pos - off, side);
            off = end;
        }
        return (view.dom.lastChild || view.dom).getBoundingClientRect();
    }

    function combineAttrs(source, target) {
        for (let name in source) {
            if (name == "class" && target.class)
                target.class += " " + source.class;
            else if (name == "style" && target.style)
                target.style += ";" + source.style;
            else
                target[name] = source[name];
        }
        return target;
    }
    function attrsEq(a, b) {
        if (a == b)
            return true;
        if (!a || !b)
            return false;
        let keysA = Object.keys(a), keysB = Object.keys(b);
        if (keysA.length != keysB.length)
            return false;
        for (let key of keysA) {
            if (keysB.indexOf(key) == -1 || a[key] !== b[key])
                return false;
        }
        return true;
    }
    function updateAttrs(dom, prev, attrs) {
        if (prev)
            for (let name in prev)
                if (!(attrs && name in attrs))
                    dom.removeAttribute(name);
        if (attrs)
            for (let name in attrs)
                if (!(prev && prev[name] == attrs[name]))
                    dom.setAttribute(name, attrs[name]);
    }

    /// Widgets added to the content are described by subclasses of this
    /// class. This makes it possible to delay creating of the DOM
    /// structure for a widget until it is needed, and to avoid redrawing
    /// widgets even when the decorations that define them are recreated.
    /// `T` can be a type of value passed to instances of the widget type.
    class WidgetType {
        /// Compare this instance to another instance of the same type.
        /// (TypeScript can't express this, but only instances of the same
        /// specific class will be passed to this method.) This is used to
        /// avoid redrawing widgets when they are replace by a new
        /// decoration of the same type. The default implementation just
        /// returns `false`, which may be wasteful.
        eq(_widget) { return false; }
        /// Update a DOM element created by a widget of the same type (but
        /// different, non-`eq` content) to reflect this widget. May return
        /// true to indicate that it could update, false to indicate it
        /// couldn't (in which case the widget will be redrawn). The default
        /// implementation just returns false.
        updateDOM(_dom) { return false; }
        /// @internal
        compare(other) {
            return this == other || this.constructor == other.constructor && this.eq(other);
        }
        /// The estimated height this widget will have, to be used when
        /// estimating the height of content that hasn't been drawn. May
        /// return -1 to indicate you don't know. The default implementation
        /// returns -1.
        get estimatedHeight() { return -1; }
        /// Can be used to configure which kinds of events inside the widget
        /// should be ignored by the editor. The default is to ignore all
        /// events.
        ignoreEvent(_event) { return true; }
        //// @internal
        get customView() { return null; }
    }
    /// The different types of blocks that can occur in an editor view.
    var BlockType;
    (function (BlockType) {
        /// A line of text.
        BlockType[BlockType["Text"] = 0] = "Text";
        /// A block widget associated with the position after it.
        BlockType[BlockType["WidgetBefore"] = 1] = "WidgetBefore";
        /// A block widget associated with the position before it.
        BlockType[BlockType["WidgetAfter"] = 2] = "WidgetAfter";
        /// A block widget [replacing](#view.Decoration^replace) a range of content.
        BlockType[BlockType["WidgetRange"] = 3] = "WidgetRange";
    })(BlockType || (BlockType = {}));
    /// A decoration provides information on how to draw or style a piece
    /// of content. You'll usually use it wrapped in a
    /// [`Range`](#rangeset.Range), which adds a start and end position.
    class Decoration extends RangeValue {
        /// @internal
        constructor(
        /// @internal
        startSide, 
        /// @internal
        endSide, 
        /// @internal
        widget, 
        /// The config object used to create this decoration.
        spec) {
            super();
            this.startSide = startSide;
            this.endSide = endSide;
            this.widget = widget;
            this.spec = spec;
        }
        /// @internal
        get heightRelevant() { return false; }
        /// Create a mark decoration, which influences the styling of the
        /// content in its range. Nested mark decorations will cause nested
        /// DOM elements to be created. Nesting order is determined by
        /// precedence of the [facet](#view.EditorView^decorations) or
        /// (below the facet-provided decorations) [view
        /// plugin](#view.PluginSpec.decorations). Such elements are broken
        /// on line boundaries and on the boundaries of higher-precedence
        /// decorations.
        static mark(spec) {
            return new MarkDecoration(spec);
        }
        /// Create a widget decoration, which adds an element at the given
        /// position.
        static widget(spec) {
            let side = spec.side || 0;
            if (spec.block)
                side += (200000000 /* BigBlock */ + 1) * (side > 0 ? 1 : -1);
            return new PointDecoration(spec, side, side, !!spec.block, spec.widget || null, false);
        }
        /// Create a replace decoration which replaces the given range with
        /// a widget, or simply hides it.
        static replace(spec) {
            let block = !!spec.block;
            let { start, end } = getInclusive(spec);
            let startSide = block ? -200000000 /* BigBlock */ * (start ? 2 : 1) : 100000000 /* BigInline */ * (start ? -1 : 1);
            let endSide = block ? 200000000 /* BigBlock */ * (end ? 2 : 1) : 100000000 /* BigInline */ * (end ? 1 : -1);
            return new PointDecoration(spec, startSide, endSide, block, spec.widget || null, true);
        }
        /// Create a line decoration, which can add DOM attributes to the
        /// line starting at the given position.
        static line(spec) {
            return new LineDecoration(spec);
        }
        /// Build a [`DecorationSet`](#view.DecorationSet) from the given
        /// decorated range or ranges.
        static set(of, sort = false) {
            return RangeSet.of(of, sort);
        }
        /// @internal
        hasHeight() { return this.widget ? this.widget.estimatedHeight > -1 : false; }
    }
    /// The empty set of decorations.
    Decoration.none = RangeSet.empty;
    class MarkDecoration extends Decoration {
        constructor(spec) {
            let { start, end } = getInclusive(spec);
            super(100000000 /* BigInline */ * (start ? -1 : 1), 100000000 /* BigInline */ * (end ? 1 : -1), null, spec);
            this.tagName = spec.tagName || "span";
            this.class = spec.class || "";
            this.attrs = spec.attributes || null;
        }
        eq(other) {
            return this == other ||
                other instanceof MarkDecoration &&
                    this.tagName == other.tagName &&
                    this.class == other.class &&
                    attrsEq(this.attrs, other.attrs);
        }
        range(from, to = from) {
            if (from >= to)
                throw new RangeError("Mark decorations may not be empty");
            return super.range(from, to);
        }
    }
    MarkDecoration.prototype.point = false;
    class LineDecoration extends Decoration {
        constructor(spec) {
            super(-100000000 /* BigInline */, -100000000 /* BigInline */, null, spec);
        }
        eq(other) {
            return other instanceof LineDecoration && attrsEq(this.spec.attributes, other.spec.attributes);
        }
        range(from, to = from) {
            if (to != from)
                throw new RangeError("Line decoration ranges must be zero-length");
            return super.range(from, to);
        }
    }
    LineDecoration.prototype.mapMode = MapMode.TrackBefore;
    LineDecoration.prototype.point = true;
    class PointDecoration extends Decoration {
        constructor(spec, startSide, endSide, block, widget, isReplace) {
            super(startSide, endSide, widget, spec);
            this.block = block;
            this.isReplace = isReplace;
            this.mapMode = !block ? MapMode.TrackDel : startSide < 0 ? MapMode.TrackBefore : MapMode.TrackAfter;
        }
        // Only relevant when this.block == true
        get type() {
            return this.startSide < this.endSide ? BlockType.WidgetRange
                : this.startSide < 0 ? BlockType.WidgetBefore : BlockType.WidgetAfter;
        }
        get heightRelevant() { return this.block || !!this.widget && this.widget.estimatedHeight >= 5; }
        eq(other) {
            return other instanceof PointDecoration &&
                widgetsEq(this.widget, other.widget) &&
                this.block == other.block &&
                this.startSide == other.startSide && this.endSide == other.endSide;
        }
        range(from, to = from) {
            if (this.isReplace && (from > to || (from == to && this.startSide > 0 && this.endSide < 0)))
                throw new RangeError("Invalid range for replacement decoration");
            if (!this.isReplace && to != from)
                throw new RangeError("Widget decorations can only have zero-length ranges");
            return super.range(from, to);
        }
    }
    PointDecoration.prototype.point = true;
    function getInclusive(spec) {
        let { inclusiveStart: start, inclusiveEnd: end } = spec;
        if (start == null)
            start = spec.inclusive;
        if (end == null)
            end = spec.inclusive;
        return { start: start || false, end: end || false };
    }
    function widgetsEq(a, b) {
        return a == b || !!(a && b && a.compare(b));
    }
    const MinRangeGap = 4;
    function addRange(from, to, ranges) {
        let last = ranges.length - 1;
        if (last >= 0 && ranges[last] + MinRangeGap > from)
            ranges[last] = Math.max(ranges[last], to);
        else
            ranges.push(from, to);
    }

    const theme = Facet.define({ combine: strs => strs.join(" ") });
    const darkTheme = Facet.define({ combine: values => values.indexOf(true) > -1 });
    const baseThemeID = StyleModule.newName();
    function expandThemeClasses(sel) {
        return sel.replace(/\$\w[\w\.]*/g, cls => {
            let parts = cls.slice(1).split("."), result = "";
            for (let i = 1; i <= parts.length; i++)
                result += ".cm-" + parts.slice(0, i).join("-");
            return result;
        });
    }
    function buildTheme(main, spec) {
        return new StyleModule(spec, {
            process(sel) {
                sel = expandThemeClasses(sel);
                return /\$/.test(sel) ? sel.replace(/\$/, main) : main + " " + sel;
            },
            extend(template, sel) {
                template = expandThemeClasses(template);
                return sel.slice(0, main.length + 1) == main + " "
                    ? main + " " + template.replace(/&/, sel.slice(main.length + 1))
                    : template.replace(/&/, sel);
            }
        });
    }
    /// Create a set of CSS class names for the given theme class, which
    /// can be added to a DOM element within an editor to make themes able
    /// to style it. Theme classes can be single words or words separated
    /// by dot characters. In the latter case, the returned classes
    /// combine those that match the full name and those that match some
    /// prefix—for example `"panel.search"` will match both the theme
    /// styles specified as `"panel.search"` and those with just
    /// `"panel"`. More specific theme classes (with more dots) take
    /// precedence over less specific ones.
    function themeClass(selector) {
        if (selector.indexOf(".") < 0)
            return "cm-" + selector;
        let parts = selector.split("."), result = "";
        for (let i = 1; i <= parts.length; i++)
            result += (result ? " " : "") + "cm-" + parts.slice(0, i).join("-");
        return result;
    }
    const baseTheme = buildTheme("." + baseThemeID, {
        $: {
            position: "relative !important",
            boxSizing: "border-box",
            "&$focused": {
                // FIXME it would be great if we could directly use the browser's
                // default focus outline, but it appears we can't, so this tries to
                // approximate that
                outline_fallback: "1px dotted #212121",
                outline: "5px auto -webkit-focus-ring-color"
            },
            display: "flex !important",
            flexDirection: "column"
        },
        $scroller: {
            display: "flex !important",
            alignItems: "flex-start !important",
            fontFamily: "monospace",
            lineHeight: 1.4,
            height: "100%",
            overflowX: "auto",
            position: "relative",
            zIndex: 0
        },
        $content: {
            margin: 0,
            flexGrow: 2,
            minHeight: "100%",
            display: "block",
            whiteSpace: "pre",
            boxSizing: "border-box",
            padding: "4px 0",
            outline: "none"
        },
        "$$light $content": { caretColor: "black" },
        "$$dark $content": { caretColor: "white" },
        $line: {
            display: "block",
            padding: "0 2px 0 4px"
        },
        $selectionLayer: {
            zIndex: -1,
            contain: "size style"
        },
        $selectionBackground: {
            position: "absolute",
        },
        "$$light $selectionBackground": {
            background: "#d9d9d9"
        },
        "$$dark $selectionBackground": {
            background: "#222"
        },
        "$$focused$light $selectionBackground": {
            background: "#d7d4f0"
        },
        "$$focused$dark $selectionBackground": {
            background: "#233"
        },
        $cursorLayer: {
            zIndex: 100,
            contain: "size style",
            pointerEvents: "none"
        },
        "$$focused $cursorLayer": {
            animation: "steps(1) cm-blink 1.2s infinite"
        },
        // Two animations defined so that we can switch between them to
        // restart the animation without forcing another style
        // recomputation.
        "@keyframes cm-blink": { "0%": {}, "50%": { visibility: "hidden" }, "100%": {} },
        "@keyframes cm-blink2": { "0%": {}, "50%": { visibility: "hidden" }, "100%": {} },
        $cursor: {
            position: "absolute",
            borderLeft: "1.2px solid #bbb",
            marginLeft: "-0.6px",
            pointerEvents: "none"
        },
        "$$dark $cursor": {
            borderLeftColor: "#444"
        },
        "$$focused $cursor": {
            borderLeft: "1.2px solid black",
        },
        $placeholder: {
            color: "#888",
            display: "inline-block"
        },
        $button: {
            verticalAlign: "middle",
            color: "inherit",
            fontSize: "70%",
            padding: ".2em 1em",
            borderRadius: "3px"
        },
        "$$light $button": {
            backgroundImage: "linear-gradient(#eff1f5, #d9d9df)",
            border: "1px solid #888",
            "&:active": {
                backgroundImage: "linear-gradient(#b4b4b4, #d0d3d6)"
            }
        },
        "$$dark $button": {
            backgroundImage: "linear-gradient(#555, #111)",
            border: "1px solid #888",
            "&:active": {
                backgroundImage: "linear-gradient(#111, #333)"
            }
        },
        $textfield: {
            verticalAlign: "middle",
            color: "inherit",
            fontSize: "70%",
            border: "1px solid silver",
            padding: ".2em .5em"
        },
        "$$light $textfield": {
            backgroundColor: "white"
        },
        "$$dark $textfield": {
            border: "1px solid #555",
            backgroundColor: "inherit"
        }
    });

    const LineClass = themeClass("line");
    class LineView extends ContentView {
        constructor() {
            super(...arguments);
            this.children = [];
            this.length = 0;
            this.prevAttrs = undefined;
            this.attrs = null;
            this.breakAfter = 0;
        }
        // Consumes source
        merge(from, to, source, takeDeco, openStart, openEnd) {
            if (source) {
                if (!(source instanceof LineView))
                    return false;
                if (!this.dom)
                    source.transferDOM(this); // Reuse source.dom when appropriate
            }
            if (takeDeco)
                this.setDeco(source ? source.attrs : null);
            mergeInlineChildren(this, from, to, source ? source.children : none$2, openStart, openEnd);
            return true;
        }
        split(at) {
            let end = new LineView;
            end.breakAfter = this.breakAfter;
            if (this.length == 0)
                return end;
            let { i, off } = this.childPos(at);
            if (off) {
                end.append(this.children[i].slice(off), 0);
                this.children[i].merge(off, this.children[i].length, null, 0, 0);
                i++;
            }
            for (let j = i; j < this.children.length; j++)
                end.append(this.children[j], 0);
            while (i > 0 && this.children[i - 1].length == 0) {
                this.children[i - 1].parent = null;
                i--;
            }
            this.children.length = i;
            this.markDirty();
            this.length = at;
            return end;
        }
        transferDOM(other) {
            if (!this.dom)
                return;
            other.setDOM(this.dom);
            other.prevAttrs = this.prevAttrs === undefined ? this.attrs : this.prevAttrs;
            this.prevAttrs = undefined;
            this.dom = null;
        }
        setDeco(attrs) {
            if (!attrsEq(this.attrs, attrs)) {
                if (this.dom) {
                    this.prevAttrs = this.attrs;
                    this.markDirty();
                }
                this.attrs = attrs;
            }
        }
        // Only called when building a line view in ContentBuilder
        append(child, openStart) {
            joinInlineInto(this, child, openStart);
        }
        // Only called when building a line view in ContentBuilder
        addLineDeco(deco) {
            let attrs = deco.spec.attributes;
            if (attrs)
                this.attrs = combineAttrs(attrs, this.attrs || {});
        }
        domAtPos(pos) {
            return inlineDOMAtPos(this.dom, this.children, pos);
        }
        // FIXME might need another hack to work around Firefox's behavior
        // of not actually displaying the cursor even though it's there in
        // the DOM
        sync(track) {
            if (!this.dom) {
                this.setDOM(document.createElement("div"));
                this.dom.className = LineClass;
                this.prevAttrs = this.attrs ? null : undefined;
            }
            if (this.prevAttrs !== undefined) {
                updateAttrs(this.dom, this.prevAttrs, this.attrs);
                this.dom.classList.add(LineClass);
                this.prevAttrs = undefined;
            }
            super.sync(track);
            let last = this.dom.lastChild;
            if (!last || (last.nodeName != "BR" && (ContentView.get(last) instanceof WidgetView))) {
                let hack = document.createElement("BR");
                hack.cmIgnore = true;
                this.dom.appendChild(hack);
            }
        }
        measureTextSize() {
            if (this.children.length == 0 || this.length > 20)
                return null;
            let totalWidth = 0;
            for (let child of this.children) {
                if (!(child instanceof TextView))
                    return null;
                let rects = clientRectsFor(child.dom);
                if (rects.length != 1)
                    return null;
                totalWidth += rects[0].width;
            }
            return { lineHeight: this.dom.getBoundingClientRect().height, charWidth: totalWidth / this.length };
        }
        coordsAt(pos, side) {
            return coordsInChildren(this, pos, side);
        }
        match(_other) { return false; }
        get type() { return BlockType.Text; }
        static find(docView, pos) {
            for (let i = 0, off = 0;; i++) {
                let block = docView.children[i], end = off + block.length;
                if (end >= pos) {
                    if (block instanceof LineView)
                        return block;
                    if (block.length)
                        return null;
                }
                off = end + block.breakAfter;
            }
        }
    }
    const none$2 = [];
    class BlockWidgetView extends ContentView {
        constructor(widget, length, type) {
            super();
            this.widget = widget;
            this.length = length;
            this.type = type;
            this.breakAfter = 0;
        }
        merge(from, to, source, _takeDeco, openStart, openEnd) {
            if (source && (!(source instanceof BlockWidgetView) || !this.widget.compare(source.widget) ||
                from > 0 && openStart <= 0 || to < this.length && openEnd <= 0))
                return false;
            this.length = from + (source ? source.length : 0) + (this.length - to);
            return true;
        }
        domAtPos(pos) {
            return pos == 0 ? DOMPos.before(this.dom) : DOMPos.after(this.dom, pos == this.length);
        }
        split(at) {
            let len = this.length - at;
            this.length = at;
            return new BlockWidgetView(this.widget, len, this.type);
        }
        get children() { return none$2; }
        sync() {
            if (!this.dom || !this.widget.updateDOM(this.dom)) {
                this.setDOM(this.widget.toDOM(this.editorView));
                this.dom.contentEditable = "false";
            }
        }
        get overrideDOMText() {
            return this.parent ? this.parent.view.state.doc.slice(this.posAtStart, this.posAtEnd) : Text.empty;
        }
        domBoundsAround() { return null; }
        match(other) {
            if (other instanceof BlockWidgetView && other.type == this.type &&
                other.widget.constructor == this.widget.constructor) {
                if (!other.widget.eq(this.widget))
                    this.markDirty(true);
                this.widget = other.widget;
                this.length = other.length;
                this.breakAfter = other.breakAfter;
                return true;
            }
            return false;
        }
        ignoreMutation() { return true; }
        ignoreEvent(event) { return this.widget.ignoreEvent(event); }
    }

    class ContentBuilder {
        constructor(doc, pos, end) {
            this.doc = doc;
            this.pos = pos;
            this.end = end;
            this.content = [];
            this.curLine = null;
            this.breakAtStart = 0;
            this.openStart = -1;
            this.openEnd = -1;
            this.text = "";
            this.textOff = 0;
            this.cursor = doc.iter();
            this.skip = pos;
        }
        posCovered() {
            if (this.content.length == 0)
                return !this.breakAtStart && this.doc.lineAt(this.pos).from != this.pos;
            let last = this.content[this.content.length - 1];
            return !last.breakAfter && !(last instanceof BlockWidgetView && last.type == BlockType.WidgetBefore);
        }
        getLine() {
            if (!this.curLine)
                this.content.push(this.curLine = new LineView);
            return this.curLine;
        }
        addWidget(view) {
            this.curLine = null;
            this.content.push(view);
        }
        finish() {
            if (!this.posCovered())
                this.getLine();
        }
        wrapMarks(view, active) {
            for (let i = active.length - 1; i >= 0; i--)
                view = new MarkView(active[i], [view], view.length);
            return view;
        }
        buildText(length, active, openStart) {
            while (length > 0) {
                if (this.textOff == this.text.length) {
                    let { value, lineBreak, done } = this.cursor.next(this.skip);
                    this.skip = 0;
                    if (done)
                        throw new Error("Ran out of text content when drawing inline views");
                    if (lineBreak) {
                        if (!this.posCovered())
                            this.getLine();
                        if (this.content.length)
                            this.content[this.content.length - 1].breakAfter = 1;
                        else
                            this.breakAtStart = 1;
                        this.curLine = null;
                        length--;
                        continue;
                    }
                    else {
                        this.text = value;
                        this.textOff = 0;
                    }
                }
                let take = Math.min(this.text.length - this.textOff, length);
                this.getLine().append(this.wrapMarks(new TextView(this.text.slice(this.textOff, this.textOff + take)), active), openStart);
                length -= take;
                this.textOff += take;
            }
        }
        span(from, to, active, openStart) {
            this.buildText(to - from, active, openStart);
            this.pos = to;
            if (this.openStart < 0)
                this.openStart = openStart;
        }
        point(from, to, deco, active, openStart) {
            let len = to - from;
            if (deco instanceof PointDecoration) {
                if (deco.block) {
                    let { type } = deco;
                    if (type == BlockType.WidgetAfter && !this.posCovered())
                        this.getLine();
                    this.addWidget(new BlockWidgetView(deco.widget || new NullWidget("div"), len, type));
                }
                else {
                    let widget = this.wrapMarks(WidgetView.create(deco.widget || new NullWidget("span"), len, deco.startSide), active);
                    this.getLine().append(widget, openStart);
                }
            }
            else if (this.doc.lineAt(this.pos).from == this.pos) { // Line decoration
                this.getLine().addLineDeco(deco);
            }
            if (len) {
                // Advance the iterator past the replaced content
                if (this.textOff + len <= this.text.length) {
                    this.textOff += len;
                }
                else {
                    this.skip += len - (this.text.length - this.textOff);
                    this.text = "";
                    this.textOff = 0;
                }
                this.pos = to;
            }
            if (this.openStart < 0)
                this.openStart = openStart;
        }
        static build(text, from, to, decorations) {
            let builder = new ContentBuilder(text, from, to);
            builder.openEnd = RangeSet.spans(decorations, from, to, builder);
            if (builder.openStart < 0)
                builder.openStart = builder.openEnd;
            builder.finish();
            return builder;
        }
    }
    class NullWidget extends WidgetType {
        constructor(tag) {
            super();
            this.tag = tag;
        }
        eq(other) { return other.tag == this.tag; }
        toDOM() { return document.createElement(this.tag); }
        updateDOM(elt) { return elt.nodeName.toLowerCase() == this.tag; }
    }

    /// Used to indicate [text direction](#view.EditorView.textDirection).
    var Direction;
    (function (Direction) {
        // (These are chosen to match the base levels, in bidi algorithm
        // terms, of spans in that direction.)
        Direction[Direction["LTR"] = 0] = "LTR";
        Direction[Direction["RTL"] = 1] = "RTL";
    })(Direction || (Direction = {}));
    const LTR = Direction.LTR, RTL = Direction.RTL;
    // Decode a string with each type encoded as log2(type)
    function dec(str) {
        let result = [];
        for (let i = 0; i < str.length; i++)
            result.push(1 << +str[i]);
        return result;
    }
    // Character types for codepoints 0 to 0xf8
    const LowTypes = dec("88888888888888888888888888888888888666888888787833333333337888888000000000000000000000000008888880000000000000000000000000088888888888888888888888888888888888887866668888088888663380888308888800000000000000000000000800000000000000000000000000000008");
    // Character types for codepoints 0x600 to 0x6f9
    const ArabicTypes = dec("4444448826627288999999999992222222222222222222222222222222222222222222222229999999999999999999994444444444644222822222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222999999949999999229989999223333333333");
    function charType(ch) {
        return ch <= 0xf7 ? LowTypes[ch] :
            0x590 <= ch && ch <= 0x5f4 ? 2 /* R */ :
                0x600 <= ch && ch <= 0x6f9 ? ArabicTypes[ch - 0x600] :
                    0x6ee <= ch && ch <= 0x8ac ? 4 /* AL */ :
                        0x2000 <= ch && ch <= 0x200b ? 256 /* NI */ :
                            ch == 0x200c ? 256 /* NI */ : 1 /* L */;
    }
    const BidiRE = /[\u0590-\u05f4\u0600-\u06ff\u0700-\u08ac]/;
    /// Represents a contiguous range of text that has a single direction
    /// (as in left-to-right or right-to-left).
    class BidiSpan {
        /// @internal
        constructor(
        /// The start of the span (relative to the start of the line).
        from, 
        /// The end of the span.
        to, 
        /// The ["bidi
        /// level"](https://unicode.org/reports/tr9/#Basic_Display_Algorithm)
        /// of the span (in this context, 0 means
        /// left-to-right, 1 means right-to-left, 2 means left-to-right
        /// number inside right-to-left text).
        level) {
            this.from = from;
            this.to = to;
            this.level = level;
        }
        /// The direction of this span.
        get dir() { return this.level % 2 ? RTL : LTR; }
        /// @internal
        side(end, dir) { return (this.dir == dir) == end ? this.to : this.from; }
        /// @internal
        static find(order, index, level, assoc) {
            let maybe = -1;
            for (let i = 0; i < order.length; i++) {
                let span = order[i];
                if (span.from <= index && span.to >= index) {
                    if (span.level == level)
                        return i;
                    // When multiple spans match, if assoc != 0, take the one that
                    // covers that side, otherwise take the one with the minimum
                    // level.
                    if (maybe < 0 || (assoc != 0 ? (assoc < 0 ? span.from < index : span.to > index) : order[maybe].level > span.level))
                        maybe = i;
                }
            }
            if (maybe < 0)
                throw new RangeError("Index out of range");
            return maybe;
        }
    }
    // Reused array of character types
    const types = [];
    function computeOrder(line, direction) {
        let len = line.length, outerType = direction == LTR ? 1 /* L */ : 2 /* R */;
        if (!line || outerType == 1 /* L */ && !BidiRE.test(line))
            return trivialOrder(len);
        // W1. Examine each non-spacing mark (NSM) in the level run, and
        // change the type of the NSM to the type of the previous
        // character. If the NSM is at the start of the level run, it will
        // get the type of sor.
        // W2. Search backwards from each instance of a European number
        // until the first strong type (R, L, AL, or sor) is found. If an
        // AL is found, change the type of the European number to Arabic
        // number.
        // W3. Change all ALs to R.
        // (Left after this: L, R, EN, AN, ET, CS, NI)
        for (let i = 0, prev = outerType, prevStrong = outerType; i < len; i++) {
            let type = charType(line.charCodeAt(i));
            if (type == 512 /* NSM */)
                type = prev;
            else if (type == 8 /* EN */ && prevStrong == 4 /* AL */)
                type = 16 /* AN */;
            types[i] = type == 4 /* AL */ ? 2 /* R */ : type;
            if (type & 7 /* Strong */)
                prevStrong = type;
            prev = type;
        }
        // W5. A sequence of European terminators adjacent to European
        // numbers changes to all European numbers.
        // W6. Otherwise, separators and terminators change to Other
        // Neutral.
        // W7. Search backwards from each instance of a European number
        // until the first strong type (R, L, or sor) is found. If an L is
        // found, then change the type of the European number to L.
        // (Left after this: L, R, EN+AN, NI)
        for (let i = 0, prev = outerType, prevStrong = outerType; i < len; i++) {
            let type = types[i];
            if (type == 128 /* CS */) {
                if (i < len - 1 && prev == types[i + 1] && (prev & 24 /* Num */))
                    type = types[i] = prev;
                else
                    types[i] = 256 /* NI */;
            }
            else if (type == 64 /* ET */) {
                let end = i + 1;
                while (end < len && types[end] == 64 /* ET */)
                    end++;
                let replace = (i && prev == 8 /* EN */) || (end < len && types[end] == 8 /* EN */) ? (prevStrong == 1 /* L */ ? 1 /* L */ : 8 /* EN */) : 256 /* NI */;
                for (let j = i; j < end; j++)
                    types[j] = replace;
                i = end - 1;
            }
            else if (type == 8 /* EN */ && prevStrong == 1 /* L */) {
                types[i] = 1 /* L */;
            }
            prev = type;
            if (type & 7 /* Strong */)
                prevStrong = type;
        }
        // N1. A sequence of neutrals takes the direction of the
        // surrounding strong text if the text on both sides has the same
        // direction. European and Arabic numbers act as if they were R in
        // terms of their influence on neutrals. Start-of-level-run (sor)
        // and end-of-level-run (eor) are used at level run boundaries.
        // N2. Any remaining neutrals take the embedding direction.
        // (Left after this: L, R, EN+AN)
        for (let i = 0; i < len; i++) {
            if (types[i] == 256 /* NI */) {
                let end = i + 1;
                while (end < len && types[end] == 256 /* NI */)
                    end++;
                let beforeL = (i ? types[i - 1] : outerType) == 1 /* L */;
                let afterL = (end < len ? types[end] : outerType) == 1 /* L */;
                let replace = beforeL == afterL ? (beforeL ? 1 /* L */ : 2 /* R */) : outerType;
                for (let j = i; j < end; j++)
                    types[j] = replace;
                i = end - 1;
            }
        }
        // Here we depart from the documented algorithm, in order to avoid
        // building up an actual levels array. Since there are only three
        // levels (0, 1, 2) in an implementation that doesn't take
        // explicit embedding into account, we can build up the order on
        // the fly, without following the level-based algorithm.
        let order = [];
        if (outerType == 1 /* L */) {
            for (let i = 0; i < len;) {
                let start = i, rtl = types[i++] != 1 /* L */;
                while (i < len && rtl == (types[i] != 1 /* L */))
                    i++;
                if (rtl) {
                    for (let j = i; j > start;) {
                        let end = j, l = types[--j] != 2 /* R */;
                        while (j > start && l == (types[j - 1] != 2 /* R */))
                            j--;
                        order.push(new BidiSpan(j, end, l ? 2 : 1));
                    }
                }
                else {
                    order.push(new BidiSpan(start, i, 0));
                }
            }
        }
        else {
            for (let i = 0; i < len;) {
                let start = i, rtl = types[i++] == 2 /* R */;
                while (i < len && rtl == (types[i] == 2 /* R */))
                    i++;
                order.push(new BidiSpan(start, i, rtl ? 1 : 2));
            }
        }
        return order;
    }
    function trivialOrder(length) {
        return [new BidiSpan(0, length, 0)];
    }
    let movedOver = "";
    function moveVisually(line, order, dir, start, forward) {
        var _a;
        let startIndex = start.head - line.from, spanI = -1;
        if (startIndex == 0) {
            if (!forward || !line.length)
                return null;
            if (order[0].level != dir) {
                startIndex = order[0].side(false, dir);
                spanI = 0;
            }
        }
        else if (startIndex == line.length) {
            if (forward)
                return null;
            let last = order[order.length - 1];
            if (last.level != dir) {
                startIndex = last.side(true, dir);
                spanI = order.length - 1;
            }
        }
        if (spanI < 0)
            spanI = BidiSpan.find(order, startIndex, (_a = start.bidiLevel) !== null && _a !== void 0 ? _a : -1, start.assoc);
        let span = order[spanI];
        // End of span. (But not end of line--that was checked for above.)
        if (startIndex == span.side(forward, dir)) {
            span = order[spanI += forward ? 1 : -1];
            startIndex = span.side(!forward, dir);
        }
        let indexForward = forward == (span.dir == dir);
        let nextIndex = line.findClusterBreak(startIndex, indexForward);
        movedOver = line.slice(Math.min(startIndex, nextIndex), Math.max(startIndex, nextIndex));
        if (nextIndex != span.side(forward, dir))
            return EditorSelection.cursor(nextIndex + line.from, indexForward ? -1 : 1, span.level);
        let nextSpan = spanI == (forward ? order.length - 1 : 0) ? null : order[spanI + (forward ? 1 : -1)];
        if (!nextSpan && span.level != dir)
            return EditorSelection.cursor(forward ? line.to : line.from, forward ? -1 : 1, dir);
        if (nextSpan && nextSpan.level < span.level)
            return EditorSelection.cursor(nextSpan.side(!forward, dir) + line.from, 0, nextSpan.level);
        return EditorSelection.cursor(nextIndex + line.from, 0, span.level);
    }

    const wrappingWhiteSpace = ["pre-wrap", "normal", "pre-line"];
    class HeightOracle {
        constructor() {
            this.doc = Text.empty;
            this.lineWrapping = false;
            this.direction = Direction.LTR;
            this.heightSamples = {};
            this.lineHeight = 14;
            this.charWidth = 7;
            this.lineLength = 30;
            // Used to track, during updateHeight, if any actual heights changed
            this.heightChanged = false;
        }
        heightForGap(from, to) {
            let lines = this.doc.lineAt(to).number - this.doc.lineAt(from).number + 1;
            if (this.lineWrapping)
                lines += Math.ceil(((to - from) - (lines * this.lineLength * 0.5)) / this.lineLength);
            return this.lineHeight * lines;
        }
        heightForLine(length) {
            if (!this.lineWrapping)
                return this.lineHeight;
            let lines = 1 + Math.max(0, Math.ceil((length - this.lineLength) / (this.lineLength - 5)));
            return lines * this.lineHeight;
        }
        setDoc(doc) { this.doc = doc; return this; }
        mustRefresh(lineHeights, whiteSpace, direction) {
            let newHeight = false;
            for (let i = 0; i < lineHeights.length; i++) {
                let h = lineHeights[i];
                if (h < 0) {
                    i++;
                }
                else if (!this.heightSamples[Math.floor(h * 10)]) { // Round to .1 pixels
                    newHeight = true;
                    this.heightSamples[Math.floor(h * 10)] = true;
                }
            }
            return newHeight || (wrappingWhiteSpace.indexOf(whiteSpace) > -1) != this.lineWrapping || this.direction != direction;
        }
        refresh(whiteSpace, direction, lineHeight, charWidth, lineLength, knownHeights) {
            let lineWrapping = wrappingWhiteSpace.indexOf(whiteSpace) > -1;
            let changed = Math.round(lineHeight) != Math.round(this.lineHeight) ||
                this.lineWrapping != lineWrapping ||
                this.direction != direction;
            this.lineWrapping = lineWrapping;
            this.direction = direction;
            this.lineHeight = lineHeight;
            this.charWidth = charWidth;
            this.lineLength = lineLength;
            if (changed) {
                this.heightSamples = {};
                for (let i = 0; i < knownHeights.length; i++) {
                    let h = knownHeights[i];
                    if (h < 0)
                        i++;
                    else
                        this.heightSamples[Math.floor(h * 10)] = true;
                }
            }
            return changed;
        }
    }
    // This object is used by `updateHeight` to make DOM measurements
    // arrive at the right nides. The `heights` array is a sequence of
    // block heights, starting from position `from`.
    class MeasuredHeights {
        constructor(from, heights) {
            this.from = from;
            this.heights = heights;
            this.index = 0;
        }
        get more() { return this.index < this.heights.length; }
    }
    /// Record used to represent information about a block-level element
    /// in the editor view.
    class BlockInfo {
        /// @internal
        constructor(
        /// The start of the element in the document.
        from, 
        /// The length of the element.
        length, 
        /// The top position of the element.
        top, 
        /// Its height.
        height, 
        /// The type of element this is. When querying lines, this may be
        /// an array of all the blocks that make up the line.
        type) {
            this.from = from;
            this.length = length;
            this.top = top;
            this.height = height;
            this.type = type;
        }
        /// The end of the element as a document position.
        get to() { return this.from + this.length; }
        /// The bottom position of the element.
        get bottom() { return this.top + this.height; }
        /// @internal
        join(other) {
            let detail = (Array.isArray(this.type) ? this.type : [this])
                .concat(Array.isArray(other.type) ? other.type : [other]);
            return new BlockInfo(this.from, this.length + other.length, this.top, this.height + other.height, detail);
        }
    }
    var QueryType;
    (function (QueryType) {
        QueryType[QueryType["ByPos"] = 0] = "ByPos";
        QueryType[QueryType["ByHeight"] = 1] = "ByHeight";
        QueryType[QueryType["ByPosNoHeight"] = 2] = "ByPosNoHeight";
    })(QueryType || (QueryType = {}));
    const Epsilon = 1e-4;
    class HeightMap {
        constructor(length, // The number of characters covered
        height, // Height of this part of the document
        flags = 2 /* Outdated */) {
            this.length = length;
            this.height = height;
            this.flags = flags;
        }
        get outdated() { return (this.flags & 2 /* Outdated */) > 0; }
        set outdated(value) { this.flags = (value ? 2 /* Outdated */ : 0) | (this.flags & ~2 /* Outdated */); }
        setHeight(oracle, height) {
            if (this.height != height) {
                if (Math.abs(this.height - height) > Epsilon)
                    oracle.heightChanged = true;
                this.height = height;
            }
        }
        // Base case is to replace a leaf node, which simply builds a tree
        // from the new nodes and returns that (HeightMapBranch and
        // HeightMapGap override this to actually use from/to)
        replace(_from, _to, nodes) {
            return HeightMap.of(nodes);
        }
        // Again, these are base cases, and are overridden for branch and gap nodes.
        decomposeLeft(_to, result) { result.push(this); }
        decomposeRight(_from, result) { result.push(this); }
        applyChanges(decorations, oldDoc, oracle, changes) {
            let me = this;
            for (let i = changes.length - 1; i >= 0; i--) {
                let { fromA, toA, fromB, toB } = changes[i];
                let start = me.lineAt(fromA, QueryType.ByPosNoHeight, oldDoc, 0, 0);
                let end = start.to >= toA ? start : me.lineAt(toA, QueryType.ByPosNoHeight, oldDoc, 0, 0);
                toB += end.to - toA;
                toA = end.to;
                while (i > 0 && start.from <= changes[i - 1].toA) {
                    fromA = changes[i - 1].fromA;
                    fromB = changes[i - 1].fromB;
                    i--;
                    if (fromA < start.from)
                        start = me.lineAt(fromA, QueryType.ByPosNoHeight, oldDoc, 0, 0);
                }
                fromB += start.from - fromA;
                fromA = start.from;
                let nodes = NodeBuilder.build(oracle, decorations, fromB, toB);
                me = me.replace(fromA, toA, nodes);
            }
            return me.updateHeight(oracle, 0);
        }
        static empty() { return new HeightMapText(0, 0); }
        // nodes uses null values to indicate the position of line breaks.
        // There are never line breaks at the start or end of the array, or
        // two line breaks next to each other, and the array isn't allowed
        // to be empty (same restrictions as return value from the builder).
        static of(nodes) {
            if (nodes.length == 1)
                return nodes[0];
            let i = 0, j = nodes.length, before = 0, after = 0;
            for (;;) {
                if (i == j) {
                    if (before > after * 2) {
                        let split = nodes[i - 1];
                        if (split.break)
                            nodes.splice(--i, 1, split.left, null, split.right);
                        else
                            nodes.splice(--i, 1, split.left, split.right);
                        j += 1 + split.break;
                        before -= split.size;
                    }
                    else if (after > before * 2) {
                        let split = nodes[j];
                        if (split.break)
                            nodes.splice(j, 1, split.left, null, split.right);
                        else
                            nodes.splice(j, 1, split.left, split.right);
                        j += 2 + split.break;
                        after -= split.size;
                    }
                    else {
                        break;
                    }
                }
                else if (before < after) {
                    let next = nodes[i++];
                    if (next)
                        before += next.size;
                }
                else {
                    let next = nodes[--j];
                    if (next)
                        after += next.size;
                }
            }
            let brk = 0;
            if (nodes[i - 1] == null) {
                brk = 1;
                i--;
            }
            else if (nodes[i] == null) {
                brk = 1;
                j++;
            }
            return new HeightMapBranch(HeightMap.of(nodes.slice(0, i)), brk, HeightMap.of(nodes.slice(j)));
        }
    }
    HeightMap.prototype.size = 1;
    class HeightMapBlock extends HeightMap {
        constructor(length, height, type) {
            super(length, height);
            this.type = type;
        }
        blockAt(_height, _doc, top, offset) {
            return new BlockInfo(offset, this.length, top, this.height, this.type);
        }
        lineAt(_value, _type, doc, top, offset) {
            return this.blockAt(0, doc, top, offset);
        }
        forEachLine(_from, _to, doc, top, offset, f) {
            f(this.blockAt(0, doc, top, offset));
        }
        updateHeight(oracle, offset = 0, _force = false, measured) {
            if (measured && measured.from <= offset && measured.more)
                this.setHeight(oracle, measured.heights[measured.index++]);
            this.outdated = false;
            return this;
        }
        toString() { return `block(${this.length})`; }
    }
    class HeightMapText extends HeightMapBlock {
        constructor(length, height) {
            super(length, height, BlockType.Text);
            this.collapsed = 0; // Amount of collapsed content in the line
            this.widgetHeight = 0; // Maximum inline widget height
        }
        replace(_from, _to, nodes) {
            let node = nodes[0];
            if (nodes.length == 1 && (node instanceof HeightMapText || node instanceof HeightMapGap && (node.flags & 4 /* SingleLine */)) &&
                Math.abs(this.length - node.length) < 10) {
                if (node instanceof HeightMapGap)
                    node = new HeightMapText(node.length, this.height);
                else
                    node.height = this.height;
                if (!this.outdated)
                    node.outdated = false;
                return node;
            }
            else {
                return HeightMap.of(nodes);
            }
        }
        updateHeight(oracle, offset = 0, force = false, measured) {
            if (measured && measured.from <= offset && measured.more)
                this.setHeight(oracle, measured.heights[measured.index++]);
            else if (force || this.outdated)
                this.setHeight(oracle, Math.max(this.widgetHeight, oracle.heightForLine(this.length - this.collapsed)));
            this.outdated = false;
            return this;
        }
        toString() {
            return `line(${this.length}${this.collapsed ? -this.collapsed : ""}${this.widgetHeight ? ":" + this.widgetHeight : ""})`;
        }
    }
    class HeightMapGap extends HeightMap {
        constructor(length) { super(length, 0); }
        lines(doc, offset) {
            let firstLine = doc.lineAt(offset).number, lastLine = doc.lineAt(offset + this.length).number;
            return { firstLine, lastLine, lineHeight: this.height / (lastLine - firstLine + 1) };
        }
        blockAt(height, doc, top, offset) {
            let { firstLine, lastLine, lineHeight } = this.lines(doc, offset);
            let line = Math.max(0, Math.min(lastLine - firstLine, Math.floor((height - top) / lineHeight)));
            let { from, length } = doc.line(firstLine + line);
            return new BlockInfo(from, length, top + lineHeight * line, lineHeight, BlockType.Text);
        }
        lineAt(value, type, doc, top, offset) {
            if (type == QueryType.ByHeight)
                return this.blockAt(value, doc, top, offset);
            if (type == QueryType.ByPosNoHeight) {
                let { from, to } = doc.lineAt(value);
                return new BlockInfo(from, to - from, 0, 0, BlockType.Text);
            }
            let { firstLine, lineHeight } = this.lines(doc, offset);
            let { from, length, number } = doc.lineAt(value);
            return new BlockInfo(from, length, top + lineHeight * (number - firstLine), lineHeight, BlockType.Text);
        }
        forEachLine(from, to, doc, top, offset, f) {
            let { firstLine, lineHeight } = this.lines(doc, offset);
            for (let pos = from; pos < to;) {
                let line = doc.lineAt(pos);
                if (pos == from)
                    top += lineHeight * (line.number - firstLine);
                f(new BlockInfo(line.from, line.length, top, top += lineHeight, BlockType.Text));
                pos = line.to + 1;
            }
        }
        replace(from, to, nodes) {
            let after = this.length - to;
            if (after > 0) {
                let last = nodes[nodes.length - 1];
                if (last instanceof HeightMapGap)
                    nodes[nodes.length - 1] = new HeightMapGap(last.length + after);
                else
                    nodes.push(null, new HeightMapGap(after - 1));
            }
            if (from > 0) {
                let first = nodes[0];
                if (first instanceof HeightMapGap)
                    nodes[0] = new HeightMapGap(from + first.length);
                else
                    nodes.unshift(new HeightMapGap(from - 1), null);
            }
            return HeightMap.of(nodes);
        }
        decomposeLeft(to, result) {
            result.push(new HeightMapGap(to - 1), null);
        }
        decomposeRight(from, result) {
            result.push(null, new HeightMapGap(this.length - from - 1));
        }
        updateHeight(oracle, offset = 0, force = false, measured) {
            let end = offset + this.length;
            if (measured && measured.from <= offset + this.length && measured.more) {
                // Fill in part of this gap with measured lines. We know there
                // can't be widgets or collapsed ranges in those lines, because
                // they would already have been added to the heightmap (gaps
                // only contain plain text).
                let nodes = [], pos = Math.max(offset, measured.from);
                if (measured.from > offset)
                    nodes.push(new HeightMapGap(measured.from - offset - 1).updateHeight(oracle, offset));
                while (pos <= end && measured.more) {
                    let len = oracle.doc.lineAt(pos).length;
                    if (nodes.length)
                        nodes.push(null);
                    let line = new HeightMapText(len, measured.heights[measured.index++]);
                    line.outdated = false;
                    nodes.push(line);
                    pos += len + 1;
                }
                if (pos <= end)
                    nodes.push(null, new HeightMapGap(end - pos).updateHeight(oracle, pos));
                oracle.heightChanged = true;
                return HeightMap.of(nodes);
            }
            else if (force || this.outdated) {
                this.setHeight(oracle, oracle.heightForGap(offset, offset + this.length));
                this.outdated = false;
            }
            return this;
        }
        toString() { return `gap(${this.length})`; }
    }
    class HeightMapBranch extends HeightMap {
        constructor(left, brk, right) {
            super(left.length + brk + right.length, left.height + right.height, brk | (left.outdated || right.outdated ? 2 /* Outdated */ : 0));
            this.left = left;
            this.right = right;
            this.size = left.size + right.size;
        }
        get break() { return this.flags & 1 /* Break */; }
        blockAt(height, doc, top, offset) {
            let mid = top + this.left.height;
            return height < mid || this.right.height == 0 ? this.left.blockAt(height, doc, top, offset)
                : this.right.blockAt(height, doc, mid, offset + this.left.length + this.break);
        }
        lineAt(value, type, doc, top, offset) {
            let rightTop = top + this.left.height, rightOffset = offset + this.left.length + this.break;
            let left = type == QueryType.ByHeight ? value < rightTop || this.right.height == 0 : value < rightOffset;
            let base = left ? this.left.lineAt(value, type, doc, top, offset)
                : this.right.lineAt(value, type, doc, rightTop, rightOffset);
            if (this.break || (left ? base.to < rightOffset : base.from > rightOffset))
                return base;
            let subQuery = type == QueryType.ByPosNoHeight ? QueryType.ByPosNoHeight : QueryType.ByPos;
            if (left)
                return base.join(this.right.lineAt(rightOffset, subQuery, doc, rightTop, rightOffset));
            else
                return this.left.lineAt(rightOffset, subQuery, doc, top, offset).join(base);
        }
        forEachLine(from, to, doc, top, offset, f) {
            let rightTop = top + this.left.height, rightOffset = offset + this.left.length + this.break;
            if (this.break) {
                if (from < rightOffset)
                    this.left.forEachLine(from, to, doc, top, offset, f);
                if (to >= rightOffset)
                    this.right.forEachLine(from, to, doc, rightTop, rightOffset, f);
            }
            else {
                let mid = this.lineAt(rightOffset, QueryType.ByPos, doc, top, offset);
                if (from < mid.from)
                    this.left.forEachLine(from, mid.from - 1, doc, top, offset, f);
                if (mid.to >= from && mid.from <= to)
                    f(mid);
                if (to > mid.to)
                    this.right.forEachLine(mid.to + 1, to, doc, rightTop, rightOffset, f);
            }
        }
        replace(from, to, nodes) {
            let rightStart = this.left.length + this.break;
            if (to < rightStart)
                return this.balanced(this.left.replace(from, to, nodes), this.right);
            if (from > this.left.length)
                return this.balanced(this.left, this.right.replace(from - rightStart, to - rightStart, nodes));
            let result = [];
            if (from > 0)
                this.decomposeLeft(from, result);
            let left = result.length;
            for (let node of nodes)
                result.push(node);
            if (from > 0)
                mergeGaps(result, left - 1);
            if (to < this.length) {
                let right = result.length;
                this.decomposeRight(to, result);
                mergeGaps(result, right);
            }
            return HeightMap.of(result);
        }
        decomposeLeft(to, result) {
            let left = this.left.length;
            if (to <= left)
                return this.left.decomposeLeft(to, result);
            result.push(this.left);
            if (this.break) {
                left++;
                if (to >= left)
                    result.push(null);
            }
            if (to > left)
                this.right.decomposeLeft(to - left, result);
        }
        decomposeRight(from, result) {
            let left = this.left.length, right = left + this.break;
            if (from >= right)
                return this.right.decomposeRight(from - right, result);
            if (from < left)
                this.left.decomposeRight(from, result);
            if (this.break && from < right)
                result.push(null);
            result.push(this.right);
        }
        balanced(left, right) {
            if (left.size > 2 * right.size || right.size > 2 * left.size)
                return HeightMap.of(this.break ? [left, null, right] : [left, right]);
            this.left = left;
            this.right = right;
            this.height = left.height + right.height;
            this.outdated = left.outdated || right.outdated;
            this.size = left.size + right.size;
            this.length = left.length + this.break + right.length;
            return this;
        }
        updateHeight(oracle, offset = 0, force = false, measured) {
            let { left, right } = this, rightStart = offset + left.length + this.break, rebalance = null;
            if (measured && measured.from <= offset + left.length && measured.more)
                rebalance = left = left.updateHeight(oracle, offset, force, measured);
            else
                left.updateHeight(oracle, offset, force);
            if (measured && measured.from <= rightStart + right.length && measured.more)
                rebalance = right = right.updateHeight(oracle, rightStart, force, measured);
            else
                right.updateHeight(oracle, rightStart, force);
            if (rebalance)
                return this.balanced(left, right);
            this.height = this.left.height + this.right.height;
            this.outdated = false;
            return this;
        }
        toString() { return this.left + (this.break ? " " : "-") + this.right; }
    }
    function mergeGaps(nodes, around) {
        let before, after;
        if (nodes[around] == null &&
            (before = nodes[around - 1]) instanceof HeightMapGap &&
            (after = nodes[around + 1]) instanceof HeightMapGap)
            nodes.splice(around - 1, 3, new HeightMapGap(before.length + 1 + after.length));
    }
    const relevantWidgetHeight = 5;
    class NodeBuilder {
        constructor(pos, oracle) {
            this.pos = pos;
            this.oracle = oracle;
            this.nodes = [];
            this.lineStart = -1;
            this.lineEnd = -1;
            this.covering = null;
            this.writtenTo = pos;
        }
        get isCovered() {
            return this.covering && this.nodes[this.nodes.length - 1] == this.covering;
        }
        span(_from, to) {
            if (this.lineStart > -1) {
                let end = Math.min(to, this.lineEnd), last = this.nodes[this.nodes.length - 1];
                if (last instanceof HeightMapText)
                    last.length += end - this.pos;
                else if (end > this.pos || !this.isCovered)
                    this.nodes.push(new HeightMapText(end - this.pos, -1));
                this.writtenTo = end;
                if (to > end) {
                    this.nodes.push(null);
                    this.writtenTo++;
                    this.lineStart = -1;
                }
            }
            this.pos = to;
        }
        point(from, to, deco) {
            if (from < to || deco.heightRelevant) {
                let height = deco.widget ? Math.max(0, deco.widget.estimatedHeight) : 0;
                let len = to - from;
                if (deco.block) {
                    this.addBlock(new HeightMapBlock(len, height, deco.type));
                }
                else if (len || height >= relevantWidgetHeight) {
                    this.addLineDeco(height, len);
                }
            }
            else if (to > from) {
                this.span(from, to);
            }
            if (this.lineEnd > -1 && this.lineEnd < this.pos)
                this.lineEnd = this.oracle.doc.lineAt(this.pos).to;
        }
        enterLine() {
            if (this.lineStart > -1)
                return;
            let { from, to } = this.oracle.doc.lineAt(this.pos);
            this.lineStart = from;
            this.lineEnd = to;
            if (this.writtenTo < from) {
                if (this.writtenTo < from - 1 || this.nodes[this.nodes.length - 1] == null)
                    this.nodes.push(this.blankContent(this.writtenTo, from - 1));
                this.nodes.push(null);
            }
            if (this.pos > from)
                this.nodes.push(new HeightMapText(this.pos - from, -1));
            this.writtenTo = this.pos;
        }
        blankContent(from, to) {
            let gap = new HeightMapGap(to - from);
            if (this.oracle.doc.lineAt(from).to == to)
                gap.flags |= 4 /* SingleLine */;
            return gap;
        }
        ensureLine() {
            this.enterLine();
            let last = this.nodes.length ? this.nodes[this.nodes.length - 1] : null;
            if (last instanceof HeightMapText)
                return last;
            let line = new HeightMapText(0, -1);
            this.nodes.push(line);
            return line;
        }
        addBlock(block) {
            this.enterLine();
            if (block.type == BlockType.WidgetAfter && !this.isCovered)
                this.ensureLine();
            this.nodes.push(block);
            this.writtenTo = this.pos = this.pos + block.length;
            if (block.type != BlockType.WidgetBefore)
                this.covering = block;
        }
        addLineDeco(height, length) {
            let line = this.ensureLine();
            line.length += length;
            line.collapsed += length;
            line.widgetHeight = Math.max(line.widgetHeight, height);
            this.writtenTo = this.pos = this.pos + length;
        }
        finish(from) {
            let last = this.nodes.length == 0 ? null : this.nodes[this.nodes.length - 1];
            if (this.lineStart > -1 && !(last instanceof HeightMapText) && !this.isCovered)
                this.nodes.push(new HeightMapText(0, -1));
            else if (this.writtenTo < this.pos || last == null)
                this.nodes.push(this.blankContent(this.writtenTo, this.pos));
            let pos = from;
            for (let node of this.nodes) {
                if (node instanceof HeightMapText)
                    node.updateHeight(this.oracle, pos);
                pos += node ? node.length : 1;
            }
            return this.nodes;
        }
        // Always called with a region that on both sides either stretches
        // to a line break or the end of the document.
        // The returned array uses null to indicate line breaks, but never
        // starts or ends in a line break, or has multiple line breaks next
        // to each other.
        static build(oracle, decorations, from, to) {
            let builder = new NodeBuilder(from, oracle);
            RangeSet.spans(decorations, from, to, builder);
            return builder.finish(from);
        }
        get minPointSize() { return 0; }
    }
    function heightRelevantDecoChanges(a, b, diff) {
        let comp = new DecorationComparator();
        RangeSet.compare(a, b, diff, comp);
        return comp.changes;
    }
    class DecorationComparator {
        constructor() {
            this.changes = [];
        }
        compareRange() { }
        comparePoint(from, to, a, b) {
            if (from < to || a && a.heightRelevant || b && b.heightRelevant)
                addRange(from, to, this.changes);
        }
        get minPointSize() { return 0; }
    }

    const none$3 = [];
    const clickAddsSelectionRange = Facet.define();
    const dragMovesSelection = Facet.define();
    const mouseSelectionStyle = Facet.define();
    const exceptionSink = Facet.define();
    const updateListener = Facet.define();
    const inputHandler = Facet.define();
    /// Log or report an unhandled exception in client code. Should
    /// probably only be used by extension code that allows client code to
    /// provide functions, and calls those functions in a context where an
    /// exception can't be propagated to calling code in a reasonable way
    /// (for example when in an event handler).
    ///
    /// Either calls a handler registered with
    /// [`EditorView.exceptionSink`](#view.EditorView^exceptionSink),
    /// `window.onerror`, if defined, or `console.error` (in which case
    /// it'll pass `context`, when given, as first argument).
    function logException(state, exception, context) {
        let handler = state.facet(exceptionSink);
        if (handler.length)
            handler[0](exception);
        else if (window.onerror)
            window.onerror(String(exception), context, undefined, undefined, exception);
        else if (context)
            console.error(context + ":", exception);
        else
            console.error(exception);
    }
    const editable = Facet.define({ combine: values => values.length ? values[0] : true });
    /// Used to [declare](#view.PluginSpec.provide) which
    /// [fields](#view.PluginValue) a [view plugin](#view.ViewPlugin)
    /// provides.
    class PluginFieldProvider {
        /// @internal
        constructor(
        /// @internal
        field, 
        /// @internal
        get) {
            this.field = field;
            this.get = get;
        }
    }
    /// Plugin fields are a mechanism for allowing plugins to provide
    /// values that can be retrieved through the
    /// [`pluginField`](#view.EditorView.pluginField) view method.
    class PluginField {
        from(get) {
            return new PluginFieldProvider(this, get);
        }
        /// Define a new plugin field.
        static define() { return new PluginField(); }
    }
    /// Plugins can provide additional scroll margins (space around the
    /// sides of the scrolling element that should be considered
    /// invisible) through this field. This can be useful when the
    /// plugin introduces elements that cover part of that element (for
    /// example a horizontally fixed gutter).
    PluginField.scrollMargins = PluginField.define();
    let nextPluginID = 0;
    const viewPlugin = Facet.define();
    /// View plugins associate stateful values with a view. They can
    /// influence the way the content is drawn, and are notified of things
    /// that happen in the view.
    class ViewPlugin {
        constructor(
        /// @internal
        id, 
        /// @internal
        create, 
        /// @internal
        fields) {
            this.id = id;
            this.create = create;
            this.fields = fields;
            this.extension = viewPlugin.of(this);
        }
        /// Define a plugin from a constructor function that creates the
        /// plugin's value, given an editor view.
        static define(create, spec) {
            let { eventHandlers, provide, decorations } = spec || {};
            let fields = [];
            if (provide)
                for (let provider of Array.isArray(provide) ? provide : [provide])
                    fields.push(provider);
            if (eventHandlers)
                fields.push(domEventHandlers.from((value) => ({ plugin: value, handlers: eventHandlers })));
            if (decorations)
                for (let get of Array.isArray(decorations) ? decorations : [decorations])
                    fields.push(pluginDecorations.from(get));
            return new ViewPlugin(nextPluginID++, create, fields);
        }
        /// Create a plugin for a class whose constructor takes a single
        /// editor view as argument.
        static fromClass(cls, spec) {
            return ViewPlugin.define(view => new cls(view), spec);
        }
    }
    // FIXME somehow ensure that no replacing decorations end up in here
    const pluginDecorations = PluginField.define();
    const domEventHandlers = PluginField.define();
    class PluginInstance {
        constructor(value, spec) {
            this.value = value;
            this.spec = spec;
        }
        static create(spec, view) {
            let value;
            try {
                value = spec.create(view);
            }
            catch (e) {
                logException(view.state, e, "CodeMirror plugin crashed");
                return PluginInstance.dummy;
            }
            return new PluginInstance(value, spec);
        }
        takeField(type, target) {
            for (let { field, get } of this.spec.fields)
                if (field == type)
                    target.push(get(this.value));
        }
        update(update) {
            if (!this.value.update)
                return this;
            try {
                this.value.update(update);
                return this;
            }
            catch (e) {
                logException(update.state, e, "CodeMirror plugin crashed");
                if (this.value.destroy)
                    try {
                        this.value.destroy();
                    }
                    catch (_) { }
                return PluginInstance.dummy;
            }
        }
        destroy(view) {
            if (this.value.destroy) {
                try {
                    this.value.destroy();
                }
                catch (e) {
                    logException(view.state, e, "CodeMirror plugin crashed");
                }
            }
        }
    }
    PluginInstance.dummy = new PluginInstance({}, ViewPlugin.define(() => ({})));
    const editorAttributes = Facet.define({
        combine: values => values.reduce((a, b) => combineAttrs(b, a), {})
    });
    const contentAttributes = Facet.define({
        combine: values => values.reduce((a, b) => combineAttrs(b, a), {})
    });
    // Provide decorations
    const decorations = Facet.define();
    const styleModule = Facet.define();
    class ChangedRange {
        constructor(fromA, toA, fromB, toB) {
            this.fromA = fromA;
            this.toA = toA;
            this.fromB = fromB;
            this.toB = toB;
        }
        join(other) {
            return new ChangedRange(Math.min(this.fromA, other.fromA), Math.max(this.toA, other.toA), Math.min(this.fromB, other.fromB), Math.max(this.toB, other.toB));
        }
        addToSet(set) {
            let i = set.length, me = this;
            for (; i > 0; i--) {
                let range = set[i - 1];
                if (range.fromA > me.toA)
                    continue;
                if (range.toA < me.fromA)
                    break;
                me = me.join(range);
                set.splice(i - 1, 1);
            }
            set.splice(i, 0, me);
            return set;
        }
        static extendWithRanges(diff, ranges) {
            if (ranges.length == 0)
                return diff;
            let result = [];
            for (let dI = 0, rI = 0, posA = 0, posB = 0;; dI++) {
                let next = dI == diff.length ? null : diff[dI], off = posA - posB;
                let end = next ? next.fromB : 1e9;
                while (rI < ranges.length && ranges[rI] < end) {
                    let from = ranges[rI], to = ranges[rI + 1];
                    let fromB = Math.max(posB, from), toB = Math.min(end, to);
                    if (fromB <= toB)
                        new ChangedRange(fromB + off, toB + off, fromB, toB).addToSet(result);
                    if (to > end)
                        break;
                    else
                        rI += 2;
                }
                if (!next)
                    return result;
                new ChangedRange(next.fromA, next.toA, next.fromB, next.toB).addToSet(result);
                posA = next.toA;
                posB = next.toB;
            }
        }
    }
    /// View [plugins](#view.ViewPlugin) are given instances of this
    /// class, which describe what happened, whenever the view is updated.
    class ViewUpdate {
        /// @internal
        constructor(
        /// The editor view that the update is associated with.
        view, 
        /// The new editor state.
        state, 
        /// The transactions involved in the update. May be empty.
        transactions = none$3) {
            this.view = view;
            this.state = state;
            this.transactions = transactions;
            /// @internal
            this.flags = 0;
            this.prevState = view.state;
            this.changes = ChangeSet.empty(this.prevState.doc.length);
            for (let tr of transactions)
                this.changes = this.changes.compose(tr.changes);
            let changedRanges = [];
            this.changes.iterChangedRanges((fromA, toA, fromB, toB) => changedRanges.push(new ChangedRange(fromA, toA, fromB, toB)));
            this.changedRanges = changedRanges;
            let focus = view.hasFocus;
            if (focus != view.inputState.notifiedFocused) {
                view.inputState.notifiedFocused = focus;
                this.flags != 1 /* Focus */;
            }
            if (this.docChanged)
                this.flags |= 2 /* Height */;
        }
        /// Tells you whether the viewport changed in this update.
        get viewportChanged() {
            return (this.flags & 4 /* Viewport */) > 0;
        }
        /// Indicates whether the line height in the editor changed in this update.
        get heightChanged() {
            return (this.flags & 2 /* Height */) > 0;
        }
        /// Returns true when the document changed or the size of the editor
        /// or the lines or characters within it has changed.
        get geometryChanged() {
            return this.docChanged || (this.flags & (16 /* Geometry */ | 2 /* Height */)) > 0;
        }
        /// True when this update indicates a focus change.
        get focusChanged() {
            return (this.flags & 1 /* Focus */) > 0;
        }
        /// Whether the document changed in this update.
        get docChanged() {
            return this.transactions.some(tr => tr.docChanged);
        }
        /// Whether the selection was explicitly set in this update.
        get selectionSet() {
            return this.transactions.some(tr => tr.selection);
        }
        /// @internal
        get empty() { return this.flags == 0 && this.transactions.length == 0; }
    }

    function visiblePixelRange(dom, paddingTop) {
        let rect = dom.getBoundingClientRect();
        let left = Math.max(0, rect.left), right = Math.min(innerWidth, rect.right);
        let top = Math.max(0, rect.top), bottom = Math.min(innerHeight, rect.bottom);
        for (let parent = dom.parentNode; parent;) { // (Cast to any because TypeScript is useless with Node types)
            if (parent.nodeType == 1) {
                if ((parent.scrollHeight > parent.clientHeight || parent.scrollWidth > parent.clientWidth) &&
                    window.getComputedStyle(parent).overflow != "visible") {
                    let parentRect = parent.getBoundingClientRect();
                    left = Math.max(left, parentRect.left);
                    right = Math.min(right, parentRect.right);
                    top = Math.max(top, parentRect.top);
                    bottom = Math.min(bottom, parentRect.bottom);
                }
                parent = parent.parentNode;
            }
            else if (parent.nodeType == 11) { // Shadow root
                parent = parent.host;
            }
            else {
                break;
            }
        }
        return { left: left - rect.left, right: right - rect.left,
            top: top - (rect.top + paddingTop), bottom: bottom - (rect.top + paddingTop) };
    }
    // Line gaps are placeholder widgets used to hide pieces of overlong
    // lines within the viewport, as a kludge to keep the editor
    // responsive when a ridiculously long line is loaded into it.
    class LineGap {
        constructor(from, to, size) {
            this.from = from;
            this.to = to;
            this.size = size;
        }
        static same(a, b) {
            if (a.length != b.length)
                return false;
            for (let i = 0; i < a.length; i++) {
                let gA = a[i], gB = b[i];
                if (gA.from != gB.from || gA.to != gB.to || gA.size != gB.size)
                    return false;
            }
            return true;
        }
        draw(wrapping) {
            return Decoration.replace({ widget: new LineGapWidget(this.size, wrapping) }).range(this.from, this.to);
        }
    }
    class LineGapWidget extends WidgetType {
        constructor(size, vertical) {
            super();
            this.size = size;
            this.vertical = vertical;
        }
        eq(other) { return other.size == this.size && other.vertical == this.vertical; }
        toDOM() {
            let elt = document.createElement("div");
            if (this.vertical) {
                elt.style.height = this.size + "px";
            }
            else {
                elt.style.width = this.size + "px";
                elt.style.height = "2px";
                elt.style.display = "inline-block";
            }
            return elt;
        }
        get estimatedHeight() { return this.vertical ? this.size : -1; }
    }
    class ViewState {
        constructor(state) {
            this.state = state;
            // These are contentDOM-local coordinates
            this.pixelViewport = { left: 0, right: window.innerWidth, top: 0, bottom: 0 };
            this.inView = true;
            this.paddingTop = 0;
            this.paddingBottom = 0;
            this.contentWidth = 0;
            this.heightOracle = new HeightOracle;
            this.heightMap = HeightMap.empty();
            this.scrollTo = null;
            // Briefly set to true when printing, to disable viewport limiting
            this.printing = false;
            this.visibleRanges = [];
            // Cursor 'assoc' is only significant when the cursor is on a line
            // wrap point, where it must stick to the character that it is
            // associated with. Since browsers don't provide a reasonable
            // interface to set or query this, when a selection is set that
            // might cause this to be signficant, this flag is set. The next
            // measure phase will check whether the cursor is on a line-wrapping
            // boundary and, if so, reset it to make sure it is positioned in
            // the right place.
            this.mustEnforceCursorAssoc = false;
            this.heightMap = this.heightMap.applyChanges(state.facet(decorations), Text.empty, this.heightOracle.setDoc(state.doc), [new ChangedRange(0, 0, 0, state.doc.length)]);
            this.viewport = this.getViewport(0, null);
            this.lineGaps = this.ensureLineGaps([]);
            this.lineGapDeco = Decoration.set(this.lineGaps.map(gap => gap.draw(false)));
            this.computeVisibleRanges();
        }
        update(update, scrollTo = null) {
            let prev = this.state;
            this.state = update.state;
            let newDeco = this.state.facet(decorations);
            let contentChanges = update.changedRanges;
            let heightChanges = ChangedRange.extendWithRanges(contentChanges, heightRelevantDecoChanges(update.prevState.facet(decorations), newDeco, update ? update.changes : ChangeSet.empty(this.state.doc.length)));
            let prevHeight = this.heightMap.height;
            this.heightMap = this.heightMap.applyChanges(newDeco, prev.doc, this.heightOracle.setDoc(this.state.doc), heightChanges);
            if (this.heightMap.height != prevHeight)
                update.flags |= 2 /* Height */;
            let viewport = heightChanges.length ? this.mapViewport(this.viewport, update.changes) : this.viewport;
            if (scrollTo && (scrollTo.head < viewport.from || scrollTo.head > viewport.to) || !this.viewportIsAppropriate(viewport))
                viewport = this.getViewport(0, scrollTo);
            if (!viewport.eq(this.viewport)) {
                this.viewport = viewport;
                update.flags |= 4 /* Viewport */;
            }
            if (this.lineGaps.length || this.viewport.to - this.viewport.from > 15000 /* MinViewPort */)
                update.flags |= this.updateLineGaps(this.ensureLineGaps(this.mapLineGaps(this.lineGaps, update.changes)));
            this.computeVisibleRanges();
            if (scrollTo)
                this.scrollTo = scrollTo;
            if (!this.mustEnforceCursorAssoc && update.selectionSet && update.view.lineWrapping &&
                update.state.selection.primary.empty && update.state.selection.primary.assoc)
                this.mustEnforceCursorAssoc = true;
        }
        measure(docView, repeated) {
            let dom = docView.dom, whiteSpace = "", direction = Direction.LTR;
            if (!repeated) {
                // Vertical padding
                let style = window.getComputedStyle(dom);
                whiteSpace = style.whiteSpace, direction = (style.direction == "rtl" ? Direction.RTL : Direction.LTR);
                this.paddingTop = parseInt(style.paddingTop) || 0;
                this.paddingBottom = parseInt(style.paddingBottom) || 0;
            }
            // Pixel viewport
            let pixelViewport = this.printing ? { top: -1e8, bottom: 1e8, left: -1e8, right: 1e8 } : visiblePixelRange(dom, this.paddingTop);
            let dTop = pixelViewport.top - this.pixelViewport.top, dBottom = pixelViewport.bottom - this.pixelViewport.bottom;
            this.pixelViewport = pixelViewport;
            this.inView = this.pixelViewport.bottom > this.pixelViewport.top && this.pixelViewport.right > this.pixelViewport.left;
            if (!this.inView)
                return 0;
            let lineHeights = docView.measureVisibleLineHeights();
            let refresh = false, bias = 0, result = 0, oracle = this.heightOracle;
            if (!repeated) {
                let contentWidth = docView.dom.clientWidth;
                if (oracle.mustRefresh(lineHeights, whiteSpace, direction) ||
                    oracle.lineWrapping && Math.abs(contentWidth - this.contentWidth) > oracle.charWidth) {
                    let { lineHeight, charWidth } = docView.measureTextSize();
                    refresh = oracle.refresh(whiteSpace, direction, lineHeight, charWidth, contentWidth / charWidth, lineHeights);
                    if (refresh) {
                        docView.minWidth = 0;
                        result |= 16 /* Geometry */;
                    }
                }
                if (this.contentWidth != contentWidth) {
                    this.contentWidth = contentWidth;
                    result |= 16 /* Geometry */;
                }
                if (dTop > 0 && dBottom > 0)
                    bias = Math.max(dTop, dBottom);
                else if (dTop < 0 && dBottom < 0)
                    bias = Math.min(dTop, dBottom);
            }
            oracle.heightChanged = false;
            this.heightMap = this.heightMap.updateHeight(oracle, 0, refresh, new MeasuredHeights(this.viewport.from, lineHeights));
            if (oracle.heightChanged)
                result |= 2 /* Height */;
            if (!this.viewportIsAppropriate(this.viewport, bias) ||
                this.scrollTo && (this.scrollTo.head < this.viewport.from || this.scrollTo.head > this.viewport.to)) {
                this.viewport = this.getViewport(bias, this.scrollTo);
                result |= 4 /* Viewport */;
            }
            if (this.lineGaps.length || this.viewport.to - this.viewport.from > 15000 /* MinViewPort */)
                result |= this.updateLineGaps(this.ensureLineGaps(refresh ? [] : this.lineGaps));
            this.computeVisibleRanges();
            if (this.mustEnforceCursorAssoc) {
                this.mustEnforceCursorAssoc = false;
                // This is done in the read stage, because moving the selection
                // to a line end is going to trigger a layout anyway, so it
                // can't be a pure write. It should be rare that it does any
                // writing.
                docView.enforceCursorAssoc();
            }
            return result;
        }
        getViewport(bias, scrollTo) {
            // This will divide VP.Margin between the top and the
            // bottom, depending on the bias (the change in viewport position
            // since the last update). It'll hold a number between 0 and 1
            let marginTop = 0.5 - Math.max(-0.5, Math.min(0.5, bias / 1000 /* Margin */ / 2));
            let map = this.heightMap, doc = this.state.doc, { top, bottom } = this.pixelViewport;
            let viewport = new Viewport(map.lineAt(top - marginTop * 1000 /* Margin */, QueryType.ByHeight, doc, 0, 0).from, map.lineAt(bottom + (1 - marginTop) * 1000 /* Margin */, QueryType.ByHeight, doc, 0, 0).to);
            // If scrollTo is given, make sure the viewport includes that position
            if (scrollTo) {
                if (scrollTo.head < viewport.from) {
                    let { top: newTop } = map.lineAt(scrollTo.head, QueryType.ByPos, doc, 0, 0);
                    viewport = new Viewport(map.lineAt(newTop - 1000 /* Margin */ / 2, QueryType.ByHeight, doc, 0, 0).from, map.lineAt(newTop + (bottom - top) + 1000 /* Margin */ / 2, QueryType.ByHeight, doc, 0, 0).to);
                }
                else if (scrollTo.head > viewport.to) {
                    let { bottom: newBottom } = map.lineAt(scrollTo.head, QueryType.ByPos, doc, 0, 0);
                    viewport = new Viewport(map.lineAt(newBottom - (bottom - top) - 1000 /* Margin */ / 2, QueryType.ByHeight, doc, 0, 0).from, map.lineAt(newBottom + 1000 /* Margin */ / 2, QueryType.ByHeight, doc, 0, 0).to);
                }
            }
            return viewport;
        }
        mapViewport(viewport, changes) {
            let from = changes.mapPos(viewport.from, -1), to = changes.mapPos(viewport.to, 1);
            return new Viewport(this.heightMap.lineAt(from, QueryType.ByPos, this.state.doc, 0, 0).from, this.heightMap.lineAt(to, QueryType.ByPos, this.state.doc, 0, 0).to);
        }
        // Checks if a given viewport covers the visible part of the
        // document and not too much beyond that.
        viewportIsAppropriate({ from, to }, bias = 0) {
            let { top } = this.heightMap.lineAt(from, QueryType.ByPos, this.state.doc, 0, 0);
            let { bottom } = this.heightMap.lineAt(to, QueryType.ByPos, this.state.doc, 0, 0);
            return (from == 0 || top <= this.pixelViewport.top - Math.max(10 /* MinCoverMargin */, Math.min(-bias, 250 /* MaxCoverMargin */))) &&
                (to == this.state.doc.length ||
                    bottom >= this.pixelViewport.bottom + Math.max(10 /* MinCoverMargin */, Math.min(bias, 250 /* MaxCoverMargin */))) &&
                (top > this.pixelViewport.top - 2 * 1000 /* Margin */ && bottom < this.pixelViewport.bottom + 2 * 1000 /* Margin */);
        }
        mapLineGaps(gaps, changes) {
            if (!gaps.length || changes.empty)
                return gaps;
            let mapped = [];
            for (let gap of gaps)
                if (!changes.touchesRange(gap.from, gap.to))
                    mapped.push(new LineGap(changes.mapPos(gap.from), changes.mapPos(gap.to), gap.size));
            return mapped;
        }
        // Computes positions in the viewport where the start or end of a
        // line should be hidden, trying to reuse existing line gaps when
        // appropriate to avoid unneccesary redraws.
        // Uses crude character-counting for the positioning and sizing,
        // since actual DOM coordinates aren't always available and
        // predictable. Relies on generous margins (see LG.Margin) to hide
        // the artifacts this might produce from the user.
        ensureLineGaps(current) {
            let gaps = [];
            // This won't work at all in predominantly right-to-left text.
            if (this.heightOracle.direction != Direction.LTR)
                return gaps;
            this.heightMap.forEachLine(this.viewport.from, this.viewport.to, this.state.doc, 0, 0, line => {
                if (line.length < 10000 /* Margin */)
                    return;
                let structure = lineStructure(line.from, line.to, this.state);
                if (structure.total < 10000 /* Margin */)
                    return;
                let viewFrom, viewTo;
                if (this.heightOracle.lineWrapping) {
                    if (line.from != this.viewport.from)
                        viewFrom = line.from;
                    else
                        viewFrom = findPosition(structure, (this.pixelViewport.top - line.top) / line.height);
                    if (line.to != this.viewport.to)
                        viewTo = line.to;
                    else
                        viewTo = findPosition(structure, (this.pixelViewport.bottom - line.top) / line.height);
                }
                else {
                    let totalWidth = structure.total * this.heightOracle.charWidth;
                    viewFrom = findPosition(structure, this.pixelViewport.left / totalWidth);
                    viewTo = findPosition(structure, this.pixelViewport.right / totalWidth);
                }
                let sel = this.state.selection.primary;
                // Make sure the gap doesn't cover a selection end
                if (sel.from <= viewFrom && sel.to >= line.from)
                    viewFrom = sel.from;
                if (sel.from <= line.to && sel.to >= viewTo)
                    viewTo = sel.to;
                let gapTo = viewFrom - 10000 /* Margin */, gapFrom = viewTo + 10000 /* Margin */;
                if (gapTo > line.from + 5000 /* HalfMargin */)
                    gaps.push(find(current, gap => gap.from == line.from && gap.to > gapTo - 5000 /* HalfMargin */ && gap.to < gapTo + 5000 /* HalfMargin */) ||
                        new LineGap(line.from, gapTo, this.gapSize(line, gapTo, true, structure)));
                if (gapFrom < line.to - 5000 /* HalfMargin */)
                    gaps.push(find(current, gap => gap.to == line.to && gap.from > gapFrom - 5000 /* HalfMargin */ &&
                        gap.from < gapFrom + 5000 /* HalfMargin */) ||
                        new LineGap(gapFrom, line.to, this.gapSize(line, gapFrom, false, structure)));
            });
            return gaps;
        }
        gapSize(line, pos, start, structure) {
            if (this.heightOracle.lineWrapping) {
                let height = line.height * findFraction(structure, pos);
                return start ? height : line.height - height;
            }
            else {
                let ratio = findFraction(structure, pos);
                return structure.total * this.heightOracle.charWidth * (start ? ratio : 1 - ratio);
            }
        }
        updateLineGaps(gaps) {
            if (!LineGap.same(gaps, this.lineGaps)) {
                this.lineGaps = gaps;
                this.lineGapDeco = Decoration.set(gaps.map(gap => gap.draw(this.heightOracle.lineWrapping)));
                return 8 /* LineGaps */;
            }
            return 0;
        }
        computeVisibleRanges() {
            let deco = this.state.facet(decorations);
            if (this.lineGaps.length)
                deco = deco.concat(this.lineGapDeco);
            let ranges = [];
            RangeSet.spans(deco, this.viewport.from, this.viewport.to, {
                span(from, to) { ranges.push({ from, to }); },
                point() { },
                minPointSize: 20
            });
            this.visibleRanges = ranges;
        }
        lineAt(pos, editorTop) {
            return this.heightMap.lineAt(pos, QueryType.ByPos, this.state.doc, editorTop + this.paddingTop, 0);
        }
        lineAtHeight(height, editorTop) {
            return this.heightMap.lineAt(height, QueryType.ByHeight, this.state.doc, editorTop + this.paddingTop, 0);
        }
        blockAtHeight(height, editorTop) {
            return this.heightMap.blockAt(height, this.state.doc, editorTop + this.paddingTop, 0);
        }
        forEachLine(from, to, f, editorTop) {
            return this.heightMap.forEachLine(from, to, this.state.doc, editorTop + this.paddingTop, 0, f);
        }
    }
    /// Indicates the range of the document that is in the visible
    /// viewport.
    class Viewport {
        constructor(from, to) {
            this.from = from;
            this.to = to;
        }
        eq(b) { return this.from == b.from && this.to == b.to; }
    }
    function lineStructure(from, to, state) {
        let ranges = [], pos = from, total = 0;
        RangeSet.spans(state.facet(decorations), from, to, {
            span() { },
            point(from, to) {
                if (from > pos) {
                    ranges.push({ from: pos, to: from });
                    total += from - pos;
                }
                pos = to;
            },
            minPointSize: 20 // We're only interested in collapsed ranges of a significant size
        });
        if (pos < to) {
            ranges.push({ from: pos, to });
            total += to - pos;
        }
        return { total, ranges };
    }
    function findPosition({ total, ranges }, ratio) {
        if (ratio <= 0)
            return ranges[0].from;
        if (ratio >= 1)
            return ranges[ranges.length - 1].to;
        let dist = Math.floor(total * ratio);
        for (let i = 0;; i++) {
            let { from, to } = ranges[i], size = to - from;
            if (dist <= size)
                return from + dist;
            dist -= size;
        }
    }
    function findFraction(structure, pos) {
        let counted = 0;
        for (let { from, to } of structure.ranges) {
            if (pos <= to) {
                counted += pos - from;
                break;
            }
            counted += to - from;
        }
        return counted / structure.total;
    }
    function find(array, f) {
        for (let val of array)
            if (f(val))
                return val;
        return undefined;
    }

    const none$4 = [];
    class DocView extends ContentView {
        constructor(view) {
            super();
            this.view = view;
            this.viewports = none$4;
            this.compositionDeco = Decoration.none;
            this.decorations = [];
            // Track a minimum width for the editor. When measuring sizes in
            // checkLayout, this is updated to point at the width of a given
            // element and its extent in the document. When a change happens in
            // that range, these are reset. That way, once we've seen a
            // line/element of a given length, we keep the editor wide enough to
            // fit at least that element, until it is changed, at which point we
            // forget it again.
            this.minWidth = 0;
            this.minWidthFrom = 0;
            this.minWidthTo = 0;
            // Track whether the DOM selection was set in a lossy way, so that
            // we don't mess it up when reading it back it
            this.impreciseAnchor = null;
            this.impreciseHead = null;
            this.setDOM(view.contentDOM);
            this.children = [new LineView];
            this.children[0].setParent(this);
            this.updateInner([new ChangedRange(0, 0, 0, view.state.doc.length)], this.updateDeco(), 0);
        }
        get root() { return this.view.root; }
        get editorView() { return this.view; }
        get length() { return this.view.state.doc.length; }
        // Update the document view to a given state. scrollIntoView can be
        // used as a hint to compute a new viewport that includes that
        // position, if we know the editor is going to scroll that position
        // into view.
        update(update) {
            var _a;
            let changedRanges = update.changedRanges;
            if (this.minWidth > 0 && changedRanges.length) {
                if (!changedRanges.every(({ fromA, toA }) => toA < this.minWidthFrom || fromA > this.minWidthTo)) {
                    this.minWidth = 0;
                }
                else {
                    this.minWidthFrom = update.changes.mapPos(this.minWidthFrom, 1);
                    this.minWidthTo = update.changes.mapPos(this.minWidthTo, 1);
                }
            }
            if (!((_a = this.view.inputState) === null || _a === void 0 ? void 0 : _a.composing))
                this.compositionDeco = Decoration.none;
            else if (update.transactions.length)
                this.compositionDeco = computeCompositionDeco(this.view, update.changes);
            // When the DOM nodes around the selection are moved to another
            // parent, Chrome sometimes reports a different selection through
            // getSelection than the one that it actually shows to the user.
            // This forces a selection update when lines are joined to work
            // around that. Issue #54
            let forceSelection = (browser.ie || browser.chrome) && !this.compositionDeco.size && update &&
                update.state.doc.lines != update.prevState.doc.lines;
            let prevDeco = this.decorations, deco = this.updateDeco();
            let decoDiff = findChangedDeco(prevDeco, deco, update.changes);
            changedRanges = ChangedRange.extendWithRanges(changedRanges, decoDiff);
            let pointerSel = update.transactions.some(tr => tr.annotation(Transaction.userEvent) == "pointerselection");
            if (this.dirty == 0 /* Not */ && changedRanges.length == 0 &&
                !(update.flags & (4 /* Viewport */ | 8 /* LineGaps */)) &&
                update.state.selection.primary.from >= this.view.viewport.from &&
                update.state.selection.primary.to <= this.view.viewport.to) {
                this.updateSelection(forceSelection, pointerSel);
                return false;
            }
            else {
                this.updateInner(changedRanges, deco, update.prevState.doc.length, forceSelection, pointerSel);
                return true;
            }
        }
        // Used both by update and checkLayout do perform the actual DOM
        // update
        updateInner(changes, deco, oldLength, forceSelection = false, pointerSel = false) {
            this.updateChildren(changes, deco, oldLength);
            this.view.observer.ignore(() => {
                // Lock the height during redrawing, since Chrome sometimes
                // messes with the scroll position during DOM mutation (though
                // no relayout is triggered and I cannot imagine how it can
                // recompute the scroll position without a layout)
                this.dom.style.height = this.view.viewState.heightMap.height + "px";
                this.dom.style.minWidth = this.minWidth ? this.minWidth + "px" : "";
                // Chrome will sometimes, when DOM mutations occur directly
                // around the selection, get confused and report a different
                // selection from the one it displays (issue #218). This tries
                // to detect that situation.
                let track = browser.chrome ? { node: getSelection(this.view.root).focusNode, written: false } : undefined;
                this.sync(track);
                this.dirty = 0 /* Not */;
                if (track === null || track === void 0 ? void 0 : track.written)
                    forceSelection = true;
                this.updateSelection(forceSelection, pointerSel);
                this.dom.style.height = "";
            });
        }
        updateChildren(changes, deco, oldLength) {
            let cursor = this.childCursor(oldLength);
            for (let i = changes.length - 1;; i--) {
                let next = i >= 0 ? changes[i] : null;
                if (!next)
                    break;
                let { fromA, toA, fromB, toB } = next;
                let { content, breakAtStart, openStart, openEnd } = ContentBuilder.build(this.view.state.doc, fromB, toB, deco);
                let { i: toI, off: toOff } = cursor.findPos(toA, 1);
                let { i: fromI, off: fromOff } = cursor.findPos(fromA, -1);
                this.replaceRange(fromI, fromOff, toI, toOff, content, breakAtStart, openStart, openEnd);
            }
        }
        replaceRange(fromI, fromOff, toI, toOff, content, breakAtStart, openStart, openEnd) {
            let before = this.children[fromI], last = content.length ? content[content.length - 1] : null;
            let breakAtEnd = last ? last.breakAfter : breakAtStart;
            // Change within a single line
            if (fromI == toI && !breakAtStart && !breakAtEnd && content.length < 2 &&
                before.merge(fromOff, toOff, content.length ? last : null, fromOff == 0, openStart, openEnd))
                return;
            let after = this.children[toI];
            // Make sure the end of the line after the update is preserved in `after`
            if (toOff < after.length || after.children.length && after.children[after.children.length - 1].length == 0) {
                // If we're splitting a line, separate part of the start line to
                // avoid that being mangled when updating the start line.
                if (fromI == toI) {
                    after = after.split(toOff);
                    toOff = 0;
                }
                // If the element after the replacement should be merged with
                // the last replacing element, update `content`
                if (!breakAtEnd && last && after.merge(0, toOff, last, true, 0, openEnd)) {
                    content[content.length - 1] = after;
                }
                else {
                    // Remove the start of the after element, if necessary, and
                    // add it to `content`.
                    if (toOff || after.children.length && after.children[0].length == 0)
                        after.merge(0, toOff, null, false, 0, openEnd);
                    content.push(after);
                }
            }
            else if (after.breakAfter) {
                // The element at `toI` is entirely covered by this range.
                // Preserve its line break, if any.
                if (last)
                    last.breakAfter = 1;
                else
                    breakAtStart = 1;
            }
            // Since we've handled the next element from the current elements
            // now, make sure `toI` points after that.
            toI++;
            before.breakAfter = breakAtStart;
            if (fromOff > 0) {
                if (!breakAtStart && content.length && before.merge(fromOff, before.length, content[0], false, openStart, 0)) {
                    before.breakAfter = content.shift().breakAfter;
                }
                else if (fromOff < before.length || before.children.length && before.children[before.children.length - 1].length == 0) {
                    before.merge(fromOff, before.length, null, false, openStart, 0);
                }
                fromI++;
            }
            // Try to merge widgets on the boundaries of the replacement
            while (fromI < toI && content.length) {
                if (this.children[toI - 1].match(content[content.length - 1]))
                    toI--, content.pop();
                else if (this.children[fromI].match(content[0]))
                    fromI++, content.shift();
                else
                    break;
            }
            if (fromI < toI || content.length)
                this.replaceChildren(fromI, toI, content);
        }
        // Sync the DOM selection to this.state.selection
        updateSelection(force = false, fromPointer = false) {
            if (!(fromPointer || this.mayControlSelection()))
                return;
            let primary = this.view.state.selection.primary;
            // FIXME need to handle the case where the selection falls inside a block range
            let anchor = this.domAtPos(primary.anchor);
            let head = this.domAtPos(primary.head);
            let domSel = getSelection(this.root);
            // If the selection is already here, or in an equivalent position, don't touch it
            if (force || !domSel.focusNode ||
                (browser.gecko && primary.empty && nextToUneditable(domSel.focusNode, domSel.focusOffset)) ||
                !isEquivalentPosition(anchor.node, anchor.offset, domSel.anchorNode, domSel.anchorOffset) ||
                !isEquivalentPosition(head.node, head.offset, domSel.focusNode, domSel.focusOffset)) {
                this.view.observer.ignore(() => {
                    if (primary.empty) {
                        // Work around https://bugzilla.mozilla.org/show_bug.cgi?id=1612076
                        if (browser.gecko) {
                            let nextTo = nextToUneditable(anchor.node, anchor.offset);
                            if (nextTo && nextTo != (1 /* Before */ | 2 /* After */)) {
                                let text = nearbyTextNode(anchor.node, anchor.offset, nextTo == 1 /* Before */ ? 1 : -1);
                                if (text)
                                    anchor = new DOMPos(text, nextTo == 1 /* Before */ ? 0 : text.nodeValue.length);
                            }
                        }
                        domSel.collapse(anchor.node, anchor.offset);
                        if (primary.bidiLevel != null && domSel.cursorBidiLevel != null)
                            domSel.cursorBidiLevel = primary.bidiLevel;
                    }
                    else if (domSel.extend) {
                        // Selection.extend can be used to create an 'inverted' selection
                        // (one where the focus is before the anchor), but not all
                        // browsers support it yet.
                        domSel.collapse(anchor.node, anchor.offset);
                        domSel.extend(head.node, head.offset);
                    }
                    else {
                        // Primitive (IE) way
                        let range = document.createRange();
                        if (primary.anchor > primary.head)
                            [anchor, head] = [head, anchor];
                        range.setEnd(head.node, head.offset);
                        range.setStart(anchor.node, anchor.offset);
                        domSel.removeAllRanges();
                        domSel.addRange(range);
                    }
                });
            }
            this.impreciseAnchor = anchor.precise ? null : new DOMPos(domSel.anchorNode, domSel.anchorOffset);
            this.impreciseHead = head.precise ? null : new DOMPos(domSel.focusNode, domSel.focusOffset);
        }
        enforceCursorAssoc() {
            let cursor = this.view.state.selection.primary;
            let sel = getSelection(this.root);
            if (!cursor.empty || !cursor.assoc || !sel.modify)
                return;
            let line = LineView.find(this, cursor.head); // FIXME provide view-line-range finding helper
            if (!line)
                return;
            let lineStart = line.posAtStart;
            if (cursor.head == lineStart || cursor.head == lineStart + line.length)
                return;
            let before = this.coordsAt(cursor.head, -1), after = this.coordsAt(cursor.head, 1);
            if (!before || !after || before.bottom > after.top)
                return;
            let dom = this.domAtPos(cursor.head + cursor.assoc);
            sel.collapse(dom.node, dom.offset);
            sel.modify("move", cursor.assoc < 0 ? "forward" : "backward", "lineboundary");
        }
        mayControlSelection() {
            return this.view.state.facet(editable) ? this.root.activeElement == this.dom : hasSelection(this.dom, getSelection(this.root));
        }
        nearest(dom) {
            for (let cur = dom; cur;) {
                let domView = ContentView.get(cur);
                if (domView && domView.rootView == this)
                    return domView;
                cur = cur.parentNode;
            }
            return null;
        }
        posFromDOM(node, offset) {
            let view = this.nearest(node);
            if (!view)
                throw new RangeError("Trying to find position for a DOM position outside of the document");
            return view.localPosFromDOM(node, offset) + view.posAtStart;
        }
        domAtPos(pos) {
            let { i, off } = this.childCursor().findPos(pos, -1);
            for (; i < this.children.length - 1;) {
                let child = this.children[i];
                if (off < child.length || child instanceof LineView)
                    break;
                i++;
                off = 0;
            }
            return this.children[i].domAtPos(off);
        }
        coordsAt(pos, side) {
            for (let off = this.length, i = this.children.length - 1;; i--) {
                let child = this.children[i], start = off - child.breakAfter - child.length;
                if (pos > start || pos == start && (child.type == BlockType.Text || !i || this.children[i - 1].breakAfter))
                    return child.coordsAt(pos - start, side);
                off = start;
            }
        }
        measureVisibleLineHeights() {
            let result = [], { from, to } = this.view.viewState.viewport;
            let minWidth = Math.max(this.view.scrollDOM.clientWidth, this.minWidth) + 1;
            for (let pos = 0, i = 0; i < this.children.length; i++) {
                let child = this.children[i], end = pos + child.length;
                if (end > to)
                    break;
                if (pos >= from) {
                    result.push(child.dom.getBoundingClientRect().height);
                    let width = child.dom.scrollWidth;
                    if (width > minWidth) {
                        this.minWidth = minWidth = width;
                        this.minWidthFrom = pos;
                        this.minWidthTo = end;
                    }
                }
                pos = end + child.breakAfter;
            }
            return result;
        }
        measureTextSize() {
            for (let child of this.children) {
                if (child instanceof LineView) {
                    let measure = child.measureTextSize();
                    if (measure)
                        return measure;
                }
            }
            // If no workable line exists, force a layout of a measurable element
            let dummy = document.createElement("div"), lineHeight, charWidth;
            dummy.className = "cm-line";
            dummy.textContent = "abc def ghi jkl mno pqr stu";
            this.view.observer.ignore(() => {
                this.dom.appendChild(dummy);
                let rect = clientRectsFor(dummy.firstChild)[0];
                lineHeight = dummy.getBoundingClientRect().height;
                charWidth = rect ? rect.width / 27 : 7;
                dummy.remove();
            });
            return { lineHeight, charWidth };
        }
        childCursor(pos = this.length) {
            // Move back to start of last element when possible, so that
            // `ChildCursor.findPos` doesn't have to deal with the edge case
            // of being after the last element.
            let i = this.children.length;
            if (i)
                pos -= this.children[--i].length;
            return new ChildCursor(this.children, pos, i);
        }
        computeBlockGapDeco() {
            let visible = this.view.viewState.viewport, viewports = [visible];
            let { head, anchor } = this.view.state.selection.primary;
            if (head < visible.from || head > visible.to) {
                let { from, to } = this.view.viewState.lineAt(head, 0);
                viewports.push(new Viewport(from, to));
            }
            if (!viewports.some(({ from, to }) => anchor >= from && anchor <= to)) {
                let { from, to } = this.view.viewState.lineAt(anchor, 0);
                viewports.push(new Viewport(from, to));
            }
            this.viewports = viewports.sort((a, b) => a.from - b.from);
            let deco = [];
            for (let pos = 0, i = 0;; i++) {
                let next = i == viewports.length ? null : viewports[i];
                let end = next ? next.from - 1 : this.length;
                if (end > pos) {
                    let height = this.view.viewState.lineAt(end, 0).bottom - this.view.viewState.lineAt(pos, 0).top;
                    deco.push(Decoration.replace({ widget: new BlockGapWidget(height), block: true, inclusive: true }).range(pos, end));
                }
                if (!next)
                    break;
                pos = next.to + 1;
            }
            return Decoration.set(deco);
        }
        updateDeco() {
            return this.decorations = [
                this.computeBlockGapDeco(),
                this.view.viewState.lineGapDeco,
                this.compositionDeco,
                ...this.view.state.facet(decorations),
                ...this.view.pluginField(pluginDecorations)
            ];
        }
        scrollPosIntoView(pos, side) {
            let rect = this.coordsAt(pos, side);
            if (!rect)
                return;
            let mLeft = 0, mRight = 0, mTop = 0, mBottom = 0;
            for (let margins of this.view.pluginField(PluginField.scrollMargins))
                if (margins) {
                    let { left, right, top, bottom } = margins;
                    if (left != null)
                        mLeft = Math.max(mLeft, left);
                    if (right != null)
                        mRight = Math.max(mRight, right);
                    if (top != null)
                        mTop = Math.max(mTop, top);
                    if (bottom != null)
                        mBottom = Math.max(mBottom, bottom);
                }
            scrollRectIntoView(this.dom, {
                left: rect.left - mLeft, top: rect.top - mTop,
                right: rect.right + mRight, bottom: rect.bottom + mBottom
            });
        }
    }
    // Browsers appear to reserve a fixed amount of bits for height
    // styles, and ignore or clip heights above that. For Chrome and
    // Firefox, this is in the 20 million range, so we try to stay below
    // that.
    const MaxNodeHeight = 1e7;
    class BlockGapWidget extends WidgetType {
        constructor(height) {
            super();
            this.height = height;
        }
        toDOM() {
            let elt = document.createElement("div");
            this.updateDOM(elt);
            return elt;
        }
        eq(other) { return other.height == this.height; }
        updateDOM(elt) {
            if (this.height < MaxNodeHeight) {
                while (elt.lastChild)
                    elt.lastChild.remove();
                elt.style.height = this.height + "px";
            }
            else {
                elt.style.height = "";
                for (let remaining = this.height; remaining > 0; remaining -= MaxNodeHeight) {
                    let fill = elt.appendChild(document.createElement("div"));
                    fill.style.height = Math.min(remaining, MaxNodeHeight) + "px";
                }
            }
            return true;
        }
        get estimatedHeight() { return this.height; }
    }
    function computeCompositionDeco(view, changes) {
        let sel = getSelection(view.root);
        let textNode = sel.focusNode && nearbyTextNode(sel.focusNode, sel.focusOffset, 0);
        if (!textNode)
            return Decoration.none;
        let cView = view.docView.nearest(textNode);
        let from, to, topNode = textNode;
        if (cView instanceof InlineView) {
            while (cView.parent instanceof InlineView)
                cView = cView.parent;
            from = cView.posAtStart;
            to = from + cView.length;
            topNode = cView.dom;
        }
        else if (cView instanceof LineView) {
            while (topNode.parentNode != cView.dom)
                topNode = topNode.parentNode;
            let prev = topNode.previousSibling;
            while (prev && !ContentView.get(prev))
                prev = prev.previousSibling;
            from = to = prev ? ContentView.get(prev).posAtEnd : cView.posAtStart;
        }
        else {
            return Decoration.none;
        }
        let newFrom = changes.mapPos(from, 1), newTo = Math.max(newFrom, changes.mapPos(to, -1));
        let text = textNode.nodeValue, { state } = view;
        if (newTo - newFrom < text.length) {
            if (state.sliceDoc(newFrom, Math.min(state.doc.length, newFrom + text.length)) == text)
                newTo = newFrom + text.length;
            else if (state.sliceDoc(Math.max(0, newTo - text.length), newTo) == text)
                newFrom = newTo - text.length;
            else
                return Decoration.none;
        }
        else if (state.sliceDoc(newFrom, newTo) != text) {
            return Decoration.none;
        }
        return Decoration.set(Decoration.replace({ widget: new CompositionWidget(topNode, textNode) }).range(newFrom, newTo));
    }
    class CompositionWidget extends WidgetType {
        constructor(top, text) {
            super();
            this.top = top;
            this.text = text;
        }
        eq(other) { return this.top == other.top && this.text == other.text; }
        toDOM() { return this.top; }
        ignoreEvent() { return false; }
        get customView() { return CompositionView; }
    }
    function nearbyTextNode(node, offset, side) {
        for (;;) {
            if (node.nodeType == 3)
                return node;
            if (node.nodeType == 1 && offset > 0 && side <= 0) {
                node = node.childNodes[offset - 1];
                offset = maxOffset(node);
            }
            else if (node.nodeType == 1 && offset < node.childNodes.length && side >= 0) {
                node = node.childNodes[offset];
                offset = 0;
            }
            else {
                return null;
            }
        }
    }
    function nextToUneditable(node, offset) {
        if (node.nodeType != 1)
            return 0;
        return (offset && node.childNodes[offset - 1].contentEditable == "false" ? 1 /* Before */ : 0) |
            (offset < node.childNodes.length && node.childNodes[offset].contentEditable == "false" ? 2 /* After */ : 0);
    }
    class DecorationComparator$1 {
        constructor() {
            this.changes = [];
        }
        compareRange(from, to) { addRange(from, to, this.changes); }
        comparePoint(from, to) { addRange(from, to, this.changes); }
    }
    function findChangedDeco(a, b, diff) {
        let comp = new DecorationComparator$1;
        RangeSet.compare(a, b, diff, comp);
        return comp.changes;
    }

    function groupAt(state, pos, bias = 1) {
        let categorize = state.charCategorizer(pos);
        let line = state.doc.lineAt(pos), linePos = pos - line.from;
        if (line.length == 0)
            return EditorSelection.cursor(pos);
        if (linePos == 0)
            bias = 1;
        else if (linePos == line.length)
            bias = -1;
        let from = linePos, to = linePos;
        if (bias < 0)
            from = line.findClusterBreak(linePos, false);
        else
            to = line.findClusterBreak(linePos, true);
        let cat = categorize(line.slice(from, to));
        while (from > 0) {
            let prev = line.findClusterBreak(from, false);
            if (categorize(line.slice(prev, from)) != cat)
                break;
            from = prev;
        }
        while (to < line.length) {
            let next = line.findClusterBreak(to, true);
            if (categorize(line.slice(to, next)) != cat)
                break;
            to = next;
        }
        return EditorSelection.range(from + line.from, to + line.from);
    }
    // Search the DOM for the {node, offset} position closest to the given
    // coordinates. Very inefficient and crude, but can usually be avoided
    // by calling caret(Position|Range)FromPoint instead.
    // FIXME holding arrow-up/down at the end of the viewport is a rather
    // common use case that will repeatedly trigger this code. Maybe
    // introduce some element of binary search after all?
    function getdx(x, rect) {
        return rect.left > x ? rect.left - x : Math.max(0, x - rect.right);
    }
    function getdy(y, rect) {
        return rect.top > y ? rect.top - y : Math.max(0, y - rect.bottom);
    }
    function yOverlap(a, b) {
        return a.top < b.bottom - 1 && a.bottom > b.top + 1;
    }
    function upTop(rect, top) {
        return top < rect.top ? { top, left: rect.left, right: rect.right, bottom: rect.bottom } : rect;
    }
    function upBot(rect, bottom) {
        return bottom > rect.bottom ? { top: rect.top, left: rect.left, right: rect.right, bottom } : rect;
    }
    function domPosAtCoords(parent, x, y) {
        let closest, closestRect, closestX, closestY;
        let above, below, aboveRect, belowRect;
        for (let child = parent.firstChild; child; child = child.nextSibling) {
            let rects = clientRectsFor(child);
            for (let i = 0; i < rects.length; i++) {
                let rect = rects[i];
                if (closestRect && yOverlap(closestRect, rect))
                    rect = upTop(upBot(rect, closestRect.bottom), closestRect.top);
                let dx = getdx(x, rect), dy = getdy(y, rect);
                if (dx == 0 && dy == 0)
                    return child.nodeType == 3 ? domPosInText(child, x, y) : domPosAtCoords(child, x, y);
                if (!closest || closestY > dy || closestY == dy && closestX > dx) {
                    closest = child;
                    closestRect = rect;
                    closestX = dx;
                    closestY = dy;
                }
                if (dx == 0) {
                    if (y > rect.bottom && (!aboveRect || aboveRect.bottom < rect.bottom)) {
                        above = child;
                        aboveRect = rect;
                    }
                    else if (y < rect.top && (!belowRect || belowRect.top > rect.top)) {
                        below = child;
                        belowRect = rect;
                    }
                }
                else if (aboveRect && yOverlap(aboveRect, rect)) {
                    aboveRect = upBot(aboveRect, rect.bottom);
                }
                else if (belowRect && yOverlap(belowRect, rect)) {
                    belowRect = upTop(belowRect, rect.top);
                }
            }
        }
        if (aboveRect && aboveRect.bottom >= y) {
            closest = above;
            closestRect = aboveRect;
        }
        else if (belowRect && belowRect.top <= y) {
            closest = below;
            closestRect = belowRect;
        }
        if (!closest)
            return { node: parent, offset: 0 };
        let clipX = Math.max(closestRect.left, Math.min(closestRect.right, x));
        if (closest.nodeType == 3)
            return domPosInText(closest, clipX, y);
        if (!closestX && closest.contentEditable == "true")
            return domPosAtCoords(closest, clipX, y);
        let offset = Array.prototype.indexOf.call(parent.childNodes, closest) +
            (x >= (closestRect.left + closestRect.right) / 2 ? 1 : 0);
        return { node: parent, offset };
    }
    function domPosInText(node, x, y) {
        let len = node.nodeValue.length, range = tempRange();
        for (let i = 0; i < len; i++) {
            range.setEnd(node, i + 1);
            range.setStart(node, i);
            let rects = range.getClientRects();
            for (let j = 0; j < rects.length; j++) {
                let rect = rects[j];
                if (rect.top == rect.bottom)
                    continue;
                if (rect.left - 1 <= x && rect.right + 1 >= x &&
                    rect.top - 1 <= y && rect.bottom + 1 >= y) {
                    let right = x >= (rect.left + rect.right) / 2, after = right;
                    if (browser.chrome || browser.gecko) {
                        // Check for RTL on browsers that support getting client
                        // rects for empty ranges.
                        range.setEnd(node, i);
                        let rectBefore = range.getBoundingClientRect();
                        if (rectBefore.left == rect.right)
                            after = !right;
                    }
                    return { node, offset: i + (after ? 1 : 0) };
                }
            }
        }
        return { node, offset: 0 };
    }
    function posAtCoords(view, { x, y }, bias = -1) {
        let content = view.contentDOM.getBoundingClientRect(), block;
        let halfLine = view.defaultLineHeight / 2;
        for (let bounced = false;;) {
            block = view.blockAtHeight(y, content.top);
            if (block.top > y || block.bottom < y) {
                bias = block.top > y ? -1 : 1;
                y = Math.min(block.bottom - halfLine, Math.max(block.top + halfLine, y));
                if (bounced)
                    return -1;
                else
                    bounced = true;
            }
            if (block.type == BlockType.Text)
                break;
            y = bias > 0 ? block.bottom + halfLine : block.top - halfLine;
        }
        let lineStart = block.from;
        // If this is outside of the rendered viewport, we can't determine a position
        if (lineStart < view.viewport.from)
            return view.viewport.from == 0 ? 0 : null;
        if (lineStart > view.viewport.to)
            return view.viewport.to == view.state.doc.length ? view.state.doc.length : null;
        // Clip x to the viewport sides
        x = Math.max(content.left + 1, Math.min(content.right - 1, x));
        let root = view.root, element = root.elementFromPoint(x, y);
        // There's visible editor content under the point, so we can try
        // using caret(Position|Range)FromPoint as a shortcut
        let node, offset = -1;
        if (element && view.contentDOM.contains(element) && !(view.docView.nearest(element) instanceof WidgetView)) {
            if (root.caretPositionFromPoint) {
                let pos = root.caretPositionFromPoint(x, y);
                if (pos)
                    ({ offsetNode: node, offset } = pos);
            }
            else if (root.caretRangeFromPoint) {
                let range = root.caretRangeFromPoint(x, y);
                if (range)
                    ({ startContainer: node, startOffset: offset } = range);
            }
        }
        // No luck, do our own (potentially expensive) search
        if (!node || !view.docView.dom.contains(node)) {
            let line = LineView.find(view.docView, lineStart);
            ({ node, offset } = domPosAtCoords(line.dom, x, y));
        }
        return view.docView.posFromDOM(node, offset);
    }
    function moveToLineBoundary(view, start, forward, includeWrap) {
        let line = view.state.doc.lineAt(start.head);
        let coords = !includeWrap || !view.lineWrapping ? null
            : view.coordsAtPos(start.assoc < 0 && start.head > line.from ? start.head - 1 : start.head);
        if (coords) {
            let editorRect = view.dom.getBoundingClientRect();
            let pos = view.posAtCoords({ x: forward == (view.textDirection == Direction.LTR) ? editorRect.right - 1 : editorRect.left + 1,
                y: (coords.top + coords.bottom) / 2 });
            if (pos != null)
                return EditorSelection.cursor(pos, forward ? -1 : 1);
        }
        let lineView = LineView.find(view.docView, start.head);
        let end = lineView ? (forward ? lineView.posAtEnd : lineView.posAtStart) : (forward ? line.to : line.from);
        return EditorSelection.cursor(end, forward ? -1 : 1);
    }
    function moveByChar(view, start, forward, by) {
        let line = view.state.doc.lineAt(start.head), spans = view.bidiSpans(line);
        for (let cur = start, check = null;;) {
            let next = moveVisually(line, spans, view.textDirection, cur, forward), char = movedOver;
            if (!next) {
                if (line.number == (forward ? view.state.doc.lines : 1))
                    return cur;
                char = "\n";
                line = view.state.doc.line(line.number + (forward ? 1 : -1));
                spans = view.bidiSpans(line);
                next = EditorSelection.cursor(forward ? line.from : line.to);
            }
            if (!check) {
                if (!by)
                    return next;
                check = by(char);
            }
            else if (!check(char)) {
                return cur;
            }
            cur = next;
        }
    }
    function byGroup(view, pos, start) {
        let categorize = view.state.charCategorizer(pos);
        let cat = categorize(start);
        return (next) => {
            let nextCat = categorize(next);
            if (cat == CharCategory.Space)
                cat = nextCat;
            return cat == nextCat;
        };
    }
    function moveVertically(view, start, forward, distance) {
        var _a;
        let startPos = start.head, dir = forward ? 1 : -1;
        if (startPos == (forward ? view.state.doc.length : 0))
            return EditorSelection.cursor(startPos);
        let startCoords = view.coordsAtPos(startPos);
        if (startCoords) {
            let rect = view.dom.getBoundingClientRect();
            let goal = (_a = start.goalColumn) !== null && _a !== void 0 ? _a : startCoords.left - rect.left;
            let resolvedGoal = rect.left + goal;
            let dist = distance !== null && distance !== void 0 ? distance : 5;
            for (let startY = dir < 0 ? startCoords.top : startCoords.bottom, extra = 0; extra < 50; extra += 10) {
                let pos = posAtCoords(view, { x: resolvedGoal, y: startY + (dist + extra) * dir }, dir);
                if (pos == null)
                    break;
                if (pos != startPos)
                    return EditorSelection.cursor(pos, undefined, undefined, goal);
            }
        }
        // Outside of the drawn viewport, use a crude column-based approach
        let { doc } = view.state, line = doc.lineAt(startPos), tabSize = view.state.tabSize;
        let goal = start.goalColumn, goalCol = 0;
        if (goal == null) {
            for (const iter = doc.iterRange(line.from, startPos); !iter.next().done;)
                goalCol = countColumn(iter.value, goalCol, tabSize);
            goal = goalCol * view.defaultCharacterWidth;
        }
        else {
            goalCol = Math.round(goal / view.defaultCharacterWidth);
        }
        if (dir < 0 && line.from == 0)
            return EditorSelection.cursor(0);
        else if (dir > 0 && line.to == doc.length)
            return EditorSelection.cursor(line.to);
        let otherLine = doc.line(line.number + dir);
        let result = otherLine.from;
        let seen = 0;
        for (const iter = doc.iterRange(otherLine.from, otherLine.to); seen >= goalCol && !iter.next().done;) {
            const { offset, leftOver } = findColumn(iter.value, seen, goalCol, tabSize);
            seen = goalCol - leftOver;
            result += offset;
        }
        return EditorSelection.cursor(result, undefined, undefined, goal);
    }

    // This will also be where dragging info and such goes
    class InputState {
        constructor(view) {
            this.lastKeyCode = 0;
            this.lastKeyTime = 0;
            this.lastSelectionOrigin = null;
            this.lastSelectionTime = 0;
            this.scrollHandlers = [];
            this.registeredEvents = [];
            this.customHandlers = [];
            this.composing = false;
            this.compositionEndedAt = 0;
            this.mouseSelection = null;
            for (let type in handlers) {
                let handler = handlers[type];
                view.contentDOM.addEventListener(type, (event) => {
                    if (!eventBelongsToEditor(view, event) || this.ignoreDuringComposition(event))
                        return;
                    if (this.mustFlushObserver(event))
                        view.observer.forceFlush();
                    if (this.runCustomHandlers(type, view, event))
                        event.preventDefault();
                    else
                        handler(view, event);
                });
                this.registeredEvents.push(type);
            }
            // Must always run, even if a custom handler handled the event
            view.contentDOM.addEventListener("keydown", (event) => {
                view.inputState.lastKeyCode = event.keyCode;
                view.inputState.lastKeyTime = Date.now();
            });
            this.notifiedFocused = view.hasFocus;
            this.ensureHandlers(view);
        }
        setSelectionOrigin(origin) {
            this.lastSelectionOrigin = origin;
            this.lastSelectionTime = Date.now();
        }
        ensureHandlers(view) {
            let handlers = this.customHandlers = view.pluginField(domEventHandlers);
            for (let set of handlers) {
                for (let type in set.handlers)
                    if (this.registeredEvents.indexOf(type) < 0 && type != "scroll") {
                        this.registeredEvents.push(type);
                        view.contentDOM.addEventListener(type, (event) => {
                            if (!eventBelongsToEditor(view, event))
                                return;
                            if (this.runCustomHandlers(type, view, event))
                                event.preventDefault();
                        });
                    }
            }
        }
        runCustomHandlers(type, view, event) {
            for (let set of this.customHandlers) {
                let handler = set.handlers[type];
                if (handler) {
                    try {
                        if (handler.call(set.plugin, event, view) || event.defaultPrevented)
                            return true;
                    }
                    catch (e) {
                        logException(view.state, e);
                    }
                }
            }
            return false;
        }
        runScrollHandlers(view, event) {
            for (let set of this.customHandlers) {
                let handler = set.handlers.scroll;
                if (handler) {
                    try {
                        handler.call(set.plugin, event, view);
                    }
                    catch (e) {
                        logException(view.state, e);
                    }
                }
            }
        }
        ignoreDuringComposition(event) {
            if (!/^key/.test(event.type))
                return false;
            if (this.composing)
                return true;
            // See https://www.stum.de/2016/06/24/handling-ime-events-in-javascript/.
            // On some input method editors (IMEs), the Enter key is used to
            // confirm character selection. On Safari, when Enter is pressed,
            // compositionend and keydown events are sometimes emitted in the
            // wrong order. The key event should still be ignored, even when
            // it happens after the compositionend event.
            if (browser.safari && event.timeStamp - this.compositionEndedAt < 500) {
                this.compositionEndedAt = 0;
                return true;
            }
            return false;
        }
        mustFlushObserver(event) {
            return event.type == "keydown" || event.type == "compositionend";
        }
        startMouseSelection(view, event, style) {
            if (this.mouseSelection)
                this.mouseSelection.destroy();
            this.mouseSelection = new MouseSelection(this, view, event, style);
        }
        update(update) {
            if (this.mouseSelection)
                this.mouseSelection.update(update);
            this.lastKeyCode = this.lastSelectionTime = 0;
        }
        destroy() {
            if (this.mouseSelection)
                this.mouseSelection.destroy();
        }
    }
    class MouseSelection {
        constructor(inputState, view, startEvent, style) {
            this.inputState = inputState;
            this.view = view;
            this.startEvent = startEvent;
            this.style = style;
            let doc = view.contentDOM.ownerDocument;
            doc.addEventListener("mousemove", this.move = this.move.bind(this));
            doc.addEventListener("mouseup", this.up = this.up.bind(this));
            this.extend = startEvent.shiftKey;
            this.multiple = view.state.facet(EditorState.allowMultipleSelections) && addsSelectionRange(view, startEvent);
            this.dragMove = dragMovesSelection$1(view, startEvent);
            this.dragging = isInPrimarySelection(view, startEvent) ? null : false;
            // When clicking outside of the selection, immediately apply the
            // effect of starting the selection
            if (this.dragging === false) {
                startEvent.preventDefault();
                this.select(startEvent);
            }
        }
        move(event) {
            if (event.buttons == 0)
                return this.destroy();
            if (this.dragging !== false)
                return;
            this.select(event);
        }
        up(event) {
            if (this.dragging == null)
                this.select(this.startEvent);
            if (!this.dragging)
                event.preventDefault();
            this.destroy();
        }
        destroy() {
            let doc = this.view.contentDOM.ownerDocument;
            doc.removeEventListener("mousemove", this.move);
            doc.removeEventListener("mouseup", this.up);
            this.inputState.mouseSelection = null;
        }
        select(event) {
            let selection = this.style.get(event, this.extend, this.multiple);
            if (!selection.eq(this.view.state.selection) || selection.primary.assoc != this.view.state.selection.primary.assoc)
                this.view.dispatch({
                    selection,
                    annotations: Transaction.userEvent.of("pointerselection"),
                    scrollIntoView: true
                });
        }
        update(update) {
            if (update.docChanged && this.dragging)
                this.dragging = this.dragging.map(update.changes);
            this.style.update(update);
        }
    }
    function addsSelectionRange(view, event) {
        let facet = view.state.facet(clickAddsSelectionRange);
        return facet.length ? facet[0](event) : browser.mac ? event.metaKey : event.ctrlKey;
    }
    function dragMovesSelection$1(view, event) {
        let facet = view.state.facet(dragMovesSelection);
        return facet.length ? facet[0](event) : browser.mac ? !event.altKey : !event.ctrlKey;
    }
    function isInPrimarySelection(view, event) {
        let { primary } = view.state.selection;
        if (primary.empty)
            return false;
        // On boundary clicks, check whether the coordinates are inside the
        // selection's client rectangles
        let sel = getSelection(view.root);
        if (sel.rangeCount == 0)
            return true;
        let rects = sel.getRangeAt(0).getClientRects();
        for (let i = 0; i < rects.length; i++) {
            let rect = rects[i];
            if (rect.left <= event.clientX && rect.right >= event.clientX &&
                rect.top <= event.clientY && rect.bottom >= event.clientY)
                return true;
        }
        return false;
    }
    function eventBelongsToEditor(view, event) {
        if (!event.bubbles)
            return true;
        if (event.defaultPrevented)
            return false;
        for (let node = event.target, cView; node != view.contentDOM; node = node.parentNode)
            if (!node || node.nodeType == 11 || ((cView = ContentView.get(node)) && cView.ignoreEvent(event)))
                return false;
        return true;
    }
    const handlers = Object.create(null);
    // This is very crude, but unfortunately both these browsers _pretend_
    // that they have a clipboard API—all the objects and methods are
    // there, they just don't work, and they are hard to test.
    const brokenClipboardAPI = (browser.ie && browser.ie_version < 15) ||
        (browser.ios && browser.webkit_version < 604);
    function capturePaste(view) {
        let parent = view.dom.parentNode;
        if (!parent)
            return;
        let target = parent.appendChild(document.createElement("textarea"));
        target.style.cssText = "position: fixed; left: -10000px; top: 10px";
        target.focus();
        setTimeout(() => {
            view.focus();
            target.remove();
            doPaste(view, target.value);
        }, 50);
    }
    function doPaste(view, input) {
        let { state } = view, changes, i = 1, text = state.toText(input);
        let byLine = text.lines == state.selection.ranges.length;
        let linewise = lastLinewiseCopy && state.selection.ranges.every(r => r.empty) && lastLinewiseCopy == text.toString();
        if (linewise) {
            changes = {
                changes: state.selection.ranges.map(r => state.doc.lineAt(r.from))
                    .filter((l, i, a) => i == 0 || a[i - 1] != l)
                    .map(line => ({ from: line.from, insert: (byLine ? text.line(i++).slice() : input) + state.lineBreak }))
            };
        }
        else if (byLine) {
            changes = state.changeByRange(range => {
                let line = text.line(i++);
                return { changes: { from: range.from, to: range.to, insert: line.slice() },
                    range: EditorSelection.cursor(range.from + line.length) };
            });
        }
        else {
            changes = state.replaceSelection(text);
        }
        view.dispatch(changes, {
            annotations: Transaction.userEvent.of("paste"),
            scrollIntoView: true
        });
    }
    function mustCapture(event) {
        let mods = (event.ctrlKey ? 1 /* Ctrl */ : 0) | (event.metaKey ? 8 /* Meta */ : 0) |
            (event.altKey ? 2 /* Alt */ : 0) | (event.shiftKey ? 4 /* Shift */ : 0);
        let code = event.keyCode, macCtrl = browser.mac && mods == 1 /* Ctrl */;
        return code == 8 || (macCtrl && code == 72) || // Backspace, Ctrl-h on Mac
            code == 46 || (macCtrl && code == 68) || // Delete, Ctrl-d on Mac
            code == 27 || // Esc
            (mods == (browser.mac ? 8 /* Meta */ : 1 /* Ctrl */) && // Ctrl/Cmd-[biyz]
                (code == 66 || code == 73 || code == 89 || code == 90));
    }
    handlers.keydown = (view, event) => {
        if (mustCapture(event))
            event.preventDefault();
        view.inputState.setSelectionOrigin("keyboardselection");
    };
    handlers.touchdown = handlers.touchmove = view => {
        view.inputState.setSelectionOrigin("pointerselection");
    };
    handlers.mousedown = (view, event) => {
        let style = null;
        for (let makeStyle of view.state.facet(mouseSelectionStyle)) {
            style = makeStyle(view, event);
            if (style)
                break;
        }
        if (!style && event.button == 0)
            style = basicMouseSelection(view, event);
        if (style) {
            if (view.root.activeElement != view.contentDOM)
                view.observer.ignore(() => focusPreventScroll(view.contentDOM));
            view.inputState.startMouseSelection(view, event, style);
        }
    };
    function rangeForClick(view, pos, bias, type) {
        if (type == 1) { // Single click
            return EditorSelection.cursor(pos, bias);
        }
        else if (type == 2) { // Double click
            return groupAt(view.state, pos, bias);
        }
        else { // Triple click
            let line = LineView.find(view.docView, pos);
            if (line)
                return EditorSelection.range(line.posAtStart, line.posAtEnd);
            let { from, to } = view.state.doc.lineAt(pos);
            return EditorSelection.range(from, to);
        }
    }
    let insideY = (y, rect) => y >= rect.top && y <= rect.bottom;
    let inside = (x, y, rect) => insideY(y, rect) && x >= rect.left && x <= rect.right;
    // Try to determine, for the given coordinates, associated with the
    // given position, whether they are related to the element before or
    // the element after the position.
    function findPositionSide(view, pos, x, y) {
        let line = LineView.find(view.docView, pos);
        if (!line)
            return 1;
        let off = pos - line.posAtStart;
        // Line boundaries point into the line
        if (off == 0)
            return 1;
        if (off == line.length)
            return -1;
        // Positions on top of an element point at that element
        let before = line.coordsAt(off, -1);
        if (before && inside(x, y, before))
            return -1;
        let after = line.coordsAt(off, 1);
        if (after && inside(x, y, after))
            return 1;
        // This is probably a line wrap point. Pick before if the point is
        // beside it.
        return before && insideY(y, before) ? -1 : 1;
    }
    function queryPos(view, event) {
        let pos = view.posAtCoords({ x: event.clientX, y: event.clientY });
        if (pos == null)
            return null;
        return { pos, bias: findPositionSide(view, pos, event.clientX, event.clientY) };
    }
    const BadMouseDetail = browser.ie && browser.ie_version <= 11;
    let lastMouseDown = null, lastMouseDownCount = 0;
    function getClickType(event) {
        if (!BadMouseDetail)
            return event.detail;
        let last = lastMouseDown;
        lastMouseDown = event;
        return lastMouseDownCount = !last || (last.timeStamp > Date.now() - 400 && Math.abs(last.clientX - event.clientX) < 2 &&
            Math.abs(last.clientY - event.clientY) < 2) ? (lastMouseDownCount + 1) % 3 : 1;
    }
    function basicMouseSelection(view, event) {
        let start = queryPos(view, event), type = getClickType(event);
        let startSel = view.state.selection;
        let last = start, lastEvent = event;
        return {
            update(update) {
                if (update.changes) {
                    if (start)
                        start.pos = update.changes.mapPos(start.pos);
                    startSel = startSel.map(update.changes);
                }
            },
            get(event, extend, multiple) {
                let cur;
                if (event.clientX == lastEvent.clientX && event.clientY == lastEvent.clientY)
                    cur = last;
                else {
                    cur = last = queryPos(view, event);
                    lastEvent = event;
                }
                if (!cur || !start)
                    return startSel;
                let range = rangeForClick(view, cur.pos, cur.bias, type);
                if (start.pos != cur.pos && !extend) {
                    let startRange = rangeForClick(view, start.pos, start.bias, type);
                    let from = Math.min(startRange.from, range.from), to = Math.max(startRange.to, range.to);
                    range = from < range.from ? EditorSelection.range(from, to) : EditorSelection.range(to, from);
                }
                if (extend)
                    return startSel.replaceRange(startSel.primary.extend(range.from, range.to));
                else if (multiple)
                    return startSel.addRange(range);
                else
                    return EditorSelection.create([range]);
            }
        };
    }
    handlers.dragstart = (view, event) => {
        let { selection: { primary } } = view.state;
        let { mouseSelection } = view.inputState;
        if (mouseSelection)
            mouseSelection.dragging = primary;
        if (event.dataTransfer) {
            event.dataTransfer.setData("Text", view.state.sliceDoc(primary.from, primary.to));
            event.dataTransfer.effectAllowed = "copyMove";
        }
    };
    handlers.drop = (view, event) => {
        if (!event.dataTransfer)
            return;
        let dropPos = view.posAtCoords({ x: event.clientX, y: event.clientY });
        let text = event.dataTransfer.getData("Text");
        if (dropPos == null || !text)
            return;
        event.preventDefault();
        let { mouseSelection } = view.inputState;
        let del = mouseSelection && mouseSelection.dragging && mouseSelection.dragMove ?
            { from: mouseSelection.dragging.from, to: mouseSelection.dragging.to } : null;
        let ins = { from: dropPos, insert: text };
        let changes = view.state.changes(del ? [del, ins] : ins);
        view.focus();
        view.dispatch({
            changes,
            selection: { anchor: changes.mapPos(dropPos, -1), head: changes.mapPos(dropPos, 1) },
            annotations: Transaction.userEvent.of("drop")
        });
    };
    handlers.paste = (view, event) => {
        view.observer.flush();
        let data = brokenClipboardAPI ? null : event.clipboardData;
        let text = data && data.getData("text/plain");
        if (text) {
            doPaste(view, text);
            event.preventDefault();
        }
        else {
            capturePaste(view);
        }
    };
    function captureCopy(view, text) {
        // The extra wrapper is somehow necessary on IE/Edge to prevent the
        // content from being mangled when it is put onto the clipboard
        let parent = view.dom.parentNode;
        if (!parent)
            return;
        let target = parent.appendChild(document.createElement("textarea"));
        target.style.cssText = "position: fixed; left: -10000px; top: 10px";
        target.value = text;
        target.focus();
        target.selectionEnd = text.length;
        target.selectionStart = 0;
        setTimeout(() => {
            target.remove();
            view.focus();
        }, 50);
    }
    function copiedRange(state) {
        let content = [], ranges = [], linewise = false;
        for (let range of state.selection.ranges)
            if (!range.empty) {
                content.push(state.sliceDoc(range.from, range.to));
                ranges.push(range);
            }
        if (!content.length) {
            // Nothing selected, do a line-wise copy
            let upto = -1;
            for (let { from } of state.selection.ranges) {
                let line = state.doc.lineAt(from);
                if (line.number > upto) {
                    content.push(line.slice());
                    ranges.push({ from: line.from, to: Math.min(state.doc.length, line.to + 1) });
                }
                upto = line.number;
            }
            linewise = true;
        }
        return { text: content.join(state.lineBreak), ranges, linewise };
    }
    let lastLinewiseCopy = null;
    handlers.copy = handlers.cut = (view, event) => {
        let { text, ranges, linewise } = copiedRange(view.state);
        if (!text)
            return;
        lastLinewiseCopy = linewise ? text : null;
        let data = brokenClipboardAPI ? null : event.clipboardData;
        if (data) {
            event.preventDefault();
            data.clearData();
            data.setData("text/plain", text);
        }
        else {
            captureCopy(view, text);
        }
        if (event.type == "cut")
            view.dispatch({
                changes: ranges,
                scrollIntoView: true,
                annotations: Transaction.userEvent.of("cut")
            });
    };
    handlers.focus = handlers.blur = view => {
        setTimeout(() => {
            if (view.hasFocus != view.inputState.notifiedFocused)
                view.update([]);
        }, 10);
    };
    handlers.beforeprint = view => {
        view.viewState.printing = true;
        view.requestMeasure();
        setTimeout(() => {
            view.viewState.printing = false;
            view.requestMeasure();
        }, 2000);
    };
    function forceClearComposition(view) {
        if (view.docView.compositionDeco.size)
            view.update([]);
    }
    handlers.compositionstart = handlers.compositionupdate = view => {
        if (!view.inputState.composing) {
            if (view.docView.compositionDeco.size) {
                view.observer.flush();
                forceClearComposition(view);
            }
            // FIXME possibly set a timeout to clear it again on Android
            view.inputState.composing = true;
        }
    };
    handlers.compositionend = view => {
        view.inputState.composing = false;
        view.inputState.compositionEndedAt = Date.now();
        setTimeout(() => {
            if (!view.inputState.composing)
                forceClearComposition(view);
        }, 50);
    };

    const observeOptions = {
        childList: true,
        characterData: true,
        subtree: true,
        characterDataOldValue: true
    };
    // IE11 has very broken mutation observers, so we also listen to
    // DOMCharacterDataModified there
    const useCharData = browser.ie && browser.ie_version <= 11;
    class DOMObserver {
        constructor(view, onChange, onScrollChanged) {
            this.view = view;
            this.onChange = onChange;
            this.onScrollChanged = onScrollChanged;
            this.active = false;
            this.ignoreSelection = new DOMSelection;
            this.delayedFlush = -1;
            this.queue = [];
            this.scrollTargets = [];
            this.intersection = null;
            this.intersecting = false;
            // Timeout for scheduling check of the parents that need scroll handlers
            this.parentCheck = -1;
            this.dom = view.contentDOM;
            this.observer = new MutationObserver(mutations => {
                for (let mut of mutations)
                    this.queue.push(mut);
                // IE11 will sometimes (on typing over a selection or
                // backspacing out a single character text node) call the
                // observer callback before actually updating the DOM
                if (browser.ie && browser.ie_version <= 11 &&
                    mutations.some(m => m.type == "childList" && m.removedNodes.length ||
                        m.type == "characterData" && m.oldValue.length > m.target.nodeValue.length))
                    this.flushSoon();
                else
                    this.flush();
            });
            if (useCharData)
                this.onCharData = (event) => {
                    this.queue.push({ target: event.target,
                        type: "characterData",
                        oldValue: event.prevValue });
                    this.flushSoon();
                };
            this.onSelectionChange = (event) => {
                if (this.view.root.activeElement != this.dom)
                    return;
                let sel = getSelection(this.view.root);
                let context = sel.anchorNode && this.view.docView.nearest(sel.anchorNode);
                if (context && context.ignoreEvent(event))
                    return;
                // Deletions on IE11 fire their events in the wrong order, giving
                // us a selection change event before the DOM changes are
                // reported.
                // (Selection.isCollapsed isn't reliable on IE)
                if (browser.ie && browser.ie_version <= 11 && !this.view.state.selection.primary.empty &&
                    sel.focusNode && isEquivalentPosition(sel.focusNode, sel.focusOffset, sel.anchorNode, sel.anchorOffset))
                    this.flushSoon();
                else
                    this.flush();
            };
            this.start();
            this.onScroll = this.onScroll.bind(this);
            window.addEventListener("scroll", this.onScroll);
            if (typeof IntersectionObserver == "function") {
                this.intersection = new IntersectionObserver(entries => {
                    if (this.parentCheck < 0)
                        this.parentCheck = setTimeout(this.listenForScroll.bind(this), 1000);
                    if (entries[entries.length - 1].intersectionRatio > 0 != this.intersecting) {
                        this.intersecting = !this.intersecting;
                        this.onScrollChanged(document.createEvent("Event"));
                    }
                }, {});
                this.intersection.observe(this.dom);
            }
            this.listenForScroll();
        }
        onScroll(e) {
            if (this.intersecting) {
                this.flush();
                this.onScrollChanged(e);
            }
        }
        listenForScroll() {
            this.parentCheck = -1;
            let i = 0, changed = null;
            for (let dom = this.dom; dom;) {
                if (dom.nodeType == 1) {
                    if (!changed && i < this.scrollTargets.length && this.scrollTargets[i] == dom)
                        i++;
                    else if (!changed)
                        changed = this.scrollTargets.slice(0, i);
                    if (changed)
                        changed.push(dom);
                    dom = dom.parentNode;
                }
                else if (dom.nodeType == 11) { // Shadow root
                    dom = dom.host;
                }
                else {
                    break;
                }
            }
            if (i < this.scrollTargets.length && !changed)
                changed = this.scrollTargets.slice(0, i);
            if (changed) {
                for (let dom of this.scrollTargets)
                    dom.removeEventListener("scroll", this.onScroll);
                for (let dom of this.scrollTargets = changed)
                    dom.addEventListener("scroll", this.onScroll);
            }
        }
        ignore(f) {
            if (!this.active)
                return f();
            try {
                this.stop();
                return f();
            }
            finally {
                this.start();
                this.clear();
            }
        }
        start() {
            if (this.active)
                return;
            this.observer.observe(this.dom, observeOptions);
            // FIXME is this shadow-root safe?
            this.dom.ownerDocument.addEventListener("selectionchange", this.onSelectionChange);
            if (useCharData)
                this.dom.addEventListener("DOMCharacterDataModified", this.onCharData);
            this.active = true;
        }
        stop() {
            if (!this.active)
                return;
            this.active = false;
            this.observer.disconnect();
            this.dom.ownerDocument.removeEventListener("selectionchange", this.onSelectionChange);
            if (useCharData)
                this.dom.removeEventListener("DOMCharacterDataModified", this.onCharData);
        }
        clearSelection() {
            this.ignoreSelection.set(getSelection(this.view.root));
        }
        // Throw away any pending changes
        clear() {
            this.observer.takeRecords();
            this.queue.length = 0;
            this.clearSelection();
        }
        flushSoon() {
            if (this.delayedFlush < 0)
                this.delayedFlush = window.setTimeout(() => { this.delayedFlush = -1; this.flush(); }, 20);
        }
        forceFlush() {
            if (this.delayedFlush >= 0) {
                window.clearTimeout(this.delayedFlush);
                this.delayedFlush = -1;
                this.flush();
            }
        }
        // Apply pending changes, if any
        flush() {
            if (this.delayedFlush >= 0)
                return;
            let records = this.queue;
            for (let mut of this.observer.takeRecords())
                records.push(mut);
            if (records.length)
                this.queue = [];
            let selection = getSelection(this.view.root);
            let newSel = !this.ignoreSelection.eq(selection) && hasSelection(this.dom, selection);
            if (records.length == 0 && !newSel)
                return;
            let from = -1, to = -1, typeOver = false;
            for (let record of records) {
                let range = this.readMutation(record);
                if (!range)
                    continue;
                if (range.typeOver)
                    typeOver = true;
                if (from == -1) {
                    ({ from, to } = range);
                }
                else {
                    from = Math.min(range.from, from);
                    to = Math.max(range.to, to);
                }
            }
            let startState = this.view.state;
            if (from > -1 || newSel)
                this.onChange(from, to, typeOver);
            if (this.view.state == startState) { // The view wasn't updated
                if (this.view.docView.dirty) {
                    this.ignore(() => this.view.docView.sync());
                    this.view.docView.dirty = 0 /* Not */;
                }
                this.view.docView.updateSelection();
            }
            this.clearSelection();
        }
        readMutation(rec) {
            let cView = this.view.docView.nearest(rec.target);
            if (!cView || cView.ignoreMutation(rec))
                return null;
            cView.markDirty();
            if (rec.type == "childList") {
                let childBefore = findChild(cView, rec.previousSibling || rec.target.previousSibling, -1);
                let childAfter = findChild(cView, rec.nextSibling || rec.target.nextSibling, 1);
                return { from: childBefore ? cView.posAfter(childBefore) : cView.posAtStart,
                    to: childAfter ? cView.posBefore(childAfter) : cView.posAtEnd, typeOver: false };
            }
            else { // "characterData"
                return { from: cView.posAtStart, to: cView.posAtEnd, typeOver: rec.target.nodeValue == rec.oldValue };
            }
        }
        destroy() {
            this.stop();
            if (this.intersection)
                this.intersection.disconnect();
            for (let dom of this.scrollTargets)
                dom.removeEventListener("scroll", this.onScroll);
            window.removeEventListener("scroll", this.onScroll);
            clearTimeout(this.parentCheck);
        }
    }
    function findChild(cView, dom, dir) {
        while (dom) {
            let curView = ContentView.get(dom);
            if (curView && curView.parent == cView)
                return curView;
            let parent = dom.parentNode;
            dom = parent != cView.dom ? parent : dir > 0 ? dom.nextSibling : dom.previousSibling;
        }
        return null;
    }

    function applyDOMChange(view, start, end, typeOver) {
        let change, newSel;
        let sel = view.state.selection.primary, bounds;
        if (start > -1 && (bounds = view.docView.domBoundsAround(start, end, 0))) {
            let { from, to } = bounds;
            let selPoints = view.docView.impreciseHead || view.docView.impreciseAnchor ? [] : selectionPoints(view.contentDOM, view.root);
            let reader = new DOMReader(selPoints, view.state.lineBreak);
            reader.readRange(bounds.startDOM, bounds.endDOM);
            newSel = selectionFromPoints(selPoints, from);
            let preferredPos = sel.from, preferredSide = null;
            // Prefer anchoring to end when Backspace is pressed
            if (view.inputState.lastKeyCode === 8 && view.inputState.lastKeyTime > Date.now() - 100) {
                preferredPos = sel.to;
                preferredSide = "end";
            }
            let diff = findDiff(view.state.sliceDoc(from, to), reader.text, preferredPos - from, preferredSide);
            if (diff)
                change = { from: from + diff.from, to: from + diff.toA,
                    insert: view.state.toText(reader.text.slice(diff.from, diff.toB)) };
        }
        else if (view.hasFocus) {
            let domSel = getSelection(view.root);
            let { impreciseHead: iHead, impreciseAnchor: iAnchor } = view.docView;
            let head = iHead && iHead.node == domSel.focusNode && iHead.offset == domSel.focusOffset ? view.state.selection.primary.head
                : view.docView.posFromDOM(domSel.focusNode, domSel.focusOffset);
            let anchor = iAnchor && iAnchor.node == domSel.anchorNode && iAnchor.offset == domSel.anchorOffset
                ? view.state.selection.primary.anchor
                : selectionCollapsed(domSel) ? head : view.docView.posFromDOM(domSel.anchorNode, domSel.anchorOffset);
            if (head != sel.head || anchor != sel.anchor)
                newSel = EditorSelection.single(anchor, head);
        }
        if (!change && !newSel)
            return;
        // Heuristic to notice typing over a selected character
        if (!change && typeOver && !sel.empty && newSel && newSel.primary.empty)
            change = { from: sel.from, to: sel.to, insert: view.state.doc.slice(sel.from, sel.to) };
        if (change) {
            let startState = view.state;
            // Android browsers don't fire reasonable key events for enter,
            // backspace, or delete. So this detects changes that look like
            // they're caused by those keys, and reinterprets them as key
            // events.
            if (browser.android &&
                ((change.from == sel.from && change.to == sel.to &&
                    change.insert.length == 1 && change.insert.lines == 2 &&
                    dispatchKey(view, "Enter", 10)) ||
                    (change.from == sel.from - 1 && change.to == sel.to && change.insert.length == 0 &&
                        dispatchKey(view, "Backspace", 8)) ||
                    (change.from == sel.from && change.to == sel.to + 1 && change.insert.length == 0 &&
                        dispatchKey(view, "Delete", 46))))
                return;
            let text = change.insert.toString();
            if (view.state.facet(inputHandler).some(h => h(view, change.from, change.to, text)))
                return;
            let tr;
            if (change.from >= sel.from && change.to <= sel.to && change.to - change.from >= (sel.to - sel.from) / 3) {
                let before = sel.from < change.from ? startState.sliceDoc(sel.from, change.from) : "";
                let after = sel.to > change.to ? startState.sliceDoc(change.to, sel.to) : "";
                tr = startState.replaceSelection(view.state.toText(before + change.insert.sliceString(0, undefined, view.state.lineBreak) +
                    after));
            }
            else {
                let changes = startState.changes(change);
                tr = {
                    changes,
                    selection: newSel && !startState.selection.primary.eq(newSel.primary) && newSel.primary.to <= changes.newLength
                        ? startState.selection.replaceRange(newSel.primary) : undefined
                };
            }
            view.dispatch(tr, { scrollIntoView: true, annotations: Transaction.userEvent.of("input") });
        }
        else if (newSel && !newSel.primary.eq(sel)) {
            let scrollIntoView = false, annotations;
            if (view.inputState.lastSelectionTime > Date.now() - 50) {
                if (view.inputState.lastSelectionOrigin == "keyboardselection")
                    scrollIntoView = true;
                else
                    annotations = Transaction.userEvent.of(view.inputState.lastSelectionOrigin);
            }
            view.dispatch({ selection: newSel, scrollIntoView, annotations });
        }
    }
    function findDiff(a, b, preferredPos, preferredSide) {
        let minLen = Math.min(a.length, b.length);
        let from = 0;
        while (from < minLen && a.charCodeAt(from) == b.charCodeAt(from))
            from++;
        if (from == minLen && a.length == b.length)
            return null;
        let toA = a.length, toB = b.length;
        while (toA > 0 && toB > 0 && a.charCodeAt(toA - 1) == b.charCodeAt(toB - 1)) {
            toA--;
            toB--;
        }
        if (preferredSide == "end") {
            let adjust = Math.max(0, from - Math.min(toA, toB));
            preferredPos -= toA + adjust - from;
        }
        if (toA < from && a.length < b.length) {
            let move = preferredPos <= from && preferredPos >= toA ? from - preferredPos : 0;
            from -= move;
            toB = from + (toB - toA);
            toA = from;
        }
        else if (toB < from) {
            let move = preferredPos <= from && preferredPos >= toB ? from - preferredPos : 0;
            from -= move;
            toA = from + (toA - toB);
            toB = from;
        }
        return { from, toA, toB };
    }
    class DOMReader {
        constructor(points, lineSep) {
            this.points = points;
            this.lineSep = lineSep;
            this.text = "";
        }
        readRange(start, end) {
            if (!start)
                return;
            let parent = start.parentNode;
            for (let cur = start;;) {
                this.findPointBefore(parent, cur);
                this.readNode(cur);
                let next = cur.nextSibling;
                if (next == end)
                    break;
                let view = ContentView.get(cur), nextView = ContentView.get(next);
                if ((view ? view.breakAfter : isBlockElement(cur)) ||
                    ((nextView ? nextView.breakAfter : isBlockElement(next)) && !(cur.nodeName == "BR" && !cur.cmIgnore)))
                    this.text += this.lineSep;
                cur = next;
            }
            this.findPointBefore(parent, end);
        }
        readNode(node) {
            if (node.cmIgnore)
                return;
            let view = ContentView.get(node);
            let fromView = view && view.overrideDOMText;
            let text;
            if (fromView != null)
                text = fromView.sliceString(0, undefined, this.lineSep);
            else if (node.nodeType == 3)
                text = node.nodeValue;
            else if (node.nodeName == "BR")
                text = node.nextSibling ? this.lineSep : "";
            else if (node.nodeType == 1)
                this.readRange(node.firstChild, null);
            if (text != null) {
                this.findPointIn(node, text.length);
                this.text += text;
            }
        }
        findPointBefore(node, next) {
            for (let point of this.points)
                if (point.node == node && node.childNodes[point.offset] == next)
                    point.pos = this.text.length;
        }
        findPointIn(node, maxLen) {
            for (let point of this.points)
                if (point.node == node)
                    point.pos = this.text.length + Math.min(point.offset, maxLen);
        }
    }
    function isBlockElement(node) {
        return node.nodeType == 1 && /^(DIV|P|LI|UL|OL|BLOCKQUOTE|DD|DT|H\d|SECTION|PRE)$/.test(node.nodeName);
    }
    class DOMPoint {
        constructor(node, offset) {
            this.node = node;
            this.offset = offset;
            this.pos = -1;
        }
    }
    function selectionPoints(dom, root) {
        let result = [];
        if (root.activeElement != dom)
            return result;
        let { anchorNode, anchorOffset, focusNode, focusOffset } = getSelection(root);
        if (anchorNode) {
            result.push(new DOMPoint(anchorNode, anchorOffset));
            if (focusNode != anchorNode || focusOffset != anchorOffset)
                result.push(new DOMPoint(focusNode, focusOffset));
        }
        return result;
    }
    function selectionFromPoints(points, base) {
        if (points.length == 0)
            return null;
        let anchor = points[0].pos, head = points.length == 2 ? points[1].pos : anchor;
        return anchor > -1 && head > -1 ? EditorSelection.single(anchor + base, head + base) : null;
    }
    function dispatchKey(view, name, code) {
        let options = { key: name, code: name, keyCode: code, which: code, cancelable: true };
        let down = new KeyboardEvent("keydown", options);
        view.contentDOM.dispatchEvent(down);
        let up = new KeyboardEvent("keyup", options);
        view.contentDOM.dispatchEvent(up);
        return down.defaultPrevented || up.defaultPrevented;
    }

    // The editor's update state machine looks something like this:
    //
    //     Idle → Updating ⇆ Idle (unchecked) → Measuring → Idle
    //                                         ↑      ↓
    //                                         Updating (measure)
    //
    // The difference between 'Idle' and 'Idle (unchecked)' lies in
    // whether a layout check has been scheduled. A regular update through
    // the `update` method updates the DOM in a write-only fashion, and
    // relies on a check (scheduled with `requestAnimationFrame`) to make
    // sure everything is where it should be and the viewport covers the
    // visible code. That check continues to measure and then optionally
    // update until it reaches a coherent state.
    /// An editor view represents the editor's user interface. It holds
    /// the editable DOM surface, and possibly other elements such as the
    /// line number gutter. It handles events and dispatches state
    /// transactions for editing actions.
    class EditorView {
        /// Construct a new view. You'll usually want to put `view.dom` into
        /// your document after creating a view, so that the user can see
        /// it.
        constructor(
        /// Configuration options.
        config = {}) {
            this.plugins = [];
            this.editorAttrs = {};
            this.contentAttrs = {};
            this.bidiCache = [];
            /// @internal
            this.updateState = 2 /* Updating */;
            /// @internal
            this.measureScheduled = -1;
            /// @internal
            this.measureRequests = [];
            this.contentDOM = document.createElement("div");
            this.scrollDOM = document.createElement("div");
            this.scrollDOM.className = themeClass("scroller");
            this.scrollDOM.appendChild(this.contentDOM);
            this.dom = document.createElement("div");
            this.dom.appendChild(this.scrollDOM);
            this._dispatch = config.dispatch || ((tr) => this.update([tr]));
            this.dispatch = this.dispatch.bind(this);
            this.root = (config.root || document);
            this.viewState = new ViewState(config.state || EditorState.create());
            this.plugins = this.state.facet(viewPlugin).map(spec => PluginInstance.create(spec, this));
            this.observer = new DOMObserver(this, (from, to, typeOver) => {
                applyDOMChange(this, from, to, typeOver);
            }, event => {
                this.inputState.runScrollHandlers(this, event);
                this.measure();
            });
            this.docView = new DocView(this);
            this.inputState = new InputState(this);
            this.mountStyles();
            this.updateAttrs();
            this.updateState = 0 /* Idle */;
            ensureGlobalHandler();
            this.requestMeasure();
            if (config.parent)
                config.parent.appendChild(this.dom);
        }
        /// The current editor state.
        get state() { return this.viewState.state; }
        /// To be able to display large documents without consuming too much
        /// memory or overloading the browser, CodeMirror only draws the
        /// code that is visible (plus a margin around it) to the DOM. This
        /// property tells you the extent of the current drawn viewport, in
        /// document positions.
        get viewport() { return this.viewState.viewport; }
        /// When there are, for example, large collapsed ranges in the
        /// viewport, its size can be a lot bigger than the actual visible
        /// content. Thus, if you are doing something like styling the
        /// content in the viewport, it is preferable to only do so for
        /// these ranges, which are the subset of the viewport that is
        /// actually drawn.
        get visibleRanges() { return this.viewState.visibleRanges; }
        /// Returns false when the editor is entirely scrolled out of view
        /// or otherwise hidden.
        get inView() { return this.viewState.inView; }
        /// Indicates whether the user is currently composing text via
        /// [IME](https://developer.mozilla.org/en-US/docs/Mozilla/IME_handling_guide).
        get composing() { return this.inputState.composing; }
        dispatch(...input) {
            this._dispatch(input.length == 1 && input[0] instanceof Transaction ? input[0]
                : this.state.update(...input));
        }
        /// Update the view for the given array of transactions. This will
        /// update the visible document and selection to match the state
        /// produced by the transactions, and notify view plugins of the
        /// change. You should usually call
        /// [`dispatch`](#view.EditorView.dispatch) instead, which uses this
        /// as a primitive.
        update(transactions) {
            if (this.updateState != 0 /* Idle */)
                throw new Error("Calls to EditorView.update are not allowed while an update is in progress");
            let redrawn = false, update;
            this.updateState = 2 /* Updating */;
            try {
                let state = this.state;
                for (let tr of transactions) {
                    if (tr.startState != state)
                        throw new RangeError("Trying to update state with a transaction that doesn't start from the previous state.");
                    state = tr.state;
                }
                update = new ViewUpdate(this, state, transactions);
                let scrollTo = transactions.some(tr => tr.scrollIntoView) ? state.selection.primary : null;
                this.viewState.update(update, scrollTo);
                this.bidiCache = CachedOrder.update(this.bidiCache, update.changes);
                if (!update.empty)
                    this.updatePlugins(update);
                redrawn = this.docView.update(update);
                if (this.state.facet(styleModule) != this.styleModules)
                    this.mountStyles();
                this.updateAttrs();
            }
            finally {
                this.updateState = 0 /* Idle */;
            }
            if (redrawn || scrollTo || this.viewState.mustEnforceCursorAssoc)
                this.requestMeasure();
            for (let listener of this.state.facet(updateListener))
                listener(update);
        }
        /// Reset the view to the given state. (This will cause the entire
        /// document to be redrawn and all view plugins to be reinitialized,
        /// so you should probably only use it when the new state isn't
        /// derived from the old state. Otherwise, use
        /// [`update`](#view.EditorView.update) instead.)
        setState(newState) {
            if (this.updateState != 0 /* Idle */)
                throw new Error("Calls to EditorView.setState are not allowed while an update is in progress");
            this.updateState = 2 /* Updating */;
            try {
                for (let plugin of this.plugins)
                    plugin.destroy(this);
                this.viewState = new ViewState(newState);
                this.plugins = newState.facet(viewPlugin).map(spec => PluginInstance.create(spec, this));
                this.docView = new DocView(this);
                this.inputState.ensureHandlers(this);
                this.mountStyles();
                this.updateAttrs();
                this.bidiCache = [];
            }
            finally {
                this.updateState = 0 /* Idle */;
            }
            this.requestMeasure();
        }
        updatePlugins(update) {
            let prevSpecs = update.prevState.facet(viewPlugin), specs = update.state.facet(viewPlugin);
            if (prevSpecs != specs) {
                let newPlugins = [], reused = [];
                for (let spec of specs) {
                    let found = prevSpecs.indexOf(spec);
                    if (found < 0) {
                        newPlugins.push(PluginInstance.create(spec, this));
                    }
                    else {
                        let plugin = this.plugins[found].update(update);
                        reused.push(plugin);
                        newPlugins.push(plugin);
                    }
                }
                for (let plugin of this.plugins)
                    if (reused.indexOf(plugin) < 0)
                        plugin.destroy(this);
                this.plugins = newPlugins;
                this.inputState.ensureHandlers(this);
            }
            else {
                for (let i = 0; i < this.plugins.length; i++)
                    this.plugins[i] = this.plugins[i].update(update);
            }
        }
        /// @internal
        measure() {
            if (this.measureScheduled > -1)
                cancelAnimationFrame(this.measureScheduled);
            this.measureScheduled = -1; // Prevent requestMeasure calls from scheduling another animation frame
            let updated = null;
            try {
                for (let i = 0;; i++) {
                    this.updateState = 1 /* Measuring */;
                    let changed = this.viewState.measure(this.docView, i > 0);
                    let measuring = this.measureRequests;
                    if (!changed && !measuring.length && this.viewState.scrollTo == null)
                        break;
                    this.measureRequests = [];
                    if (i > 5) {
                        console.warn("Viewport failed to stabilize");
                        break;
                    }
                    let measured = measuring.map(m => {
                        try {
                            return m.read(this);
                        }
                        catch (e) {
                            logException(this.state, e);
                            return BadMeasure;
                        }
                    });
                    let update = new ViewUpdate(this, this.state);
                    update.flags |= changed;
                    if (!updated)
                        updated = update;
                    else
                        updated.flags |= changed;
                    this.updateState = 2 /* Updating */;
                    this.updatePlugins(update);
                    this.updateAttrs();
                    if (changed)
                        this.docView.update(update);
                    for (let i = 0; i < measuring.length; i++)
                        if (measured[i] != BadMeasure) {
                            try {
                                measuring[i].write(measured[i], this);
                            }
                            catch (e) {
                                logException(this.state, e);
                            }
                        }
                    if (this.viewState.scrollTo) {
                        this.docView.scrollPosIntoView(this.viewState.scrollTo.head, this.viewState.scrollTo.assoc);
                        this.viewState.scrollTo = null;
                    }
                    if (!(changed & 4 /* Viewport */) && this.measureRequests.length == 0)
                        break;
                }
            }
            finally {
                this.updateState = 0 /* Idle */;
            }
            this.measureScheduled = -1;
            if (updated)
                for (let listener of this.state.facet(updateListener))
                    listener(updated);
        }
        /// Get the CSS classes for the currently active editor themes.
        get themeClasses() {
            return baseThemeID + " " +
                (this.state.facet(darkTheme) ? "cm-dark" : "cm-light") + " " +
                this.state.facet(theme);
        }
        updateAttrs() {
            let editorAttrs = combineAttrs(this.state.facet(editorAttributes), {
                class: themeClass("wrap") + (this.hasFocus ? " cm-focused " : " ") + this.themeClasses
            });
            updateAttrs(this.dom, this.editorAttrs, editorAttrs);
            this.editorAttrs = editorAttrs;
            let contentAttrs = combineAttrs(this.state.facet(contentAttributes), {
                spellcheck: "false",
                contenteditable: String(this.state.facet(editable)),
                class: themeClass("content"),
                style: `${browser.tabSize}: ${this.state.tabSize}`,
                role: "textbox",
                "aria-multiline": "true"
            });
            updateAttrs(this.contentDOM, this.contentAttrs, contentAttrs);
            this.contentAttrs = contentAttrs;
        }
        mountStyles() {
            this.styleModules = this.state.facet(styleModule);
            StyleModule.mount(this.root, this.styleModules.concat(baseTheme).reverse());
        }
        /// Find the DOM parent node and offset (child offset if `node` is
        /// an element, character offset when it is a text node) at the
        /// given document position.
        domAtPos(pos) {
            return this.docView.domAtPos(pos);
        }
        /// Find the document position at the given DOM node. Can be useful
        /// for associating positions with DOM events. Will raise an error
        /// when `node` isn't part of the editor content.
        posAtDOM(node, offset = 0) {
            return this.docView.posFromDOM(node, offset);
        }
        readMeasured() {
            if (this.updateState == 2 /* Updating */)
                throw new Error("Reading the editor layout isn't allowed during an update");
            if (this.updateState == 0 /* Idle */ && this.measureScheduled > -1)
                this.measure();
        }
        /// Make sure plugins get a chance to measure the DOM before the
        /// next frame. Calling this is preferable to messing with the DOM
        /// directly from, for example, an even handler, because it'll make
        /// sure measuring and drawing done by other components is
        /// synchronized, avoiding unnecessary DOM layout computations.
        requestMeasure(request) {
            if (this.measureScheduled < 0)
                this.measureScheduled = requestAnimationFrame(() => this.measure());
            if (request) {
                if (request.key != null)
                    for (let i = 0; i < this.measureRequests.length; i++) {
                        if (this.measureRequests[i].key === request.key) {
                            this.measureRequests[i] = request;
                            return;
                        }
                    }
                this.measureRequests.push(request);
            }
        }
        /// Collect all values provided by the active plugins for a given
        /// field.
        pluginField(field) {
            // FIXME make this error when called during plugin updating
            let result = [];
            for (let plugin of this.plugins)
                plugin.takeField(field, result);
            return result;
        }
        /// Get the value of a specific plugin, if present. Note that
        /// plugins that crash can be dropped from a view, so even when you
        /// know you registered a given plugin, it is recommended to check
        /// the return value of this method.
        plugin(plugin) {
            for (let inst of this.plugins)
                if (inst.spec == plugin)
                    return inst.value;
            return null;
        }
        /// Find the line or block widget at the given vertical position.
        /// `editorTop`, if given, provides the vertical position of the top
        /// of the editor. It defaults to the editor's screen position
        /// (which will force a DOM layout).
        blockAtHeight(height, editorTop) {
            this.readMeasured();
            return this.viewState.blockAtHeight(height, ensureTop(editorTop, this.contentDOM));
        }
        /// Find information for the visual line (see
        /// [`visualLineAt`](#view.EditorView.visualLineAt)) at the given
        /// vertical position. The resulting block info might hold another
        /// array of block info structs in its `type` field if this line
        /// consists of more than one block.
        ///
        /// Heights are interpreted relative to the given `editorTop`
        /// position. When not given, the top position of the editor's
        /// [content element](#view.EditorView.contentDOM) is taken.
        visualLineAtHeight(height, editorTop) {
            this.readMeasured();
            return this.viewState.lineAtHeight(height, ensureTop(editorTop, this.contentDOM));
        }
        /// Find the extent and height of the visual line (the content shown
        /// in the editor as a line, which may be smaller than a document
        /// line when broken up by block widgets, or bigger than a document
        /// line when line breaks are covered by replaced decorations) at
        /// the given position.
        ///
        /// Vertical positions are computed relative to the `editorTop`
        /// argument. You can pass `view.dom.getBoundingClientRect().top`
        /// here to get screen coordinates.
        visualLineAt(pos, editorTop = 0) {
            return this.viewState.lineAt(pos, editorTop);
        }
        /// Iterate over the height information of the lines in the
        /// viewport.
        viewportLines(f, editorTop) {
            let { from, to } = this.viewport;
            this.viewState.forEachLine(from, to, f, ensureTop(editorTop, this.contentDOM));
        }
        /// The editor's total content height.
        get contentHeight() {
            return this.viewState.heightMap.height + this.viewState.paddingTop + this.viewState.paddingBottom;
        }
        /// Move a cursor position by [grapheme
        /// cluster](#text.nextClusterBreak). `forward` determines whether
        /// the motion is away from the line start, or towards it. Motion in
        /// bidirectional text is in visual order, in the editor's [text
        /// direction](#view.EditorView.textDirection). When the start
        /// position was the last one on the line, the returned position
        /// will be across the line break. If there is no further line, the
        /// original position is returned.
        moveByChar(start, forward, by) {
            return moveByChar(this, start, forward, by);
        }
        /// Move a cursor position across the next group of either
        /// [letters](#state.EditorState.charCategorizer) or non-letter
        /// non-whitespace characters.
        moveByGroup(start, forward) {
            return moveByChar(this, start, forward, initial => byGroup(this, start.head, initial));
        }
        /// Move to the next line boundary in the given direction. If
        /// `includeWrap` is true, line wrapping is on, and there is a
        /// further wrap point on the current line, the wrap point will be
        /// returned. Otherwise this function will return the start or end
        /// of the line.
        moveToLineBoundary(start, forward, includeWrap = true) {
            return moveToLineBoundary(this, start, forward, includeWrap);
        }
        /// Move a cursor position vertically. When `distance` isn't given,
        /// it defaults to moving to the next line (including wrapped
        /// lines). Otherwise, `distance` should provide a positive distance
        /// in pixels.
        ///
        /// When `start` has a
        /// [`goalColumn`](#state.SelectionRange.goalColumn), the vertical
        /// motion will use that as a target horizontal position. Otherwise,
        /// the cursor's own horizontal position is used. The returned
        /// cursor will have its goal column set to whichever column was
        /// used.
        moveVertically(start, forward, distance) {
            return moveVertically(this, start, forward, distance);
        }
        /// Scroll the given document position into view.
        scrollPosIntoView(pos) {
            this.viewState.scrollTo = EditorSelection.cursor(pos);
            this.requestMeasure();
        }
        /// Get the document position at the given screen coordinates.
        /// Returns null if no valid position could be found.
        posAtCoords(coords) {
            this.readMeasured();
            return posAtCoords(this, coords);
        }
        /// Get the screen coordinates at the given document position.
        coordsAtPos(pos, side = 1) {
            this.readMeasured();
            let rect = this.docView.coordsAt(pos, side);
            if (!rect || rect.left == rect.right)
                return rect;
            let line = this.state.doc.lineAt(pos), order = this.bidiSpans(line);
            let span = order[BidiSpan.find(order, pos - line.from, -1, side)];
            return flattenRect(rect, (span.dir == Direction.LTR) == (side > 0));
        }
        /// The default width of a character in the editor. May not
        /// accurately reflect the width of all characters.
        get defaultCharacterWidth() { return this.viewState.heightOracle.charWidth; }
        /// The default height of a line in the editor.
        get defaultLineHeight() { return this.viewState.heightOracle.lineHeight; }
        /// The text direction (`direction` CSS property) of the editor.
        get textDirection() { return this.viewState.heightOracle.direction; }
        /// Whether this editor [wraps lines](#view.EditorView.lineWrapping)
        /// (as determined by the `white-space` CSS property of its content
        /// element).
        get lineWrapping() { return this.viewState.heightOracle.lineWrapping; }
        /// Returns the bidirectional text structure of the given line
        /// (which should be in the current document) as an array of span
        /// objects. The order of these spans matches the [text
        /// direction](#view.EditorView.textDirection)—if that is
        /// left-to-right, the leftmost spans come first, otherwise the
        /// rightmost spans come first.
        bidiSpans(line) {
            if (line.length > MaxBidiLine)
                return trivialOrder(line.length);
            let dir = this.textDirection;
            for (let entry of this.bidiCache)
                if (entry.from == line.from && entry.dir == dir)
                    return entry.order;
            let order = computeOrder(line.slice(), this.textDirection);
            this.bidiCache.push(new CachedOrder(line.from, line.to, dir, order));
            return order;
        }
        /// Check whether the editor has focus.
        get hasFocus() {
            return document.hasFocus() && this.root.activeElement == this.contentDOM;
        }
        /// Put focus on the editor.
        focus() {
            this.observer.ignore(() => {
                focusPreventScroll(this.contentDOM);
                this.docView.updateSelection();
            });
        }
        /// Clean up this editor view, removing its element from the
        /// document, unregistering event handlers, and notifying
        /// plugins. The view instance can no longer be used after
        /// calling this.
        destroy() {
            for (let plugin of this.plugins)
                plugin.destroy(this);
            this.inputState.destroy();
            this.dom.remove();
            this.observer.destroy();
            if (this.measureScheduled > -1)
                cancelAnimationFrame(this.measureScheduled);
        }
        /// Facet that can be used to add DOM event handlers. The value
        /// should be an object mapping event names to handler functions. The
        /// first such function to return true will be assumed to have handled
        /// that event, and no other handlers or built-in behavior will be
        /// activated for it.
        /// These are registered on the [content
        /// element](#view.EditorView.contentDOM), except for `scroll`
        /// handlers, which will be called any time the editor's [scroll
        /// element](#view.EditorView.scrollDOM) or one of its parent nodes
        /// is scrolled.
        static domEventHandlers(handlers) {
            return ViewPlugin.define(() => ({}), { eventHandlers: handlers });
        }
        /// Create a theme extension. The first argument can be a
        /// [`style-mod`](https://github.com/marijnh/style-mod#documentation)
        /// style spec providing the styles for the theme. These will be
        /// prefixed with a generated class for the style.
        ///
        /// It is highly recommended you use _theme classes_, rather than
        /// regular CSS classes, in your selectors. These are prefixed with
        /// a `$` instead of a `.`, and will be expanded (as with
        /// [`themeClass`](#view.themeClass)) to one or more prefixed class
        /// names. So for example `$content` targets the editor's [content
        /// element](#view.EditorView.contentDOM).
        ///
        /// Because the selectors will be prefixed with a scope class,
        /// directly matching the editor's [wrapper
        /// element](#view.EditorView.dom), which is the element on which
        /// the scope class will be added, needs to be explicitly
        /// differentiated by adding an additional `$` to the front of the
        /// pattern. For example `$$focused $panel` will expand to something
        /// like `.[scope].cm-focused .cm-panel`.
        ///
        /// When `dark` is set to true, the theme will be marked as dark,
        /// which will add the `$dark` selector to the wrapper element (as
        /// opposed to `$light` when a light theme is active).
        static theme(spec, options) {
            let prefix = StyleModule.newName();
            let result = [theme.of(prefix), styleModule.of(buildTheme(`.${baseThemeID}.${prefix}`, spec))];
            if (options && options.dark)
                result.push(darkTheme.of(true));
            return result;
        }
        /// Create an extension that adds styles to the base theme. The
        /// given object works much like the one passed to
        /// [`theme`](#view.EditorView^theme). You'll often want to qualify
        /// base styles with `$dark` or `$light` so they only apply when
        /// there is a dark or light theme active. For example `"$$dark
        /// $myHighlight"`.
        static baseTheme(spec) {
            return precedence(styleModule.of(buildTheme("." + baseThemeID, spec)), "fallback");
        }
    }
    /// Facet to add a [style
    /// module](https://github.com/marijnh/style-mod#documentation) to
    /// an editor view. The view will ensure that the module is
    /// registered in its [document
    /// root](#view.EditorView.constructor^config.root).
    EditorView.styleModule = styleModule;
    /// An input handler can be used to override the way changes to the
    /// content are handled. A handler is passed the document positions
    /// between which the change was found, and the new content. When it
    /// returns true, no further input handlers are called and the
    /// default behavior is prevented.
    EditorView.inputHandler = inputHandler;
    /// Allows you to provide a function that should be called when the
    /// library catches an exception from an extension (mostly from view
    /// plugins, but may be used by other extensions to route exceptions
    /// from user-code-provided callbacks). This is mostly useful for
    /// debugging and logging. See [`logException`](#view.logException).
    EditorView.exceptionSink = exceptionSink;
    /// A facet that can be used to have a listener function be notified
    /// every time the view updates.
    EditorView.updateListener = updateListener;
    /// Facet that controls whether the editor content is editable. When
    /// its the highest-precedence value is `false`, editing is
    /// disabled, and the content element will no longer have its
    /// `contenteditable` attribute set to `true`. (Note that this
    /// doesn't affect API calls that change the editor content, even
    /// when those are bound to keys or buttons.)
    EditorView.editable = editable;
    /// Facet used to configure whether a given selection drag event
    /// should move or copy the selection. The given predicate will be
    /// called with the `mousedown` event, and can return `true` when
    /// the drag should move the content.
    EditorView.dragMovesSelection = dragMovesSelection;
    /// Facet used to configure whether a given selecting click adds
    /// a new range to the existing selection or replaces it entirely.
    EditorView.clickAddsSelectionRange = clickAddsSelectionRange;
    /// Allows you to influence the way mouse selection happens. The
    /// functions in this facet will be called for a `mousedown` event
    /// on the editor, and can return an object that overrides the way a
    /// selection is computed from that mouse click or drag.
    EditorView.mouseSelectionStyle = mouseSelectionStyle;
    /// A facet that determines which [decorations](#view.Decoration)
    /// are shown in the view. See also [view
    /// plugins](#view.EditorView^decorations), which have a separate
    /// mechanism for providing decorations.
    EditorView.decorations = decorations;
    /// An extension that enables line wrapping in the editor.
    EditorView.lineWrapping = EditorView.theme({ $content: { whiteSpace: "pre-wrap" } });
    /// Facet that provides attributes for the editor's editable DOM
    /// element.
    EditorView.contentAttributes = contentAttributes;
    /// Facet that provides editor DOM attributes for the editor's
    /// outer element.
    EditorView.editorAttributes = editorAttributes;
    // Maximum line length for which we compute accurate bidi info
    const MaxBidiLine = 4096;
    function ensureTop(given, dom) {
        return given == null ? dom.getBoundingClientRect().top : given;
    }
    let resizeDebounce = -1;
    function ensureGlobalHandler() {
        window.addEventListener("resize", () => {
            if (resizeDebounce == -1)
                resizeDebounce = setTimeout(handleResize, 50);
        });
    }
    function handleResize() {
        resizeDebounce = -1;
        let found = document.querySelectorAll(".cm-content");
        for (let i = 0; i < found.length; i++) {
            let docView = ContentView.get(found[i]);
            if (docView)
                docView.editorView.requestMeasure();
        }
    }
    const BadMeasure = {};
    class CachedOrder {
        constructor(from, to, dir, order) {
            this.from = from;
            this.to = to;
            this.dir = dir;
            this.order = order;
        }
        static update(cache, changes) {
            if (changes.empty)
                return cache;
            let result = [], lastDir = cache.length ? cache[cache.length - 1].dir : Direction.LTR;
            for (let i = Math.max(0, cache.length - 10); i < cache.length; i++) {
                let entry = cache[i];
                if (entry.dir == lastDir && !changes.touchesRange(entry.from, entry.to))
                    result.push(new CachedOrder(changes.mapPos(entry.from, 1), changes.mapPos(entry.to, -1), entry.dir, entry.order));
            }
            return result;
        }
    }

    const currentPlatform = typeof navigator == "undefined" ? "key"
        : /Mac/.test(navigator.platform) ? "mac"
            : /Win/.test(navigator.platform) ? "win"
                : /Linux|X11/.test(navigator.platform) ? "linux"
                    : "key";
    function normalizeKeyName(name, platform) {
        const parts = name.split(/-(?!$)/);
        let result = parts[parts.length - 1];
        if (result == "Space")
            result = " ";
        let alt, ctrl, shift, meta;
        for (let i = 0; i < parts.length - 1; ++i) {
            const mod = parts[i];
            if (/^(cmd|meta|m)$/i.test(mod))
                meta = true;
            else if (/^a(lt)?$/i.test(mod))
                alt = true;
            else if (/^(c|ctrl|control)$/i.test(mod))
                ctrl = true;
            else if (/^s(hift)?$/i.test(mod))
                shift = true;
            else if (/^mod$/i.test(mod)) {
                if (platform == "mac")
                    meta = true;
                else
                    ctrl = true;
            }
            else
                throw new Error("Unrecognized modifier name: " + mod);
        }
        if (alt)
            result = "Alt-" + result;
        if (ctrl)
            result = "Ctrl-" + result;
        if (meta)
            result = "Meta-" + result;
        if (shift)
            result = "Shift-" + result;
        return result;
    }
    function modifiers(name, event, shift) {
        if (event.altKey)
            name = "Alt-" + name;
        if (event.ctrlKey)
            name = "Ctrl-" + name;
        if (event.metaKey)
            name = "Meta-" + name;
        if (shift !== false && event.shiftKey)
            name = "Shift-" + name;
        return name;
    }
    const keymaps = Facet.define();
    const handleKeyEvents = EditorView.domEventHandlers({
        keydown(event, view) {
            return runHandlers(view.state.facet(keymaps), event, view, "editor");
        }
    });
    /// Create a view extension that registers a keymap.
    ///
    /// You can add multiple keymap extensions to an editor. Their
    /// priorities determine their precedence (the ones specified early or
    /// with high priority get checked first). When a handler has returned
    /// `true` for a given key, no further handlers are called.
    ///
    /// When a key is bound multiple times (either in a single keymap or
    /// in separate maps), the bound commands all get a chance to handle
    /// the key stroke, in order of precedence, until one of them returns
    /// true.
    function keymap(bindings, platform) {
        return [handleKeyEvents, keymaps.of(buildKeymap(bindings, platform))];
    }
    /// Run the key handlers registered for a given scope. Returns true if
    /// any of them handled the event.
    function runScopeHandlers(view, event, scope) {
        return runHandlers(view.state.facet(keymaps), event, view, scope);
    }
    let storedPrefix = null;
    const PrefixTimeout = 4000;
    function buildKeymap(bindings, platform = currentPlatform) {
        let bound = Object.create(null);
        let isPrefix = Object.create(null);
        let checkPrefix = (name, is) => {
            let current = isPrefix[name];
            if (current == null)
                isPrefix[name] = is;
            else if (current != is)
                throw new Error("Key binding " + name + " is used both as a regular binding and as a multi-stroke prefix");
        };
        let add = (scope, key, command, preventDefault) => {
            let scopeObj = bound[scope] || (bound[scope] = Object.create(null));
            let parts = key.split(/ (?!$)/).map(k => normalizeKeyName(k, platform));
            for (let i = 1; i < parts.length; i++) {
                let prefix = parts.slice(0, i).join(" ");
                checkPrefix(prefix, true);
                if (!scopeObj[prefix])
                    scopeObj[prefix] = {
                        preventDefault: true,
                        commands: [(view) => {
                                let ourObj = storedPrefix = { view, prefix, scope };
                                setTimeout(() => { if (storedPrefix == ourObj)
                                    storedPrefix = null; }, PrefixTimeout);
                                return true;
                            }]
                    };
            }
            let full = parts.join(" ");
            checkPrefix(full, false);
            let binding = scopeObj[full] || (scopeObj[full] = { preventDefault: false, commands: [] });
            binding.commands.push(command);
            if (preventDefault)
                binding.preventDefault = true;
        };
        for (let b of bindings) {
            let name = b[platform] || b.key;
            if (!name)
                continue;
            for (let scope of b.scope ? b.scope.split(" ") : ["editor"]) {
                add(scope, name, b.run, b.preventDefault);
                if (b.shift)
                    add(scope, "Shift-" + name, b.shift, b.preventDefault);
            }
        }
        return bound;
    }
    function runHandlers(maps, event, view, scope) {
        let name = keyName(event), isChar = name.length == 1 && name != " ";
        let prefix = "";
        if (storedPrefix && storedPrefix.view == view && storedPrefix.scope == scope) {
            prefix = storedPrefix.prefix + " ";
            storedPrefix = null;
        }
        let fallthrough = !!prefix;
        let runFor = (binding) => {
            if (binding) {
                for (let cmd of binding.commands)
                    if (cmd(view))
                        return true;
                if (binding.preventDefault)
                    fallthrough = true;
            }
            return false;
        };
        for (let map of maps) {
            let scopeObj = map[scope], baseName;
            if (!scopeObj)
                continue;
            if (runFor(scopeObj[prefix + modifiers(name, event, !isChar)]))
                return true;
            if (isChar && (event.shiftKey || event.altKey || event.metaKey) &&
                (baseName = base[event.keyCode]) && baseName != name) {
                if (runFor(scopeObj[prefix + modifiers(baseName, event, true)]))
                    return true;
            }
            else if (isChar && event.shiftKey) {
                if (runFor(scopeObj[prefix + modifiers(name, event, true)]))
                    return true;
            }
        }
        return fallthrough;
    }

    const CanHidePrimary = !browser.ios; // FIXME test IE
    const selectionConfig = Facet.define({
        combine(configs) {
            return combineConfig(configs, {
                cursorBlinkRate: 1200,
                drawRangeCursor: true
            }, {
                cursorBlinkRate: (a, b) => Math.min(a, b),
                drawRangeCursor: (a, b) => a || b
            });
        }
    });
    /// Returns an extension that hides the browser's native selection and
    /// cursor, replacing the selection with a background behind the text
    /// (labeled with the `$selectionBackground` theme class), and the
    /// cursors with elements overlaid over the code (using
    /// `$cursor.primary` and `$cursor.secondary`).
    ///
    /// This allows the editor to display secondary selection ranges, and
    /// tends to produce a type of selection more in line with that users
    /// expect in a text editor (the native selection styling will often
    /// leave gaps between lines and won't fill the horizontal space after
    /// a line when the selection continues past it).
    ///
    /// It does have a performance cost, in that it requires an extra DOM
    /// layout cycle for many updates (the selection is drawn based on DOM
    /// layout information that's only available after laying out the
    /// content).
    function drawSelection(config = {}) {
        return [
            selectionConfig.of(config),
            drawSelectionPlugin,
            hideNativeSelection
        ];
    }
    class Piece {
        constructor(left, top, width, height, className) {
            this.left = left;
            this.top = top;
            this.width = width;
            this.height = height;
            this.className = className;
        }
        draw() {
            let elt = document.createElement("div");
            elt.className = this.className;
            elt.style.left = this.left + "px";
            elt.style.top = this.top + "px";
            if (this.width >= 0)
                elt.style.width = this.width + "px";
            elt.style.height = this.height + "px";
            return elt;
        }
        eq(p) {
            return this.left == p.left && this.top == p.top && this.width == p.width && this.height == p.height &&
                this.className == p.className;
        }
    }
    const drawSelectionPlugin = ViewPlugin.fromClass(class {
        constructor(view) {
            this.view = view;
            this.rangePieces = [];
            this.cursors = [];
            this.measureReq = { read: this.readPos.bind(this), write: this.drawSel.bind(this) };
            this.selectionLayer = view.scrollDOM.appendChild(document.createElement("div"));
            this.selectionLayer.className = themeClass("selectionLayer");
            this.selectionLayer.setAttribute("aria-hidden", "true");
            this.cursorLayer = view.scrollDOM.appendChild(document.createElement("div"));
            this.cursorLayer.className = themeClass("cursorLayer");
            this.cursorLayer.setAttribute("aria-hidden", "true");
            view.requestMeasure(this.measureReq);
            this.setBlinkRate();
        }
        setBlinkRate() {
            this.cursorLayer.style.animationDuration = this.view.state.facet(selectionConfig).cursorBlinkRate + "ms";
        }
        update(update) {
            let confChanged = update.prevState.facet(selectionConfig) != update.state.facet(selectionConfig);
            if (confChanged || update.selectionSet || update.geometryChanged || update.viewportChanged)
                this.view.requestMeasure(this.measureReq);
            if (update.transactions.some(tr => tr.scrollIntoView))
                this.cursorLayer.style.animationName = this.cursorLayer.style.animationName == "cm-blink" ? "cm-blink2" : "cm-blink";
            if (confChanged)
                this.setBlinkRate();
        }
        readPos() {
            let { state } = this.view, conf = state.facet(selectionConfig);
            let rangePieces = state.selection.ranges.map(r => r.empty ? [] : measureRange(this.view, r)).reduce((a, b) => a.concat(b));
            let cursors = [];
            for (let r of state.selection.ranges) {
                let prim = r == state.selection.primary;
                if (r.empty ? !prim || CanHidePrimary : conf.drawRangeCursor) {
                    let piece = measureCursor(this.view, r, prim);
                    if (piece)
                        cursors.push(piece);
                }
            }
            return { rangePieces, cursors };
        }
        drawSel({ rangePieces, cursors }) {
            if (rangePieces.length != this.rangePieces.length || rangePieces.some((p, i) => !p.eq(this.rangePieces[i]))) {
                this.selectionLayer.textContent = "";
                for (let p of rangePieces)
                    this.selectionLayer.appendChild(p.draw());
                this.rangePieces = rangePieces;
            }
            if (cursors.length != this.cursors.length || cursors.some((c, i) => !c.eq(this.cursors[i]))) {
                this.cursorLayer.textContent = "";
                for (let c of cursors)
                    this.cursorLayer.appendChild(c.draw());
                this.cursors = cursors;
            }
        }
        destroy() {
            this.selectionLayer.remove();
            this.cursorLayer.remove();
        }
    });
    const themeSpec = {
        $line: {
            "& ::selection": { backgroundColor: "transparent !important" },
            "&::selection": { backgroundColor: "transparent !important" }
        }
    };
    if (CanHidePrimary)
        themeSpec.$line.caretColor = "transparent !important";
    const hideNativeSelection = precedence(EditorView.theme(themeSpec), "override");
    const selectionClass = themeClass("selectionBackground");
    function getBase(view) {
        let rect = view.scrollDOM.getBoundingClientRect();
        return { left: rect.left - view.scrollDOM.scrollLeft, top: rect.top - view.scrollDOM.scrollTop };
    }
    function wrappedLine(view, pos, inside) {
        let range = EditorSelection.cursor(pos);
        return { from: Math.max(inside.from, view.moveToLineBoundary(range, false, true).from),
            to: Math.min(inside.to, view.moveToLineBoundary(range, true, true).from) };
    }
    function measureRange(view, range) {
        if (range.to <= view.viewport.from || range.from >= view.viewport.to)
            return [];
        let from = Math.max(range.from, view.viewport.from), to = Math.min(range.to, view.viewport.to);
        let ltr = view.textDirection == Direction.LTR;
        let content = view.contentDOM, contentRect = content.getBoundingClientRect(), base = getBase(view);
        let lineStyle = window.getComputedStyle(content.firstChild);
        let leftSide = contentRect.left + parseInt(lineStyle.paddingLeft);
        let rightSide = contentRect.right - parseInt(lineStyle.paddingRight);
        let visualStart = view.visualLineAt(from);
        let visualEnd = view.visualLineAt(to);
        if (view.lineWrapping) {
            visualStart = wrappedLine(view, from, visualStart);
            visualEnd = wrappedLine(view, to, visualEnd);
        }
        if (visualStart.from == visualEnd.from) {
            return pieces(drawForLine(range.from, range.to));
        }
        else {
            let top = drawForLine(range.from, null);
            let bottom = drawForLine(null, range.to);
            let between = [];
            if (visualStart.to < visualEnd.from - 1)
                between.push(piece(leftSide, top.bottom, rightSide, bottom.top));
            else if (top.bottom < bottom.top && bottom.top - top.bottom < 4)
                top.bottom = bottom.top = (top.bottom + bottom.top) / 2;
            return pieces(top).concat(between).concat(pieces(bottom));
        }
        function piece(left, top, right, bottom) {
            return new Piece(left - base.left, top - base.top, right - left, bottom - top, selectionClass);
        }
        function pieces({ top, bottom, horizontal }) {
            let pieces = [];
            for (let i = 0; i < horizontal.length; i += 2)
                pieces.push(piece(horizontal[i], top, horizontal[i + 1], bottom));
            return pieces;
        }
        // Gets passed from/to in line-local positions
        function drawForLine(from, to) {
            let top = 1e9, bottom = -1e9, horizontal = [];
            function addSpan(from, fromOpen, to, toOpen, dir) {
                let fromCoords = view.coordsAtPos(from, 1), toCoords = view.coordsAtPos(to, -1);
                top = Math.min(fromCoords.top, toCoords.top, top);
                bottom = Math.max(fromCoords.bottom, toCoords.bottom, bottom);
                if (dir == Direction.LTR)
                    horizontal.push(ltr && fromOpen ? leftSide : fromCoords.left, ltr && toOpen ? rightSide : toCoords.right);
                else
                    horizontal.push(!ltr && toOpen ? leftSide : toCoords.left, !ltr && fromOpen ? rightSide : fromCoords.right);
            }
            let start = from !== null && from !== void 0 ? from : view.moveToLineBoundary(EditorSelection.cursor(to, 1), false).head;
            let end = to !== null && to !== void 0 ? to : view.moveToLineBoundary(EditorSelection.cursor(from, -1), true).head;
            // Split the range by visible range and document line
            for (let r of view.visibleRanges)
                if (r.to > start && r.from < end) {
                    for (let pos = Math.max(r.from, start), endPos = Math.min(r.to, end);;) {
                        let docLine = view.state.doc.lineAt(pos);
                        for (let span of view.bidiSpans(docLine)) {
                            let spanFrom = span.from + docLine.from, spanTo = span.to + docLine.from;
                            if (spanFrom >= endPos)
                                break;
                            if (spanTo > pos)
                                addSpan(Math.max(spanFrom, pos), from == null && spanFrom <= start, Math.min(spanTo, endPos), to == null && spanTo >= end, span.dir);
                        }
                        pos = docLine.to + 1;
                        if (pos >= endPos)
                            break;
                    }
                }
            if (horizontal.length == 0) {
                let coords = view.coordsAtPos(start, -1);
                top = Math.min(coords.top, top);
                bottom = Math.max(coords.bottom, bottom);
            }
            return { top, bottom, horizontal };
        }
    }
    const primaryCursorClass = themeClass("cursor.primary");
    const cursorClass = themeClass("cursor.secondary");
    function measureCursor(view, cursor, primary) {
        let pos = view.coordsAtPos(cursor.head, cursor.assoc || 1);
        if (!pos)
            return null;
        let base = getBase(view);
        return new Piece(pos.left - base.left, pos.top - base.top, -1, pos.bottom - pos.top, primary ? primaryCursorClass : cursorClass);
    }

    const Specials = /[\u0000-\u0008\u000a-\u001f\u007f-\u009f\u00ad\u061c\u200b-\u200c\u200e\u200f\u2028\u2029\ufeff\ufff9-\ufffc]/gu;
    const Names = {
        0: "null",
        7: "bell",
        8: "backspace",
        10: "newline",
        11: "vertical tab",
        13: "carriage return",
        27: "escape",
        8203: "zero width space",
        8204: "zero width non-joiner",
        8205: "zero width joiner",
        8206: "left-to-right mark",
        8207: "right-to-left mark",
        8232: "line separator",
        8233: "paragraph separator",
        65279: "zero width no-break space",
        65532: "object replacement"
    };
    let _supportsTabSize = null;
    function supportsTabSize() {
        if (_supportsTabSize == null && typeof document != "undefined" && document.body) {
            let styles = document.body.style;
            _supportsTabSize = (styles.tabSize || styles.MozTabSize) != null;
        }
        return _supportsTabSize || false;
    }
    const UnicodeRegexpSupport = /x/.unicode != null ? "gu" : "g";
    const specialCharConfig = Facet.define({
        combine(configs) {
            // FIXME make configurations compose properly
            let config = combineConfig(configs, {
                render: null,
                specialChars: Specials,
                addSpecialChars: null
            });
            if (config.replaceTabs = !supportsTabSize())
                config.specialChars = new RegExp("\t|" + config.specialChars.source, UnicodeRegexpSupport);
            if (config.addSpecialChars)
                config.specialChars = new RegExp(config.specialChars.source + "|" + config.addSpecialChars.source, UnicodeRegexpSupport);
            return config;
        }
    });
    /// Returns an extension that installs highlighting of special
    /// characters.
    function highlightSpecialChars(
    /// Configuration options.
    config = {}) {
        let ext = [specialCharConfig.of(config), specialCharPlugin];
        if (!supportsTabSize())
            ext.push(tabStyle);
        return ext;
    }
    const specialCharPlugin = ViewPlugin.fromClass(class {
        constructor(view) {
            this.view = view;
            this.decorations = Decoration.none;
            this.decorationCache = Object.create(null);
            this.recompute();
        }
        update(update) {
            let confChange = update.prevState.facet(specialCharConfig) != update.state.facet(specialCharConfig);
            if (confChange)
                this.decorationCache = Object.create(null);
            if (confChange || update.changes.length || update.viewportChanged)
                this.recompute();
        }
        recompute() {
            let decorations = [];
            for (let { from, to } of this.view.visibleRanges)
                this.getDecorationsFor(from, to, decorations);
            this.decorations = Decoration.set(decorations);
        }
        getDecorationsFor(from, to, target) {
            let config = this.view.state.facet(specialCharConfig);
            let { doc } = this.view.state;
            for (let pos = from, cursor = doc.iterRange(from, to), m; !cursor.next().done;) {
                if (!cursor.lineBreak) {
                    while (m = config.specialChars.exec(cursor.value)) {
                        let code = codePointAt(m[0], 0), deco;
                        if (code == null)
                            continue;
                        if (code == 9) {
                            let line = doc.lineAt(pos + m.index);
                            let size = this.view.state.tabSize, col = countColumn(doc.sliceString(line.from, pos + m.index), 0, size);
                            deco = Decoration.replace({ widget: new TabWidget((size - (col % size)) * this.view.defaultCharacterWidth) });
                        }
                        else {
                            deco = this.decorationCache[code] ||
                                (this.decorationCache[code] = Decoration.replace({ widget: new SpecialCharWidget(config, code) }));
                        }
                        target.push(deco.range(pos + m.index, pos + m.index + m[0].length));
                    }
                }
                pos += cursor.value.length;
            }
        }
    }, {
        decorations: v => v.decorations
    });
    // Assigns placeholder characters from the Control Pictures block to
    // ASCII control characters
    function placeHolder(code) {
        if (code >= 32)
            return null;
        if (code == 10)
            return "\u2424";
        return String.fromCharCode(9216 + code);
    }
    const DefaultPlaceholder = "\u2022";
    class SpecialCharWidget extends WidgetType {
        constructor(options, code) {
            super();
            this.options = options;
            this.code = code;
        }
        eq(other) { return other.code == this.code; }
        toDOM() {
            let ph = placeHolder(this.code) || DefaultPlaceholder;
            let desc = "Control character " + (Names[this.code] || this.code);
            let custom = this.options.render && this.options.render(this.code, desc, ph);
            if (custom)
                return custom;
            let span = document.createElement("span");
            span.textContent = ph;
            span.title = desc;
            span.setAttribute("aria-label", desc);
            span.style.color = "red";
            return span;
        }
        ignoreEvent() { return false; }
    }
    class TabWidget extends WidgetType {
        constructor(width) {
            super();
            this.width = width;
        }
        eq(other) { return other.width == this.width; }
        toDOM() {
            let span = document.createElement("span");
            span.textContent = "\t";
            span.className = tab;
            span.style.width = this.width + "px";
            return span;
        }
        ignoreEvent() { return false; }
    }
    const tab = StyleModule.newName(), tabStyle = EditorView.styleModule.of(new StyleModule({
        ["." + tab]: {
            display: "inline-block",
            overflow: "hidden",
            verticalAlign: "bottom"
        }
    }));

    const fromHistory = Annotation.define();
    /// Transaction annotation that will prevent that annotation from
    /// being combined with other annotations in the undo history. Given
    /// `"before"`, it'll prevent merging with previous transactions. With
    /// `"after"`, subsequent transactions won't be combined with this
    /// one. With `"full"`, the transaction is isolated on both sides.
    const isolateHistory = Annotation.define();
    /// This facet provides a way to register functions that, given a
    /// transaction, provide a set of effects that the history should
    /// store when inverting the transaction. This can be used to
    /// integrate some kinds of effects in the history, so that they can
    /// be undone (and redone again).
    const invertedEffects = Facet.define();
    const historyConfig = Facet.define({
        combine(configs) {
            return combineConfig(configs, {
                minDepth: 100,
                newGroupDelay: 500
            }, { minDepth: Math.max, newGroupDelay: Math.min });
        }
    });
    const historyField = StateField.define({
        create() {
            return HistoryState.empty;
        },
        update(state, tr) {
            let config = tr.state.facet(historyConfig);
            let fromHist = tr.annotation(fromHistory);
            if (fromHist) {
                let item = HistEvent.fromTransaction(tr), from = fromHist.side;
                let other = from == 0 /* Done */ ? state.undone : state.done;
                if (item)
                    other = updateBranch(other, other.length, config.minDepth, item);
                else
                    other = addSelection(other, tr.startState.selection);
                return new HistoryState(from == 0 /* Done */ ? fromHist.rest : other, from == 0 /* Done */ ? other : fromHist.rest);
            }
            let isolate = tr.annotation(isolateHistory);
            if (isolate == "full" || isolate == "before")
                state = state.isolate();
            if (tr.annotation(Transaction.addToHistory) === false)
                return tr.changes.length ? state.addMapping(tr.changes.desc) : state;
            let event = HistEvent.fromTransaction(tr);
            let time = tr.annotation(Transaction.time), userEvent = tr.annotation(Transaction.userEvent);
            if (event)
                state = state.addChanges(event, time, userEvent, config.newGroupDelay, config.minDepth);
            else if (tr.selection)
                state = state.addSelection(tr.startState.selection, time, userEvent, config.newGroupDelay);
            if (isolate == "full" || isolate == "after")
                state = state.isolate();
            return state;
        }
    });
    /// Create a history extension with the given configuration.
    function history(config = {}) {
        // FIXME register beforeinput handler
        return [
            historyField,
            historyConfig.of(config)
        ];
    }
    function cmd(side, selection) {
        return function ({ state, dispatch }) {
            let historyState = state.field(historyField, false);
            if (!historyState)
                return false;
            let tr = historyState.pop(side, state, selection);
            if (!tr)
                return false;
            dispatch(tr);
            return true;
        };
    }
    /// Undo a single group of history events. Returns false if no group
    /// was available.
    const undo = cmd(0 /* Done */, false);
    /// Redo a group of history events. Returns false if no group was
    /// available.
    const redo = cmd(1 /* Undone */, false);
    /// Undo a selection change.
    const undoSelection = cmd(0 /* Done */, true);
    /// Redo a selection change.
    const redoSelection = cmd(1 /* Undone */, true);
    // History events store groups of changes or effects that need to be
    // undone/redone together.
    class HistEvent {
        constructor(
        // The changes in this event. Normal events hold at least one
        // change or effect. But it may be necessary to store selection
        // events before the first change, in which case a special type of
        // instance is created which doesn't hold any changes, with
        // changes == startSelection == undefined
        changes, 
        // The effects associated with this event
        effects, mapped, 
        // The selection before this event
        startSelection, 
        // Stores selection changes after this event, to be used for
        // selection undo/redo.
        selectionsAfter) {
            this.changes = changes;
            this.effects = effects;
            this.mapped = mapped;
            this.startSelection = startSelection;
            this.selectionsAfter = selectionsAfter;
        }
        setSelAfter(after) {
            return new HistEvent(this.changes, this.effects, this.mapped, this.startSelection, after);
        }
        // This does not check `addToHistory` and such, it assumes the
        // transaction needs to be converted to an item. Returns null when
        // there are no changes or effects in the transaction.
        static fromTransaction(tr) {
            let effects = none$5;
            for (let invert of tr.startState.facet(invertedEffects)) {
                let result = invert(tr);
                if (result.length)
                    effects = effects.concat(result);
            }
            if (!effects.length && tr.changes.empty)
                return null;
            return new HistEvent(tr.changes.invert(tr.startState.doc), effects, undefined, tr.startState.selection, none$5);
        }
        static selection(selections) {
            return new HistEvent(undefined, none$5, undefined, undefined, selections);
        }
    }
    function updateBranch(branch, to, maxLen, newEvent) {
        let start = to + 1 > maxLen + 20 ? to - maxLen - 1 : 0;
        let newBranch = branch.slice(start, to);
        newBranch.push(newEvent);
        return newBranch;
    }
    function isAdjacent(a, b) {
        let ranges = [], isAdjacent = false;
        a.iterChangedRanges((f, t) => ranges.push(f, t));
        b.iterChangedRanges((_f, _t, f, t) => {
            for (let i = 0; i < ranges.length;) {
                let from = ranges[i++], to = ranges[i++];
                if (t >= from && f <= to)
                    isAdjacent = true;
            }
        });
        return isAdjacent;
    }
    function eqSelectionShape(a, b) {
        return a.ranges.length == b.ranges.length &&
            a.ranges.filter((r, i) => r.empty != b.ranges[i].empty).length === 0;
    }
    function conc(a, b) {
        return !a.length ? b : !b.length ? a : a.concat(b);
    }
    const none$5 = [];
    const MaxSelectionsPerEvent = 200;
    function addSelection(branch, selection) {
        if (!branch.length) {
            return [HistEvent.selection([selection])];
        }
        else {
            let lastEvent = branch[branch.length - 1];
            let sels = lastEvent.selectionsAfter.slice(Math.max(0, lastEvent.selectionsAfter.length - MaxSelectionsPerEvent));
            if (sels.length && sels[sels.length - 1].eq(selection))
                return branch;
            sels.push(selection);
            return updateBranch(branch, branch.length - 1, 1e9, lastEvent.setSelAfter(sels));
        }
    }
    // Assumes the top item has one or more selectionAfter values
    function popSelection(branch) {
        let last = branch[branch.length - 1];
        let newBranch = branch.slice();
        newBranch[branch.length - 1] = last.setSelAfter(last.selectionsAfter.slice(0, last.selectionsAfter.length - 1));
        return newBranch;
    }
    // Add a mapping to the top event in the given branch. If this maps
    // away all the changes and effects in that item, drop it and
    // propagate the mapping to the next item.
    function addMappingToBranch(branch, mapping) {
        if (!branch.length)
            return branch;
        let length = branch.length, selections = none$5;
        while (length) {
            let event = mapEvent(branch[length - 1], mapping, selections);
            if (event.changes && !event.changes.empty || event.effects.length) { // Event survived mapping
                let result = branch.slice(0, length);
                result[length - 1] = event;
                return result;
            }
            else { // Drop this event, since there's no changes or effects left
                mapping = event.mapped;
                length--;
                selections = event.selectionsAfter;
            }
        }
        return selections.length ? [HistEvent.selection(selections)] : none$5;
    }
    function mapEvent(event, mapping, extraSelections) {
        let selections = conc(event.selectionsAfter.length ? event.selectionsAfter.map(s => s.map(mapping)) : none$5, extraSelections);
        // Change-less events don't store mappings (they are always the last event in a branch)
        if (!event.changes)
            return HistEvent.selection(selections);
        let mappedChanges = event.changes.map(mapping), before = mapping.mapDesc(event.changes, true);
        let fullMapping = event.mapped ? event.mapped.composeDesc(before) : before;
        return new HistEvent(mappedChanges, StateEffect.mapEffects(event.effects, mapping), fullMapping, event.startSelection.map(before), selections);
    }
    class HistoryState {
        constructor(done, undone, prevTime = 0, prevUserEvent = undefined) {
            this.done = done;
            this.undone = undone;
            this.prevTime = prevTime;
            this.prevUserEvent = prevUserEvent;
        }
        isolate() {
            return this.prevTime ? new HistoryState(this.done, this.undone) : this;
        }
        addChanges(event, time, userEvent, newGroupDelay, maxLen) {
            let done = this.done, lastEvent = done[done.length - 1];
            if (lastEvent && lastEvent.changes &&
                time - this.prevTime < newGroupDelay &&
                !lastEvent.selectionsAfter.length &&
                lastEvent.changes.length && event.changes &&
                isAdjacent(lastEvent.changes, event.changes)) {
                done = updateBranch(done, done.length - 1, maxLen, new HistEvent(event.changes.compose(lastEvent.changes), conc(event.effects, lastEvent.effects), lastEvent.mapped, lastEvent.startSelection, none$5));
            }
            else {
                done = updateBranch(done, done.length, maxLen, event);
            }
            return new HistoryState(done, none$5, time, userEvent);
        }
        addSelection(selection, time, userEvent, newGroupDelay) {
            let last = this.done.length ? this.done[this.done.length - 1].selectionsAfter : none$5;
            if (last.length > 0 &&
                time - this.prevTime < newGroupDelay &&
                userEvent == "keyboardselection" && this.prevUserEvent == userEvent &&
                eqSelectionShape(last[last.length - 1], selection))
                return this;
            return new HistoryState(addSelection(this.done, selection), this.undone, time, userEvent);
        }
        addMapping(mapping) {
            return new HistoryState(addMappingToBranch(this.done, mapping), addMappingToBranch(this.undone, mapping), this.prevTime, this.prevUserEvent);
        }
        pop(side, state, selection) {
            let branch = side == 0 /* Done */ ? this.done : this.undone;
            if (branch.length == 0)
                return null;
            let event = branch[branch.length - 1];
            if (selection && event.selectionsAfter.length) {
                return state.update({
                    selection: event.selectionsAfter[event.selectionsAfter.length - 1],
                    annotations: fromHistory.of({ side, rest: popSelection(branch) })
                });
            }
            else if (!event.changes) {
                return null;
            }
            else {
                let rest = branch.length == 1 ? none$5 : branch.slice(0, branch.length - 1);
                if (event.mapped)
                    rest = addMappingToBranch(rest, event.mapped);
                return state.update({
                    changes: event.changes,
                    selection: event.startSelection,
                    effects: event.effects,
                    annotations: fromHistory.of({ side, rest }),
                    filter: false
                });
            }
        }
    }
    HistoryState.empty = new HistoryState(none$5, none$5);
    /// Default key bindings for the undo history.
    ///
    /// - Mod-z: [`undo`](#history.undo).
    /// - Mod-y (Mod-Shift-z on macOS): [`redo`](#history.redo).
    /// - Mod-u: [`undoSelection`](#history.undoSelection).
    /// - Alt-u (Mod-Shift-u on macOS): [`redoSelection`](#history.redoSelection).
    const historyKeymap = [
        { key: "Mod-z", run: undo, preventDefault: true },
        { key: "Mod-y", mac: "Mod-Shift-z", run: redo, preventDefault: true },
        { key: "Mod-u", run: undoSelection, preventDefault: true },
        { key: "Alt-u", mac: "Mod-Shift-u", run: redoSelection, preventDefault: true }
    ];

    /// The default maximum length of a `TreeBuffer` node.
    const DefaultBufferLength = 1024;
    let nextPropID = 0;
    const CachedNode = new WeakMap();
    /// Each [node type](#tree.NodeType) can have metadata associated with
    /// it in props. Instances of this class represent prop names.
    class NodeProp {
        /// Create a new node prop type. You can optionally pass a
        /// `deserialize` function.
        constructor({ deserialize } = {}) {
            this.id = nextPropID++;
            this.deserialize = deserialize || (() => {
                throw new Error("This node type doesn't define a deserialize function");
            });
        }
        /// Create a string-valued node prop whose deserialize function is
        /// the identity function.
        static string() { return new NodeProp({ deserialize: str => str }); }
        /// Create a number-valued node prop whose deserialize function is
        /// just `Number`.
        static number() { return new NodeProp({ deserialize: Number }); }
        /// Creates a boolean-valued node prop whose deserialize function
        /// returns true for any input.
        static flag() { return new NodeProp({ deserialize: () => true }); }
        /// Store a value for this prop in the given object. This can be
        /// useful when building up a prop object to pass to the
        /// [`NodeType`](#tree.NodeType) constructor. Returns its first
        /// argument.
        set(propObj, value) {
            propObj[this.id] = value;
            return propObj;
        }
        /// This is meant to be used with
        /// [`NodeSet.extend`](#tree.NodeSet.extend) or
        /// [`Parser.withProps`](#lezer.Parser.withProps) to compute prop
        /// values for each node type in the set. Takes a [match
        /// object](#tree.NodeType^match) or function that returns undefined
        /// if the node type doesn't get this prop, and the prop's value if
        /// it does.
        add(match) {
            if (typeof match != "function")
                match = NodeType.match(match);
            return (type) => {
                let result = match(type);
                return result === undefined ? null : [this, result];
            };
        }
    }
    /// Prop that is used to describe matching delimiters. For opening
    /// delimiters, this holds an array of node names (written as a
    /// space-separated string when declaring this prop in a grammar)
    /// for the node types of closing delimiters that match it.
    NodeProp.closedBy = new NodeProp({ deserialize: str => str.split(" ") });
    /// The inverse of [`openedBy`](#tree.NodeProp^closedBy). This is
    /// attached to closing delimiters, holding an array of node names
    /// of types of matching opening delimiters.
    NodeProp.openedBy = new NodeProp({ deserialize: str => str.split(" ") });
    /// Used to assign node types to groups (for example, all node
    /// types that represent an expression could be tagged with an
    /// `"Expression"` group).
    NodeProp.group = new NodeProp({ deserialize: str => str.split(" ") });
    const noProps = Object.create(null);
    /// Each node in a syntax tree has a node type associated with it.
    class NodeType {
        /// @internal
        constructor(
        /// The name of the node type. Not necessarily unique, but if the
        /// grammar was written properly, different node types with the
        /// same name within a node set should play the same semantic
        /// role.
        name, 
        /// @internal
        props, 
        /// The id of this node in its set. Corresponds to the term ids
        /// used in the parser.
        id, 
        /// @internal
        flags = 0) {
            this.name = name;
            this.props = props;
            this.id = id;
            this.flags = flags;
        }
        static define(spec) {
            let props = spec.props && spec.props.length ? Object.create(null) : noProps;
            let flags = (spec.top ? 1 /* Top */ : 0) | (spec.skipped ? 2 /* Skipped */ : 0) |
                (spec.error ? 4 /* Error */ : 0) | (spec.name == null ? 8 /* Anonymous */ : 0);
            let type = new NodeType(spec.name || "", props, spec.id, flags);
            if (spec.props)
                for (let src of spec.props) {
                    if (!Array.isArray(src))
                        src = src(type);
                    if (src)
                        src[0].set(props, src[1]);
                }
            return type;
        }
        /// Retrieves a node prop for this type. Will return `undefined` if
        /// the prop isn't present on this node.
        prop(prop) { return this.props[prop.id]; }
        /// True when this is the top node of a grammar.
        get isTop() { return (this.flags & 1 /* Top */) > 0; }
        /// True when this node is produced by a skip rule.
        get isSkipped() { return (this.flags & 2 /* Skipped */) > 0; }
        /// Indicates whether this is an error node.
        get isError() { return (this.flags & 4 /* Error */) > 0; }
        /// When true, this node type doesn't correspond to a user-declared
        /// named node, for example because it is used to cache repetition.
        get isAnonymous() { return (this.flags & 8 /* Anonymous */) > 0; }
        /// Returns true when this node's name or one of its
        /// [groups](#tree.NodeProp^group) matches the given string.
        is(name) {
            if (typeof name == 'string') {
                if (this.name == name)
                    return true;
                let group = this.prop(NodeProp.group);
                return group ? group.indexOf(name) > -1 : false;
            }
            return this.id == name;
        }
        /// Create a function from node types to arbitrary values by
        /// specifying an object whose property names are node or
        /// [group](#tree.NodeProp^group) names. Often useful with
        /// [`NodeProp.add`](#tree.NodeProp.add). You can put multiple
        /// names, separated by spaces, in a single property name to map
        /// multiple node names to a single value.
        static match(map) {
            let direct = Object.create(null);
            for (let prop in map)
                for (let name of prop.split(" "))
                    direct[name] = map[prop];
            return (node) => {
                for (let groups = node.prop(NodeProp.group), i = -1; i < (groups ? groups.length : 0); i++) {
                    let found = direct[i < 0 ? node.name : groups[i]];
                    if (found)
                        return found;
                }
            };
        }
    }
    /// An empty dummy node type to use when no actual type is available.
    NodeType.none = new NodeType("", Object.create(null), 0, 8 /* Anonymous */);
    /// A node set holds a collection of node types. It is used to
    /// compactly represent trees by storing their type ids, rather than a
    /// full pointer to the type object, in a number array. Each parser
    /// [has](#lezer.Parser.nodeSet) a node set, and [tree
    /// buffers](#tree.TreeBuffer) can only store collections of nodes
    /// from the same set. A set can have a maximum of 2**16 (65536)
    /// node types in it, so that the ids fit into 16-bit typed array
    /// slots.
    class NodeSet {
        /// Create a set with the given types. The `id` property of each
        /// type should correspond to its position within the array.
        constructor(
        /// The node types in this set, by id.
        types) {
            this.types = types;
            for (let i = 0; i < types.length; i++)
                if (types[i].id != i)
                    throw new RangeError("Node type ids should correspond to array positions when creating a node set");
        }
        /// Create a copy of this set with some node properties added. The
        /// arguments to this method should be created with
        /// [`NodeProp.add`](#tree.NodeProp.add).
        extend(...props) {
            let newTypes = [];
            for (let type of this.types) {
                let newProps = null;
                for (let source of props) {
                    let add = source(type);
                    if (add) {
                        if (!newProps)
                            newProps = Object.assign({}, type.props);
                        add[0].set(newProps, add[1]);
                    }
                }
                newTypes.push(newProps ? new NodeType(type.name, newProps, type.id, type.flags) : type);
            }
            return new NodeSet(newTypes);
        }
    }
    /// A piece of syntax tree. There are two ways to approach these
    /// trees: the way they are actually stored in memory, and the
    /// convenient way.
    ///
    /// Syntax trees are stored as a tree of `Tree` and `TreeBuffer`
    /// objects. By packing detail information into `TreeBuffer` leaf
    /// nodes, the representation is made a lot more memory-efficient.
    ///
    /// However, when you want to actually work with tree nodes, this
    /// representation is very awkward, so most client code will want to
    /// use the `TreeCursor` interface instead, which provides a view on
    /// some part of this data structure, and can be used to move around
    /// to adjacent nodes.
    class Tree {
        /// Construct a new tree. You usually want to go through
        /// [`Tree.build`](#tree.Tree^build) instead.
        constructor(type, 
        /// The tree's child nodes. Children small enough to fit in a
        /// `TreeBuffer will be represented as such, other children can be
        /// further `Tree` instances with their own internal structure.
        children, 
        /// The positions (offsets relative to the start of this tree) of
        /// the children.
        positions, 
        /// The total length of this tree
        length) {
            this.type = type;
            this.children = children;
            this.positions = positions;
            this.length = length;
        }
        /// @internal
        toString() {
            let children = this.children.map(c => c.toString()).join();
            return !this.type.name ? children :
                (/\W/.test(this.type.name) && !this.type.isError ? JSON.stringify(this.type.name) : this.type.name) +
                    (children.length ? "(" + children + ")" : "");
        }
        /// Get a [tree cursor](#tree.TreeCursor) rooted at this tree. When
        /// `pos` is given, the cursor is [moved](#tree.TreeCursor.moveTo)
        /// to the given position and side.
        cursor(pos, side = 0) {
            let scope = (pos != null && CachedNode.get(this)) || this.topNode;
            let cursor = new TreeCursor(scope);
            if (pos != null) {
                cursor.moveTo(pos, side);
                CachedNode.set(this, cursor._tree);
            }
            return cursor;
        }
        /// Get a [tree cursor](#tree.TreeCursor) that, unlike regular
        /// cursors, doesn't skip [anonymous](#tree.NodeType.isAnonymous)
        /// nodes.
        fullCursor() {
            return new TreeCursor(this.topNode, true);
        }
        /// Get a [syntax node](#tree.SyntaxNode) object for the top of the
        /// tree.
        get topNode() {
            return new TreeNode(this, 0, 0, null);
        }
        /// Get the [syntax node](#tree.SyntaxNode) at the given position.
        /// If `side` is -1, this will move into nodes that end at the
        /// position. If 1, it'll move into nodes that start at the
        /// position. With 0, it'll only enter nodes that cover the position
        /// from both sides.
        resolve(pos, side = 0) {
            return this.cursor(pos, side).node;
        }
        /// Iterate over the tree and its children, calling `enter` for any
        /// node that touches the `from`/`to` region (if given) before
        /// running over such a node's children, and `leave` (if given) when
        /// leaving the node. When `enter` returns `false`, the given node
        /// will not have its children iterated over (or `leave` called).
        iterate(spec) {
            let { enter, leave, from = 0, to = this.length } = spec;
            for (let c = this.cursor();;) {
                let mustLeave = false;
                if (c.from <= to && c.to >= from && (c.type.isAnonymous || enter(c.type, c.from, c.to) !== false)) {
                    if (c.firstChild())
                        continue;
                    if (!c.type.isAnonymous)
                        mustLeave = true;
                }
                for (;;) {
                    if (mustLeave && leave)
                        leave(c.type, c.from, c.to);
                    mustLeave = c.type.isAnonymous;
                    if (c.nextSibling())
                        break;
                    if (!c.parent())
                        return;
                    mustLeave = true;
                }
            }
        }
        /// Balance the direct children of this tree.
        balance(maxBufferLength = DefaultBufferLength) {
            return this.children.length <= BalanceBranchFactor ? this
                : balanceRange(this.type, NodeType.none, this.children, this.positions, 0, this.children.length, 0, maxBufferLength, this.length, 0);
        }
        /// Build a tree from a postfix-ordered buffer of node information,
        /// or a cursor over such a buffer.
        static build(data) { return buildTree(data); }
    }
    /// The empty tree
    Tree.empty = new Tree(NodeType.none, [], [], 0);
    // For trees that need a context hash attached, we're using this
    // kludge which assigns an extra property directly after
    // initialization (creating a single new object shape).
    function withHash(tree, hash) {
        if (hash)
            tree.contextHash = hash;
        return tree;
    }
    /// Tree buffers contain (type, start, end, endIndex) quads for each
    /// node. In such a buffer, nodes are stored in prefix order (parents
    /// before children, with the endIndex of the parent indicating which
    /// children belong to it)
    class TreeBuffer {
        /// Create a tree buffer @internal
        constructor(
        /// @internal
        buffer, 
        // The total length of the group of nodes in the buffer.
        length, 
        /// @internal
        set, type = NodeType.none) {
            this.buffer = buffer;
            this.length = length;
            this.set = set;
            this.type = type;
        }
        /// @internal
        toString() {
            let result = [];
            for (let index = 0; index < this.buffer.length;) {
                result.push(this.childString(index));
                index = this.buffer[index + 3];
            }
            return result.join(",");
        }
        /// @internal
        childString(index) {
            let id = this.buffer[index], endIndex = this.buffer[index + 3];
            let type = this.set.types[id], result = type.name;
            if (/\W/.test(result) && !type.isError)
                result = JSON.stringify(result);
            index += 4;
            if (endIndex == index)
                return result;
            let children = [];
            while (index < endIndex) {
                children.push(this.childString(index));
                index = this.buffer[index + 3];
            }
            return result + "(" + children.join(",") + ")";
        }
        /// @internal
        findChild(startIndex, endIndex, dir, after) {
            let { buffer } = this, pick = -1;
            for (let i = startIndex; i != endIndex; i = buffer[i + 3]) {
                if (after != -100000000 /* None */) {
                    let start = buffer[i + 1], end = buffer[i + 2];
                    if (dir > 0) {
                        if (end > after)
                            pick = i;
                        if (end > after)
                            break;
                    }
                    else {
                        if (start < after)
                            pick = i;
                        if (end >= after)
                            break;
                    }
                }
                else {
                    pick = i;
                    if (dir > 0)
                        break;
                }
            }
            return pick;
        }
    }
    class TreeNode {
        constructor(node, from, index, _parent) {
            this.node = node;
            this.from = from;
            this.index = index;
            this._parent = _parent;
        }
        get type() { return this.node.type; }
        get name() { return this.node.type.name; }
        get to() { return this.from + this.node.length; }
        nextChild(i, dir, after, full = false) {
            for (let parent = this;;) {
                for (let { children, positions } = parent.node, e = dir > 0 ? children.length : -1; i != e; i += dir) {
                    let next = children[i], start = positions[i] + parent.from;
                    if (after != -100000000 /* None */ && (dir < 0 ? start >= after : start + next.length <= after))
                        continue;
                    if (next instanceof TreeBuffer) {
                        let index = next.findChild(0, next.buffer.length, dir, after == -100000000 /* None */ ? -100000000 /* None */ : after - start);
                        if (index > -1)
                            return new BufferNode(new BufferContext(parent, next, i, start), null, index);
                    }
                    else if (full || (!next.type.isAnonymous || hasChild(next))) {
                        let inner = new TreeNode(next, start, i, parent);
                        return full || !inner.type.isAnonymous ? inner : inner.nextChild(dir < 0 ? next.children.length - 1 : 0, dir, after);
                    }
                }
                if (full || !parent.type.isAnonymous)
                    return null;
                i = parent.index + dir;
                parent = parent._parent;
                if (!parent)
                    return null;
            }
        }
        get firstChild() { return this.nextChild(0, 1, -100000000 /* None */); }
        get lastChild() { return this.nextChild(this.node.children.length - 1, -1, -100000000 /* None */); }
        childAfter(pos) { return this.nextChild(0, 1, pos); }
        childBefore(pos) { return this.nextChild(this.node.children.length - 1, -1, pos); }
        nextSignificantParent() {
            let val = this;
            while (val.type.isAnonymous && val._parent)
                val = val._parent;
            return val;
        }
        get parent() {
            return this._parent ? this._parent.nextSignificantParent() : null;
        }
        get nextSibling() {
            return this._parent ? this._parent.nextChild(this.index + 1, 1, -1) : null;
        }
        get prevSibling() {
            return this._parent ? this._parent.nextChild(this.index - 1, -1, -1) : null;
        }
        get cursor() { return new TreeCursor(this); }
        resolve(pos, side = 0) {
            return this.cursor.moveTo(pos, side).node;
        }
        getChild(type, before = null, after = null) {
            let r = getChildren(this, type, before, after);
            return r.length ? r[0] : null;
        }
        getChildren(type, before = null, after = null) {
            return getChildren(this, type, before, after);
        }
        /// @internal
        toString() { return this.node.toString(); }
    }
    function getChildren(node, type, before, after) {
        let cur = node.cursor, result = [];
        if (!cur.firstChild())
            return result;
        if (before != null)
            while (!cur.type.is(before))
                if (!cur.nextSibling())
                    return result;
        for (;;) {
            if (after != null && cur.type.is(after))
                return result;
            if (cur.type.is(type))
                result.push(cur.node);
            if (!cur.nextSibling())
                return after == null ? result : [];
        }
    }
    class BufferContext {
        constructor(parent, buffer, index, start) {
            this.parent = parent;
            this.buffer = buffer;
            this.index = index;
            this.start = start;
        }
    }
    class BufferNode {
        constructor(context, _parent, index) {
            this.context = context;
            this._parent = _parent;
            this.index = index;
            this.type = context.buffer.set.types[context.buffer.buffer[index]];
        }
        get name() { return this.type.name; }
        get from() { return this.context.start + this.context.buffer.buffer[this.index + 1]; }
        get to() { return this.context.start + this.context.buffer.buffer[this.index + 2]; }
        child(dir, after) {
            let { buffer } = this.context;
            let index = buffer.findChild(this.index + 4, buffer.buffer[this.index + 3], dir, after == -100000000 /* None */ ? -100000000 /* None */ : after - this.context.start);
            return index < 0 ? null : new BufferNode(this.context, this, index);
        }
        get firstChild() { return this.child(1, -100000000 /* None */); }
        get lastChild() { return this.child(-1, -100000000 /* None */); }
        childAfter(pos) { return this.child(1, pos); }
        childBefore(pos) { return this.child(-1, pos); }
        get parent() {
            return this._parent || this.context.parent.nextSignificantParent();
        }
        externalSibling(dir) {
            return this._parent ? null : this.context.parent.nextChild(this.context.index + dir, dir, -1);
        }
        get nextSibling() {
            let { buffer } = this.context;
            let after = buffer.buffer[this.index + 3];
            if (after < (this._parent ? buffer.buffer[this._parent.index + 3] : buffer.buffer.length))
                return new BufferNode(this.context, this._parent, after);
            return this.externalSibling(1);
        }
        get prevSibling() {
            let { buffer } = this.context;
            let parentStart = this._parent ? this._parent.index + 4 : 0;
            if (this.index == parentStart)
                return this.externalSibling(-1);
            return new BufferNode(this.context, this._parent, buffer.findChild(parentStart, this.index, -1, -100000000 /* None */));
        }
        get cursor() { return new TreeCursor(this); }
        resolve(pos, side = 0) {
            return this.cursor.moveTo(pos, side).node;
        }
        /// @internal
        toString() { return this.context.buffer.childString(this.index); }
        getChild(type, before = null, after = null) {
            let r = getChildren(this, type, before, after);
            return r.length ? r[0] : null;
        }
        getChildren(type, before = null, after = null) {
            return getChildren(this, type, before, after);
        }
    }
    /// A tree cursor object focuses on a given node in a syntax tree, and
    /// allows you to move to adjacent nodes.
    class TreeCursor {
        /// @internal
        constructor(node, full = false) {
            this.full = full;
            this.buffer = null;
            this.stack = [];
            this.index = 0;
            this.bufferNode = null;
            if (node instanceof TreeNode) {
                this.yieldNode(node);
            }
            else {
                this._tree = node.context.parent;
                this.buffer = node.context;
                for (let n = node._parent; n; n = n._parent)
                    this.stack.unshift(n.index);
                this.bufferNode = node;
                this.yieldBuf(node.index);
            }
        }
        /// Shorthand for `.type.name`.
        get name() { return this.type.name; }
        yieldNode(node) {
            if (!node)
                return false;
            this._tree = node;
            this.type = node.type;
            this.from = node.from;
            this.to = node.to;
            return true;
        }
        yieldBuf(index, type) {
            this.index = index;
            let { start, buffer } = this.buffer;
            this.type = type || buffer.set.types[buffer.buffer[index]];
            this.from = start + buffer.buffer[index + 1];
            this.to = start + buffer.buffer[index + 2];
            return true;
        }
        yield(node) {
            if (!node)
                return false;
            if (node instanceof TreeNode) {
                this.buffer = null;
                return this.yieldNode(node);
            }
            this.buffer = node.context;
            return this.yieldBuf(node.index, node.type);
        }
        /// @internal
        toString() {
            return this.buffer ? this.buffer.buffer.childString(this.index) : this._tree.toString();
        }
        /// @internal
        enter(dir, after) {
            if (!this.buffer)
                return this.yield(this._tree.nextChild(dir < 0 ? this._tree.node.children.length - 1 : 0, dir, after, this.full));
            let { buffer } = this.buffer;
            let index = buffer.findChild(this.index + 4, buffer.buffer[this.index + 3], dir, after == -100000000 /* None */ ? -100000000 /* None */ : after - this.buffer.start);
            if (index < 0)
                return false;
            this.stack.push(this.index);
            return this.yieldBuf(index);
        }
        /// Move the cursor to this node's first child. When this returns
        /// false, the node has no child, and the cursor has not been moved.
        firstChild() { return this.enter(1, -100000000 /* None */); }
        /// Move the cursor to this node's last child.
        lastChild() { return this.enter(-1, -100000000 /* None */); }
        /// Move the cursor to the first child that starts at or after `pos`.
        childAfter(pos) { return this.enter(1, pos); }
        /// Move to the last child that ends at or before `pos`.
        childBefore(pos) { return this.enter(-1, pos); }
        /// Move the node's parent node, if this isn't the top node.
        parent() {
            if (!this.buffer)
                return this.yieldNode(this.full ? this._tree._parent : this._tree.parent);
            if (this.stack.length)
                return this.yieldBuf(this.stack.pop());
            let parent = this.full ? this.buffer.parent : this.buffer.parent.nextSignificantParent();
            this.buffer = null;
            return this.yieldNode(parent);
        }
        /// @internal
        sibling(dir) {
            if (!this.buffer)
                return !this._tree._parent ? false
                    : this.yield(this._tree._parent.nextChild(this._tree.index + dir, dir, -100000000 /* None */, this.full));
            let { buffer } = this.buffer, d = this.stack.length - 1;
            if (dir < 0) {
                let parentStart = d < 0 ? 0 : this.stack[d] + 4;
                if (this.index != parentStart)
                    return this.yieldBuf(buffer.findChild(parentStart, this.index, -1, -100000000 /* None */));
            }
            else {
                let after = buffer.buffer[this.index + 3];
                if (after < (d < 0 ? buffer.buffer.length : buffer.buffer[this.stack[d] + 3]))
                    return this.yieldBuf(after);
            }
            return d < 0 ? this.yield(this.buffer.parent.nextChild(this.buffer.index + dir, dir, -100000000 /* None */, this.full)) : false;
        }
        /// Move to this node's next sibling, if any.
        nextSibling() { return this.sibling(1); }
        /// Move to this node's previous sibling, if any.
        prevSibling() { return this.sibling(-1); }
        atLastNode(dir) {
            let index, parent, { buffer } = this;
            if (buffer) {
                if (dir > 0) {
                    if (this.index < buffer.buffer.buffer.length)
                        return false;
                }
                else {
                    for (let i = 0; i < this.index; i++)
                        if (buffer.buffer.buffer[i + 3] < this.index)
                            return false;
                }
                ({ index, parent } = buffer);
            }
            else {
                ({ index, _parent: parent } = this._tree);
            }
            for (; parent; { index, _parent: parent } = parent) {
                for (let i = index + dir, e = dir < 0 ? -1 : parent.node.children.length; i != e; i += dir) {
                    let child = parent.node.children[i];
                    if (this.full || !child.type.isAnonymous || child instanceof TreeBuffer || hasChild(child))
                        return false;
                }
            }
            return true;
        }
        move(dir) {
            if (this.enter(dir, -100000000 /* None */))
                return true;
            for (;;) {
                if (this.sibling(dir))
                    return true;
                if (this.atLastNode(dir) || !this.parent())
                    return false;
            }
        }
        /// Move to the next node in a
        /// [pre-order](https://en.wikipedia.org/wiki/Tree_traversal#Pre-order_(NLR))
        /// traversal, going from a node to its first child or, if the
        /// current node is empty, its next sibling or the next sibling of
        /// the first parent node that has one.
        next() { return this.move(1); }
        /// Move to the next node in a last-to-first pre-order traveral. A
        /// node is followed by ist last child or, if it has none, its
        /// previous sibling or the previous sibling of the first parent
        /// node that has one.
        prev() { return this.move(-1); }
        /// Move the cursor to the innermost node that covers `pos`. If
        /// `side` is -1, it will enter nodes that end at `pos`. If it is 1,
        /// it will enter nodes that start at `pos`.
        moveTo(pos, side = 0) {
            // Move up to a node that actually holds the position, if possible
            while (this.from == this.to ||
                (side < 1 ? this.from >= pos : this.from > pos) ||
                (side > -1 ? this.to <= pos : this.to < pos))
                if (!this.parent())
                    break;
            // Then scan down into child nodes as far as possible
            for (;;) {
                if (side < 0 ? !this.childBefore(pos) : !this.childAfter(pos))
                    break;
                if (this.from == this.to ||
                    (side < 1 ? this.from >= pos : this.from > pos) ||
                    (side > -1 ? this.to <= pos : this.to < pos)) {
                    this.parent();
                    break;
                }
            }
            return this;
        }
        /// Get a [syntax node](#tree.SyntaxNode) at the cursor's current
        /// position.
        get node() {
            if (!this.buffer)
                return this._tree;
            let cache = this.bufferNode, result = null, depth = 0;
            if (cache && cache.context == this.buffer) {
                scan: for (let index = this.index, d = this.stack.length; d >= 0;) {
                    for (let c = cache; c; c = c._parent)
                        if (c.index == index) {
                            if (index == this.index)
                                return c;
                            result = c;
                            depth = d + 1;
                            break scan;
                        }
                    index = this.stack[--d];
                }
            }
            for (let i = depth; i < this.stack.length; i++)
                result = new BufferNode(this.buffer, result, this.stack[i]);
            return this.bufferNode = new BufferNode(this.buffer, result, this.index);
        }
        /// Get the [tree](#tree.Tree) that represents the current node, if
        /// any. Will return null when the node is in a [tree
        /// buffer](#tree.TreeBuffer).
        get tree() {
            return this.buffer ? null : this._tree.node;
        }
    }
    function hasChild(tree) {
        return tree.children.some(ch => !ch.type.isAnonymous || ch instanceof TreeBuffer || hasChild(ch));
    }
    class FlatBufferCursor {
        constructor(buffer, index) {
            this.buffer = buffer;
            this.index = index;
        }
        get id() { return this.buffer[this.index - 4]; }
        get start() { return this.buffer[this.index - 3]; }
        get end() { return this.buffer[this.index - 2]; }
        get size() { return this.buffer[this.index - 1]; }
        get pos() { return this.index; }
        next() { this.index -= 4; }
        fork() { return new FlatBufferCursor(this.buffer, this.index); }
    }
    const BalanceBranchFactor = 8;
    function buildTree(data) {
        var _a;
        let { buffer, nodeSet, topID = 0, maxBufferLength = DefaultBufferLength, reused = [], minRepeatType = nodeSet.types.length } = data;
        let cursor = Array.isArray(buffer) ? new FlatBufferCursor(buffer, buffer.length) : buffer;
        let types = nodeSet.types;
        let contextHash = 0;
        function takeNode(parentStart, minPos, children, positions, inRepeat) {
            let { id, start, end, size } = cursor;
            let startPos = start - parentStart;
            if (size < 0) {
                if (size == -1) { // Reused node
                    children.push(reused[id]);
                    positions.push(startPos);
                }
                else { // Context change
                    contextHash = id;
                }
                cursor.next();
                return;
            }
            let type = types[id], node, buffer;
            if (end - start <= maxBufferLength && (buffer = findBufferSize(cursor.pos - minPos, inRepeat))) {
                // Small enough for a buffer, and no reused nodes inside
                let data = new Uint16Array(buffer.size - buffer.skip);
                let endPos = cursor.pos - buffer.size, index = data.length;
                while (cursor.pos > endPos)
                    index = copyToBuffer(buffer.start, data, index, inRepeat);
                node = new TreeBuffer(data, end - buffer.start, nodeSet, inRepeat < 0 ? NodeType.none : types[inRepeat]);
                startPos = buffer.start - parentStart;
            }
            else { // Make it a node
                let endPos = cursor.pos - size;
                cursor.next();
                let localChildren = [], localPositions = [];
                let localInRepeat = id >= minRepeatType ? id : -1;
                while (cursor.pos > endPos) {
                    if (cursor.id == localInRepeat)
                        cursor.next();
                    else
                        takeNode(start, endPos, localChildren, localPositions, localInRepeat);
                }
                localChildren.reverse();
                localPositions.reverse();
                if (localInRepeat > -1 && localChildren.length > BalanceBranchFactor)
                    node = balanceRange(type, type, localChildren, localPositions, 0, localChildren.length, 0, maxBufferLength, end - start, contextHash);
                else
                    node = withHash(new Tree(type, localChildren, localPositions, end - start), contextHash);
            }
            children.push(node);
            positions.push(startPos);
        }
        function findBufferSize(maxSize, inRepeat) {
            // Scan through the buffer to find previous siblings that fit
            // together in a TreeBuffer, and don't contain any reused nodes
            // (which can't be stored in a buffer).
            // If `inRepeat` is > -1, ignore node boundaries of that type for
            // nesting, but make sure the end falls either at the start
            // (`maxSize`) or before such a node.
            let fork = cursor.fork();
            let size = 0, start = 0, skip = 0, minStart = fork.end - maxBufferLength;
            let result = { size: 0, start: 0, skip: 0 };
            scan: for (let minPos = fork.pos - maxSize; fork.pos > minPos;) {
                // Pretend nested repeat nodes of the same type don't exist
                if (fork.id == inRepeat) {
                    // Except that we store the current state as a valid return
                    // value.
                    result.size = size;
                    result.start = start;
                    result.skip = skip;
                    skip += 4;
                    size += 4;
                    fork.next();
                    continue;
                }
                let nodeSize = fork.size, startPos = fork.pos - nodeSize;
                if (nodeSize < 0 || startPos < minPos || fork.start < minStart)
                    break;
                let localSkipped = fork.id >= minRepeatType ? 4 : 0;
                let nodeStart = fork.start;
                fork.next();
                while (fork.pos > startPos) {
                    if (fork.size < 0)
                        break scan;
                    if (fork.id >= minRepeatType)
                        localSkipped += 4;
                    fork.next();
                }
                start = nodeStart;
                size += nodeSize;
                skip += localSkipped;
            }
            if (inRepeat < 0 || size == maxSize) {
                result.size = size;
                result.start = start;
                result.skip = skip;
            }
            return result.size > 4 ? result : undefined;
        }
        function copyToBuffer(bufferStart, buffer, index, inRepeat) {
            let { id, start, end, size } = cursor;
            cursor.next();
            if (id == inRepeat)
                return index;
            let startIndex = index;
            if (size > 4) {
                let endPos = cursor.pos - (size - 4);
                while (cursor.pos > endPos)
                    index = copyToBuffer(bufferStart, buffer, index, inRepeat);
            }
            if (id < minRepeatType) { // Don't copy repeat nodes into buffers
                buffer[--index] = startIndex;
                buffer[--index] = end - bufferStart;
                buffer[--index] = start - bufferStart;
                buffer[--index] = id;
            }
            return index;
        }
        let children = [], positions = [];
        while (cursor.pos > 0)
            takeNode(data.start || 0, 0, children, positions, -1);
        let length = (_a = data.length) !== null && _a !== void 0 ? _a : (children.length ? positions[0] + children[0].length : 0);
        return new Tree(types[topID], children.reverse(), positions.reverse(), length);
    }
    function balanceRange(outerType, innerType, children, positions, from, to, start, maxBufferLength, length, contextHash) {
        let localChildren = [], localPositions = [];
        if (length <= maxBufferLength) {
            for (let i = from; i < to; i++) {
                localChildren.push(children[i]);
                localPositions.push(positions[i] - start);
            }
        }
        else {
            let maxChild = Math.max(maxBufferLength, Math.ceil(length * 1.5 / BalanceBranchFactor));
            for (let i = from; i < to;) {
                let groupFrom = i, groupStart = positions[i];
                i++;
                for (; i < to; i++) {
                    let nextEnd = positions[i] + children[i].length;
                    if (nextEnd - groupStart > maxChild)
                        break;
                }
                if (i == groupFrom + 1) {
                    let only = children[groupFrom];
                    if (only instanceof Tree && only.type == innerType && only.length > maxChild << 1) { // Too big, collapse
                        for (let j = 0; j < only.children.length; j++) {
                            localChildren.push(only.children[j]);
                            localPositions.push(only.positions[j] + groupStart - start);
                        }
                        continue;
                    }
                    localChildren.push(only);
                }
                else if (i == groupFrom + 1) {
                    localChildren.push(children[groupFrom]);
                }
                else {
                    let inner = balanceRange(innerType, innerType, children, positions, groupFrom, i, groupStart, maxBufferLength, positions[i - 1] + children[i - 1].length - groupStart, contextHash);
                    if (innerType != NodeType.none && !containsType(inner.children, innerType))
                        inner = withHash(new Tree(NodeType.none, inner.children, inner.positions, inner.length), contextHash);
                    localChildren.push(inner);
                }
                localPositions.push(groupStart - start);
            }
        }
        return withHash(new Tree(outerType, localChildren, localPositions, length), contextHash);
    }
    function containsType(nodes, type) {
        for (let elt of nodes)
            if (elt.type == type)
                return true;
        return false;
    }
    /// Tree fragments are used during [incremental
    /// parsing](#lezer.ParseOptions.fragments) to track parts of old
    /// trees that can be reused in a new parse. An array of fragments is
    /// used to track regions of an old tree whose nodes might be reused
    /// in new parses. Use the static
    /// [`applyChanges`](#tree.TreeFragment^applyChanges) method to update
    /// fragments for document changes.
    class TreeFragment {
        constructor(
        /// The start of the unchanged range pointed to by this fragment.
        /// This refers to an offset in the _updated_ document (as opposed
        /// to the original tree).
        from, 
        /// The end of the unchanged range.
        to, 
        /// The tree that this fragment is based on.
        tree, 
        /// The offset between the fragment's tree and the document that
        /// this fragment can be used against. Add this when going from
        /// document to tree positions, subtract it to go from tree to
        /// document positions.
        offset, open) {
            this.from = from;
            this.to = to;
            this.tree = tree;
            this.offset = offset;
            this.open = open;
        }
        get openStart() { return (this.open & 1 /* Start */) > 0; }
        get openEnd() { return (this.open & 2 /* End */) > 0; }
        /// Apply a set of edits to an array of fragments, removing or
        /// splitting fragments as necessary to remove edited ranges, and
        /// adjusting offsets for fragments that moved.
        static applyChanges(fragments, changes, minGap = 128) {
            if (!changes.length)
                return fragments;
            let result = [];
            let fI = 1, nextF = fragments.length ? fragments[0] : null;
            let cI = 0, pos = 0, off = 0;
            for (;;) {
                let nextC = cI < changes.length ? changes[cI++] : null;
                let nextPos = nextC ? nextC.fromA : 1e9;
                if (nextPos - pos >= minGap)
                    while (nextF && nextF.from < nextPos) {
                        let cut = nextF;
                        if (pos >= cut.from || nextPos <= cut.to || off) {
                            let fFrom = Math.max(cut.from, pos) - off, fTo = Math.min(cut.to, nextPos) - off;
                            cut = fFrom >= fTo ? null :
                                new TreeFragment(fFrom, fTo, cut.tree, cut.offset + off, (cI > 0 ? 1 /* Start */ : 0) | (nextC ? 2 /* End */ : 0));
                        }
                        if (cut)
                            result.push(cut);
                        if (nextF.to > nextPos)
                            break;
                        nextF = fI < fragments.length ? fragments[fI++] : null;
                    }
                if (!nextC)
                    break;
                pos = nextC.toA;
                off = nextC.toA - nextC.toB;
            }
            return result;
        }
        /// Create a set of fragments from a freshly parsed tree, or update
        /// an existing set of fragments by replacing the ones that overlap
        /// with a tree with content from the new tree. When `partial` is
        /// true, the parse is treated as incomplete, and the token at its
        /// end is not included in [`safeTo`](#tree.TreeFragment.safeTo).
        static addTree(tree, fragments = [], partial = false) {
            let result = [new TreeFragment(0, tree.length, tree, 0, partial ? 2 /* End */ : 0)];
            for (let f of fragments)
                if (f.to > tree.length)
                    result.push(f);
            return result;
        }
    }
    // Creates an `Input` that is backed by a single, flat string.
    function stringInput(input) { return new StringInput(input); }
    class StringInput {
        constructor(string, length = string.length) {
            this.string = string;
            this.length = length;
        }
        get(pos) {
            return pos < 0 || pos >= this.length ? -1 : this.string.charCodeAt(pos);
        }
        lineAfter(pos) {
            if (pos < 0)
                return "";
            let end = this.string.indexOf("\n", pos);
            return this.string.slice(pos, end < 0 ? this.length : Math.min(end, this.length));
        }
        read(from, to) { return this.string.slice(from, Math.min(this.length, to)); }
        clip(at) { return new StringInput(this.string, at); }
    }

    let nextTagID = 0;
    /// Highlighting tags are markers that denote a highlighting category.
    /// They are [associated](#highlight.styleTags) with parts of a syntax
    /// tree by a language mode, and then mapped to an actual CSS style by
    /// a [highlight style](#highlight.highlightStyle).
    ///
    /// CodeMirror uses a mostly-closed set of tags for generic
    /// highlighters, so that the list of things that a theme must style
    /// is clear and bounded (as opposed to traditional open string-based
    /// systems, which make it hard for highlighting themes to cover all
    /// the tokens produced by the various languages).
    ///
    /// It _is_ possible to [define](#highlight.Tag^define) your own
    /// highlighting tags for system-internal use (where you control both
    /// the language package and the highlighter), but such tags will not
    /// be picked up by other highlighters (though you can derive them
    /// from standard tags to allow the highlighters to fall back to
    /// those).
    class Tag {
        /// @internal
        constructor(
        /// The set of tags that match this tag, starting with this one
        /// itself, sorted in order of decreasing specificity. @internal
        set, 
        /// The base unmodified tag that this one is based on, if it's
        /// modified @internal
        base, 
        /// The modifiers applied to this.base @internal
        modified) {
            this.set = set;
            this.base = base;
            this.modified = modified;
            /// @internal
            this.id = nextTagID++;
        }
        /// Define a new tag. If `parent` is given, the tag is treated as a
        /// sub-tag of that parent, and [highlight
        /// styles](#highlight.highlightStyle) that don't mention this tag
        /// will try to fall back to the parent tag (or grandparent tag,
        /// etc).
        static define(parent) {
            if (parent === null || parent === void 0 ? void 0 : parent.base)
                throw new Error("Can not derive from a modified tag");
            let tag = new Tag([], null, []);
            tag.set.push(tag);
            if (parent)
                for (let t of parent.set)
                    tag.set.push(t);
            return tag;
        }
        /// Define a tag _modifier_, which is a function that, given a tag,
        /// will return a tag that is a subtag of the original. Applying the
        /// same modifier to a twice tag will return the same value (`m1(t1)
        /// == m1(t1)`) and applying multiple modifiers will, regardless or
        /// order, produce the same tag (`m1(m2(t1)) == m2(m1(t1))`).
        ///
        /// When multiple modifiers are applied to a given base tag, each
        /// smaller set of modifiers is registered as a parent, so that for
        /// example `m1(m2(m3(t1)))` is a subtype of `m1(m2(t1))`,
        /// `m1(m3(t1)`, and so on.
        static defineModifier() {
            let mod = new Modifier;
            return (tag) => {
                if (tag.modified.indexOf(mod) > -1)
                    return tag;
                return Modifier.get(tag.base || tag, tag.modified.concat(mod).sort((a, b) => a.id - b.id));
            };
        }
    }
    let nextModifierID = 0;
    class Modifier {
        constructor() {
            this.instances = [];
            this.id = nextModifierID++;
        }
        static get(base, mods) {
            if (!mods.length)
                return base;
            let exists = mods[0].instances.find(t => t.base == base && sameArray$1(mods, t.modified));
            if (exists)
                return exists;
            let set = [], tag = new Tag(set, base, mods);
            for (let m of mods)
                m.instances.push(tag);
            let configs = permute(mods);
            for (let parent of base.set)
                for (let config of configs)
                    set.push(Modifier.get(parent, config));
            return tag;
        }
    }
    function sameArray$1(a, b) {
        return a.length == b.length && a.every((x, i) => x == b[i]);
    }
    function permute(array) {
        let result = [array];
        for (let i = 0; i < array.length; i++) {
            for (let a of permute(array.slice(0, i).concat(array.slice(i + 1))))
                result.push(a);
        }
        return result;
    }
    /// This function is used to add a set of tags to a language syntax
    /// via
    /// [`Parser.configure`](https://lezer.codemirror.net/docs/ref#lezer.Parser.configure).
    ///
    /// The argument object maps node selectors to [highlighting
    /// tags](#highlight.Tag) or arrays of tags.
    ///
    /// Node selectors may hold one or more (space-separated) node paths.
    /// Such a path can be a [node
    /// name](https://lezer.codemirror.net/docs/ref#tree.NodeType.name),
    /// or multiple node names (or `*` wildcards) separated by slash
    /// characters, as in `"Block/Declaration/VariableName"`. Such a path
    /// matches the final node but only if its direct parent nodes are the
    /// other nodes mentioned. A `*` in such a path matches any parent,
    /// but only a single level—wildcards that match multiple parents
    /// aren't supported, both for efficiency reasons and because Lezer
    /// trees make it rather hard to reason about what they would match.)
    ///
    /// A path can be ended with `/...` to indicate that the tag assigned
    /// to the node should also apply to all parent nodes, even if they
    /// match their own style (by default, only the innermost style is
    /// used).
    ///
    /// When a path ends in `!`, as in `Attribute!`, no further matching
    /// happens for the node's child nodes, and the entire node gets the
    /// given style.
    ///
    /// In this notation, node names that contain `/`, `!`, `*`, or `...`
    /// must be quoted as JSON strings.
    ///
    /// For example:
    ///
    /// ```javascript
    /// parser.withProps(
    ///   styleTags({
    ///     // Style Number and BigNumber nodes
    ///     "Number BigNumber": tags.number,
    ///     // Style Escape nodes whose parent is String
    ///     "String/Escape": tags.escape,
    ///     // Style anything inside Attributes nodes
    ///     "Attributes!": tags.meta,
    ///     // Add a style to all content inside Italic nodes
    ///     "Italic/...": tags.emphasis,
    ///     // Style InvalidString nodes as both `string` and `invalid`
    ///     "InvalidString": tags.string + tags.invalid,
    ///     // Style the node named "/" as punctuation
    ///     '"/"': tags.punctuation
    ///   })
    /// )
    /// ```
    function styleTags(spec) {
        let byName = Object.create(null);
        for (let prop in spec) {
            let tags = spec[prop];
            if (!Array.isArray(tags))
                tags = [tags];
            for (let part of prop.split(" "))
                if (part) {
                    let pieces = [], mode = 2 /* Normal */, rest = part;
                    for (let pos = 0;;) {
                        if (rest == "..." && pos > 0 && pos + 3 == part.length) {
                            mode = 1 /* Inherit */;
                            break;
                        }
                        let m = /^"(?:[^"\\]|\\.)*?"|[^\/!]+/.exec(rest);
                        if (!m)
                            throw new RangeError("Invalid path: " + part);
                        pieces.push(m[0] == "*" ? null : m[0][0] == '"' ? JSON.parse(m[0]) : m[0]);
                        pos += m[0].length;
                        if (pos == part.length)
                            break;
                        let next = part[pos++];
                        if (pos == part.length && next == "!") {
                            mode = 0 /* Opaque */;
                            break;
                        }
                        if (next != "/")
                            throw new RangeError("Invalid path: " + part);
                        rest = part.slice(pos);
                    }
                    let last = pieces.length - 1, inner = pieces[last];
                    if (!inner)
                        throw new RangeError("Invalid path: " + part);
                    let rule = new Rule(tags, mode, last > 0 ? pieces.slice(0, last) : null);
                    byName[inner] = rule.sort(byName[inner]);
                }
        }
        return ruleNodeProp.add(byName);
    }
    const ruleNodeProp = new NodeProp();
    const highlightStyleProp = Facet.define({
        combine(stylings) { return stylings.length ? stylings[0] : null; }
    });
    /// Create a highlighter style that associates the given styles to the
    /// given tags. The spec's property names must be
    /// [tags](#highlight.Tag) or lists of tags (which can be concatenated
    /// with `+`). The values should be
    /// [`style-mod`](https://github.com/marijnh/style-mod#documentation)
    /// style objects that define the CSS for that tag.
    ///
    /// The CSS rules created for a highlighter will be emitted in the
    /// order of the spec's properties. That means that for elements that
    /// have multiple tags associated with them, styles defined further
    /// down in the list will have a higher CSS precedence than styles
    /// defined earlier.
    function highlightStyle(...specs) {
        let styling = new Styling(specs);
        return [
            highlightStyleProp.of(styling),
            EditorView.styleModule.of(styling.module)
        ];
    }
    class Rule {
        constructor(tags, mode, context, next) {
            this.tags = tags;
            this.mode = mode;
            this.context = context;
            this.next = next;
        }
        sort(other) {
            if (!other || other.depth < this.depth) {
                this.next = other;
                return this;
            }
            other.next = this.sort(other.next);
            return other;
        }
        get depth() { return this.context ? this.context.length : 0; }
    }
    class Styling {
        constructor(spec) {
            this.map = Object.create(null);
            let modSpec = Object.create(null);
            for (let style of spec) {
                let cls = StyleModule.newName();
                modSpec["." + cls] = Object.assign({}, style, { tag: null });
                let tags = style.tag;
                if (!Array.isArray(tags))
                    tags = [tags];
                for (let tag of tags)
                    this.map[tag.id] = cls;
            }
            this.module = new StyleModule(modSpec);
        }
        match(tag) {
            for (let t of tag.set) {
                let match = this.map[t.id];
                if (match) {
                    if (t != tag)
                        this.map[tag.id] = match;
                    return match;
                }
            }
            return this.map[tag.id] = null;
        }
    }
    /// Returns an extension that installs a highlighter that uses the
    /// tree produced by the given language, along with the current
    /// [highlight style](#highlight.highlightStyle), to style the
    /// document. If no highlight style is active, this plugin won't do
    /// any highlighting.
    function treeHighlighter(language) {
        return precedence(ViewPlugin.define(view => new TreeHighlighter(view, language), {
            decorations: v => v.decorations
        }), "fallback");
    }
    class TreeHighlighter {
        constructor(view, language) {
            this.language = language;
            // Reused stacks for buildDeco
            this.nodeStack = [""];
            this.classStack = [""];
            this.inheritStack = [""];
            this.markCache = Object.create(null);
            this.tree = language.getTree(view.state);
            this.decorations = this.buildDeco(view);
        }
        update(update) {
            if (this.language.getTree(update.state).length < update.view.viewport.to) {
                this.decorations = this.decorations.map(update.changes);
            }
            else {
                let tree = this.language.getTree(update.state);
                if (tree != this.tree || update.viewportChanged) {
                    this.tree = tree;
                    this.decorations = this.buildDeco(update.view);
                }
            }
        }
        buildDeco(view) {
            const style = view.state.facet(highlightStyleProp);
            if (!style)
                return Decoration.none;
            let builder = new RangeSetBuilder();
            let start, curClass, depth;
            let flush = (pos, style) => {
                if (pos > start && style) {
                    let mark = this.markCache[style] || (this.markCache[style] = Decoration.mark({ class: style }));
                    builder.add(start, pos, mark);
                }
                start = pos;
            };
            let { nodeStack, classStack, inheritStack } = this;
            for (let { from, to } of view.visibleRanges) {
                curClass = "";
                depth = 0;
                start = from;
                this.tree.iterate({
                    from, to,
                    enter: (type, start) => {
                        depth++;
                        let inheritedClass = inheritStack[depth - 1];
                        let cls = inheritedClass;
                        let rule = type.prop(ruleNodeProp), opaque = false;
                        while (rule) {
                            if (!rule.context || matchContext(rule.context, nodeStack, depth)) {
                                for (let tag of rule.tags) {
                                    let st = style.match(tag);
                                    if (st) {
                                        if (cls)
                                            cls += " ";
                                        cls += st;
                                        if (rule.mode == 1 /* Inherit */)
                                            inheritedClass = cls;
                                        else if (rule.mode == 0 /* Opaque */)
                                            opaque = true;
                                    }
                                }
                                break;
                            }
                            rule = rule.next;
                        }
                        if (cls != curClass) {
                            flush(start, curClass);
                            curClass = cls;
                        }
                        if (opaque) {
                            depth--;
                            return false;
                        }
                        classStack[depth] = cls;
                        inheritStack[depth] = inheritedClass;
                        nodeStack[depth] = type.name;
                        return undefined;
                    },
                    leave: (_t, _s, end) => {
                        depth--;
                        let backTo = classStack[depth];
                        if (backTo != curClass) {
                            flush(Math.min(to, end), curClass);
                            curClass = backTo;
                        }
                    }
                });
            }
            return builder.finish();
        }
    }
    function matchContext(context, stack, depth) {
        if (context.length > depth - 1)
            return false;
        for (let d = depth - 1, i = context.length - 1; i >= 0; i--, d--) {
            let check = context[i];
            if (check && check != stack[d])
                return false;
        }
        return true;
    }
    const t = Tag.define;
    const comment = t(), name = t(), literal = t(), string = t(literal), number = t(literal), content = t(), heading = t(content), keyword = t(), operator = t(), punctuation = t(), bracket = t(punctuation), meta = t();
    /// The default set of highlighting [tags](#highlight.Tag^define) used
    /// by regular language packages and themes.
    ///
    /// This collection is heavily biasted towards programming language,
    /// and necessarily incomplete. A full ontology of syntactic
    /// constructs would fill a stack of books, and be impractical to
    /// write themes for. So try to make do with this set, possibly
    /// encoding more information with flags. If all else fails, [open an
    /// issue](https://github.com/codemirror/codemirror.next) to propose a
    /// new type, or [define](#highlight.Tag^define) a custom tag for your
    /// use case.
    ///
    /// Note that it is not obligatory to always attach the most specific
    /// tag possible to an element—if your grammar can't easily
    /// distinguish a certain type of element, it is okay to style it as
    /// its more general variant.
    /// 
    /// For tags that extend some parent tag, the documentation links to
    /// the parent.
    const tags = {
        /// A comment.
        comment,
        /// A line [comment](#highlight.tags.comment).
        lineComment: t(comment),
        /// A block [comment](#highlight.tags.comment).
        blockComment: t(comment),
        /// A documentation [comment](#highlight.tags.comment).
        docComment: t(comment),
        /// Any kind of identifier.
        name,
        /// The [name](#highlight.tags.name) of a variable.
        variableName: t(name),
        /// A type or tag [name](#highlight.tags.name).
        typeName: t(name),
        /// A property, field, or attribute [name](#highlight.tags.name).
        propertyName: t(name),
        /// The [name](#highlight.tags.name) of a class.
        className: t(name),
        /// A label [name](#highlight.tags.name).
        labelName: t(name),
        /// A namespace [name](#highlight.tags.name).
        namespace: t(name),
        /// The [name](#highlight.tags.name) of a macro.
        macroName: t(name),
        /// A literal value.
        literal,
        /// A string [literal](#highlight.tags.literal).
        string,
        /// A documentation [string](#highlight.tags.string).
        docString: t(string),
        /// A character literal (subtag of [string](#highlight.tags.string)).
        character: t(string),
        /// A number [literal](#highlight.tags.literal).
        number,
        /// An integer [number](#highlight.tags.number) literal.
        integer: t(number),
        /// A floating-point [number](#highlight.tags.number) literal.
        float: t(number),
        /// A boolean [literal](#highlight.tags.literal).
        bool: t(literal),
        /// Regular expression [literal](#highlight.tags.literal).
        regexp: t(literal),
        /// An escape [literal](#highlight.tags.literal), for example a
        /// backslash escape in a string.
        escape: t(literal),
        /// A color [literal](#highlight.tags.literal).
        color: t(literal),
        /// A URL [literal](#highlight.tags.literal).
        url: t(literal),
        /// A language keyword.
        keyword,
        /// The [keyword](#highlight.tags.keyword) for the self or this
        /// object.
        self: t(keyword),
        /// The [keyword](#highlight.tags.keyword) for null.
        null: t(keyword),
        /// A [keyword](#highlight.tags.keyword) denoting some atomic value.
        atom: t(keyword),
        /// A [keyword](#highlight.tags.keyword) that represents a unit.
        unit: t(keyword),
        /// A modifier [keyword](#highlight.tags.keyword).
        modifier: t(keyword),
        /// A [keyword](#highlight.tags.keyword) that acts as an operator.
        operatorKeyword: t(keyword),
        /// A control-flow related [keyword](#highlight.tags.keyword).
        controlKeyword: t(keyword),
        /// A [keyword](#highlight.tags.keyword) that defines something.
        definitionKeyword: t(keyword),
        /// An operator.
        operator,
        /// An [operator](#highlight.tags.operator) that defines something.
        derefOperator: t(operator),
        /// Arithmetic-related [operator](#highlight.tags.operator).
        arithmeticOperator: t(operator),
        /// Logical [operator](#highlight.tags.operator).
        logicOperator: t(operator),
        /// Bit [operator](#highlight.tags.operator).
        bitwiseOperator: t(operator),
        /// Comparison [operator](#highlight.tags.operator).
        compareOperator: t(operator),
        /// [Operator](#highlight.tags.operator) that updates its operand.
        updateOperator: t(operator),
        /// [Operator](#highlight.tags.operator) that defines something.
        definitionOperator: t(operator),
        /// Type-related [operator](#highlight.tags.operator).
        typeOperator: t(operator),
        /// Control-flow [operator](#highlight.tags.operator).
        controlOperator: t(operator),
        /// Program or markup punctuation.
        punctuation,
        /// [Punctuation](#highlight.tags.punctuation) that separates
        /// things.
        separator: t(punctuation),
        /// Bracket-style [punctuation](#highlight.tags.punctuation).
        bracket,
        /// Angle [brackets](#highlight.tags.bracket) (usually `<` and `>`
        /// tokens).
        angleBracket: t(bracket),
        /// Square [brackets](#highlight.tags.bracket) (usually `[` and `]`
        /// tokens).
        squareBracket: t(bracket),
        /// Parentheses (usually `(` and `)` tokens). Subtag of
        /// [bracket](#highlight.tags.bracket)).
        paren: t(bracket),
        /// Braces (usually `{` and `}` tokens). Subtag of
        /// [bracket](#highlight.tags.bracket)).
        brace: t(bracket),
        /// Content, for example plain text in XML or markup documents.
        content,
        /// [Content](#highlight.tags.content) that represents a heading.
        heading,
        /// A level 1 [heading](#highlight.tags.heading).
        heading1: t(heading),
        /// A level 2 [heading](#highlight.tags.heading).
        heading2: t(heading),
        /// A level 3 [heading](#highlight.tags.heading).
        heading3: t(heading),
        /// A level 4 [heading](#highlight.tags.heading).
        heading4: t(heading),
        /// A level 5 [heading](#highlight.tags.heading).
        heading5: t(heading),
        /// A level 6 [heading](#highlight.tags.heading).
        heading6: t(heading),
        /// [Content](#highlight.tags.content) that represents a list or
        /// list marker.
        list: t(content),
        /// [Content](#highlight.tags.content) that represents a quote.
        quote: t(content),
        /// [Content](#highlight.tags.content) that is emphasized.
        emphasis: t(content),
        /// [Content](#highlight.tags.content) that is styled strong.
        strong: t(content),
        /// [Content](#highlight.tags.content) that is part of a link.
        link: t(content),
        /// [Content](#highlight.tags.content) that is styled as code or
        /// monospace.
        monospace: t(content),
        /// Inserted content in a change-tracking format.
        inserted: t(),
        /// Deleted content.
        deleted: t(),
        /// Changed content.
        changed: t(),
        /// An invalid or unsyntactic element.
        invalid: t(),
        /// Metadata or meta-instruction.
        meta,
        /// [Metadata](#highlight.tags.meta) that applies to the entire
        /// document.
        documentMeta: t(meta),
        /// [Metadata](#highlight.tags.meta) that annotates or adds
        /// attributes to a given syntactic element.
        annotation: t(meta),
        /// Processing instruction or preprocessor directive. Subtag of
        /// [meta](#highlight.tags.meta)).
        processingInstruction: t(meta),
        /// [Modifier](#highlight.Tag^defineModifier) that indicates that a
        /// given element is being defined. Expected to be used with the
        /// various [name](#highlight.tags.name) tags.
        definition: Tag.defineModifier(),
        /// [Modifier](#highlight.Tag^defineModifier) that indicates that
        /// something is constant. Mostly expected to be used with
        /// [variable names](#highlight.tags.variableName).
        constant: Tag.defineModifier(),
        /// [Modifier](#highlight.Tag^defineModifier) used to indicate that a
        /// [variable name](#highlight.tags.variableName) is being called or
        /// being defined as a function.
        function: Tag.defineModifier(),
        /// [Modifier](#highlight.Tag^defineModifier) that can be applied to
        /// [names](#highlight.tags.name) to indicate that they belong to
        /// the standard environment.
        standard: Tag.defineModifier(),
        /// [Modifier](#highlight.Tag^defineModifier) that indicates a given
        /// [names](#highlight.tags.name) is local to some scope.
        local: Tag.defineModifier(),
        /// A generic variant [modifier](#highlight.Tag^defineModifier) that
        /// can be used to tag language-specific alternative variants of
        /// some common tag. It is recommended for themes to define special
        /// forms of at least the [string](#highlight.tags.string) and
        /// [variable name](#highlight.tags.variableName) tags, since those
        /// come up a lot.
        special: Tag.defineModifier()
    };
    /// A default highlight style (works well with light themes).
    const defaultHighlightStyle = precedence(highlightStyle({ tag: tags.deleted,
        textDecoration: "line-through" }, { tag: [tags.inserted, tags.link],
        textDecoration: "underline" }, { tag: tags.heading,
        textDecoration: "underline",
        fontWeight: "bold" }, { tag: tags.emphasis,
        fontStyle: "italic" }, { tag: tags.strong,
        fontWeight: "bold" }, { tag: tags.keyword,
        color: "#708" }, { tag: [tags.atom, tags.bool, tags.url],
        color: "#219" }, { tag: tags.number,
        color: "#164" }, { tag: tags.string,
        color: "#a11" }, { tag: [tags.regexp, tags.escape, tags.special(tags.string)],
        color: "#e40" }, { tag: tags.definition(tags.variableName),
        color: "#00f" }, { tag: tags.typeName,
        color: "#085" }, { tag: tags.className,
        color: "#167" }, { tag: tags.special(tags.variableName),
        color: "#256" }, { tag: tags.definition(tags.propertyName),
        color: "#00c" }, { tag: tags.comment,
        color: "#940" }, { tag: tags.meta,
        color: "#555" }, { tag: tags.invalid,
        color: "#f00" }), "fallback");

    /// The facet used to associate a language with an editor state.
    const language = Facet.define();
    /// Node prop stored on a grammar's top node to indicate the facet used
    /// to store language data related to that language.
    const languageDataProp = new NodeProp();
    /// Helper function to define a facet (to be added to the top syntax
    /// node(s) for a language via
    /// [`languageDataProp`](#language.languageDataProp)), that will be
    /// used to associate language data with the language. You
    /// probably only need this when subclassing
    /// [`Language`](#language.Language).
    function defineLanguageFacet(baseData) {
        return Facet.define({
            combine: baseData ? values => values.concat(baseData) : undefined
        });
    }
    /// A language object manages parsing and per-language
    /// [metadata](#state.EditorState.languageDataAt). Parse data is
    /// managed as a [Lezer](https://lezer.codemirror.net) tree. You'll
    /// want to subclass this class for custom parsers, or use the
    /// [`LezerLanguage`](#language.LezerLanguage) or
    /// [`StreamLanguage`](#stream-parser.StreamLanguage) abstractions for
    /// [Lezer](https://lezer.codemirror.net/) or stream parsers.
    class Language {
        constructor(
        /// The [language data](#state.EditorState.languageDataAt) data
        /// facet used for this language.
        data, parser, extraExtensions = []) {
            this.data = data;
            // Kludge to define EditorState.tree as a debugging helper,
            // without the EditorState package actually knowing about
            // languages and lezer trees.
            if (!EditorState.prototype.hasOwnProperty("tree"))
                Object.defineProperty(EditorState.prototype, "tree", { get() { return syntaxTree(this); } });
            let setState = StateEffect.define();
            this.parser = parser;
            this.field = StateField.define({
                create(state) {
                    let parseState = new EditorParseContext(parser, state, [], Tree.empty, { from: 0, to: state.doc.length }, []);
                    if (!parseState.work(25 /* Apply */))
                        parseState.takeTree();
                    return new LanguageState(parseState);
                },
                update(value, tr) {
                    for (let e of tr.effects)
                        if (e.is(setState))
                            return e.value;
                    return value.apply(tr);
                }
            });
            this.extension = [
                language.of(this),
                this.field,
                ViewPlugin.define(view => new ParseWorker(view, this.field, setState)),
                treeHighlighter(this),
                EditorState.languageData.of((state, pos) => state.facet(this.languageDataFacetAt(state, pos)))
            ].concat(extraExtensions);
        }
        /// Retrieve the parser tree for a given state.
        getTree(state) {
            return state.field(this.field).tree;
        }
        /// Try to get a parse tree that spans at least up to `upto`. The
        /// method will do at most `timeout` milliseconds of work to parse
        /// up to that point if the tree isn't already available.
        ensureTree(state, upto, timeout = 100) {
            let parse = state.field(this.field).context;
            return parse.tree.length >= upto || parse.work(timeout, upto) ? parse.tree : null;
        }
        /// @internal
        languageDataFacetAt(state, pos) {
            let tree = this.getTree(state);
            let target = tree.resolve(pos, -1);
            while (target) {
                let facet = target.type.prop(languageDataProp);
                if (facet)
                    return facet;
                target = target.parent;
            }
            return this.data;
        }
    }
    /// A subclass of `Language` for use with
    /// [Lezer](https://lezer.codemirror.net/docs/ref#lezer.Parser)
    /// parsers.
    class LezerLanguage extends Language {
        constructor(data, parser) {
            super(data, parser);
            this.parser = parser;
        }
        /// Define a language from a parser.
        static define(spec) {
            let data = defineLanguageFacet(spec.languageData);
            return new LezerLanguage(data, spec.parser.configure({
                props: [languageDataProp.add(type => type.isTop ? data : undefined)]
            }));
        }
        /// Create a new instance of this language with a reconfigured
        /// version of its parser.
        configure(options) {
            return new LezerLanguage(this.data, this.parser.configure(options));
        }
        languageDataFacetAt(state, pos) {
            return this.parser.hasNested ? super.languageDataFacetAt(state, pos) : this.data;
        }
    }
    /// Get the syntax tree for a state, which is the current (possibly
    /// incomplete) parse tree of the [language](#language.Language) with
    /// the highest precedence, or the empty tree if there is no language
    /// available.
    function syntaxTree(state) {
        let lang = state.facet(language);
        return lang.length ? lang[0].getTree(state) : Tree.empty;
    }
    // Lezer-style Input object for a Text document.
    class DocInput {
        constructor(doc, length = doc.length) {
            this.doc = doc;
            this.length = length;
            this.cursorPos = 0;
            this.string = "";
            this.prevString = "";
            this.cursor = doc.iter();
        }
        syncTo(pos) {
            if (pos < this.cursorPos) { // Reset the cursor if we have to go back
                this.cursor = this.doc.iter();
                this.cursorPos = 0;
            }
            this.prevString = pos == this.cursorPos ? this.string : "";
            this.string = this.cursor.next(pos - this.cursorPos).value;
            this.cursorPos = pos + this.string.length;
            return this.cursorPos - this.string.length;
        }
        get(pos) {
            if (pos >= this.length)
                return -1;
            let stringStart = this.cursorPos - this.string.length;
            if (pos < stringStart || pos >= this.cursorPos) {
                if (pos < stringStart && pos >= stringStart - this.prevString.length)
                    return this.prevString.charCodeAt(pos - (stringStart - this.prevString.length));
                stringStart = this.syncTo(pos);
            }
            return this.string.charCodeAt(pos - stringStart);
        }
        lineAfter(pos) {
            if (pos >= this.length || pos < 0)
                return "";
            let stringStart = this.cursorPos - this.string.length;
            if (pos < stringStart || pos >= this.cursorPos)
                stringStart = this.syncTo(pos);
            let off = pos - stringStart, result = "";
            while (!this.cursor.lineBreak) {
                result += off ? this.string.slice(off) : this.string;
                if (this.cursorPos >= this.length) {
                    if (this.cursorPos > this.length)
                        result = result.slice(0, result.length - (this.cursorPos - this.length));
                    break;
                }
                this.syncTo(this.cursorPos);
                off = 0;
            }
            return result;
        }
        read(from, to) {
            let stringStart = this.cursorPos - this.string.length;
            if (from < stringStart || to >= this.cursorPos)
                return this.doc.sliceString(from, to);
            else
                return this.string.slice(from - stringStart, to - stringStart);
        }
        clip(at) {
            return new DocInput(this.doc, at);
        }
    }
    /// A parse context provided to parsers working on the editor content.
    class EditorParseContext {
        /// @internal
        constructor(parser, 
        /// The current editor state.
        state, 
        /// Tree fragments that can be reused by new parses.
        fragments = [], 
        /// @internal
        tree, 
        /// The current editor viewport, or some approximation thereof.
        /// Intended to be used for opportunistically avoiding work (in
        /// which case
        /// [`skipUntilInView`](#language.EditorParseContext.skipUntilInView)
        /// should be called to make sure the parser is restarted when the
        /// skipped region becomes visible).
        viewport, 
        /// @internal
        skipped) {
            this.parser = parser;
            this.state = state;
            this.fragments = fragments;
            this.tree = tree;
            this.viewport = viewport;
            this.skipped = skipped;
            this.parse = null;
            /// @internal
            this.skippedUntil = [];
        }
        /// @internal
        // FIXME do something with badness again
        work(time, upto) {
            if (this.tree != Tree.empty && (upto == null ? this.tree.length == this.state.doc.length : this.tree.length >= upto))
                return true;
            if (!this.parse)
                this.parse = this.parser.startParse(new DocInput(this.state.doc), 0, this);
            let endTime = Date.now() + time;
            for (;;) {
                let done = this.parse.advance();
                if (done) {
                    this.fragments = TreeFragment.addTree(done);
                    this.parse = null;
                    this.tree = done;
                    return true;
                }
                else if (upto != null && this.parse.pos >= upto) {
                    this.takeTree();
                    return true;
                }
                if (Date.now() > endTime)
                    return false;
            }
        }
        /// @internal
        takeTree() {
            if (this.parse && this.parse.pos > this.tree.length) {
                this.tree = this.parse.forceFinish();
                this.fragments = TreeFragment.addTree(this.tree, this.fragments, true);
            }
        }
        /// @internal
        changes(changes, newState) {
            let { fragments, tree, viewport, skipped } = this;
            this.takeTree();
            if (!changes.empty) {
                let ranges = [];
                changes.iterChangedRanges((fromA, toA, fromB, toB) => ranges.push({ fromA, toA, fromB, toB }));
                fragments = TreeFragment.applyChanges(fragments, ranges);
                tree = Tree.empty;
                viewport = { from: changes.mapPos(viewport.from, -1), to: changes.mapPos(viewport.to, 1) };
                if (this.skipped.length) {
                    skipped = [];
                    for (let r of this.skipped) {
                        let from = changes.mapPos(r.from, 1), to = changes.mapPos(r.to, -1);
                        if (from < to)
                            skipped.push({ from, to });
                    }
                }
            }
            return new EditorParseContext(this.parser, newState, fragments, tree, viewport, skipped);
        }
        /// @internal
        updateViewport(viewport) {
            this.viewport = viewport;
            let startLen = this.skipped.length;
            for (let i = 0; i < this.skipped.length; i++) {
                let { from, to } = this.skipped[i];
                if (from < viewport.to && to > viewport.from) {
                    this.cutFragments(from, to);
                    this.skipped.splice(i--, 1);
                }
            }
            return this.skipped.length < startLen;
        }
        /// @internal
        cutFragments(from, to) {
            this.fragments = TreeFragment.applyChanges(this.fragments, [{ fromA: from, toA: to, fromB: from, toB: to }]);
        }
        /// @internal
        reset() {
            if (this.parse) {
                this.takeTree();
                this.parse = null;
            }
        }
        skipUntilInView(from, to) {
            this.skipped.push({ from, to });
        }
        skipUntil(from, to, until) {
            this.skippedUntil.push({ from, to, until });
        }
    }
    class LanguageState {
        constructor(
        // A mutable parse state that is used to preserve work done during
        // the lifetime of a state when moving to the next state.
        context) {
            this.context = context;
            this.tree = context.tree;
        }
        apply(tr) {
            if (!tr.docChanged)
                return this;
            let newState = this.context.changes(tr.changes, tr.state);
            newState.work(25 /* Apply */);
            return new LanguageState(newState);
        }
    }
    let requestIdle = typeof window != "undefined" && window.requestIdleCallback ||
        ((callback, { timeout }) => setTimeout(callback, timeout));
    let cancelIdle = typeof window != "undefined" && window.cancelIdleCallback || clearTimeout;
    // FIXME figure out some way to back off from full re-parses when the
    // document is large—you could waste a lot of battery re-parsing a
    // multi-megabyte document every time you insert a backtick, even if
    // it happens in the background.
    class ParseWorker {
        constructor(view, field, setState) {
            this.view = view;
            this.field = field;
            this.setState = setState;
            this.working = -1;
            this.work = this.work.bind(this);
            this.scheduleWork();
        }
        update(update) {
            if (update.docChanged)
                this.scheduleWork();
            let cx = this.view.state.field(this.field).context;
            if (update.viewportChanged && cx.updateViewport(update.view.viewport)) {
                cx.reset();
                this.scheduleWork();
            }
            this.takeSkipped(cx);
        }
        scheduleWork() {
            if (this.working > -1)
                return;
            let { state } = this.view, field = state.field(this.field);
            if (field.tree.length >= state.doc.length)
                return;
            this.working = requestIdle(this.work, { timeout: 200 /* Pause */ });
        }
        work(deadline) {
            this.working = -1;
            let { state } = this.view, field = state.field(this.field);
            if (field.tree.length >= state.doc.length)
                return;
            field.context.work(deadline ? Math.max(25 /* MinSlice */, deadline.timeRemaining()) : 100 /* Slice */);
            if (field.context.tree.length >= state.doc.length) {
                this.view.dispatch({ effects: this.setState.of(new LanguageState(field.context)) });
                this.takeSkipped(field.context);
            }
            else {
                this.scheduleWork();
            }
        }
        takeSkipped(context) {
            while (context.skippedUntil.length) {
                let { from, to, until } = context.skippedUntil.pop();
                until.then(() => {
                    let field = this.view.state.field(this.field, false);
                    if (field && field.context == context) {
                        context.cutFragments(from, to);
                        context.reset();
                        this.scheduleWork();
                    }
                });
            }
        }
        destroy() {
            if (this.working >= 0)
                cancelIdle(this.working);
        }
    }

    /// Facet that defines a way to provide a function that computes the
    /// appropriate indentation depth at the start of a given line, or
    /// `null` to indicate no appropriate indentation could be determined.
    const indentService = Facet.define();
    /// Facet for overriding the unit by which indentation happens.
    /// Should be a string consisting either entirely of spaces or
    /// entirely of tabs. When not set, this defaults to 2 spaces.
    const indentUnit = Facet.define({
        combine: values => {
            if (!values.length)
                return "  ";
            if (!/^(?: +|\t+)$/.test(values[0]))
                throw new Error("Invalid indent unit: " + JSON.stringify(values[0]));
            return values[0];
        }
    });
    /// Return the _column width_ of an indent unit in the state.
    /// Determined by the [`indentUnit`](#language.indentUnit)
    /// facet, and [`tabSize`](#state.EditorState^tabSize) when that
    /// contains tabs.
    function getIndentUnit(state) {
        let unit = state.facet(indentUnit);
        return unit.charCodeAt(0) == 9 ? state.tabSize * unit.length : unit.length;
    }
    /// Create an indentation string that covers columns 0 to `cols`.
    /// Will use tabs for as much of the columns as possible when the
    /// [`indentUnit`](#language.indentUnit) facet contains
    /// tabs.
    function indentString(state, cols) {
        let result = "", ts = state.tabSize;
        if (state.facet(indentUnit).charCodeAt(0) == 9)
            while (cols >= ts) {
                result += "\t";
                cols -= ts;
            }
        for (let i = 0; i < cols; i++)
            result += " ";
        return result;
    }
    /// Get the indentation at the given position. Will first consult any
    /// [indent services](#language.indentService) that are registered,
    /// and if none of those return an indentation, this will check the
    /// syntax tree for the [indent node prop](#language.indentNodeProp)
    /// and use that if found. Returns a number when an indentation could
    /// be determined, and null otherwise.
    function getIndentation(context, pos) {
        if (context instanceof EditorState)
            context = new IndentContext(context);
        for (let service of context.state.facet(indentService)) {
            let result = service(context, pos);
            if (result != null)
                return result;
        }
        let tree = syntaxTree(context.state);
        return tree ? syntaxIndentation(context, tree, pos) : null;
    }
    /// Indentation contexts are used when calling [indentation
    /// services](#language.indentService). They provide helper utilities
    /// useful in indentation logic, and can selectively override the
    /// indentation reported for some lines.
    class IndentContext {
        /// Create an indent context.
        constructor(
        /// The editor state.
        state, 
        /// @internal
        options = {}) {
            this.state = state;
            this.options = options;
            this.unit = getIndentUnit(state);
        }
        /// Get the text directly after `pos`, either the entire line
        /// or the next 100 characters, whichever is shorter.
        textAfterPos(pos) {
            var _a, _b;
            let sim = (_a = this.options) === null || _a === void 0 ? void 0 : _a.simulateBreak;
            if (pos == sim && ((_b = this.options) === null || _b === void 0 ? void 0 : _b.simulateDoubleBreak))
                return "";
            return this.state.sliceDoc(pos, Math.min(pos + 100, sim != null && sim > pos ? sim : 1e9, this.state.doc.lineAt(pos).to));
        }
        /// find the column position (taking tabs into account) of the given
        /// position in the given string.
        countColumn(line, pos) {
            return countColumn(pos < 0 ? line : line.slice(0, pos), 0, this.state.tabSize);
        }
        /// Find the indentation column of the given document line.
        lineIndent(line) {
            var _a;
            let override = (_a = this.options) === null || _a === void 0 ? void 0 : _a.overrideIndentation;
            if (override) {
                let overriden = override(line.from);
                if (overriden > -1)
                    return overriden;
            }
            let text = line.slice(0, Math.min(100, line.length));
            return this.countColumn(text, text.search(/\S/));
        }
        /// Find the column for the given position.
        column(pos) {
            var _a;
            let line = this.state.doc.lineAt(pos), text = line.slice(0, pos - line.from);
            let result = this.countColumn(text, pos - line.from);
            let override = ((_a = this.options) === null || _a === void 0 ? void 0 : _a.overrideIndentation) ? this.options.overrideIndentation(line.from) : -1;
            if (override > -1)
                result += override - this.countColumn(text, text.search(/\S/));
            return result;
        }
    }
    /// A syntax tree node prop used to associate indentation strategies
    /// with node types. Such a strategy is a function from an indentation
    /// context to a column number or null, where null indicates that no
    /// definitive indentation can be determined.
    const indentNodeProp = new NodeProp();
    // Compute the indentation for a given position from the syntax tree.
    function syntaxIndentation(cx, ast, pos) {
        let tree = ast.resolve(pos);
        // Enter previous nodes that end in empty error terms, which means
        // they were broken off by error recovery, so that indentation
        // works even if the constructs haven't been finished.
        for (let scan = tree, scanPos = pos;;) {
            let last = scan.childBefore(scanPos);
            if (!last)
                break;
            if (last.type.isError && last.from == last.to) {
                tree = scan;
                scanPos = last.from;
            }
            else {
                scan = last;
                scanPos = scan.to + 1;
            }
        }
        for (; tree; tree = tree.parent) {
            let strategy = indentStrategy(tree);
            if (strategy)
                return strategy(new TreeIndentContext(cx, pos, tree));
        }
        return null;
    }
    function ignoreClosed(cx) {
        var _a, _b;
        return cx.pos == ((_a = cx.options) === null || _a === void 0 ? void 0 : _a.simulateBreak) && ((_b = cx.options) === null || _b === void 0 ? void 0 : _b.simulateDoubleBreak);
    }
    function indentStrategy(tree) {
        let strategy = tree.type.prop(indentNodeProp);
        if (strategy)
            return strategy;
        let first = tree.firstChild, close;
        if (first && (close = first.type.prop(NodeProp.closedBy))) {
            let last = tree.lastChild, closed = last && close.indexOf(last.name) > -1;
            return cx => delimitedStrategy(cx, true, 1, undefined, closed && !ignoreClosed(cx) ? last.from : undefined);
        }
        return tree.parent == null ? topIndent : null;
    }
    function topIndent() { return 0; }
    /// Objects of this type provide context information and helper
    /// methods to indentation functions.
    class TreeIndentContext extends IndentContext {
        /// @internal
        constructor(base, 
        /// The position at which indentation is being computed.
        pos, 
        /// The syntax tree node for which the indentation strategy is
        /// registered.
        node) {
            super(base.state, base.options);
            this.pos = pos;
            this.node = node;
        }
        /// Get the text directly after `this.pos`, either the entire line
        /// or the next 100 characters, whichever is shorter.
        get textAfter() {
            return this.textAfterPos(this.pos);
        }
        /// Get the indentation at the reference line for `this.node`, which
        /// is the line on which it starts, unless there is a node that is
        /// _not_ a parent of this node covering the start of that line. If
        /// so, the line at the start of that node is tried, again skipping
        /// on if it is covered by another such node.
        get baseIndent() {
            let line = this.state.doc.lineAt(this.node.from);
            // Skip line starts that are covered by a sibling (or cousin, etc)
            for (;;) {
                let atBreak = this.node.resolve(line.from);
                while (atBreak.parent && atBreak.parent.from == atBreak.from)
                    atBreak = atBreak.parent;
                if (isParent(atBreak, this.node))
                    break;
                line = this.state.doc.lineAt(atBreak.from);
            }
            return this.lineIndent(line);
        }
    }
    function isParent(parent, of) {
        for (let cur = of; cur; cur = cur.parent)
            if (parent == cur)
                return true;
        return false;
    }
    // Check whether a delimited node is aligned (meaning there are
    // non-skipped nodes on the same line as the opening delimiter). And
    // if so, return the opening token.
    function bracketedAligned(context) {
        var _a;
        let tree = context.node;
        let openToken = tree.childAfter(tree.from), last = tree.lastChild;
        if (!openToken)
            return null;
        let sim = (_a = context.options) === null || _a === void 0 ? void 0 : _a.simulateBreak;
        let openLine = context.state.doc.lineAt(openToken.from);
        let lineEnd = sim == null || sim <= openLine.from ? openLine.to : Math.min(openLine.to, sim);
        for (let pos = openToken.to;;) {
            let next = tree.childAfter(pos);
            if (!next || next == last)
                return null;
            if (!next.type.isSkipped)
                return next.from < lineEnd ? openToken : null;
            pos = next.to;
        }
    }
    function delimitedStrategy(context, align, units, closing, closedAt) {
        let after = context.textAfter, space = after.match(/^\s*/)[0].length;
        let closed = closing && after.slice(space, space + closing.length) == closing || closedAt == context.pos + space;
        let aligned = align ? bracketedAligned(context) : null;
        if (aligned)
            return closed ? context.column(aligned.from) : context.column(aligned.to);
        return context.baseIndent + (closed ? 0 : context.unit * units);
    }
    const DontIndentBeyond = 200;
    /// Enables reindentation on input. When a language defines an
    /// `indentOnInput` field in its [language
    /// data](#state.EditorState.languageDataAt), which must hold a regular
    /// expression, the line at the cursor will be reindented whenever new
    /// text is typed and the input from the start of the line up to the
    /// cursor matches that regexp.
    ///
    /// To avoid unneccesary reindents, it is recommended to start the
    /// regexp with `^` (usually followed by `\s*`), and end it with `$`.
    /// For example, `/^\s*\}$` will reindent when a closing brace is
    /// added at the start of a line.
    function indentOnInput() {
        return EditorState.transactionFilter.of(tr => {
            if (!tr.docChanged || tr.annotation(Transaction.userEvent) != "input")
                return tr;
            let rules = tr.startState.languageDataAt("indentOnInput", tr.startState.selection.primary.head);
            if (!rules.length)
                return tr;
            let doc = tr.newDoc, { head } = tr.newSelection.primary, line = doc.lineAt(head);
            if (head > line.from + DontIndentBeyond)
                return tr;
            let lineStart = doc.sliceString(line.from, head);
            if (!rules.some(r => r.test(lineStart)))
                return tr;
            let { state } = tr, last = -1, changes = [];
            for (let { head } of state.selection.ranges) {
                let line = state.doc.lineAt(head);
                if (line.from == last)
                    continue;
                last = line.from;
                let indent = getIndentation(state, line.from);
                if (indent == null)
                    continue;
                let cur = /^\s*/.exec(line.slice(0, Math.min(line.length, DontIndentBeyond)))[0];
                let norm = indentString(state, indent);
                if (cur != norm)
                    changes.push({ from: line.from, to: line.from + cur.length, insert: norm });
            }
            return changes.length ? [tr, { changes }] : tr;
        });
    }

    /// A facet that registers a code folding service. When called with
    /// the extent of a line, such a function should return a range
    /// object when a foldable that starts on that line (but continues
    /// beyond it), if one can be found.
    const foldService = Facet.define();
    /// This node prop is used to associate folding information with node
    /// types. Given a syntax node, it should check whether that tree is
    /// foldable and return the range that can be collapsed when it is.
    const foldNodeProp = new NodeProp();
    function syntaxFolding(state, start, end) {
        let tree = syntaxTree(state);
        if (tree.length == 0)
            return null;
        let inner = tree.resolve(end);
        let found = null;
        for (let cur = inner; cur; cur = cur.parent) {
            if (cur.to <= end || cur.from > end)
                continue;
            if (found && cur.from < start)
                break;
            let prop = cur.type.prop(foldNodeProp);
            if (prop) {
                let value = prop(cur, state);
                if (value && value.from <= end && value.from >= start && value.to > end)
                    found = value;
            }
        }
        return found;
    }
    /// Check whether the given line is foldable. First asks any fold
    /// services registered through
    /// [`foldService`](#language.foldService), and if none of them return
    /// a result, tries to query the [fold node
    /// prop](#language.foldNodeProp) of syntax nodes that cover the end
    /// of the line.
    function foldable(state, lineStart, lineEnd) {
        for (let service of state.facet(foldService)) {
            let result = service(state, lineStart, lineEnd);
            if (result)
                return result;
        }
        return syntaxFolding(state, lineStart, lineEnd);
    }

    /// A gutter marker represents a bit of information attached to a line
    /// in a specific gutter. Your own custom markers have to extend this
    /// class.
    class GutterMarker extends RangeValue {
        /// @internal
        compare(other) {
            return this == other || this.constructor == other.constructor && this.eq(other);
        }
        /// Render the DOM node for this marker, if any.
        toDOM(_view) { return null; }
        /// Create a range that places this marker at the given position.
        at(pos) { return new Range(pos, pos, this); }
    }
    GutterMarker.prototype.elementClass = "";
    GutterMarker.prototype.mapMode = MapMode.TrackBefore;
    const defaults = {
        style: "",
        renderEmptyElements: false,
        elementStyle: "",
        markers: () => RangeSet.empty,
        lineMarker: () => null,
        initialSpacer: null,
        updateSpacer: null,
        domEventHandlers: {}
    };
    const activeGutters = Facet.define();
    /// Define an editor gutter.
    function gutter(config) {
        return [gutters(), activeGutters.of(Object.assign(Object.assign({}, defaults), config))];
    }
    const baseTheme$1 = EditorView.baseTheme({
        $gutters: {
            display: "flex",
            height: "100%",
            boxSizing: "border-box",
            left: 0
        },
        "$$light $gutters": {
            backgroundColor: "#f5f5f5",
            color: "#999",
            borderRight: "1px solid #ddd"
        },
        "$$dark $gutters": {
            backgroundColor: "#333338",
            color: "#ccc"
        },
        $gutter: {
            display: "flex !important",
            flexDirection: "column",
            flexShrink: 0,
            boxSizing: "border-box",
            height: "100%",
            overflow: "hidden"
        },
        $gutterElement: {
            boxSizing: "border-box"
        },
        "$gutterElement.lineNumber": {
            padding: "0 3px 0 5px",
            minWidth: "20px",
            textAlign: "right",
            whiteSpace: "nowrap"
        }
    });
    const unfixGutters = Facet.define({
        combine: values => values.some(x => x)
    });
    /// The gutter-drawing plugin is automatically enabled when you add a
    /// gutter, but you can use this function to explicitly configure it.
    ///
    /// Unless `fixed` is explicitly set to `false`, the gutters are
    /// fixed, meaning they don't scroll along with the content
    /// horizontally.
    function gutters(config) {
        let result = [
            gutterView,
            baseTheme$1
        ];
        if (config && config.fixed === false)
            result.push(unfixGutters.of(true));
        return result;
    }
    const gutterView = ViewPlugin.fromClass(class {
        constructor(view) {
            this.view = view;
            this.dom = document.createElement("div");
            this.dom.className = themeClass("gutters");
            this.dom.setAttribute("aria-hidden", "true");
            this.gutters = view.state.facet(activeGutters).map(conf => new SingleGutterView(view, conf));
            for (let gutter of this.gutters)
                this.dom.appendChild(gutter.dom);
            this.fixed = !view.state.facet(unfixGutters);
            if (this.fixed) {
                // FIXME IE11 fallback, which doesn't support position: sticky,
                // by using position: relative + event handlers that realign the
                // gutter (or just force fixed=false on IE11?)
                this.dom.style.position = "sticky";
            }
            view.scrollDOM.insertBefore(this.dom, view.contentDOM);
        }
        update(update) {
            if (!this.updateGutters(update))
                return;
            let contexts = this.gutters.map(gutter => new UpdateContext(gutter, this.view.viewport));
            this.view.viewportLines(line => {
                let text;
                if (Array.isArray(line.type)) {
                    for (let b of line.type)
                        if (b.type == BlockType.Text) {
                            text = b;
                            break;
                        }
                }
                else {
                    text = line.type == BlockType.Text ? line : undefined;
                }
                if (!text)
                    return;
                for (let cx of contexts)
                    cx.line(this.view, text);
            }, 0);
            for (let cx of contexts)
                cx.finish();
            this.dom.style.minHeight = this.view.contentHeight + "px";
            if (update.state.facet(unfixGutters) != !this.fixed) {
                this.fixed = !this.fixed;
                this.dom.style.position = this.fixed ? "sticky" : "";
            }
        }
        updateGutters(update) {
            let prev = update.prevState.facet(activeGutters), cur = update.state.facet(activeGutters);
            let change = update.docChanged || update.heightChanged || update.viewportChanged;
            if (prev == cur) {
                for (let gutter of this.gutters)
                    if (gutter.update(update))
                        change = true;
            }
            else {
                change = true;
                let gutters = [];
                for (let conf of cur) {
                    let known = prev.indexOf(conf);
                    if (known < 0) {
                        gutters.push(new SingleGutterView(this.view, conf));
                    }
                    else {
                        this.gutters[known].update(update);
                        gutters.push(this.gutters[known]);
                    }
                }
                for (let g of this.gutters)
                    g.dom.remove();
                for (let g of gutters)
                    this.dom.appendChild(g.dom);
                this.gutters = gutters;
            }
            return change;
        }
        destroy() {
            this.dom.remove();
        }
    }, {
        provide: PluginField.scrollMargins.from(value => {
            if (value.gutters.length == 0 || !value.fixed)
                return null;
            return value.view.textDirection == Direction.LTR ? { left: value.dom.offsetWidth } : { right: value.dom.offsetWidth };
        })
    });
    class UpdateContext {
        constructor(gutter, viewport) {
            this.gutter = gutter;
            this.localMarkers = [];
            this.i = 0;
            this.height = 0;
            this.cursor = RangeSet.iter(Array.isArray(gutter.markers) ? gutter.markers : [gutter.markers], viewport.from);
        }
        line(view, line) {
            if (this.localMarkers.length)
                this.localMarkers = [];
            while (this.cursor.value && this.cursor.from <= line.from) {
                if (this.cursor.from == line.from)
                    this.localMarkers.push(this.cursor.value);
                this.cursor.next();
            }
            let forLine = this.gutter.config.lineMarker(view, line, this.localMarkers);
            if (forLine)
                this.localMarkers.unshift(forLine);
            let gutter = this.gutter;
            if (this.localMarkers.length == 0 && !gutter.config.renderEmptyElements)
                return;
            let above = line.top - this.height;
            if (this.i == gutter.elements.length) {
                let newElt = new GutterElement(view, line.height, above, this.localMarkers, gutter.elementClass);
                gutter.elements.push(newElt);
                gutter.dom.appendChild(newElt.dom);
            }
            else {
                let markers = this.localMarkers, elt = gutter.elements[this.i];
                if (sameMarkers(markers, elt.markers)) {
                    markers = elt.markers;
                    this.localMarkers.length = 0;
                }
                elt.update(view, line.height, above, markers, gutter.elementClass);
            }
            this.height = line.bottom;
            this.i++;
        }
        finish() {
            let gutter = this.gutter;
            while (gutter.elements.length > this.i)
                gutter.dom.removeChild(gutter.elements.pop().dom);
        }
    }
    class SingleGutterView {
        constructor(view, config) {
            this.view = view;
            this.config = config;
            this.elements = [];
            this.spacer = null;
            this.dom = document.createElement("div");
            this.dom.className = themeClass("gutter" + (this.config.style ? "." + this.config.style : ""));
            this.elementClass = themeClass("gutterElement" + (this.config.style ? "." + this.config.style : ""));
            for (let prop in config.domEventHandlers) {
                this.dom.addEventListener(prop, (event) => {
                    let line = view.visualLineAtHeight(event.clientY, view.contentDOM.getBoundingClientRect().top);
                    if (config.domEventHandlers[prop](view, line, event))
                        event.preventDefault();
                });
            }
            this.markers = config.markers(view.state);
            if (config.initialSpacer) {
                this.spacer = new GutterElement(view, 0, 0, [config.initialSpacer(view)], this.elementClass);
                this.dom.appendChild(this.spacer.dom);
                this.spacer.dom.style.cssText += "visibility: hidden; pointer-events: none";
            }
        }
        update(update) {
            let prevMarkers = this.markers;
            this.markers = this.config.markers(update.state);
            if (this.spacer && this.config.updateSpacer) {
                let updated = this.config.updateSpacer(this.spacer.markers[0], update);
                if (updated != this.spacer.markers[0])
                    this.spacer.update(update.view, 0, 0, [updated], this.elementClass);
            }
            return this.markers != prevMarkers;
        }
    }
    class GutterElement {
        constructor(view, height, above, markers, eltClass) {
            this.height = -1;
            this.above = 0;
            this.dom = document.createElement("div");
            this.update(view, height, above, markers, eltClass);
        }
        update(view, height, above, markers, cssClass) {
            if (this.height != height)
                this.dom.style.height = (this.height = height) + "px";
            if (this.above != above)
                this.dom.style.marginTop = (this.above = above) ? above + "px" : "";
            if (this.markers != markers) {
                this.markers = markers;
                for (let ch; ch = this.dom.lastChild;)
                    ch.remove();
                let cls = cssClass;
                for (let m of markers) {
                    let dom = m.toDOM(view);
                    if (dom)
                        this.dom.appendChild(dom);
                    let c = m.elementClass;
                    if (c)
                        cls += " " + c;
                }
                this.dom.className = cls;
            }
        }
    }
    function sameMarkers(a, b) {
        if (a.length != b.length)
            return false;
        for (let i = 0; i < a.length; i++)
            if (!a[i].compare(b[i]))
                return false;
        return true;
    }
    /// Facet used to provide markers to the line number gutter.
    const lineNumberMarkers = Facet.define();
    const lineNumberConfig = Facet.define({
        combine(values) {
            return combineConfig(values, { formatNumber: String, domEventHandlers: {} }, {
                domEventHandlers(a, b) {
                    let result = Object.assign({}, a);
                    for (let event in b) {
                        let exists = result[event], add = b[event];
                        result[event] = exists ? (view, line, event) => exists(view, line, event) || add(view, line, event) : add;
                    }
                    return result;
                }
            });
        }
    });
    class NumberMarker extends GutterMarker {
        constructor(number) {
            super();
            this.number = number;
        }
        eq(other) { return this.number == other.number; }
        toDOM(view) {
            let config = view.state.facet(lineNumberConfig);
            return document.createTextNode(config.formatNumber(this.number));
        }
    }
    const lineNumberGutter = gutter({
        style: "lineNumber",
        markers(state) { return state.facet(lineNumberMarkers); },
        lineMarker(view, line, others) {
            if (others.length)
                return null;
            // FIXME try to make the line number queries cheaper?
            return new NumberMarker(view.state.doc.lineAt(line.from).number);
        },
        initialSpacer(view) {
            return new NumberMarker(maxLineNumber(view.state.doc.lines));
        },
        updateSpacer(spacer, update) {
            let max = maxLineNumber(update.view.state.doc.lines);
            return max == spacer.number ? spacer : new NumberMarker(max);
        }
    });
    /// Create a line number gutter extension. The order in which the
    /// gutters appear is determined by their extension priority.
    function lineNumbers(config = {}) {
        return [
            lineNumberConfig.of(config),
            lineNumberGutter
        ];
    }
    function maxLineNumber(lines) {
        let last = 9;
        while (last < lines)
            last = last * 10 + 9;
        return last;
    }

    function mapRange(range, mapping) {
        let from = mapping.mapPos(range.from, 1), to = mapping.mapPos(range.to, -1);
        return from >= to ? undefined : { from, to };
    }
    const foldEffect = StateEffect.define({ map: mapRange });
    const unfoldEffect = StateEffect.define({ map: mapRange });
    function selectedLines(view) {
        let lines = [];
        for (let { head } of view.state.selection.ranges) {
            if (lines.some(l => l.from <= head && l.to >= head))
                continue;
            lines.push(view.visualLineAt(head));
        }
        return lines;
    }
    const foldState = StateField.define({
        create() {
            return Decoration.none;
        },
        update(folded, tr) {
            folded = folded.map(tr.changes);
            for (let e of tr.effects) {
                if (e.is(foldEffect) && !foldExists(folded, e.value.from, e.value.to))
                    folded = folded.update({ add: [foldWidget.range(e.value.from, e.value.to)] });
                else if (e.is(unfoldEffect)) {
                    folded = folded.update({ filter: (from, to) => e.value.from != from || e.value.to != to,
                        filterFrom: e.value.from, filterTo: e.value.to });
                }
            }
            // Clear folded ranges that cover the selection head
            if (tr.selection) {
                let onSelection = false, { head } = tr.selection.primary;
                folded.between(head, head, (a, b) => { if (a < head && b > head)
                    onSelection = true; });
                if (onSelection)
                    folded = folded.update({
                        filterFrom: head,
                        filterTo: head,
                        filter: (a, b) => b <= head || a >= head
                    });
            }
            return folded;
        },
        provide: [EditorView.decorations]
    });
    function foldInside(state, from, to) {
        var _a;
        let found = null;
        (_a = state.field(foldState, false)) === null || _a === void 0 ? void 0 : _a.between(from, to, (from, to) => {
            if (!found || found.from > from)
                found = ({ from, to });
        });
        return found;
    }
    function foldExists(folded, from, to) {
        let found = false;
        folded.between(from, from, (a, b) => { if (a == from && b == to)
            found = true; });
        return found;
    }
    function maybeEnable(state) {
        return state.field(foldState, false) ? undefined : { append: codeFolding() };
    }
    /// Fold the lines that are selected, if possible.
    const foldCode = view => {
        for (let line of selectedLines(view)) {
            let range = foldable(view.state, line.from, line.to);
            if (range) {
                view.dispatch({ effects: foldEffect.of(range),
                    reconfigure: maybeEnable(view.state) });
                return true;
            }
        }
        return false;
    };
    /// Unfold folded ranges on selected lines.
    const unfoldCode = view => {
        if (!view.state.field(foldState, false))
            return false;
        let effects = [];
        for (let line of selectedLines(view)) {
            let folded = foldInside(view.state, line.from, line.to);
            if (folded)
                effects.push(unfoldEffect.of(folded));
        }
        if (effects.length)
            view.dispatch({ effects });
        return effects.length > 0;
    };
    /// Fold all top-level foldable ranges.
    const foldAll = view => {
        let { state } = view, effects = [];
        for (let pos = 0; pos < state.doc.length;) {
            let line = view.visualLineAt(pos), range = foldable(state, line.from, line.to);
            if (range)
                effects.push(foldEffect.of(range));
            pos = (range ? view.visualLineAt(range.to) : line).to + 1;
        }
        if (effects.length)
            view.dispatch({ effects, reconfigure: maybeEnable(view.state) });
        return !!effects.length;
    };
    /// Unfold all folded code.
    const unfoldAll = view => {
        let field = view.state.field(foldState, false);
        if (!field || !field.size)
            return false;
        let effects = [];
        field.between(0, view.state.doc.length, (from, to) => { effects.push(unfoldEffect.of({ from, to })); });
        view.dispatch({ effects });
        return true;
    };
    /// Default fold-related key bindings.
    ///
    ///  - Ctrl-Shift-[ (Cmd-Alt-[ on macOS): [`foldCode`](#fold.foldCode).
    ///  - Ctrl-Shift-] (Cmd-Alt-] on macOS): [`unfoldCode`](#fold.unfoldCode).
    ///  - Ctrl-Alt-[: [`foldAll`](#fold.foldAll).
    ///  - Ctrl-Alt-]: [`unfoldAll`](#fold.unfoldAll).
    const foldKeymap = [
        { key: "Ctrl-Shift-[", mac: "Cmd-Alt-[", run: foldCode },
        { key: "Ctrl-Shift-]", mac: "Cmd-Alt-]", run: unfoldCode },
        { key: "Ctrl-Alt-[", run: foldAll },
        { key: "Ctrl-Alt-]", run: unfoldAll }
    ];
    const defaultConfig = {
        placeholderDOM: null,
        placeholderText: "…"
    };
    const foldConfig = Facet.define({
        combine(values) { return combineConfig(values, defaultConfig); }
    });
    /// Create an extension that configures code folding.
    function codeFolding(config) {
        let result = [foldState, baseTheme$2];
        if (config)
            result.push(foldConfig.of(config));
        return result;
    }
    const foldWidget = Decoration.replace({ widget: new class extends WidgetType {
            ignoreEvents() { return false; }
            toDOM(view) {
                let { state } = view, conf = state.facet(foldConfig);
                if (conf.placeholderDOM)
                    return conf.placeholderDOM();
                let element = document.createElement("span");
                element.textContent = conf.placeholderText;
                element.setAttribute("aria-label", state.phrase("folded code"));
                element.title = state.phrase("unfold");
                element.className = themeClass("foldPlaceholder");
                element.onclick = event => {
                    let line = view.visualLineAt(view.posAtDOM(event.target));
                    let folded = foldInside(view.state, line.from, line.to);
                    if (folded)
                        view.dispatch({ effects: unfoldEffect.of(folded) });
                    event.preventDefault();
                };
                return element;
            }
        } });
    const foldGutterDefaults = {
        openText: "⌄",
        closedText: "›"
    };
    class FoldMarker extends GutterMarker {
        constructor(config, open) {
            super();
            this.config = config;
            this.open = open;
        }
        eq(other) { return this.config == other.config && this.open == other.open; }
        toDOM(view) {
            let span = document.createElement("span");
            span.textContent = this.open ? this.config.openText : this.config.closedText;
            span.title = view.state.phrase(this.open ? "Fold line" : "Unfold line");
            return span;
        }
    }
    /// Create an extension that registers a fold gutter, which shows a
    /// fold status indicator before lines which can be clicked to fold or
    /// unfold the line.
    function foldGutter(config = {}) {
        let fullConfig = Object.assign(Object.assign({}, foldGutterDefaults), config);
        let canFold = new FoldMarker(fullConfig, true), canUnfold = new FoldMarker(fullConfig, false);
        return [
            gutter({
                style: "foldGutter",
                lineMarker(view, line) {
                    // FIXME optimize this. At least don't run it for updates that
                    // don't change anything relevant
                    let folded = foldInside(view.state, line.from, line.to);
                    if (folded)
                        return canUnfold;
                    if (foldable(view.state, line.from, line.to))
                        return canFold;
                    return null;
                },
                initialSpacer() {
                    return new FoldMarker(fullConfig, false);
                },
                domEventHandlers: {
                    click: (view, line) => {
                        let folded = foldInside(view.state, line.from, line.to);
                        if (folded) {
                            view.dispatch({ effects: unfoldEffect.of(folded) });
                            return true;
                        }
                        let range = foldable(view.state, line.from, line.to);
                        if (range) {
                            view.dispatch({ effects: foldEffect.of(range) });
                            return true;
                        }
                        return false;
                    }
                }
            }),
            codeFolding()
        ];
    }
    const baseTheme$2 = EditorView.baseTheme({
        $foldPlaceholder: {
            backgroundColor: "#eee",
            border: "1px solid #ddd",
            color: "#888",
            borderRadius: ".2em",
            margin: "0 1px",
            padding: "0 1px",
            cursor: "pointer"
        },
        "$gutterElement.foldGutter": {
            padding: "0 1px",
            cursor: "pointer"
        }
    });

    const baseTheme$3 = EditorView.baseTheme({
        $matchingBracket: { color: "#0b0" },
        $nonmatchingBracket: { color: "#a22" }
    });
    const DefaultScanDist = 10000, DefaultBrackets = "()[]{}";
    const bracketMatchingConfig = Facet.define({
        combine(configs) {
            return combineConfig(configs, {
                afterCursor: true,
                brackets: DefaultBrackets,
                maxScanDistance: DefaultScanDist
            });
        }
    });
    const matchingMark = Decoration.mark({ class: themeClass("matchingBracket") }), nonmatchingMark = Decoration.mark({ class: themeClass("nonmatchingBracket") });
    const bracketMatchingState = StateField.define({
        create() { return Decoration.none; },
        update(deco, tr) {
            if (!tr.docChanged && !tr.selection)
                return deco;
            let decorations = [];
            let config = tr.state.facet(bracketMatchingConfig);
            for (let range of tr.state.selection.ranges) {
                if (!range.empty)
                    continue;
                let match = matchBrackets(tr.state, range.head, -1, config)
                    || (range.head > 0 && matchBrackets(tr.state, range.head - 1, 1, config))
                    || (config.afterCursor &&
                        (matchBrackets(tr.state, range.head, 1, config) ||
                            (range.head < tr.state.doc.length && matchBrackets(tr.state, range.head + 1, -1, config))));
                if (!match)
                    continue;
                let mark = match.matched ? matchingMark : nonmatchingMark;
                decorations.push(mark.range(match.start.from, match.start.to));
                if (match.end)
                    decorations.push(mark.range(match.end.from, match.end.to));
            }
            return Decoration.set(decorations, true);
        },
        provide: [EditorView.decorations]
    });
    const bracketMatchingUnique = [
        bracketMatchingState,
        baseTheme$3
    ];
    /// Create an extension that enables bracket matching. Whenever the
    /// cursor is next to a bracket, that bracket and the one it matches
    /// are highlighted. Or, when no matching bracket is found, another
    /// highlighting style is used to indicate this.
    function bracketMatching(config = {}) {
        return [bracketMatchingConfig.of(config), bracketMatchingUnique];
    }
    function matchingNodes(node, dir, brackets) {
        let byProp = node.prop(dir < 0 ? NodeProp.openedBy : NodeProp.closedBy);
        if (byProp)
            return byProp;
        if (node.name.length == 1) {
            let index = brackets.indexOf(node.name);
            if (index > -1 && index % 2 == (dir < 0 ? 1 : 0))
                return [brackets[index + dir]];
        }
        return null;
    }
    /// Find the matching bracket for the token at `pos`, scanning
    /// direction `dir`. Only the `brackets` and `maxScanDistance`
    /// properties are used from `config`, if given. Returns null if no
    /// bracket was found at `pos`, or a match result otherwise.
    function matchBrackets(state, pos, dir, config = {}) {
        let maxScanDistance = config.maxScanDistance || DefaultScanDist, brackets = config.brackets || DefaultBrackets;
        let tree = syntaxTree(state), sub = tree.resolve(pos, dir), matches;
        if (matches = matchingNodes(sub.type, dir, brackets))
            return matchMarkedBrackets(state, pos, dir, sub, matches, brackets);
        else
            return matchPlainBrackets(state, pos, dir, tree, sub.type, maxScanDistance, brackets);
    }
    function matchMarkedBrackets(_state, _pos, dir, token, matching, brackets) {
        let parent = token.parent, firstToken = { from: token.from, to: token.to };
        let depth = 0, cursor = parent === null || parent === void 0 ? void 0 : parent.cursor;
        if (cursor && (dir < 0 ? cursor.childBefore(token.from) : cursor.childAfter(token.to)))
            do {
                if (dir < 0 ? cursor.to <= token.from : cursor.from >= token.to) {
                    if (depth == 0 && matching.indexOf(cursor.type.name) > -1) {
                        return { start: firstToken, end: { from: cursor.from, to: cursor.to }, matched: true };
                    }
                    else if (matchingNodes(cursor.type, dir, brackets)) {
                        depth++;
                    }
                    else if (matchingNodes(cursor.type, -dir, brackets)) {
                        depth--;
                        if (depth == 0)
                            return { start: firstToken, end: { from: cursor.from, to: cursor.to }, matched: false };
                    }
                }
            } while (dir < 0 ? cursor.prevSibling() : cursor.nextSibling());
        return { start: firstToken, matched: false };
    }
    function matchPlainBrackets(state, pos, dir, tree, tokenType, maxScanDistance, brackets) {
        let startCh = dir < 0 ? state.sliceDoc(pos - 1, pos) : state.sliceDoc(pos, pos + 1);
        let bracket = brackets.indexOf(startCh);
        if (bracket < 0 || (bracket % 2 == 0) != (dir > 0))
            return null;
        let startToken = { from: dir < 0 ? pos - 1 : pos, to: dir > 0 ? pos + 1 : pos };
        let iter = state.doc.iterRange(pos, dir > 0 ? state.doc.length : 0), depth = 0;
        for (let distance = 0; !(iter.next()).done && distance <= maxScanDistance;) {
            let text = iter.value;
            if (dir < 0)
                distance += text.length;
            let basePos = pos + distance * dir;
            for (let pos = dir > 0 ? 0 : text.length - 1, end = dir > 0 ? text.length : -1; pos != end; pos += dir) {
                let found = brackets.indexOf(text[pos]);
                if (found < 0 || tree.resolve(basePos + pos, 1).type != tokenType)
                    continue;
                if ((found % 2 == 0) == (dir > 0)) {
                    depth++;
                }
                else if (depth == 1) { // Closing
                    return { start: startToken, end: { from: basePos + pos, to: basePos + pos + 1 }, matched: (found >> 1) == (bracket >> 1) };
                }
                else {
                    depth--;
                }
            }
            if (dir > 0)
                distance += text.length;
        }
        return iter.done ? { start: startToken, matched: false } : null;
    }

    function updateSel(sel, by) {
        return EditorSelection.create(sel.ranges.map(by), sel.primaryIndex);
    }
    function setSel(state, selection) {
        return state.update({ selection, scrollIntoView: true, annotations: Transaction.userEvent.of("keyboardselection") });
    }
    function moveSel({ state, dispatch }, how) {
        let selection = updateSel(state.selection, how);
        if (selection.eq(state.selection))
            return false;
        dispatch(setSel(state, selection));
        return true;
    }
    function rangeEnd(range, forward) {
        return EditorSelection.cursor(forward ? range.to : range.from);
    }
    function cursorByChar(view, forward) {
        return moveSel(view, range => range.empty ? view.moveByChar(range, forward) : rangeEnd(range, forward));
    }
    /// Move the selection one character to the left (which is backward in
    /// left-to-right text, forward in right-to-left text).
    const cursorCharLeft = view => cursorByChar(view, view.textDirection != Direction.LTR);
    /// Move the selection one character to the right.
    const cursorCharRight = view => cursorByChar(view, view.textDirection == Direction.LTR);
    function cursorByGroup(view, forward) {
        return moveSel(view, range => range.empty ? view.moveByGroup(range, forward) : rangeEnd(range, forward));
    }
    /// Move the selection across one group of word or non-word (but also
    /// non-space) characters.
    const cursorGroupLeft = view => cursorByGroup(view, view.textDirection != Direction.LTR);
    /// Move the selection one group to the right.
    const cursorGroupRight = view => cursorByGroup(view, view.textDirection == Direction.LTR);
    /// Move the selection one group forward.
    const cursorGroupForward = view => cursorByGroup(view, true);
    /// Move the selection one group backward.
    const cursorGroupBackward = view => cursorByGroup(view, false);
    function interestingNode(state, node, bracketProp) {
        if (node.type.prop(bracketProp))
            return true;
        let len = node.to - node.from;
        return len && (len > 2 || /[^\s,.;:]/.test(state.sliceDoc(node.from, node.to))) || node.firstChild;
    }
    function moveBySyntax(state, start, forward) {
        let pos = syntaxTree(state).resolve(start.head);
        let bracketProp = forward ? NodeProp.closedBy : NodeProp.openedBy;
        // Scan forward through child nodes to see if there's an interesting
        // node ahead.
        for (let at = start.head;;) {
            let next = forward ? pos.childAfter(at) : pos.childBefore(at);
            if (!next)
                break;
            if (interestingNode(state, next, bracketProp))
                pos = next;
            else
                at = forward ? next.to : next.from;
        }
        let bracket = pos.type.prop(bracketProp), match, newPos;
        if (bracket && (match = forward ? matchBrackets(state, pos.from, 1) : matchBrackets(state, pos.to, -1)) && match.matched)
            newPos = forward ? match.end.to : match.end.from;
        else
            newPos = forward ? pos.to : pos.from;
        return EditorSelection.cursor(newPos, forward ? -1 : 1);
    }
    /// Move the cursor over the next syntactic element to the left.
    const cursorSyntaxLeft = view => moveSel(view, range => moveBySyntax(view.state, range, view.textDirection != Direction.LTR));
    /// Move the cursor over the next syntactic element to the right.
    const cursorSyntaxRight = view => moveSel(view, range => moveBySyntax(view.state, range, view.textDirection == Direction.LTR));
    function cursorByLine(view, forward) {
        return moveSel(view, range => range.empty ? view.moveVertically(range, forward) : rangeEnd(range, forward));
    }
    /// Move the selection one line up.
    const cursorLineUp = view => cursorByLine(view, false);
    /// Move the selection one line down.
    const cursorLineDown = view => cursorByLine(view, true);
    function cursorByPage(view, forward) {
        return moveSel(view, range => range.empty ? view.moveVertically(range, forward, view.dom.clientHeight) : rangeEnd(range, forward));
    }
    /// Move the selection one page up.
    const cursorPageUp = view => cursorByPage(view, false);
    /// Move the selection one page down.
    const cursorPageDown = view => cursorByPage(view, true);
    function moveByLineBoundary(view, start, forward) {
        let line = view.visualLineAt(start.head), moved = view.moveToLineBoundary(start, forward);
        if (moved.head == start.head && moved.head != (forward ? line.to : line.from))
            moved = view.moveToLineBoundary(start, forward, false);
        if (!forward && moved.head == line.from && line.length) {
            let space = /^\s*/.exec(view.state.sliceDoc(line.from, Math.min(line.from + 100, line.to)))[0].length;
            if (space && start.head > line.from + space)
                moved = EditorSelection.cursor(line.from + space);
        }
        return moved;
    }
    /// Move the selection to the next line wrap point, or to the end of
    /// the line if there isn't one left on this line.
    const cursorLineBoundaryForward = view => moveSel(view, range => moveByLineBoundary(view, range, true));
    /// Move the selection to previous line wrap point, or failing that to
    /// the start of the line.
    const cursorLineBoundaryBackward = view => moveSel(view, range => moveByLineBoundary(view, range, false));
    /// Move the selection to the start of the line.
    const cursorLineStart = view => moveSel(view, range => EditorSelection.cursor(view.visualLineAt(range.head).from, 1));
    /// Move the selection to the end of the line.
    const cursorLineEnd = view => moveSel(view, range => EditorSelection.cursor(view.visualLineAt(range.head).to, -1));
    function toMatchingBracket(state, dispatch, extend) {
        let found = false, selection = updateSel(state.selection, range => {
            let matching = matchBrackets(state, range.head, -1)
                || matchBrackets(state, range.head, 1)
                || (range.head > 0 && matchBrackets(state, range.head - 1, 1))
                || (range.head < state.doc.length && matchBrackets(state, range.head + 1, -1));
            if (!matching || !matching.end)
                return range;
            found = true;
            let head = matching.start.from == range.head ? matching.end.to : matching.end.from;
            return extend ? EditorSelection.range(range.anchor, head) : EditorSelection.cursor(head);
        });
        if (!found)
            return false;
        dispatch(setSel(state, selection));
        return true;
    }
    /// Move the selection to the bracket matching the one it is currently
    /// on, if any.
    const cursorMatchingBracket = ({ state, dispatch }) => toMatchingBracket(state, dispatch, false);
    function extendSel(view, how) {
        let selection = updateSel(view.state.selection, range => {
            let head = how(range);
            return EditorSelection.range(range.anchor, head.head, head.goalColumn);
        });
        if (selection.eq(view.state.selection))
            return false;
        view.dispatch(setSel(view.state, selection));
        return true;
    }
    function selectByChar(view, forward) {
        return extendSel(view, range => view.moveByChar(range, forward));
    }
    /// Move the selection head one character to the left, while leaving
    /// the anchor in place.
    const selectCharLeft = view => selectByChar(view, view.textDirection != Direction.LTR);
    /// Move the selection head one character to the right.
    const selectCharRight = view => selectByChar(view, view.textDirection == Direction.LTR);
    function selectByGroup(view, forward) {
        return extendSel(view, range => view.moveByGroup(range, forward));
    }
    /// Move the selection head one [group](#commands.cursorGroupLeft) to
    /// the left.
    const selectGroupLeft = view => selectByGroup(view, view.textDirection != Direction.LTR);
    /// Move the selection head one group to the right.
    const selectGroupRight = view => selectByGroup(view, view.textDirection == Direction.LTR);
    /// Move the selection head one group forward.
    const selectGroupForward = view => selectByGroup(view, true);
    /// Move the selection head one group backward.
    const selectGroupBackward = view => selectByGroup(view, false);
    /// Move the selection head over the next syntactic element to the left.
    const selectSyntaxLeft = view => extendSel(view, range => moveBySyntax(view.state, range, view.textDirection != Direction.LTR));
    /// Move the selection head over the next syntactic element to the right.
    const selectSyntaxRight = view => extendSel(view, range => moveBySyntax(view.state, range, view.textDirection == Direction.LTR));
    function selectByLine(view, forward) {
        return extendSel(view, range => view.moveVertically(range, forward));
    }
    /// Move the selection head one line up.
    const selectLineUp = view => selectByLine(view, false);
    /// Move the selection head one line down.
    const selectLineDown = view => selectByLine(view, true);
    function selectByPage(view, forward) {
        return extendSel(view, range => view.moveVertically(range, forward, view.dom.clientHeight));
    }
    /// Move the selection head one page up.
    const selectPageUp = view => selectByPage(view, false);
    /// Move the selection head one page down.
    const selectPageDown = view => selectByPage(view, true);
    /// Move the selection head to the next line boundary.
    const selectLineBoundaryForward = view => extendSel(view, range => moveByLineBoundary(view, range, true));
    /// Move the selection head to the previous line boundary.
    const selectLineBoundaryBackward = view => extendSel(view, range => moveByLineBoundary(view, range, false));
    /// Move the selection head to the start of the line.
    const selectLineStart = view => extendSel(view, range => EditorSelection.cursor(view.visualLineAt(range.head).from));
    /// Move the selection head to the end of the line.
    const selectLineEnd = view => extendSel(view, range => EditorSelection.cursor(view.visualLineAt(range.head).to));
    /// Move the selection to the start of the document.
    const cursorDocStart = ({ state, dispatch }) => {
        dispatch(setSel(state, { anchor: 0 }));
        return true;
    };
    /// Move the selection to the end of the document.
    const cursorDocEnd = ({ state, dispatch }) => {
        dispatch(setSel(state, { anchor: state.doc.length }));
        return true;
    };
    /// Move the selection head to the start of the document.
    const selectDocStart = ({ state, dispatch }) => {
        dispatch(setSel(state, { anchor: state.selection.primary.anchor, head: 0 }));
        return true;
    };
    /// Move the selection head to the end of the document.
    const selectDocEnd = ({ state, dispatch }) => {
        dispatch(setSel(state, { anchor: state.selection.primary.anchor, head: state.doc.length }));
        return true;
    };
    /// Select the entire document.
    const selectAll = ({ state, dispatch }) => {
        dispatch(state.update({ selection: { anchor: 0, head: state.doc.length }, annotations: Transaction.userEvent.of("keyboardselection") }));
        return true;
    };
    /// Expand the selection to cover entire lines.
    const selectLine = ({ state, dispatch }) => {
        let ranges = selectedLineBlocks(state).map(({ from, to }) => EditorSelection.range(from, Math.min(to + 1, state.doc.length)));
        dispatch(state.update({ selection: new EditorSelection(ranges), annotations: Transaction.userEvent.of("keyboardselection") }));
        return true;
    };
    /// Select the next syntactic construct that is larger than the
    /// selection. Note that this will only work insofar as the language
    /// [provider](#language.language) you use builds up a full
    /// syntax tree.
    const selectParentSyntax = ({ state, dispatch }) => {
        let selection = updateSel(state.selection, range => {
            var _a;
            let context = syntaxTree(state).resolve(range.head, 1);
            while (!((context.from < range.from && context.to >= range.to) ||
                (context.to > range.to && context.from <= range.from) ||
                !((_a = context.parent) === null || _a === void 0 ? void 0 : _a.parent)))
                context = context.parent;
            return EditorSelection.range(context.to, context.from);
        });
        dispatch(setSel(state, selection));
        return true;
    };
    /// Simplify the current selection. When multiple ranges are selected,
    /// reduce it to its primary range. Otherwise, if the selection is
    /// non-empty, convert it to a cursor selection.
    const simplifySelection = ({ state, dispatch }) => {
        let cur = state.selection, selection = null;
        if (cur.ranges.length > 1)
            selection = new EditorSelection([cur.primary]);
        else if (!cur.primary.empty)
            selection = new EditorSelection([EditorSelection.cursor(cur.primary.head)]);
        if (!selection)
            return false;
        dispatch(setSel(state, selection));
        return true;
    };
    function deleteBy(view, by) {
        let { state } = view, changes = state.changeByRange(range => {
            let { from, to } = range;
            if (from == to) {
                let towards = by(from);
                from = Math.min(from, towards);
                to = Math.max(to, towards);
            }
            return from == to ? { range } : { changes: { from, to }, range: EditorSelection.cursor(from) };
        });
        if (changes.changes.empty)
            return false;
        view.dispatch(changes, { scrollIntoView: true, annotations: Transaction.userEvent.of("delete") });
        return true;
    }
    const deleteByChar = (view, forward, codePoint) => deleteBy(view, pos => {
        let { state } = view, line = state.doc.lineAt(pos), before;
        if (!forward && pos > line.from && pos < line.from + 200 &&
            !/[^ \t]/.test(before = line.slice(0, pos - line.from))) {
            if (before[before.length - 1] == "\t")
                return pos - 1;
            let col = countColumn(before, 0, state.tabSize), drop = col % getIndentUnit(state) || getIndentUnit(state);
            for (let i = 0; i < drop && before[before.length - 1 - i] == " "; i++)
                pos--;
            return pos;
        }
        let target;
        if (codePoint) {
            let next = line.slice(pos - line.from + (forward ? 0 : -2), pos - line.from + (forward ? 2 : 0));
            let size = next ? codePointSize(codePointAt(next, 0)) : 1;
            target = forward ? Math.min(state.doc.length, pos + size) : Math.max(0, pos - size);
        }
        else {
            target = line.findClusterBreak(pos - line.from, forward) + line.from;
        }
        if (target == pos && line.number != (forward ? state.doc.lines : 1))
            target += forward ? 1 : -1;
        return target;
    });
    /// Delete the selection, or, for cursor selections, the code point
    /// before the cursor.
    const deleteCodePointBackward = view => deleteByChar(view, false, true);
    /// Delete the selection, or, for cursor selections, the character
    /// before the cursor.
    const deleteCharBackward = view => deleteByChar(view, false, false);
    /// Delete the selection or the character after the cursor.
    const deleteCharForward = view => deleteByChar(view, true, false);
    const deleteByGroup = (view, forward) => deleteBy(view, pos => {
        let { state } = view, line = state.doc.lineAt(pos), categorize = state.charCategorizer(pos);
        for (let cat = null;;) {
            let next, nextChar;
            if (pos == (forward ? line.to : line.from)) {
                if (line.number == (forward ? state.doc.lines : 1))
                    break;
                line = state.doc.line(line.number + (forward ? 1 : -1));
                next = forward ? line.from : line.to;
                nextChar = "\n";
            }
            else {
                next = line.findClusterBreak(pos - line.from, forward) + line.from;
                nextChar = line.slice(Math.min(pos, next) - line.from, Math.max(pos, next) - line.from);
            }
            let nextCat = categorize(nextChar);
            if (cat != null && nextCat != cat)
                break;
            if (nextCat != CharCategory.Space)
                cat = nextCat;
            pos = next;
        }
        return pos;
    });
    /// Delete the selection or backward until the end of the next
    /// [group](#view.EditorView.moveByGroup).
    const deleteGroupBackward = view => deleteByGroup(view, false);
    /// Delete the selection or forward until the end of the next group.
    const deleteGroupForward = view => deleteByGroup(view, true);
    /// Delete the selection, or, if it is a cursor selection, delete to
    /// the end of the line. If the cursor is directly at the end of the
    /// line, delete the line break after it.
    const deleteToLineEnd = view => deleteBy(view, pos => {
        let lineEnd = view.visualLineAt(pos).to;
        if (pos < lineEnd)
            return lineEnd;
        return Math.max(view.state.doc.length, pos + 1);
    });
    /// Replace each selection range with a line break, leaving the cursor
    /// on the line before the break.
    const splitLine = ({ state, dispatch }) => {
        let changes = state.changeByRange(range => {
            return { changes: { from: range.from, to: range.to, insert: Text.of(["", ""]) },
                range: EditorSelection.cursor(range.from) };
        });
        dispatch(state.update(changes, { scrollIntoView: true, annotations: Transaction.userEvent.of("input") }));
        return true;
    };
    /// Flip the characters before and after the cursor(s).
    const transposeChars = ({ state, dispatch }) => {
        let changes = state.changeByRange(range => {
            if (!range.empty || range.from == 0 || range.from == state.doc.length)
                return { range };
            let pos = range.from, line = state.doc.lineAt(pos);
            let from = pos == line.from ? pos - 1 : line.findClusterBreak(pos - line.from, false) + line.from;
            let to = pos == line.to ? pos + 1 : line.findClusterBreak(pos - line.from, true) + line.from;
            return { changes: { from, to, insert: state.doc.slice(pos, to).append(state.doc.slice(from, pos)) },
                range: EditorSelection.cursor(to) };
        });
        if (changes.changes.empty)
            return false;
        dispatch(state.update(changes, { scrollIntoView: true }));
        return true;
    };
    function selectedLineBlocks(state) {
        let blocks = [], upto = -1;
        for (let range of state.selection.ranges) {
            let startLine = state.doc.lineAt(range.from), endLine = state.doc.lineAt(range.to);
            if (upto == startLine.number)
                blocks[blocks.length - 1].to = endLine.to;
            else
                blocks.push({ from: startLine.from, to: endLine.to });
            upto = endLine.number;
        }
        return blocks;
    }
    function moveLine(state, dispatch, forward) {
        let changes = [];
        for (let block of selectedLineBlocks(state)) {
            if (forward ? block.to == state.doc.length : block.from == 0)
                continue;
            let nextLine = state.doc.lineAt(forward ? block.to + 1 : block.from - 1);
            if (forward)
                changes.push({ from: block.to, to: nextLine.to }, { from: block.from, insert: nextLine.slice() + state.lineBreak });
            else
                changes.push({ from: nextLine.from, to: block.from }, { from: block.to, insert: state.lineBreak + nextLine.slice() });
        }
        if (!changes.length)
            return false;
        dispatch(state.update({ changes, scrollIntoView: true }));
        return true;
    }
    /// Move the selected lines up one line.
    const moveLineUp = ({ state, dispatch }) => moveLine(state, dispatch, false);
    /// Move the selected lines down one line.
    const moveLineDown = ({ state, dispatch }) => moveLine(state, dispatch, true);
    function copyLine(state, dispatch, forward) {
        let changes = [];
        for (let block of selectedLineBlocks(state)) {
            if (forward)
                changes.push({ from: block.from, insert: state.doc.slice(block.from, block.to) + state.lineBreak });
            else
                changes.push({ from: block.to, insert: state.lineBreak + state.doc.slice(block.from, block.to) });
        }
        dispatch(state.update({ changes, scrollIntoView: true }));
        return true;
    }
    /// Create a copy of the selected lines. Keep the selection in the top copy.
    const copyLineUp = ({ state, dispatch }) => copyLine(state, dispatch, false);
    /// Create a copy of the selected lines. Keep the selection in the bottom copy.
    const copyLineDown = ({ state, dispatch }) => copyLine(state, dispatch, true);
    /// Delete selected lines.
    const deleteLine = view => {
        let { state } = view, changes = state.changes(selectedLineBlocks(state).map(({ from, to }) => {
            if (from > 0)
                from--;
            else if (to < state.doc.length)
                to++;
            return { from, to };
        }));
        let selection = updateSel(state.selection, range => view.moveVertically(range, true)).map(changes);
        view.dispatch({ changes, selection, scrollIntoView: true });
        return true;
    };
    function isBetweenBrackets(state, pos) {
        if (/\(\)|\[\]|\{\}/.test(state.sliceDoc(pos - 1, pos + 1)))
            return { from: pos, to: pos };
        let context = syntaxTree(state).resolve(pos);
        let before = context.childBefore(pos), after = context.childAfter(pos), closedBy;
        if (before && after && before.to <= pos && after.from >= pos &&
            (closedBy = before.type.prop(NodeProp.closedBy)) && closedBy.indexOf(after.name) > -1)
            return { from: before.to, to: after.from };
        return null;
    }
    /// Replace the selection with a newline and indent the newly created
    /// line(s). If the current line consists only of whitespace, this
    /// will also delete that whitespace. When the cursor is between
    /// matching brackets, an additional newline will be inserted after
    /// the cursor.
    const insertNewlineAndIndent = ({ state, dispatch }) => {
        let changes = state.changeByRange(({ from, to }) => {
            let explode = from == to && isBetweenBrackets(state, from);
            let cx = new IndentContext(state, { simulateBreak: from, simulateDoubleBreak: !!explode });
            let indent = getIndentation(cx, from);
            if (indent == null)
                indent = /^\s*/.exec(state.doc.lineAt(from).slice(0, 50))[0].length;
            let line = state.doc.lineAt(from);
            while (to < line.to && /\s/.test(line.slice(to - line.from, to + 1 - line.from)))
                to++;
            if (explode)
                ({ from, to } = explode);
            else if (from > line.from && from < line.from + 100 && !/\S/.test(line.slice(0, from)))
                from = line.from;
            let insert = ["", indentString(state, indent)];
            if (explode)
                insert.push(indentString(state, cx.lineIndent(line)));
            return { changes: { from, to, insert: Text.of(insert) },
                range: EditorSelection.cursor(from + 1 + indent) };
        });
        dispatch(state.update(changes, { scrollIntoView: true }));
        return true;
    };
    function changeBySelectedLine(state, f) {
        let atLine = -1;
        return state.changeByRange(range => {
            let changes = [];
            for (let line = state.doc.lineAt(range.from);;) {
                if (line.number > atLine) {
                    f(line, changes, range);
                    atLine = line.number;
                }
                if (range.to <= line.to)
                    break;
                line = state.doc.lineAt(line.to + 1);
            }
            let changeSet = state.changes(changes);
            return { changes,
                range: EditorSelection.range(changeSet.mapPos(range.anchor, 1), changeSet.mapPos(range.head, 1)) };
        });
    }
    /// Auto-indent the selected lines. This uses the [indentation service
    /// facet](#language.indentService) as source for auto-indent
    /// information.
    const indentSelection = ({ state, dispatch }) => {
        let updated = Object.create(null);
        let context = new IndentContext(state, { overrideIndentation: start => {
                let found = updated[start];
                return found == null ? -1 : found;
            } });
        let changes = changeBySelectedLine(state, (line, changes, range) => {
            let indent = getIndentation(context, line.from);
            if (indent == null)
                return;
            let cur = /^\s*/.exec(line.slice(0, Math.min(line.length, 200)))[0];
            let norm = indentString(state, indent);
            if (cur != norm || range.from < line.from + cur.length) {
                updated[line.from] = indent;
                changes.push({ from: line.from, to: line.from + cur.length, insert: norm });
            }
        });
        if (!changes.changes.empty)
            dispatch(state.update(changes));
        return true;
    };
    /// Add a [unit](#language.indentUnit) of indentation to all selected
    /// lines.
    const indentMore = ({ state, dispatch }) => {
        dispatch(state.update(changeBySelectedLine(state, (line, changes) => {
            changes.push({ from: line.from, insert: state.facet(indentUnit) });
        })));
        return true;
    };
    /// Remove a [unit](#language.indentUnit) of indentation from all
    /// selected lines.
    const indentLess = ({ state, dispatch }) => {
        dispatch(state.update(changeBySelectedLine(state, (line, changes) => {
            let lineStart = line.slice(0, Math.min(line.length, 200));
            let space = /^\s*/.exec(lineStart)[0];
            if (!space)
                return;
            let col = countColumn(space, 0, state.tabSize), keep = 0;
            let insert = indentString(state, Math.max(0, col - getIndentUnit(state)));
            while (keep < space.length && keep < insert.length && space.charCodeAt(keep) == insert.charCodeAt(keep))
                keep++;
            changes.push({ from: line.from + keep, to: line.from + space.length, insert: insert.slice(keep) });
        })));
        return true;
    };
    /// Array of key bindings containing the Emacs-style bindings that are
    /// available on macOS by default.
    ///
    ///  - Ctrl-b: [`cursorCharLeft`](#commands.cursorCharLeft) ([`selectCharLeft`](#commands.selectCharLeft) with Shift)
    ///  - Ctrl-f: [`cursorCharRight`](#commands.cursorCharRight) ([`selectCharRight`](#commands.selectCharRight) with Shift)
    ///  - Ctrl-p: [`cursorLineUp`](#commands.cursorLineUp) ([`selectLineUp`](#commands.selectLineUp) with Shift)
    ///  - Ctrl-n: [`cursorLineDown`](#commands.cursorLineDown) ([`selectLineDown`](#commands.selectLineDown) with Shift)
    ///  - Ctrl-a: [`cursorLineStart`](#commands.cursorLineStart) ([`selectLineStart`](#commands.selectLineStart) with Shift)
    ///  - Ctrl-e: [`cursorLineEnd`](#commands.cursorLineEnd) ([`selectLineEnd`](#commands.selectLineEnd) with Shift)
    ///  - Ctrl-d: [`deleteCharForward`](#commands.deleteCharForward)
    ///  - Ctrl-h: [`deleteCharBackward`](#commands.deleteCharBackward)
    ///  - Ctrl-k: [`deleteToLineEnd`](#commands.deleteToLineEnd)
    ///  - Alt-d: [`deleteGroupForward`](#commands.deleteGroupForward)
    ///  - Ctrl-Alt-h: [`deleteGroupBackward`](#commands.deleteGroupBackward)
    ///  - Ctrl-o: [`splitLine`](#commands.splitLine)
    ///  - Ctrl-t: [`transposeChars`](#commands.transposeChars)
    ///  - Alt-f: [`cursorGroupForward`](#commands.cursorGroupForward) ([`selectGroupForward`](#commands.selectGroupForward) with Shift)
    ///  - Alt-b: [`cursorGroupBackward`](#commands.cursorGroupBackward) ([`selectGroupBackward`](#commands.selectGroupBackward) with Shift)
    ///  - Alt-<: [`cursorDocStart`](#commands.cursorDocStart)
    ///  - Alt->: [`cursorDocEnd`](#commands.cursorDocEnd)
    ///  - Ctrl-v: [`cursorPageDown`](#commands.cursorPageDown)
    ///  - Alt-v: [`cursorPageUp`](#commands.cursorPageUp)
    const emacsStyleKeymap = [
        { key: "Ctrl-b", run: cursorCharLeft, shift: selectCharLeft },
        { key: "Ctrl-f", run: cursorCharRight, shift: selectCharRight },
        { key: "Ctrl-p", run: cursorLineUp, shift: selectLineUp },
        { key: "Ctrl-n", run: cursorLineDown, shift: selectLineDown },
        { key: "Ctrl-a", run: cursorLineStart, shift: selectLineStart },
        { key: "Ctrl-e", run: cursorLineEnd, shift: selectLineEnd },
        { key: "Ctrl-d", run: deleteCharForward },
        { key: "Ctrl-h", run: deleteCharBackward },
        { key: "Ctrl-k", run: deleteToLineEnd },
        { key: "Alt-d", run: deleteGroupForward },
        { key: "Ctrl-Alt-h", run: deleteGroupBackward },
        { key: "Ctrl-o", run: splitLine },
        { key: "Ctrl-t", run: transposeChars },
        { key: "Alt-f", run: cursorGroupForward, shift: selectGroupForward },
        { key: "Alt-b", run: cursorGroupBackward, shift: selectGroupBackward },
        { key: "Alt-<", run: cursorDocStart },
        { key: "Alt->", run: cursorDocEnd },
        { key: "Ctrl-v", run: cursorPageDown },
        { key: "Alt-v", run: cursorPageUp },
    ];
    /// An array of key bindings closely sticking to platform-standard or
    /// widely used bindings. (This includes the bindings from
    /// [`emacsStyleKeymap`](#commands.emacsStyleKeymap), with their `key`
    /// property changed to `mac`.)
    ///
    ///  - ArrowLeft: [`cursorCharLeft`](#commands.cursorCharLeft) ([`selectCharLeft`](#commands.selectCharLeft) with Shift)
    ///  - ArrowRight: [`cursorCharRight`](#commands.cursorCharRight) ([`selectCharRight`](#commands.selectCharRight) with Shift)
    ///  - Ctrl-ArrowLeft (Alt-ArrowLeft on macOS): [`cursorGroupLeft`](#commands.cursorGroupLeft) ([`selectGroupLeft`](#commands.selectGroupLeft) with Shift)
    ///  - Ctrl-ArrowRight (Alt-ArrowRight on macOS): [`cursorGroupRight`](#commands.cursorGroupRight) ([`selectGroupRight`](#commands.selectGroupRight) with Shift)
    ///  - Cmd-ArrowLeft (on macOS): [`cursorLineStart`](#commands.cursorLineStart) ([`selectLineStart`](#commands.selectLineStart) with Shift)
    ///  - Cmd-ArrowRight (on macOS): [`cursorLineEnd`](#commands.cursorLineEnd) ([`selectLineEnd`](#commands.selectLineEnd) with Shift)
    ///  - ArrowUp: [`cursorLineUp`](#commands.cursorLineUp) ([`selectLineUp`](#commands.selectLineUp) with Shift)
    ///  - ArrowDown: [`cursorLineDown`](#commands.cursorLineDown) ([`selectLineDown`](#commands.selectLineDown) with Shift)
    ///  - Cmd-ArrowUp (on macOS): [`cursorDocStart`](#commands.cursorDocStart) ([`selectDocStart`](#commands.selectDocStart) with Shift)
    ///  - Cmd-ArrowDown (on macOS): [`cursorDocEnd`](#commands.cursorDocEnd) ([`selectDocEnd`](#commands.selectDocEnd) with Shift)
    ///  - Ctrl-ArrowUp (on macOS): [`cursorPageUp`](#commands.cursorPageUp) ([`selectPageUp`](#commands.selectPageUp) with Shift)
    ///  - Ctrl-ArrowDown (on macOS): [`cursorPageDown`](#commands.cursorPageDown) ([`selectPageDown`](#commands.selectPageDown) with Shift)
    ///  - PageUp: [`cursorPageUp`](#commands.cursorPageUp) ([`selectPageUp`](#commands.selectPageUp) with Shift)
    ///  - PageDown: [`cursorPageDown`](#commands.cursorPageDown) ([`selectPageDown`](#commands.selectPageDown) with Shift)
    ///  - Home: [`cursorLineBoundaryBackward`](#commands.cursorLineBoundaryBackward) ([`selectLineBoundaryBackward`](#commands.selectLineBoundaryBackward) with Shift)
    ///  - End: [`cursorLineBoundaryForward`](#commands.cursorLineBoundaryForward) ([`selectLineBoundaryForward`](#commands.selectLineBoundaryForward) with Shift)
    ///  - Ctrl-Home (Cmd-Home on macOS): [`cursorDocStart`](#commands.cursorDocStart) ([`selectDocStart`](#commands.selectDocStart) with Shift)
    ///  - Ctrl-End (Cmd-Home on macOS): [`cursorDocEnd`](#commands.cursorDocEnd) ([`selectDocEnd`](#commands.selectDocEnd) with Shift)
    ///  - Enter: [`insertNewlineAndIndent`](#commands.insertNewlineAndIndent)
    ///  - Ctrl-a (Cmd-a on macOS): [`selectAll`](#commands.selectAll)
    ///  - Backspace: [`deleteCodePointBackward`](#commands.deleteCodePointBackward)
    ///  - Delete: [`deleteCharForward`](#commands.deleteCharForward)
    ///  - Ctrl-Backspace (Alt-Backspace on macOS): [`deleteGroupBackward`](#commands.deleteGroupBackward)
    ///  - Ctrl-Delete (Alt-Delete on macOS): [`deleteGroupForward`](#commands.deleteGroupForward)
    const standardKeymap = [
        { key: "ArrowLeft", run: cursorCharLeft, shift: selectCharLeft },
        { key: "Mod-ArrowLeft", mac: "Alt-ArrowLeft", run: cursorGroupLeft, shift: selectGroupLeft },
        { mac: "Cmd-ArrowLeft", run: cursorLineStart, shift: selectLineStart },
        { key: "ArrowRight", run: cursorCharRight, shift: selectCharRight },
        { key: "Mod-ArrowRight", mac: "Alt-ArrowRight", run: cursorGroupRight, shift: selectGroupRight },
        { mac: "Cmd-ArrowRight", run: cursorLineEnd, shift: selectLineEnd },
        { key: "ArrowUp", run: cursorLineUp, shift: selectLineUp },
        { mac: "Cmd-ArrowUp", run: cursorDocStart, shift: selectDocStart },
        { mac: "Ctrl-ArrowUp", run: cursorPageUp, shift: selectPageUp },
        { key: "ArrowDown", run: cursorLineDown, shift: selectLineDown },
        { mac: "Cmd-ArrowDown", run: cursorDocEnd, shift: selectDocEnd },
        { mac: "Ctrl-ArrowDown", run: cursorPageDown, shift: selectPageDown },
        { key: "PageUp", run: cursorPageUp, shift: selectPageUp },
        { key: "PageDown", run: cursorPageDown, shift: selectPageDown },
        { key: "Home", run: cursorLineBoundaryBackward, shift: selectLineBoundaryBackward },
        { key: "Mod-Home", run: cursorDocStart, shift: selectDocStart },
        { key: "End", run: cursorLineBoundaryForward, shift: selectLineBoundaryForward },
        { key: "Mod-End", run: cursorDocEnd, shift: selectDocEnd },
        { key: "Enter", run: insertNewlineAndIndent },
        { key: "Mod-a", run: selectAll },
        { key: "Backspace", run: deleteCodePointBackward },
        { key: "Delete", run: deleteCharForward },
        { key: "Mod-Backspace", mac: "Alt-Backspace", run: deleteGroupBackward },
        { key: "Mod-Delete", mac: "Alt-Delete", run: deleteGroupForward },
    ].concat(emacsStyleKeymap.map(b => ({ mac: b.key, run: b.run, shift: b.shift })));
    /// The default keymap. Includes all bindings from
    /// [`standardKeymap`](#commands.standardKeymap) plus the following:
    ///
    /// - Alt-ArrowLeft (Ctrl-ArrowLeft on macOS): [`cursorSyntaxLeft`](#commands.cursorSyntaxLeft) ([`selectSyntaxLeft`](#commands.selectSyntaxLeft) with Shift)
    /// - Alt-ArrowRight (Ctrl-ArrowRight on macOS): [`cursorSyntaxRight`](#commands.cursorSyntaxRight) ([`selectSyntaxRight`](#commands.selectSyntaxRight) with Shift)
    /// - Alt-ArrowUp: [`moveLineUp`](#commands.moveLineUp)
    /// - Alt-ArrowDown: [`moveLineDown`](#commands.moveLineDown)
    /// - Shift-Alt-ArrowUp: [`copyLineUp`](#commands.copyLineUp)
    /// - Shift-Alt-ArrowDown: [`copyLineDown`](#commands.copyLineDown)
    /// - Escape: [`simplifySelection`](#commands.simplifySelection)
    /// - Ctrl-l (Cmd-l on macOS): [`selectLine`](#commands.selectLine)
    /// - Ctrl-i (Cmd-i on macOS): [`selectParentSyntax`](#commands.selectParentSyntax)
    /// - Ctrl-[ (Cmd-[ on macOS): [`indentLess`](#commands.indentLess)
    /// - Ctrl-] (Cmd-] on macOS): [`indentMore`](#commands.indentMore)
    /// - Ctrl-Alt-\\ (Cmd-Alt-\\ on macOS): [`indentSelection`](#commands.indentSelection)
    /// - Shift-Ctrl-k (Shift-Cmd-k on macOS): [`deleteLine`](#commands.deleteLine)
    /// - Shift-Ctrl-\\ (Shift-Cmd-\\ on macOS): [`cursorMatchingBracket`](#commands.cursorMatchingBracket)
    const defaultKeymap = [
        { key: "Alt-ArrowLeft", mac: "Ctrl-ArrowLeft", run: cursorSyntaxLeft, shift: selectSyntaxLeft },
        { key: "Alt-ArrowRight", mac: "Ctrl-ArrowRight", run: cursorSyntaxRight, shift: selectSyntaxRight },
        { key: "Alt-ArrowUp", run: moveLineUp },
        { key: "Shift-Alt-ArrowUp", run: copyLineUp },
        { key: "Alt-ArrowDown", run: moveLineDown },
        { key: "Shift-Alt-ArrowDown", run: copyLineDown },
        { key: "Escape", run: simplifySelection },
        { key: "Mod-l", run: selectLine },
        { key: "Mod-i", run: selectParentSyntax },
        { key: "Mod-[", run: indentLess },
        { key: "Mod-]", run: indentMore },
        { key: "Mod-Alt-\\", run: indentSelection },
        { key: "Shift-Mod-k", run: deleteLine },
        { key: "Shift-Mod-\\", run: cursorMatchingBracket }
    ].concat(standardKeymap);

    const defaults$1 = {
        brackets: ["(", "[", "{", "'", '"'],
        before: ")]}'\":;>"
    };
    const closeBracketEffect = StateEffect.define({
        map(value, mapping) {
            let mapped = mapping.mapPos(value, -1, MapMode.TrackAfter);
            return mapped == null ? undefined : mapped;
        }
    });
    const skipBracketEffect = StateEffect.define({
        map(value, mapping) { return mapping.mapPos(value); }
    });
    const closedBracket = new class extends RangeValue {
    };
    closedBracket.startSide = 1;
    closedBracket.endSide = -1;
    const bracketState = StateField.define({
        create() { return RangeSet.empty; },
        update(value, tr) {
            if (tr.selection) {
                let lineStart = tr.state.doc.lineAt(tr.selection.primary.head).from;
                let prevLineStart = tr.startState.doc.lineAt(tr.startState.selection.primary.head).from;
                if (lineStart != tr.changes.mapPos(prevLineStart, -1))
                    value = RangeSet.empty;
            }
            value = value.map(tr.changes);
            for (let effect of tr.effects) {
                if (effect.is(closeBracketEffect))
                    value = value.update({ add: [closedBracket.range(effect.value, effect.value + 1)] });
                else if (effect.is(skipBracketEffect))
                    value = value.update({ filter: from => from != effect.value });
            }
            return value;
        }
    });
    /// Extension to enable bracket-closing behavior. When a closeable
    /// bracket is typed, its closing bracket is immediately inserted
    /// after the cursor. When closing a bracket directly in front of that
    /// closing bracket, the cursor moves over the existing bracket.
    function closeBrackets() {
        return [EditorView.inputHandler.of(handleInput), bracketState];
    }
    const definedClosing = "()[]{}<>";
    function closing(ch) {
        for (let i = 0; i < definedClosing.length; i += 2)
            if (definedClosing.charCodeAt(i) == ch)
                return definedClosing.charAt(i + 1);
        return fromCodePoint(ch < 128 ? ch : ch + 1);
    }
    function config(state, pos) {
        return state.languageDataAt("closeBrackets", pos)[0] || defaults$1;
    }
    function handleInput(view, from, to, insert) {
        if (view.composing)
            return false;
        let sel = view.state.selection.primary;
        if (insert.length > 2 || insert.length == 2 && codePointSize(codePointAt(insert, 0)) == 1 ||
            from != sel.from || to != sel.to)
            return false;
        let tr = handleInsertion(view.state, insert);
        if (!tr)
            return false;
        view.dispatch(tr);
        return true;
    }
    /// Command that implements deleting a pair of matching brackets when
    /// the cursor is between them.
    const deleteBracketPair = ({ state, dispatch }) => {
        let conf = config(state, state.selection.primary.head);
        let tokens = conf.brackets || defaults$1.brackets;
        let dont = null, changes = state.changeByRange(range => {
            if (range.empty) {
                let before = prevChar(state.doc, range.head);
                for (let token of tokens) {
                    if (token == before && nextChar(state.doc, range.head) == closing(codePointAt(token, 0)))
                        return { changes: { from: range.head - token.length, to: range.head + token.length },
                            range: EditorSelection.cursor(range.head - token.length),
                            annotations: Transaction.userEvent.of("delete") };
                }
            }
            return { range: dont = range };
        });
        if (!dont)
            dispatch(state.update(changes, { scrollIntoView: true }));
        return !dont;
    };
    /// Close-brackets related key bindings. Binds Backspace to
    /// [`deleteBracketPair`](#closebrackets.deleteBracketPair).
    const closeBracketsKeymap = [
        { key: "Backspace", run: deleteBracketPair }
    ];
    /// Implements the extension's behavior on text insertion. @internal
    function handleInsertion(state, ch) {
        let conf = config(state, state.selection.primary.head);
        let tokens = conf.brackets || defaults$1.brackets;
        for (let tok of tokens) {
            let closed = closing(codePointAt(tok, 0));
            if (ch == tok)
                return closed == tok ? handleSame(state, tok, tokens.indexOf(tok + tok + tok) > -1)
                    : handleOpen(state, tok, closed, conf.before || defaults$1.before);
            if (ch == closed && closedBracketAt(state, state.selection.primary.from))
                return handleClose(state, tok, closed);
        }
        return null;
    }
    function closedBracketAt(state, pos) {
        let found = false;
        state.field(bracketState).between(0, state.doc.length, from => {
            if (from == pos)
                found = true;
        });
        return found;
    }
    function nextChar(doc, pos) {
        let next = doc.sliceString(pos, pos + 2);
        return next.slice(0, codePointSize(codePointAt(next, 0)));
    }
    function prevChar(doc, pos) {
        let prev = doc.sliceString(pos - 2, pos);
        return codePointSize(codePointAt(prev, 0)) == prev.length ? prev : prev.slice(1);
    }
    function handleOpen(state, open, close, closeBefore) {
        let dont = null, changes = state.changeByRange(range => {
            if (!range.empty)
                return { changes: [{ insert: open, from: range.from }, { insert: close, from: range.to }],
                    effects: closeBracketEffect.of(range.to + open.length),
                    range: EditorSelection.range(range.anchor + open.length, range.head + open.length) };
            let next = nextChar(state.doc, range.head);
            if (!next || /\s/.test(next) || closeBefore.indexOf(next) > -1)
                return { changes: { insert: open + close, from: range.head },
                    effects: closeBracketEffect.of(range.head + open.length),
                    range: EditorSelection.cursor(range.head + open.length) };
            return { range: dont = range };
        });
        return dont ? null : state.update(changes, {
            scrollIntoView: true,
            annotations: Transaction.userEvent.of("input")
        });
    }
    function handleClose(state, _open, close) {
        let dont = null, moved = state.selection.ranges.map(range => {
            if (range.empty && nextChar(state.doc, range.head) == close)
                return EditorSelection.cursor(range.head + close.length);
            return dont = range;
        });
        return dont ? null : state.update({
            selection: EditorSelection.create(moved, state.selection.primaryIndex),
            scrollIntoView: true,
            effects: state.selection.ranges.map(({ from }) => skipBracketEffect.of(from))
        });
    }
    // Handles cases where the open and close token are the same, and
    // possibly triple quotes (as in `"""abc"""`-style quoting).
    function handleSame(state, token, allowTriple) {
        let dont = null, changes = state.changeByRange(range => {
            if (!range.empty)
                return { changes: [{ insert: token, from: range.from }, { insert: token, from: range.to }],
                    effects: closeBracketEffect.of(range.to + token.length),
                    range: EditorSelection.range(range.anchor + token.length, range.head + token.length) };
            let pos = range.head, next = nextChar(state.doc, pos);
            if (next == token) {
                if (nodeStart(state, pos)) {
                    return { changes: { insert: token + token, from: pos },
                        effects: closeBracketEffect.of(pos + token.length),
                        range: EditorSelection.cursor(pos + token.length) };
                }
                else if (closedBracketAt(state, pos)) {
                    let isTriple = allowTriple && state.sliceDoc(pos, pos + token.length * 3) == token + token + token;
                    return { range: EditorSelection.cursor(pos + token.length * (isTriple ? 3 : 1)),
                        effects: skipBracketEffect.of(pos) };
                }
            }
            else if (allowTriple && state.sliceDoc(pos - 2 * token.length, pos) == token + token &&
                nodeStart(state, pos - 2 * token.length)) {
                return { changes: { insert: token + token + token + token, from: pos },
                    effects: closeBracketEffect.of(pos + token.length),
                    range: EditorSelection.cursor(pos + token.length) };
            }
            else if (state.charCategorizer(pos)(next) != CharCategory.Word) {
                let prev = state.sliceDoc(pos - 1, pos);
                if (prev != token && state.charCategorizer(pos)(prev) != CharCategory.Word)
                    return { changes: { insert: token + token, from: pos },
                        effects: closeBracketEffect.of(pos + token.length),
                        range: EditorSelection.cursor(pos + token.length) };
            }
            return { range: dont = range };
        });
        return dont ? null : state.update(changes, {
            scrollIntoView: true,
            annotations: Transaction.userEvent.of("input")
        });
    }
    function nodeStart(state, pos) {
        let tree = syntaxTree(state).resolve(pos + 1);
        return tree.parent && tree.from == pos;
    }

    const panelConfig = Facet.define({
        combine(configs) {
            let topContainer, bottomContainer;
            for (let c of configs) {
                topContainer = topContainer || c.topContainer;
                bottomContainer = bottomContainer || c.bottomContainer;
            }
            return { topContainer, bottomContainer };
        }
    });
    /// Enables the panel-managing extension.
    function panels(config) {
        let ext = [panelPlugin, baseTheme$4];
        if (config)
            ext.push(panelConfig.of(config));
        return ext;
    }
    /// Opening a panel is done by providing an object describing the
    /// panel through this facet.
    const showPanel = Facet.define();
    /// Get the active panel created by the given constructor, if any.
    /// This can be useful when you need access to your panels' DOM
    /// structure.
    function getPanel(view, panel) {
        let plugin = view.plugin(panelPlugin);
        let index = view.state.facet(showPanel).indexOf(panel);
        return plugin && index > -1 ? plugin.panels[index] : null;
    }
    const panelPlugin = ViewPlugin.fromClass(class {
        constructor(view) {
            this.specs = view.state.facet(showPanel);
            this.panels = this.specs.map(spec => spec(view));
            let conf = view.state.facet(panelConfig);
            this.top = new PanelGroup(view, true, conf.topContainer);
            this.bottom = new PanelGroup(view, false, conf.bottomContainer);
            this.top.sync(this.panels.filter(p => p.top));
            this.bottom.sync(this.panels.filter(p => !p.top));
            for (let p of this.panels) {
                p.dom.className += " " + panelClass(p);
                if (p.mount)
                    p.mount();
            }
        }
        update(update) {
            let conf = update.state.facet(panelConfig);
            if (this.top.container != conf.topContainer) {
                this.top.sync([]);
                this.top = new PanelGroup(update.view, true, conf.topContainer);
            }
            if (this.bottom.container != conf.bottomContainer) {
                this.bottom.sync([]);
                this.bottom = new PanelGroup(update.view, false, conf.bottomContainer);
            }
            this.top.syncClasses();
            this.bottom.syncClasses();
            let specs = update.state.facet(showPanel);
            if (specs != this.specs) {
                let panels = [], top = [], bottom = [], mount = [];
                for (let spec of specs) {
                    let known = this.specs.indexOf(spec), panel;
                    if (known < 0) {
                        panel = spec(update.view);
                        mount.push(panel);
                    }
                    else {
                        panel = this.panels[known];
                        if (panel.update)
                            panel.update(update);
                    }
                    panels.push(panel);
                    (panel.top ? top : bottom).push(panel);
                }
                this.specs = specs;
                this.panels = panels;
                this.top.sync(top);
                this.bottom.sync(bottom);
                for (let p of mount) {
                    p.dom.className += " " + panelClass(p);
                    if (p.mount)
                        p.mount();
                }
            }
            else {
                for (let p of this.panels)
                    if (p.update)
                        p.update(update);
            }
        }
        destroy() {
            this.top.sync([]);
            this.bottom.sync([]);
        }
    }, {
        provide: PluginField.scrollMargins.from(value => ({ top: value.top.scrollMargin(), bottom: value.bottom.scrollMargin() }))
    });
    function panelClass(panel) {
        return themeClass(panel.style ? `panel.${panel.style}` : "panel");
    }
    class PanelGroup {
        constructor(view, top, container) {
            this.view = view;
            this.top = top;
            this.container = container;
            this.dom = undefined;
            this.classes = "";
            this.panels = [];
            this.syncClasses();
        }
        sync(panels) {
            this.panels = panels;
            this.syncDOM();
        }
        syncDOM() {
            if (this.panels.length == 0) {
                if (this.dom) {
                    this.dom.remove();
                    this.dom = undefined;
                }
                return;
            }
            if (!this.dom) {
                this.dom = document.createElement("div");
                this.dom.className = themeClass(this.top ? "panels.top" : "panels.bottom");
                this.dom.style[this.top ? "top" : "bottom"] = "0";
                let parent = this.container || this.view.dom;
                parent.insertBefore(this.dom, this.top ? parent.firstChild : null);
            }
            let curDOM = this.dom.firstChild;
            for (let panel of this.panels) {
                if (panel.dom.parentNode == this.dom) {
                    while (curDOM != panel.dom)
                        curDOM = rm$1(curDOM);
                    curDOM = curDOM.nextSibling;
                }
                else {
                    this.dom.insertBefore(panel.dom, curDOM);
                }
            }
            while (curDOM)
                curDOM = rm$1(curDOM);
        }
        scrollMargin() {
            return !this.dom || this.container ? 0
                : Math.max(0, this.top ? this.dom.getBoundingClientRect().bottom - this.view.scrollDOM.getBoundingClientRect().top
                    : this.view.scrollDOM.getBoundingClientRect().bottom - this.dom.getBoundingClientRect().top);
        }
        syncClasses() {
            if (!this.container || this.classes == this.view.themeClasses)
                return;
            for (let cls of this.classes.split(" "))
                if (cls)
                    this.container.classList.remove(cls);
            for (let cls of (this.classes = this.view.themeClasses).split(" "))
                if (cls)
                    this.container.classList.add(cls);
        }
    }
    function rm$1(node) {
        let next = node.nextSibling;
        node.remove();
        return next;
    }
    const baseTheme$4 = EditorView.baseTheme({
        $panels: {
            boxSizing: "border-box",
            position: "sticky",
            left: 0,
            right: 0
        },
        "$$light $panels": {
            backgroundColor: "#f5f5f5",
            color: "black"
        },
        "$$light $panels.top": {
            borderBottom: "1px solid #ddd"
        },
        "$$light $panels.bottom": {
            borderTop: "1px solid #ddd"
        },
        "$$dark $panels": {
            backgroundColor: "#333338",
            color: "white"
        }
    });

    const basicNormalize = typeof String.prototype.normalize == "function" ? x => x.normalize("NFKD") : x => x;
    /// A search cursor provides an iterator over text matches in a
    /// document.
    class SearchCursor {
        /// Create a text cursor. The query is the search string, `from` to
        /// `to` provides the region to search.
        ///
        /// When `normalize` is given, it will be called, on both the query
        /// string and the content it is matched against, before comparing.
        /// You can, for example, create a case-insensitive search by
        /// passing `s => s.toLowerCase()`.
        ///
        /// Text is always normalized with
        /// [`.normalize("NFKD")`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/normalize)
        /// (when supported).
        constructor(text, query, from = 0, to = text.length, normalize) {
            /// The current match (only holds a meaningful value after
            /// [`next`](#search.SearchCursor.next) has been called and when
            /// `done` is false).
            this.value = { from: 0, to: 0 };
            /// Whether the end of the iterated region has been reached.
            this.done = false;
            this.matches = [];
            this.buffer = "";
            this.bufferPos = 0;
            this.iter = text.iterRange(from, to);
            this.bufferStart = from;
            this.normalize = normalize ? x => normalize(basicNormalize(x)) : basicNormalize;
            this.query = this.normalize(query);
        }
        peek() {
            if (this.bufferPos == this.buffer.length) {
                this.bufferStart += this.buffer.length;
                this.iter.next();
                if (this.iter.done)
                    return -1;
                this.bufferPos = 0;
                this.buffer = this.iter.value;
            }
            return this.buffer.charCodeAt(this.bufferPos);
        }
        /// Look for the next match. Updates the iterator's
        /// [`value`](#search.SearchCursor.value) and
        /// [`done`](#search.SearchCursor.done) properties. Should be called
        /// at least once before using the cursor.
        next() {
            for (;;) {
                let next = this.peek();
                if (next < 0) {
                    this.done = true;
                    return this;
                }
                let str = String.fromCharCode(next), start = this.bufferStart + this.bufferPos;
                this.bufferPos++;
                for (;;) {
                    let peek = this.peek();
                    if (peek < 0xDC00 || peek >= 0xE000)
                        break;
                    this.bufferPos++;
                    str += String.fromCharCode(peek);
                }
                let norm = this.normalize(str);
                for (let i = 0, pos = start;; i++) {
                    let code = norm.charCodeAt(i);
                    let match = this.match(code, pos);
                    if (match) {
                        this.value = match;
                        return this;
                    }
                    if (i == norm.length - 1)
                        break;
                    if (pos == start && i < str.length && str.charCodeAt(i) == code)
                        pos++;
                }
            }
        }
        match(code, pos) {
            let match = null;
            for (let i = 0; i < this.matches.length; i += 2) {
                let index = this.matches[i], keep = false;
                if (this.query.charCodeAt(index) == code) {
                    if (index == this.query.length - 1) {
                        match = { from: this.matches[i + 1], to: pos + 1 };
                    }
                    else {
                        this.matches[i]++;
                        keep = true;
                    }
                }
                if (!keep) {
                    this.matches.splice(i, 2);
                    i -= 2;
                }
            }
            if (this.query.charCodeAt(0) == code) {
                if (this.query.length == 1)
                    match = { from: pos, to: pos + 1 };
                else
                    this.matches.push(1, pos);
            }
            return match;
        }
    }

    class Query {
        constructor(search, replace, caseInsensitive) {
            this.search = search;
            this.replace = replace;
            this.caseInsensitive = caseInsensitive;
        }
        eq(other) {
            return this.search == other.search && this.replace == other.replace && this.caseInsensitive == other.caseInsensitive;
        }
        cursor(doc, from = 0, to = doc.length) {
            return new SearchCursor(doc, this.search, from, to, this.caseInsensitive ? x => x.toLowerCase() : undefined);
        }
        get valid() { return !!this.search; }
    }
    const setQuery = StateEffect.define();
    const togglePanel = StateEffect.define();
    const searchState = StateField.define({
        create() {
            return new SearchState(new Query("", "", false), []);
        },
        update(value, tr) {
            for (let effect of tr.effects) {
                if (effect.is(setQuery))
                    value = new SearchState(effect.value, value.panel);
                else if (effect.is(togglePanel))
                    value = new SearchState(value.query, effect.value ? [createSearchPanel] : []);
            }
            return value;
        },
        provide: [showPanel.nFrom(s => s.panel)]
    });
    class SearchState {
        constructor(query, panel) {
            this.query = query;
            this.panel = panel;
        }
    }
    const matchMark = Decoration.mark({ class: themeClass("searchMatch") }), selectedMatchMark = Decoration.mark({ class: themeClass("searchMatch.selected") });
    const searchHighlighter = ViewPlugin.fromClass(class {
        constructor(view) {
            this.view = view;
            this.decorations = this.highlight(view.state.field(searchState));
        }
        update(update) {
            let state = update.state.field(searchState);
            if (state != update.prevState.field(searchState) || update.docChanged || update.selectionSet)
                this.decorations = this.highlight(state);
        }
        highlight({ query, panel }) {
            if (!panel.length || !query.valid)
                return Decoration.none;
            let state = this.view.state, viewport = this.view.viewport;
            let cursor = query.cursor(state.doc, Math.max(0, viewport.from - query.search.length), Math.min(viewport.to + query.search.length, state.doc.length));
            let builder = new RangeSetBuilder();
            while (!cursor.next().done) {
                let { from, to } = cursor.value;
                let selected = state.selection.ranges.some(r => r.from == from && r.to == to);
                builder.add(from, to, selected ? selectedMatchMark : matchMark);
            }
            return builder.finish();
        }
    }, {
        decorations: v => v.decorations
    });
    function searchCommand(f) {
        return view => {
            let state = view.state.field(searchState, false);
            return state && state.query.valid ? f(view, state) : openSearchPanel(view);
        };
    }
    function findNextMatch(doc, from, query) {
        let cursor = query.cursor(doc, from).next();
        if (cursor.done) {
            cursor = query.cursor(doc, 0, from + query.search.length - 1).next();
            if (cursor.done)
                return null;
        }
        return cursor.value;
    }
    /// Open the search panel if it isn't already open, and move the
    /// selection to the first match after the current primary selection.
    /// Will wrap around to the start of the document when it reaches the
    /// end.
    const findNext = searchCommand((view, state) => {
        let { from, to } = view.state.selection.primary;
        let next = findNextMatch(view.state.doc, view.state.selection.primary.from + 1, state.query);
        if (!next || next.from == from && next.to == to)
            return false;
        view.dispatch({ selection: { anchor: next.from, head: next.to }, scrollIntoView: true });
        maybeAnnounceMatch(view);
        return true;
    });
    const FindPrevChunkSize = 10000;
    // Searching in reverse is, rather than implementing inverted search
    // cursor, done by scanning chunk after chunk forward.
    function findPrevInRange(query, doc, from, to) {
        for (let pos = to;;) {
            let start = Math.max(from, pos - FindPrevChunkSize - query.search.length);
            let cursor = query.cursor(doc, start, pos), range = null;
            while (!cursor.next().done)
                range = cursor.value;
            if (range)
                return range;
            if (start == from)
                return null;
            pos -= FindPrevChunkSize;
        }
    }
    /// Move the selection to the previous instance of the search query,
    /// before the current primary selection. Will wrap past the start
    /// of the document to start searching at the end again.
    const findPrevious = searchCommand((view, { query }) => {
        let { state } = view;
        let range = findPrevInRange(query, state.doc, 0, state.selection.primary.to - 1) ||
            findPrevInRange(query, state.doc, state.selection.primary.from + 1, state.doc.length);
        if (!range)
            return false;
        view.dispatch({ selection: { anchor: range.from, head: range.to }, scrollIntoView: true });
        maybeAnnounceMatch(view);
        return true;
    });
    /// Select all instances of the search query.
    const selectMatches = searchCommand((view, { query }) => {
        let cursor = query.cursor(view.state.doc), ranges = [];
        while (!cursor.next().done)
            ranges.push(EditorSelection.range(cursor.value.from, cursor.value.to));
        if (!ranges.length)
            return false;
        view.dispatch({ selection: EditorSelection.create(ranges) });
        return true;
    });
    /// Select all instances of the currently selected text.
    const selectSelectionMatches = ({ state, dispatch }) => {
        let sel = state.selection;
        if (sel.ranges.length > 1 || sel.primary.empty)
            return false;
        let { from, to } = sel.primary;
        let ranges = [], primary = 0;
        for (let cur = new SearchCursor(state.doc, state.sliceDoc(from, to)); !cur.next().done;) {
            if (ranges.length > 1000)
                return false;
            if (cur.value.from == from)
                primary = ranges.length;
            ranges.push(EditorSelection.range(cur.value.from, cur.value.to));
        }
        dispatch(state.update({ selection: new EditorSelection(ranges, primary) }));
        return true;
    };
    /// Replace the current match of the search query.
    const replaceNext = searchCommand((view, { query }) => {
        let { state } = view, next = findNextMatch(state.doc, state.selection.primary.from, query);
        if (!next)
            return false;
        let { from, to } = state.selection.primary, changes = [], selection;
        if (next.from == from && next.to == to) {
            changes.push({ from: next.from, to: next.to, insert: query.replace });
            next = findNextMatch(state.doc, next.to, query);
        }
        if (next) {
            let off = changes.length == 0 || changes[0].from >= next.to ? 0 : next.to - next.from - query.replace.length;
            selection = { anchor: next.from - off, head: next.to - off };
        }
        view.dispatch({ changes, selection, scrollIntoView: !!selection });
        if (next)
            maybeAnnounceMatch(view);
        return true;
    });
    /// Replace all instances of the search query with the given
    /// replacement.
    const replaceAll = searchCommand((view, { query }) => {
        let cursor = query.cursor(view.state.doc), changes = [];
        while (!cursor.next().done) {
            let { from, to } = cursor.value;
            changes.push({ from, to, insert: query.replace });
        }
        if (!changes.length)
            return false;
        view.dispatch({ changes });
        return true;
    });
    function createSearchPanel(view) {
        let { query } = view.state.field(searchState);
        return {
            dom: buildPanel({
                view,
                query,
                updateQuery(q) {
                    if (!query.eq(q)) {
                        query = q;
                        view.dispatch({ effects: setQuery.of(query) });
                    }
                }
            }),
            mount() {
                this.dom.querySelector("[name=search]").select();
            },
            pos: 80,
            style: "search"
        };
    }
    /// Make sure the search panel is open and focused.
    const openSearchPanel = view => {
        let state = view.state.field(searchState, false);
        if (state && state.panel.length) {
            let panel = getPanel(view, createSearchPanel);
            if (!panel)
                return false;
            panel.dom.querySelector("[name=search]").focus();
        }
        else {
            view.dispatch({ effects: togglePanel.of(true),
                reconfigure: state ? undefined : { append: searchExtensions } });
        }
        return true;
    };
    /// Close the search panel.
    const closeSearchPanel = view => {
        let state = view.state.field(searchState, false);
        if (!state || !state.panel.length)
            return false;
        let panel = getPanel(view, createSearchPanel);
        if (panel && panel.dom.contains(view.root.activeElement))
            view.focus();
        view.dispatch({ effects: togglePanel.of(false) });
        return true;
    };
    /// Default search-related key bindings.
    ///
    ///  * Mod-f: [`openSearchPanel`](#search.openSearchPanel)
    ///  * F3, Mod-g: [`findNext`](#search.findNext)
    ///  * Shift-F3, Shift-Mod-g: [`findPrevious`](#search.findPrevious)
    const searchKeymap = [
        { key: "Mod-f", run: openSearchPanel, scope: "editor search-panel" },
        { key: "F3", run: findNext, shift: findPrevious, scope: "editor search-panel" },
        { key: "Mod-g", run: findNext, shift: findPrevious, scope: "editor search-panel" },
        { key: "Escape", run: closeSearchPanel, scope: "editor search-panel" },
        { key: "Mod-Shift-l", run: selectSelectionMatches }
    ];
    function elt(name, props = null, children = []) {
        let e = document.createElement(name);
        if (props)
            for (let prop in props) {
                let value = props[prop];
                if (typeof value == "string")
                    e.setAttribute(prop, value);
                else
                    e[prop] = value;
            }
        for (let child of children)
            e.appendChild(typeof child == "string" ? document.createTextNode(child) : child);
        return e;
    }
    // FIXME sync when search state changes independently
    function buildPanel(conf) {
        function p(phrase) { return conf.view.state.phrase(phrase); }
        let searchField = elt("input", {
            value: conf.query.search,
            placeholder: p("Find"),
            "aria-label": p("Find"),
            class: themeClass("textfield"),
            name: "search",
            onchange: update,
            onkeyup: update
        });
        let replaceField = elt("input", {
            value: conf.query.replace,
            placeholder: p("Replace"),
            "aria-label": p("Replace"),
            class: themeClass("textfield"),
            name: "replace",
            onchange: update,
            onkeyup: update
        });
        let caseField = elt("input", {
            type: "checkbox",
            name: "case",
            checked: !conf.query.caseInsensitive,
            onchange: update
        });
        function update() {
            conf.updateQuery(new Query(searchField.value, replaceField.value, !caseField.checked));
        }
        function keydown(e) {
            if (runScopeHandlers(conf.view, e, "search-panel")) {
                e.preventDefault();
            }
            else if (e.keyCode == 13 && e.target == searchField) {
                e.preventDefault();
                (e.shiftKey ? findPrevious : findNext)(conf.view);
            }
            else if (e.keyCode == 13 && e.target == replaceField) {
                e.preventDefault();
                replaceNext(conf.view);
            }
        }
        function button(name, onclick, content) {
            return elt("button", { class: themeClass("button"), name, onclick }, content);
        }
        let panel = elt("div", { onkeydown: keydown }, [
            searchField,
            button("next", () => findNext(conf.view), [p("next")]),
            button("prev", () => findPrevious(conf.view), [p("previous")]),
            button("select", () => selectMatches(conf.view), [p("all")]),
            elt("label", null, [caseField, "match case"]),
            elt("br"),
            replaceField,
            button("replace", () => replaceNext(conf.view), [p("replace")]),
            button("replaceAll", () => replaceAll(conf.view), [p("replace all")]),
            elt("button", { name: "close", onclick: () => closeSearchPanel(conf.view), "aria-label": p("close") }, ["×"]),
            elt("div", { style: "position: absolute; top: -10000px", "aria-live": "polite" })
        ]);
        return panel;
    }
    const AnnounceMargin = 30;
    const Break = /[\s\.,:;?!]/;
    // FIXME this is a kludge
    function maybeAnnounceMatch(view) {
        let { from, to } = view.state.selection.primary;
        let lineStart = view.state.doc.lineAt(from).from, lineEnd = view.state.doc.lineAt(to).to;
        let start = Math.max(lineStart, from - AnnounceMargin), end = Math.min(lineEnd, to + AnnounceMargin);
        let text = view.state.sliceDoc(start, end);
        if (start != lineStart) {
            for (let i = 0; i < AnnounceMargin; i++)
                if (!Break.test(text[i + 1]) && Break.test(text[i])) {
                    text = text.slice(i);
                    break;
                }
        }
        if (end != lineEnd) {
            for (let i = text.length - 1; i > text.length - AnnounceMargin; i--)
                if (!Break.test(text[i - 1]) && Break.test(text[i])) {
                    text = text.slice(0, i);
                    break;
                }
        }
        let panel = getPanel(view, createSearchPanel);
        if (!panel || !panel.dom.contains(view.root.activeElement))
            return;
        let live = panel.dom.querySelector("div[aria-live]");
        live.textContent = view.state.phrase("current match") + ". " + text;
    }
    const baseTheme$5 = EditorView.baseTheme({
        "$panel.search": {
            padding: "2px 6px 4px",
            position: "relative",
            "& [name=close]": {
                position: "absolute",
                top: "0",
                right: "4px",
                backgroundColor: "inherit",
                border: "none",
                font: "inherit",
                padding: 0,
                margin: 0
            },
            "& input, & button": {
                margin: ".2em .5em .2em 0"
            },
            "& label": {
                fontSize: "80%"
            }
        },
        "$$light $searchMatch": { backgroundColor: "#ffff0054" },
        "$$dark $searchMatch": { backgroundColor: "#00ffff8a" },
        "$$light $searchMatch.selected": { backgroundColor: "#ff6a0054" },
        "$$dark $searchMatch.selected": { backgroundColor: "#ff00ff8a" }
    });
    const searchExtensions = [
        searchState,
        precedence(searchHighlighter, "override"),
        panels(),
        baseTheme$5
    ];

    const Outside = "-10000px";
    const tooltipPlugin = ViewPlugin.fromClass(class {
        constructor(view) {
            this.view = view;
            this.inView = true;
            this.measureReq = { read: this.readMeasure.bind(this), write: this.writeMeasure.bind(this), key: this };
            this.tooltips = view.state.facet(showTooltip);
            this.tooltipViews = this.tooltips.map(tp => this.createTooltip(tp));
        }
        update(update) {
            let tooltips = update.state.facet(showTooltip);
            if (tooltips == this.tooltips) {
                for (let t of this.tooltipViews)
                    if (t.update)
                        t.update(update);
            }
            else {
                let views = [];
                for (let i = 0; i < tooltips.length; i++) {
                    let tip = tooltips[i], known = -1;
                    for (let i = 0; i < this.tooltips.length; i++)
                        if (this.tooltips[i].create == tip.create)
                            known = i;
                    if (known < 0) {
                        views[i] = this.createTooltip(tip);
                    }
                    else {
                        let tooltipView = views[i] = this.tooltipViews[known];
                        if (tooltipView.update)
                            tooltipView.update(update);
                    }
                }
                for (let t of this.tooltipViews)
                    if (views.indexOf(t) < 0)
                        t.dom.remove();
                this.tooltips = tooltips;
                this.tooltipViews = views;
                this.maybeMeasure();
            }
        }
        createTooltip(tooltip) {
            let tooltipView = tooltip.create(this.view);
            tooltipView.dom.className = themeClass("tooltip" + (tooltip.style ? "." + tooltip.style : ""));
            this.view.dom.appendChild(tooltipView.dom);
            if (tooltipView.mount)
                tooltipView.mount(this.view);
            return tooltipView;
        }
        destroy() {
            for (let { dom } of this.tooltipViews)
                dom.remove();
        }
        readMeasure() {
            return {
                editor: this.view.dom.getBoundingClientRect(),
                pos: this.tooltips.map(t => this.view.coordsAtPos(t.pos)),
                size: this.tooltipViews.map(({ dom }) => dom.getBoundingClientRect()),
                innerWidth: window.innerWidth,
                innerHeight: window.innerHeight
            };
        }
        writeMeasure(measured) {
            let { editor } = measured;
            for (let i = 0; i < this.tooltipViews.length; i++) {
                let tooltip = this.tooltips[i], tView = this.tooltipViews[i], { dom } = tView;
                let pos = measured.pos[i], size = measured.size[i];
                // Hide tooltips that are outside of the editor.
                if (!pos || pos.bottom <= editor.top || pos.top >= editor.bottom || pos.right <= editor.left || pos.left >= editor.right) {
                    dom.style.top = Outside;
                    continue;
                }
                let width = size.right - size.left, height = size.bottom - size.top;
                let left = this.view.textDirection == Direction.LTR ? Math.min(pos.left, measured.innerWidth - width)
                    : Math.max(0, pos.left - width);
                let above = !!tooltip.above;
                if (!tooltip.strictSide &&
                    (above ? pos.top - (size.bottom - size.top) < 0 : pos.bottom + (size.bottom - size.top) > measured.innerHeight))
                    above = !above;
                dom.style.top = (above ? pos.top - height : pos.bottom) + "px";
                dom.style.left = left + "px";
                dom.classList.toggle("cm-tooltip-above", above);
                dom.classList.toggle("cm-tooltip-below", !above);
                if (tView.positioned)
                    tView.positioned();
            }
        }
        maybeMeasure() {
            if (this.tooltips.length) {
                if (this.view.inView || this.inView)
                    this.view.requestMeasure(this.measureReq);
                this.inView = this.view.inView;
            }
        }
    }, {
        eventHandlers: {
            scroll() { this.maybeMeasure(); }
        }
    });
    const baseTheme$6 = EditorView.baseTheme({
        $tooltip: {
            position: "fixed",
            border: "1px solid #ddd",
            backgroundColor: "#f5f5f5",
            zIndex: 100
        }
    });
    /// Supporting extension for displaying tooltips. Allows
    /// [`showTooltip`](#tooltip.showTooltip) to be used to define
    /// tooltips.
    function tooltips() {
        return [tooltipPlugin, baseTheme$6];
    }
    /// Behavior by which an extension can provide a tooltip to be shown.
    const showTooltip = Facet.define();
    const HoverTime = 750, HoverMaxDist = 10;
    class HoverPlugin {
        constructor(view, source, field, setHover) {
            this.view = view;
            this.source = source;
            this.field = field;
            this.setHover = setHover;
            this.lastMouseMove = null;
            this.hoverTimeout = -1;
            this.mouseInside = false;
            this.checkHover = this.checkHover.bind(this);
            view.dom.addEventListener("mouseenter", this.mouseenter = this.mouseenter.bind(this));
            view.dom.addEventListener("mouseleave", this.mouseleave = this.mouseleave.bind(this));
            view.dom.addEventListener("mousemove", this.mousemove = this.mousemove.bind(this));
        }
        get active() {
            return this.view.state.field(this.field);
        }
        checkHover() {
            this.hoverTimeout = -1;
            if (!this.mouseInside || this.active)
                return;
            let now = Date.now(), lastMove = this.lastMouseMove;
            if (now - lastMove.timeStamp < HoverTime) {
                this.hoverTimeout = setTimeout(this.checkHover, HoverTime - (now - lastMove.timeStamp));
                return;
            }
            let pos = this.view.contentDOM.contains(lastMove.target)
                ? this.view.posAtCoords({ x: lastMove.clientX, y: lastMove.clientY }) : -1;
            let open = pos == null ? null : this.source(this.view, (from, to) => {
                return from <= pos && to >= pos && (from == to || isOverRange(this.view, from, to, lastMove.clientX, lastMove.clientY));
            });
            if (open)
                this.view.dispatch({ effects: this.setHover.of(open) });
        }
        mousemove(event) {
            var _a;
            this.lastMouseMove = event;
            if (this.hoverTimeout < 0)
                this.hoverTimeout = setTimeout(this.checkHover, HoverTime);
            let tooltip = this.active;
            if (tooltip && !isInTooltip(event.target)) {
                let { pos } = tooltip, end = (_a = tooltip.end) !== null && _a !== void 0 ? _a : pos;
                if ((pos == end ? this.view.posAtCoords({ x: event.clientX, y: event.clientY }) != pos
                    : !isOverRange(this.view, pos, end, event.clientX, event.clientY, HoverMaxDist)))
                    this.view.dispatch({ effects: this.setHover.of(null) });
            }
        }
        mouseenter() {
            this.mouseInside = true;
        }
        mouseleave() {
            this.mouseInside = false;
            if (this.active)
                this.view.dispatch({ effects: this.setHover.of(null) });
        }
        destroy() {
            clearTimeout(this.hoverTimeout);
            this.view.dom.removeEventListener("mouseenter", this.mouseenter);
            this.view.dom.removeEventListener("mouseleave", this.mouseleave);
            this.view.dom.removeEventListener("mousemove", this.mousemove);
        }
    }
    function isInTooltip(elt) {
        for (let cur = elt; cur; cur = cur.parentNode)
            if (cur.nodeType == 1 && cur.classList.contains("cm-tooltip"))
                return true;
        return false;
    }
    function isOverRange(view, from, to, x, y, margin = 0) {
        let range = document.createRange();
        let fromDOM = view.domAtPos(from), toDOM = view.domAtPos(to);
        range.setEnd(toDOM.node, toDOM.offset);
        range.setStart(fromDOM.node, fromDOM.offset);
        let rects = range.getClientRects();
        for (let i = 0; i < rects.length; i++) {
            let rect = rects[i];
            let dist = Math.max(rect.top - y, y - rect.bottom, rect.left - x, x - rect.right);
            if (dist <= margin)
                return true;
        }
        return false;
    }
    /// Enable a hover tooltip, which shows up when the pointer hovers
    /// over ranges of text. The callback should, for each hoverable
    /// range, call its `check` argument to see if that range is being
    /// hovered over, and return a tooltip description when it is.
    function hoverTooltip(source, options = {}) {
        const setHover = StateEffect.define();
        const hoverState = StateField.define({
            create() { return null; },
            update(value, tr) {
                if (value && (options.hideOnChange && (tr.docChanged || tr.selection)))
                    return null;
                for (let effect of tr.effects)
                    if (effect.is(setHover))
                        return effect.value;
                if (value && tr.docChanged) {
                    let newPos = tr.changes.mapPos(value.pos, -1, MapMode.TrackDel);
                    if (newPos == null)
                        return null;
                    let copy = Object.assign(Object.create(null), value);
                    copy.pos = newPos;
                    if (value.end != null)
                        copy.end = tr.changes.mapPos(value.end);
                    return copy;
                }
                return value;
            },
            provide: [showTooltip.nFrom(v => v ? [v] : [])]
        });
        return [
            hoverState,
            ViewPlugin.define(view => new HoverPlugin(view, source, hoverState, setHover)),
            tooltips()
        ];
    }

    /// An instance of this is passed to completion source functions.
    class CompletionContext {
        /// Create a new completion context. (Mostly useful for testing
        /// completion sources—in the editor, the extension will create
        /// these for you.)
        constructor(
        /// The editor state that the completion happens in.
        state, 
        /// The position at which the completion happens.
        pos, 
        /// Indicates whether completion was activated explicitly, or
        /// implicitly by typing. The usual way to respond to this is to
        /// only return completions when either there is part of a
        /// completable entity at the cursor, or explicit is true.
        explicit) {
            this.state = state;
            this.pos = pos;
            this.explicit = explicit;
            /// @internal
            this.abortListeners = [];
        }
        /// Get the extent, content, and (if there is a token) type of the
        /// token before `this.pos`.
        tokenBefore(types) {
            let token = syntaxTree(this.state).resolve(this.pos, -1);
            while (token && types.indexOf(token.name) < 0)
                token = token.parent;
            return token ? { from: token.from, to: this.pos,
                text: this.state.sliceDoc(token.from, this.pos),
                type: token.type } : null;
        }
        /// Get the match of the given expression directly before the
        /// cursor.
        matchBefore(expr) {
            let line = this.state.doc.lineAt(this.pos);
            let start = Math.max(line.from, this.pos - 250);
            let str = line.slice(start - line.from, this.pos - line.from);
            let found = str.search(ensureAnchor(expr, false));
            return found < 0 ? null : { from: start + found, to: this.pos, text: str.slice(found) };
        }
        /// Yields true when the query has been aborted. Can be useful in
        /// asynchronous queries to avoid doing work that will be ignored.
        get aborted() { return this.abortListeners == null; }
        /// Allows you to register abort handlers, which will be called when
        /// the query is
        /// [aborted](#autocomplete.CompletionContext.aborted).
        addEventListener(_type, listener) {
            if (this.abortListeners)
                this.abortListeners.push(listener);
        }
    }
    class Option {
        constructor(completion, source, match) {
            this.completion = completion;
            this.source = source;
            this.match = match;
        }
    }
    function cur(state) { return state.selection.primary.head; }
    // Make sure the given regexp has a $ at its end and, if `start` is
    // true, a ^ at its start.
    function ensureAnchor(expr, start) {
        var _a;
        let { source } = expr;
        let addStart = start && source[0] != "^", addEnd = source[source.length - 1] != "$";
        if (!addStart && !addEnd)
            return expr;
        return new RegExp(`${addStart ? "^" : ""}(?:${source})${addEnd ? "$" : ""}`, (_a = expr.flags) !== null && _a !== void 0 ? _a : (expr.ignoreCase ? "i" : ""));
    }
    function applyCompletion(view, option) {
        let apply = option.completion.apply || option.completion.label;
        let result = option.source;
        if (typeof apply == "string") {
            view.dispatch({
                changes: { from: result.from, to: result.to, insert: apply },
                selection: { anchor: result.from + apply.length }
            });
        }
        else {
            apply(view, option.completion, result.from, result.to);
        }
    }

    // A pattern matcher for fuzzy completion matching. Create an instance
    // once for a pattern, and then use that to match any number of
    // completions.
    class FuzzyMatcher {
        constructor(pattern) {
            this.pattern = pattern;
            this.chars = [];
            this.folded = [];
            // Buffers reused by calls to `match` to track matched character
            // positions.
            this.any = [];
            this.precise = [];
            this.byWord = [];
            for (let p = 0; p < pattern.length;) {
                let char = codePointAt(pattern, p), size = codePointSize(char);
                this.chars.push(char);
                let part = pattern.slice(p, p + size), upper = part.toUpperCase();
                this.folded.push(codePointAt(upper == part ? part.toLowerCase() : upper, 0));
                p += size;
            }
            this.astral = pattern.length != this.chars.length;
        }
        // Matches a given word (completion) against the pattern (input).
        // Will return null for no match, and otherwise an array that starts
        // with the match score, followed by any number of `from, to` pairs
        // indicating the matched parts of `word`.
        //
        // The score is a number that is more negative the worse the match
        // is. See `Penalty` above.
        match(word) {
            if (this.pattern.length == 0)
                return [0];
            if (word.length < this.pattern.length)
                return null;
            let { chars, folded, any, precise, byWord } = this;
            // For single-character queries, only match when they occur right
            // at the start
            if (chars.length == 1) {
                let first = codePointAt(word, 0);
                return first == chars[0] ? [0, 0, codePointSize(first)]
                    : first == folded[0] ? [-200 /* CaseFold */, 0, codePointSize(first)] : null;
            }
            let direct = word.indexOf(this.pattern);
            if (direct == 0)
                return [0, 0, this.pattern.length];
            let len = chars.length, anyTo = 0;
            if (direct < 0) {
                for (let i = 0, e = Math.min(word.length, 200); i < e && anyTo < len;) {
                    let next = codePointAt(word, i);
                    if (next == chars[anyTo] || next == folded[anyTo])
                        any[anyTo++] = i;
                    i += codePointSize(next);
                }
                // No match, exit immediately
                if (anyTo < len)
                    return null;
            }
            let preciseTo = 0;
            let byWordTo = 0, byWordFolded = false;
            let adjacentTo = 0, adjacentStart = -1, adjacentEnd = -1;
            for (let i = 0, e = Math.min(word.length, 200), prevType = 0 /* NonWord */; i < e && byWordTo < len;) {
                let next = codePointAt(word, i);
                if (direct < 0) {
                    if (preciseTo < len && next == chars[preciseTo])
                        precise[preciseTo++] = i;
                    if (adjacentTo < len) {
                        if (next == chars[adjacentTo] || next == folded[adjacentTo]) {
                            if (adjacentTo == 0)
                                adjacentStart = i;
                            adjacentEnd = i;
                            adjacentTo++;
                        }
                        else {
                            adjacentTo = 0;
                        }
                    }
                }
                let ch, type = next < 0xff
                    ? (next >= 48 && next <= 57 || next >= 97 && next <= 122 ? 2 /* Lower */ : next >= 65 && next <= 90 ? 1 /* Upper */ : 0 /* NonWord */)
                    : ((ch = fromCodePoint(next)) != ch.toLowerCase() ? 1 /* Upper */ : ch != ch.toUpperCase() ? 2 /* Lower */ : 0 /* NonWord */);
                if (type == 1 /* Upper */ || prevType == 0 /* NonWord */ && type != 0 /* NonWord */ &&
                    (this.chars[byWordTo] == next || (this.folded[byWordTo] == next && (byWordFolded = true))))
                    byWord[byWordTo++] = i;
                prevType = type;
                i += codePointSize(next);
            }
            if (byWordTo == len && byWord[0] == 0)
                return this.result(-100 /* ByWord */ + (byWordFolded ? -200 /* CaseFold */ : 0), byWord, word);
            if (adjacentTo == len && adjacentStart == 0)
                return [-200 /* CaseFold */, 0, adjacentEnd];
            if (direct > -1)
                return [-700 /* NotStart */, direct, direct + this.pattern.length];
            if (adjacentTo == len)
                return [-200 /* CaseFold */ + -700 /* NotStart */, adjacentStart, adjacentEnd];
            if (byWordTo == len)
                return this.result(-100 /* ByWord */ + (byWordFolded ? -200 /* CaseFold */ : 0) + -700 /* NotStart */, byWord, word);
            return chars.length == 2 ? null : this.result((any[0] ? -700 /* NotStart */ : 0) + -200 /* CaseFold */ + -1100 /* Gap */, any, word);
        }
        result(score, positions, word) {
            let result = [score], i = 1;
            for (let pos of positions) {
                let to = pos + (this.astral ? codePointSize(codePointAt(word, pos)) : 1);
                if (i > 1 && result[i - 1] == pos)
                    result[i - 1] = to;
                else {
                    result[i++] = pos;
                    result[i++] = to;
                }
            }
            return result;
        }
    }

    const completionConfig = Facet.define({
        combine(configs) {
            return combineConfig(configs, {
                activateOnTyping: true,
                override: null,
                maxRenderedOptions: 100
            });
        }
    });

    const MaxInfoWidth = 300;
    const baseTheme$7 = EditorView.baseTheme({
        "$tooltip.autocomplete": {
            "& > ul": {
                fontFamily: "monospace",
                overflowY: "auto",
                whiteSpace: "nowrap",
                maxHeight: "10em",
                listStyle: "none",
                margin: 0,
                padding: 0,
                "& > li": {
                    cursor: "pointer",
                    padding: "1px 1em 1px 3px",
                    lineHeight: 1.2
                },
                "& > li[aria-selected]": {
                    background_fallback: "#bdf",
                    backgroundColor: "Highlight",
                    color_fallback: "white",
                    color: "HighlightText"
                }
            }
        },
        "$completionListIncompleteTop:before, $completionListIncompleteBottom:after": {
            content: '"···"',
            opacity: 0.5,
            display: "block",
            textAlign: "center"
        },
        "$tooltip.completionInfo": {
            position: "absolute",
            padding: "3px 9px",
            width: "max-content",
            maxWidth: MaxInfoWidth + "px",
        },
        "$tooltip.completionInfo.left": { right: "100%" },
        "$tooltip.completionInfo.right": { left: "100%" },
        "$$light $snippetField": { backgroundColor: "#ddd" },
        "$$dark $snippetField": { backgroundColor: "#333" },
        "$snippetFieldPosition": {
            verticalAlign: "text-top",
            width: 0,
            height: "1.15em",
            margin: "0 -0.7px -.7em",
            borderLeft: "1.4px dotted #888"
        },
        $completionMatchedText: {
            textDecoration: "underline"
        },
        $completionDetail: {
            marginLeft: "0.5em",
            fontStyle: "italic"
        },
        $completionIcon: {
            fontSize: "90%",
            width: ".8em",
            display: "inline-block",
            textAlign: "center",
            paddingRight: ".6em",
            opacity: "0.6"
        },
        "$completionIcon.function, $completionIcon.method": {
            "&:after": { content: "'ƒ'" }
        },
        "$completionIcon.class": {
            "&:after": { content: "'○'" }
        },
        "$completionIcon.interface": {
            "&:after": { content: "'◌'" }
        },
        "$completionIcon.variable": {
            "&:after": { content: "'𝑥'" }
        },
        "$completionIcon.constant": {
            "&:after": { content: "'𝐶'" }
        },
        "$completionIcon.type": {
            "&:after": { content: "'𝑡'" }
        },
        "$completionIcon.enum": {
            "&:after": { content: "'∪'" }
        },
        "$completionIcon.property": {
            "&:after": { content: "'□'" }
        },
        "$completionIcon.keyword": {
            "&:after": { content: "'🔑\uFE0E'" } // Disable emoji rendering
        },
        "$completionIcon.namespace": {
            "&:after": { content: "'▢'" }
        },
        "$completionIcon.text": {
            "&:after": { content: "'abc'", fontSize: "50%", verticalAlign: "middle" }
        }
    });

    function createListBox(options, id, range) {
        const ul = document.createElement("ul");
        ul.id = id;
        ul.setAttribute("role", "listbox");
        ul.setAttribute("aria-expanded", "true");
        for (let i = range.from; i < range.to; i++) {
            let { completion, match } = options[i];
            const li = ul.appendChild(document.createElement("li"));
            li.id = id + "-" + i;
            let icon = li.appendChild(document.createElement("div"));
            icon.className = themeClass("completionIcon" + (completion.type ? "." + completion.type : ""));
            icon.setAttribute("aria-hidden", "true");
            let labelElt = li.appendChild(document.createElement("span"));
            labelElt.className = themeClass("completionLabel");
            let { label, detail } = completion, off = 0;
            for (let j = 1; j < match.length;) {
                let from = match[j++], to = match[j++];
                if (from > off)
                    labelElt.appendChild(document.createTextNode(label.slice(off, from)));
                let span = labelElt.appendChild(document.createElement("span"));
                span.appendChild(document.createTextNode(label.slice(from, to)));
                span.className = themeClass("completionMatchedText");
                off = to;
            }
            if (off < label.length)
                labelElt.appendChild(document.createTextNode(label.slice(off)));
            if (detail) {
                let detailElt = li.appendChild(document.createElement("span"));
                detailElt.className = themeClass("completionDetail");
                detailElt.textContent = detail;
            }
            li.setAttribute("role", "option");
        }
        if (range.from)
            ul.classList.add(themeClass("completionListIncompleteTop"));
        if (range.to < options.length)
            ul.classList.add(themeClass("completionListIncompleteBottom"));
        return ul;
    }
    function createInfoDialog(option) {
        let dom = document.createElement("div");
        dom.className = themeClass("tooltip.completionInfo");
        let { info } = option.completion;
        if (typeof info == "string")
            dom.textContent = info;
        else
            dom.appendChild(info(option.completion));
        return dom;
    }
    function rangeAroundSelected(total, selected, max) {
        if (total <= max)
            return { from: 0, to: total };
        if (selected <= (total >> 1)) {
            let off = Math.floor(selected / max);
            return { from: off * max, to: (off + 1) * max };
        }
        let off = Math.floor((total - selected) / max);
        return { from: total - (off + 1) * max, to: total - off * max };
    }
    class CompletionTooltip {
        constructor(view, stateField) {
            this.view = view;
            this.stateField = stateField;
            this.info = null;
            this.placeInfo = {
                read: () => this.measureInfo(),
                write: (pos) => this.positionInfo(pos),
                key: this
            };
            let cState = view.state.field(stateField);
            let { options, selected } = cState.open;
            let config = view.state.facet(completionConfig);
            this.range = rangeAroundSelected(options.length, selected, config.maxRenderedOptions);
            this.dom = document.createElement("div");
            this.dom.addEventListener("mousedown", (e) => {
                for (let dom = e.target, match; dom && dom != this.dom; dom = dom.parentNode) {
                    if (dom.nodeName == "LI" && (match = /-(\d+)$/.exec(dom.id)) && +match[1] < options.length) {
                        applyCompletion(view, options[+match[1]]);
                        e.preventDefault();
                        return;
                    }
                }
            });
            this.list = this.dom.appendChild(createListBox(options, cState.id, this.range));
            this.list.addEventListener("scroll", () => {
                if (this.info)
                    this.view.requestMeasure(this.placeInfo);
            });
        }
        mount() { this.updateSel(); }
        update(update) {
            if (update.state.field(this.stateField) != update.prevState.field(this.stateField))
                this.updateSel();
        }
        positioned() {
            if (this.info)
                this.view.requestMeasure(this.placeInfo);
        }
        updateSel() {
            let cState = this.view.state.field(this.stateField), open = cState.open;
            if (open.selected < this.range.from || open.selected >= this.range.to) {
                this.range = rangeAroundSelected(open.options.length, open.selected, this.view.state.facet(completionConfig).maxRenderedOptions);
                this.list.remove();
                this.list = this.dom.appendChild(createListBox(open.options, cState.id, this.range));
                this.list.addEventListener("scroll", () => {
                    if (this.info)
                        this.view.requestMeasure(this.placeInfo);
                });
            }
            if (this.updateSelectedOption(open.selected)) {
                if (this.info) {
                    this.info.remove();
                    this.info = null;
                }
                let option = open.options[open.selected];
                if (option.completion.info) {
                    this.info = this.dom.appendChild(createInfoDialog(option));
                    this.view.requestMeasure(this.placeInfo);
                }
            }
        }
        updateSelectedOption(selected) {
            let set = null;
            for (let opt = this.list.firstChild, i = this.range.from; opt; opt = opt.nextSibling, i++) {
                if (i == selected) {
                    if (!opt.hasAttribute("aria-selected")) {
                        opt.setAttribute("aria-selected", "true");
                        set = opt;
                    }
                }
                else {
                    if (opt.hasAttribute("aria-selected"))
                        opt.removeAttribute("aria-selected");
                }
            }
            if (set)
                scrollIntoView(this.list, set);
            return set;
        }
        measureInfo() {
            let sel = this.dom.querySelector("[aria-selected]");
            if (!sel)
                return null;
            let rect = this.dom.getBoundingClientRect();
            let top = sel.getBoundingClientRect().top - rect.top;
            if (top < 0 || top > this.list.clientHeight - 10)
                return null;
            let left = this.view.textDirection == Direction.RTL;
            let spaceLeft = rect.left, spaceRight = innerWidth - rect.right;
            if (left && spaceLeft < Math.min(MaxInfoWidth, spaceRight))
                left = false;
            else if (!left && spaceRight < Math.min(MaxInfoWidth, spaceLeft))
                left = true;
            return { top, left };
        }
        positionInfo(pos) {
            if (this.info && pos) {
                this.info.style.top = pos.top + "px";
                this.info.classList.toggle("cm-tooltip-completionInfo-left", pos.left);
                this.info.classList.toggle("cm-tooltip-completionInfo-right", !pos.left);
            }
        }
    }
    // We allocate a new function instance every time the completion
    // changes to force redrawing/repositioning of the tooltip
    function completionTooltip(stateField) {
        return (view) => new CompletionTooltip(view, stateField);
    }
    function scrollIntoView(container, element) {
        let parent = container.getBoundingClientRect();
        let self = element.getBoundingClientRect();
        if (self.top < parent.top)
            container.scrollTop -= parent.top - self.top;
        else if (self.bottom > parent.bottom)
            container.scrollTop += self.bottom - parent.bottom;
    }

    const MaxOptions = 300;
    function sortOptions(active, state) {
        let options = [];
        for (let a of active)
            if (a.hasResult()) {
                let matcher = new FuzzyMatcher(state.sliceDoc(a.from, a.to)), match;
                for (let option of a.result.options)
                    if (match = matcher.match(option.label)) {
                        if (option.boost != null)
                            match[0] += option.boost;
                        options.push(new Option(option, a, match));
                    }
            }
        options.sort(cmpOption);
        return options.length > MaxOptions ? options.slice(0, MaxOptions) : options;
    }
    class CompletionDialog {
        constructor(options, attrs, tooltip, timestamp, selected) {
            this.options = options;
            this.attrs = attrs;
            this.tooltip = tooltip;
            this.timestamp = timestamp;
            this.selected = selected;
        }
        setSelected(selected, id) {
            return selected == this.selected || selected >= this.options.length ? this
                : new CompletionDialog(this.options, makeAttrs(id, selected), this.tooltip, this.timestamp, selected);
        }
        static build(active, state, id, prev) {
            let options = sortOptions(active, state);
            if (!options.length)
                return null;
            let selected = 0;
            if (prev) {
                let selectedValue = prev.options[prev.selected].completion;
                for (let i = 0; i < options.length && !selected; i++) {
                    if (options[i].completion == selectedValue)
                        selected = i;
                }
            }
            return new CompletionDialog(options, makeAttrs(id, selected), [{
                    pos: active.reduce((a, b) => b.hasResult() ? Math.min(a, b.from) : a, 1e8),
                    style: "autocomplete",
                    create: completionTooltip(completionState)
                }], prev ? prev.timestamp : Date.now(), selected);
        }
        map(changes) {
            return new CompletionDialog(this.options, this.attrs, [Object.assign(Object.assign({}, this.tooltip[0]), { pos: changes.mapPos(this.tooltip[0].pos) })], this.timestamp, this.selected);
        }
    }
    class CompletionState {
        constructor(active, id, open) {
            this.active = active;
            this.id = id;
            this.open = open;
        }
        static start() {
            return new CompletionState(none$6, "cm-ac-" + Math.floor(Math.random() * 2e6).toString(36), null);
        }
        update(tr) {
            let { state } = tr, conf = state.facet(completionConfig);
            let sources = conf.override || state.languageDataAt("autocomplete", cur(state));
            let active = sources.map(source => {
                let value = this.active.find(s => s.source == source) || new ActiveSource(source, 0 /* Inactive */, false);
                return value.update(tr, conf);
            });
            if (active.length == this.active.length && active.every((a, i) => a == this.active[i]))
                active = this.active;
            let open = tr.selection || active.some(a => a.hasResult() && tr.changes.touchesRange(a.from, a.to)) ||
                !sameResults(active, this.active) ? CompletionDialog.build(active, state, this.id, this.open)
                : this.open && tr.docChanged ? this.open.map(tr.changes) : this.open;
            for (let effect of tr.effects)
                if (effect.is(setSelectedEffect))
                    open = open && open.setSelected(effect.value, this.id);
            return active == this.active && open == this.open ? this : new CompletionState(active, this.id, open);
        }
        get tooltip() { return this.open ? this.open.tooltip : none$6; }
        get attrs() { return this.open ? this.open.attrs : baseAttrs; }
    }
    function sameResults(a, b) {
        if (a == b)
            return true;
        for (let iA = 0, iB = 0;;) {
            while (iA < a.length && !a[iA].hasResult)
                iA++;
            while (iB < b.length && !b[iB].hasResult)
                iB++;
            let endA = iA == a.length, endB = iB == b.length;
            if (endA || endB)
                return endA == endB;
            if (a[iA++].result != b[iB++].result)
                return false;
        }
    }
    function makeAttrs(id, selected) {
        return {
            "aria-autocomplete": "list",
            "aria-activedescendant": id + "-" + selected,
            "aria-owns": id
        };
    }
    const baseAttrs = { "aria-autocomplete": "list" }, none$6 = [];
    function cmpOption(a, b) {
        let dScore = b.match[0] - a.match[0];
        if (dScore)
            return dScore;
        let lA = a.completion.label, lB = b.completion.label;
        return lA < lB ? -1 : lA == lB ? 0 : 1;
    }
    class ActiveSource {
        constructor(source, state, explicit) {
            this.source = source;
            this.state = state;
            this.explicit = explicit;
        }
        hasResult() { return false; }
        update(tr, conf) {
            let event = tr.annotation(Transaction.userEvent), value = this;
            if (event == "input" || event == "delete")
                value = value.handleUserEvent(tr, event, conf);
            else if (tr.docChanged)
                value = value.handleChange(tr);
            else if (tr.selection && value.state != 0 /* Inactive */)
                value = new ActiveSource(value.source, 0 /* Inactive */, false);
            for (let effect of tr.effects) {
                if (effect.is(startCompletionEffect))
                    value = new ActiveSource(value.source, 1 /* Pending */, effect.value);
                else if (effect.is(closeCompletionEffect))
                    value = new ActiveSource(value.source, 0 /* Inactive */, false);
                else if (effect.is(setActiveEffect))
                    for (let active of effect.value)
                        if (active.source == value.source)
                            value = active;
            }
            return value;
        }
        handleUserEvent(_tr, type, conf) {
            return type == "delete" || !conf.activateOnTyping ? this : new ActiveSource(this.source, 1 /* Pending */, false);
        }
        handleChange(tr) {
            return tr.changes.touchesRange(cur(tr.startState)) ? new ActiveSource(this.source, 0 /* Inactive */, false) : this;
        }
    }
    class ActiveResult extends ActiveSource {
        constructor(source, explicit, result, from, to, span) {
            super(source, 2 /* Result */, explicit);
            this.result = result;
            this.from = from;
            this.to = to;
            this.span = span;
        }
        hasResult() { return true; }
        handleUserEvent(tr, type, conf) {
            let from = tr.changes.mapPos(this.from), to = tr.changes.mapPos(this.to, 1);
            let pos = cur(tr.state);
            if ((this.explicit ? pos < from : pos <= from) || pos > to)
                return new ActiveSource(this.source, type == "input" && conf.activateOnTyping ? 1 /* Pending */ : 0 /* Inactive */, false);
            if (this.span && (from == to || this.span.test(tr.state.sliceDoc(from, to))))
                return new ActiveResult(this.source, this.explicit, this.result, from, to, this.span);
            return new ActiveSource(this.source, 1 /* Pending */, this.explicit);
        }
        handleChange(tr) {
            return tr.changes.touchesRange(this.from, this.to)
                ? new ActiveSource(this.source, 0 /* Inactive */, false)
                : new ActiveResult(this.source, this.explicit, this.result, tr.changes.mapPos(this.from), tr.changes.mapPos(this.to, 1), this.span);
        }
        map(mapping) {
            return new ActiveResult(this.source, this.explicit, this.result, mapping.mapPos(this.from), mapping.mapPos(this.to, 1), this.span);
        }
    }
    const startCompletionEffect = StateEffect.define();
    const closeCompletionEffect = StateEffect.define();
    const setActiveEffect = StateEffect.define({
        map(sources, mapping) { return sources.map(s => s.hasResult() && !mapping.empty ? s.map(mapping) : s); }
    });
    const setSelectedEffect = StateEffect.define();
    const completionState = StateField.define({
        create() { return CompletionState.start(); },
        update(value, tr) { return value.update(tr); },
        provide: [
            showTooltip.nFrom(state => state.tooltip),
            EditorView.contentAttributes.from(state => state.attrs)
        ]
    });

    const CompletionInteractMargin = 75;
    /// Returns a command that moves the completion selection forward or
    /// backward by the given amount.
    function moveCompletionSelection(forward, by = "option") {
        return (view) => {
            let cState = view.state.field(completionState, false);
            if (!cState || !cState.open || Date.now() - cState.open.timestamp < CompletionInteractMargin)
                return false;
            let step = 1, tooltip;
            if (by == "page" && (tooltip = view.dom.querySelector(".cm-tooltip-autocomplete")))
                step = Math.max(2, Math.floor(tooltip.offsetHeight / tooltip.firstChild.offsetHeight));
            let selected = cState.open.selected + step * (forward ? 1 : -1), { length } = cState.open.options;
            if (selected < 0)
                selected = by == "page" ? 0 : length - 1;
            else if (selected >= length)
                selected = by == "page" ? length - 1 : 0;
            view.dispatch({ effects: setSelectedEffect.of(selected) });
            return true;
        };
    }
    /// Accept the current completion.
    const acceptCompletion = (view) => {
        let cState = view.state.field(completionState, false);
        if (!cState || !cState.open || Date.now() - cState.open.timestamp < CompletionInteractMargin)
            return false;
        applyCompletion(view, cState.open.options[cState.open.selected]);
        return true;
    };
    /// Explicitly start autocompletion.
    const startCompletion = (view) => {
        let cState = view.state.field(completionState, false);
        if (!cState)
            return false;
        view.dispatch({ effects: startCompletionEffect.of(true) });
        return true;
    };
    /// Close the currently active completion.
    const closeCompletion = (view) => {
        let cState = view.state.field(completionState, false);
        if (!cState || !cState.active.some(a => a.state != 0 /* Inactive */))
            return false;
        view.dispatch({ effects: closeCompletionEffect.of(null) });
        return true;
    };
    class RunningQuery {
        constructor(source, context) {
            this.source = source;
            this.context = context;
            this.time = Date.now();
            this.updates = [];
            // Note that 'undefined' means 'not done yet', whereas 'null' means
            // 'query returned null'.
            this.done = undefined;
        }
    }
    const DebounceTime = 50, MaxUpdateCount = 50, MinAbortTime = 1000;
    const completionPlugin = ViewPlugin.fromClass(class {
        constructor(view) {
            this.view = view;
            this.debounceUpdate = -1;
            this.running = [];
            this.debounceAccept = -1;
            this.composing = 0 /* None */;
            for (let active of view.state.field(completionState).active)
                if (active.state == 1 /* Pending */)
                    this.startQuery(active);
        }
        update(update) {
            let cState = update.state.field(completionState);
            if (!update.selectionSet && !update.docChanged && update.prevState.field(completionState) == cState)
                return;
            let doesReset = update.transactions.some(tr => {
                let event = tr.annotation(Transaction.userEvent);
                return (tr.selection || tr.docChanged) && event != "input" && event != "delete";
            });
            for (let i = 0; i < this.running.length; i++) {
                let query = this.running[i];
                if (doesReset ||
                    query.updates.length + update.transactions.length > MaxUpdateCount && query.time - Date.now() > MinAbortTime) {
                    for (let handler of query.context.abortListeners) {
                        try {
                            handler();
                        }
                        catch (e) {
                            logException(this.view.state, e);
                        }
                    }
                    query.context.abortListeners = null;
                    this.running.splice(i--, 1);
                }
                else {
                    query.updates.push(...update.transactions);
                }
            }
            if (this.debounceUpdate > -1)
                clearTimeout(this.debounceUpdate);
            this.debounceUpdate = cState.active.some(a => a.state == 1 /* Pending */ && !this.running.some(q => q.source == a.source))
                ? setTimeout(() => this.startUpdate(), DebounceTime) : -1;
            if (this.composing != 0 /* None */)
                for (let tr of update.transactions) {
                    if (tr.annotation(Transaction.userEvent) == "input")
                        this.composing = 2 /* Changed */;
                    else if (this.composing == 2 /* Changed */ && tr.selection)
                        this.composing = 3 /* ChangedAndMoved */;
                }
        }
        startUpdate() {
            this.debounceUpdate = -1;
            let { state } = this.view, cState = state.field(completionState);
            for (let active of cState.active) {
                if (active.state == 1 /* Pending */ && !this.running.some(r => r.source == active.source))
                    this.startQuery(active);
            }
        }
        startQuery(active) {
            let { state } = this.view, pos = cur(state);
            let context = new CompletionContext(state, pos, active.explicit);
            let pending = new RunningQuery(active.source, context);
            this.running.push(pending);
            Promise.resolve(active.source(context)).then(result => {
                if (!pending.context.aborted) {
                    pending.done = result || null;
                    this.scheduleAccept();
                }
            }, err => {
                this.view.dispatch({ effects: closeCompletionEffect.of(null) });
                logException(this.view.state, err);
            });
        }
        scheduleAccept() {
            if (this.running.every(q => q.done !== undefined))
                this.accept();
            else if (this.debounceAccept < 0)
                this.debounceAccept = setTimeout(() => this.accept(), DebounceTime);
        }
        // For each finished query in this.running, try to create a result
        // or, if appropriate, restart the query.
        accept() {
            var _a;
            if (this.debounceAccept > -1)
                clearTimeout(this.debounceAccept);
            this.debounceAccept = -1;
            let updated = [];
            let conf = this.view.state.facet(completionConfig);
            for (let i = 0; i < this.running.length; i++) {
                let query = this.running[i];
                if (query.done === undefined)
                    continue;
                this.running.splice(i--, 1);
                if (query.done) {
                    let active = new ActiveResult(query.source, query.context.explicit, query.done, query.done.from, (_a = query.done.to) !== null && _a !== void 0 ? _a : cur(query.updates.length ? query.updates[0].startState : this.view.state), query.done.span ? ensureAnchor(query.done.span, true) : null);
                    // Replay the transactions that happened since the start of
                    // the request and see if that preserves the result
                    for (let tr of query.updates)
                        active = active.update(tr, conf);
                    if (active.hasResult()) {
                        updated.push(active);
                        continue;
                    }
                }
                let current = this.view.state.field(completionState).active.find(a => a.source == query.source);
                if (current && current.state == 1 /* Pending */) {
                    if (query.done == null) {
                        // Explicitly failed. Should clear the pending status if it
                        // hasn't been re-set in the meantime.
                        let active = new ActiveSource(query.source, 0 /* Inactive */, false);
                        for (let tr of query.updates)
                            active = active.update(tr, conf);
                        if (active.state != 1 /* Pending */)
                            updated.push(active);
                    }
                    else {
                        // Cleared by subsequent transactions. Restart.
                        this.startQuery(current);
                    }
                }
            }
            if (updated.length)
                this.view.dispatch({ effects: setActiveEffect.of(updated) });
        }
    }, {
        eventHandlers: {
            compositionstart() {
                this.composing = 1 /* Started */;
            },
            compositionend() {
                if (this.composing == 3 /* ChangedAndMoved */)
                    this.view.dispatch({ effects: startCompletionEffect.of(false) });
                this.composing = 0 /* None */;
            }
        } // See https://github.com/microsoft/TypeScript-DOM-lib-generator/pull/949
    });
    let fieldMarker = Decoration.widget({ widget: new class extends WidgetType {
            toDOM() {
                let span = document.createElement("span");
                span.className = themeClass("snippetFieldPosition");
                return span;
            }
            ignoreEvent() { return false; }
        } });
    let fieldRange = Decoration.mark({ class: themeClass("snippetField") });
    class ActiveSnippet {
        constructor(ranges, active) {
            this.ranges = ranges;
            this.active = active;
            this.deco = Decoration.set(ranges.map(r => (r.from == r.to ? fieldMarker : fieldRange).range(r.from, r.to)));
        }
        map(changes) {
            return new ActiveSnippet(this.ranges.map(r => r.map(changes)), this.active);
        }
        selectionInsideField(sel) {
            return sel.ranges.every(range => this.ranges.some(r => r.field == this.active && r.from <= range.from && r.to >= range.to));
        }
    }
    const setActive = StateEffect.define({
        map(value, changes) { return value && value.map(changes); }
    });
    const moveToField = StateEffect.define();
    const snippetState = StateField.define({
        create() { return null; },
        update(value, tr) {
            for (let effect of tr.effects) {
                if (effect.is(setActive))
                    return effect.value;
                if (effect.is(moveToField) && value)
                    return new ActiveSnippet(value.ranges, effect.value);
            }
            if (value && tr.docChanged)
                value = value.map(tr.changes);
            if (value && tr.selection && !value.selectionInsideField(tr.selection))
                value = null;
            return value;
        },
        provide: [EditorView.decorations.from(val => val ? val.deco : Decoration.none)]
    });
    function fieldSelection(ranges, field) {
        return EditorSelection.create(ranges.filter(r => r.field == field).map(r => EditorSelection.range(r.from, r.to)));
    }
    function moveField(dir) {
        return ({ state, dispatch }) => {
            let active = state.field(snippetState, false);
            if (!active || dir < 0 && active.active == 0)
                return false;
            let next = active.active + dir, last = dir > 0 && !active.ranges.some(r => r.field == next + dir);
            dispatch(state.update({
                selection: fieldSelection(active.ranges, next),
                effects: setActive.of(last ? null : new ActiveSnippet(active.ranges, next))
            }));
            return true;
        };
    }
    const clearSnippet = ({ state, dispatch }) => {
        let active = state.field(snippetState, false);
        if (!active)
            return false;
        dispatch(state.update({ effects: setActive.of(null) }));
        return true;
    };
    const snippetKeymap = precedence(keymap([
        { key: "Tab", run: moveField(1), shift: moveField(-1) },
        { key: "Escape", run: clearSnippet }
    ]), "override");

    /// Returns an extension that enables autocompletion.
    function autocompletion(config = {}) {
        return [
            completionState,
            completionConfig.of(config),
            completionPlugin,
            baseTheme$7,
            tooltips(),
            precedence(keymap([
                { key: "ArrowDown", run: moveCompletionSelection(true) },
                { key: "ArrowUp", run: moveCompletionSelection(false) },
                { key: "PageDown", run: moveCompletionSelection(true, "page") },
                { key: "PageUp", run: moveCompletionSelection(false, "page") },
                { key: "Enter", run: acceptCompletion }
            ]), "override")
        ];
    }
    /// Basic keybindings for autocompletion.
    ///
    ///  - Ctrl-Space (Cmd-Space on macOS): [`startCompletion`](#autocomplete.startCompletion)
    ///  - Escape: [`closeCompletion`](#autocomplete.closeCompletion)
    const completionKeymap = [
        { key: "Mod-Space", run: startCompletion },
        { key: "Escape", run: closeCompletion }
    ];

    /// Comments or uncomments the current `SelectionRange` using line-comments.
    /// The line-comment token is defined on a language basis.
    const toggleLineComment = target => {
        return dispatch(toggleLineCommentWithOption(CommentOption.Toggle), target);
    };
    /// Comments or uncomments the current `SelectionRange` using block-comments.
    /// The block-comment tokens are defined on a language basis.
    const toggleBlockComment = target => {
        return dispatch(toggleBlockCommentWithOption(CommentOption.Toggle), target);
    };
    /// Default key bindings for this package.
    ///
    ///  - Ctrl-/ (Cmd-/ on macOS): [\`toggleLineComment\`](#comment.toggleLineComment).
    ///  - Shift-Alt-a: [\`toggleBlockComment\`](#comment.toggleBlockComment).
    const commentKeymap = [
        { key: "Mod-/", run: toggleLineComment },
        { key: "Alt-A", run: toggleBlockComment }
    ];
    function dispatch(cmd, target) {
        const tr = cmd(target.state);
        if (!tr)
            return false;
        target.dispatch(tr);
        return true;
    }
    var CommentOption;
    (function (CommentOption) {
        CommentOption[CommentOption["Toggle"] = 0] = "Toggle";
        CommentOption[CommentOption["OnlyComment"] = 1] = "OnlyComment";
        CommentOption[CommentOption["OnlyUncomment"] = 2] = "OnlyUncomment";
    })(CommentOption || (CommentOption = {}));
    function getConfig(state, pos = state.selection.primary.head) {
        return state.languageDataAt("commentTokens", pos)[0] || {};
    }
    const toggleBlockCommentWithOption = (option) => (state) => {
        const config = getConfig(state);
        return config.block ? new BlockCommenter(config.block.open, config.block.close).toggle(option, state) : null;
    };
    const toggleLineCommentWithOption = (option) => (state) => {
        const config = getConfig(state);
        return config.line ? new LineCommenter(config.line).toggle(option, state) : null;
    };
    // This class performs toggle, comment and uncomment
    // of block comments in languages that support them.
    // The `open` and `close` arguments refer to the open and close
    // tokens of which this `BlockCommenter` is made up.
    class BlockCommenter {
        constructor(open, close, margin = " ") {
            this.open = open;
            this.close = close;
            this.margin = margin;
        }
        toggle(option, state) {
            const selectionCommented = this.isSelectionCommented(state);
            if (selectionCommented !== null) {
                if (option !== CommentOption.OnlyComment) {
                    return state.update({
                        changes: selectionCommented.map(({ open, close }) => [
                            { from: open.pos - this.open.length, to: open.pos + open.margin },
                            { from: close.pos - close.margin, to: close.pos + this.close.length }
                        ])
                    });
                }
            }
            else {
                if (option !== CommentOption.OnlyUncomment) {
                    return state.update(state.changeByRange(range => {
                        const shift = (this.open + this.margin).length;
                        return {
                            changes: [{ from: range.from, insert: this.open + this.margin },
                                { from: range.to, insert: this.margin + this.close }],
                            range: EditorSelection.range(range.anchor + shift, range.head + shift)
                        };
                    }));
                }
            }
            return null;
        }
        /// Determines whether all selection ranges in `state` are block-commented.
        isSelectionCommented(state) {
            let result = [];
            for (const range of state.selection.ranges) {
                const x = this.isRangeCommented(state, range);
                if (x === null)
                    return null;
                result.push(x);
            }
            return result;
        }
        /// Determines if the `range` is block-commented in the given `state`.
        /// The `range` must be a valid range in `state`.
        isRangeCommented(state, range) {
            let textBefore = state.sliceDoc(range.from - SearchMargin, range.from);
            let textAfter = state.sliceDoc(range.to, range.to + SearchMargin);
            let spaceBefore = /\s*$/.exec(textBefore)[0].length, spaceAfter = /^\s*/.exec(textAfter)[0].length;
            let beforeOff = textBefore.length - spaceBefore;
            if (textBefore.slice(beforeOff - this.open.length, beforeOff) == this.open &&
                textAfter.slice(spaceAfter, spaceAfter + this.close.length) == this.close) {
                return { open: { pos: range.from - spaceBefore, margin: spaceBefore && 1 },
                    close: { pos: range.to + spaceAfter, margin: spaceAfter && 1 } };
            }
            let startText, endText;
            if (range.to - range.from <= 2 * SearchMargin) {
                startText = endText = state.sliceDoc(range.from, range.to);
            }
            else {
                startText = state.sliceDoc(range.from, range.from + SearchMargin);
                endText = state.sliceDoc(range.to - SearchMargin, range.to);
            }
            let startSpace = /^\s*/.exec(startText)[0].length, endSpace = /\s*$/.exec(endText)[0].length;
            let endOff = endText.length - endSpace - this.close.length;
            if (startText.slice(startSpace, startSpace + this.open.length) == this.open &&
                endText.slice(endOff, endOff + this.close.length) == this.close) {
                return { open: { pos: range.from + startSpace + this.open.length,
                        margin: /\s/.test(startText.charAt(startSpace + this.open.length)) ? 1 : 0 },
                    close: { pos: range.to - endSpace - this.close.length,
                        margin: /\s/.test(endText.charAt(endOff - 1)) ? 1 : 0 } };
            }
            return null;
        }
    }
    const SearchMargin = 50;
    // This class performs toggle, comment and uncomment
    // of line comments in languages that support them.
    // The `lineCommentToken` argument refer to the token of
    // which this `LineCommenter` is made up.
    class LineCommenter {
        constructor(lineCommentToken, margin = " ") {
            this.lineCommentToken = lineCommentToken;
            this.margin = margin;
        }
        toggle(option, state) {
            const linesAcrossSelection = [];
            const linesAcrossRange = {};
            for (let i = 0; i < state.selection.ranges.length; i++) {
                const lines = getLinesInRange(state.doc, state.selection.ranges[i]);
                linesAcrossSelection.push(...lines);
                linesAcrossRange[i] = lines;
            }
            const column = this.isRangeCommented(state, linesAcrossSelection);
            if (column.isRangeLineSkipped) {
                if (option != CommentOption.OnlyComment) {
                    let changes = [];
                    for (let i = 0; i < state.selection.ranges.length; i++) {
                        const lines = linesAcrossRange[i];
                        for (const line of lines) {
                            if (lines.length > 1 && column.isLineSkipped[line.number])
                                continue;
                            const pos = line.from + column.minCol;
                            const posAfter = column.minCol + this.lineCommentToken.length;
                            const marginLen = line.slice(posAfter, posAfter + 1) == " " ? 1 : 0;
                            changes.push({ from: pos, to: pos + this.lineCommentToken.length + marginLen });
                        }
                    }
                    return state.update({ changes });
                }
            }
            else {
                if (option != CommentOption.OnlyUncomment) {
                    let changes = [];
                    for (let i = 0; i < state.selection.ranges.length; i++) {
                        const lines = linesAcrossRange[i];
                        for (const line of lines) {
                            if (lines.length <= 1 || !column.isLineSkipped[line.number])
                                changes.push({ from: line.from + column.minCol, insert: this.lineCommentToken + this.margin });
                        }
                    }
                    return state.update({ changes });
                }
            }
            return null;
        }
        isRangeCommented(_state, lines) {
            let minCol = Infinity;
            let isRangeLineDiscarded = true;
            const isLineSkipped = [];
            for (const line of lines) {
                const str = line.slice(0, Math.min(line.length, SearchMargin));
                const col = /^\s*/.exec(str)[0].length;
                if ((lines.length == 1 || col < str.length) && col < minCol) {
                    minCol = col;
                }
                if (isRangeLineDiscarded && (lines.length == 1 || col < str.length) &&
                    str.slice(col, col + this.lineCommentToken.length) != this.lineCommentToken) {
                    isRangeLineDiscarded = false;
                }
                isLineSkipped[line.number] = col == str.length;
            }
            return { minCol: minCol, isRangeLineSkipped: isRangeLineDiscarded, isLineSkipped: isLineSkipped };
        }
    }
    // Computes the lines spanned by `range`.
    /// @internal
    function getLinesInRange(doc, range) {
        let line = doc.lineAt(range.from);
        const lines = [];
        while (line.from + line.length < range.to ||
            (line.from <= range.to && range.to <= line.to)) {
            lines.push(line);
            if (line.number + 1 <= doc.lines) {
                line = doc.line(line.number + 1);
            }
            else {
                break;
            }
        }
        return lines;
    }

    // Don't compute precise column positions for line offsets above this
    // (since it could get expensive). Assume offset==column for them.
    const MaxOff = 2000;
    function rectangleFor(state, a, b) {
        let startLine = Math.min(a.line, b.line), endLine = Math.max(a.line, b.line);
        let ranges = [];
        if (a.off > MaxOff || b.off > MaxOff || a.col < 0 || b.col < 0) {
            let startOff = Math.min(a.off, b.off), endOff = Math.max(a.off, b.off);
            for (let i = startLine; i <= endLine; i++) {
                let line = state.doc.line(i);
                if (line.length <= endOff)
                    ranges.push(EditorSelection.range(line.from + startOff, line.to + endOff));
            }
        }
        else {
            let startCol = Math.min(a.col, b.col), endCol = Math.max(a.col, b.col);
            for (let i = startLine; i <= endLine; i++) {
                let line = state.doc.line(i), str = line.length > MaxOff ? line.slice(0, 2 * endCol) : line.slice();
                let start = findColumn(str, 0, startCol, state.tabSize), end = findColumn(str, 0, endCol, state.tabSize);
                if (!start.leftOver)
                    ranges.push(EditorSelection.range(line.from + start.offset, line.from + end.offset));
            }
        }
        return ranges;
    }
    function absoluteColumn(view, x) {
        let ref = view.coordsAtPos(view.viewport.from);
        return ref ? Math.round(Math.abs((ref.left - x) / view.defaultCharacterWidth)) : -1;
    }
    function getPos(view, event) {
        let offset = view.posAtCoords({ x: event.clientX, y: event.clientY });
        if (offset == null)
            return null;
        let line = view.state.doc.lineAt(offset), off = offset - line.from;
        let col = off > MaxOff ? -1
            : off == line.length ? absoluteColumn(view, event.clientX)
                : countColumn(line.slice(0, offset - line.from), 0, view.state.tabSize);
        return { line: line.number, col, off };
    }
    function rectangleSelectionStyle(view, event) {
        let start = getPos(view, event), startSel = view.state.selection;
        if (!start)
            return null;
        return {
            update(update) {
                if (update.docChanged) {
                    let newStart = update.changes.mapPos(update.prevState.doc.line(start.line).from);
                    let newLine = update.state.doc.lineAt(newStart);
                    start = { line: newLine.number, col: start.col, off: Math.min(start.off, newLine.length) };
                    startSel = startSel.map(update.changes);
                }
            },
            get(event, _extend, multiple) {
                let cur = getPos(view, event);
                if (!cur)
                    return startSel;
                let ranges = rectangleFor(view.state, start, cur);
                if (!ranges.length)
                    return startSel;
                if (multiple)
                    return EditorSelection.create(ranges.concat(startSel.ranges));
                else
                    return EditorSelection.create(ranges);
            }
        };
    }
    /// Create an extension that enables rectangular selections. By
    /// default, it will rect to left mouse drag with the alt key held
    /// down. When such a selection occurs, the text within the rectangle
    /// that was dragged over will be selected, as one selection
    /// [range](#state.SelectionRange) per line. You can pass a custom
    /// predicate function, which takes a `mousedown` event and returns
    /// true if it should be used for rectangular selection.
    function rectangularSelection(eventFilter) {
        let filter = eventFilter || (e => e.altKey && e.button == 0);
        return EditorView.mouseSelectionStyle.of((view, event) => filter(event) ? rectangleSelectionStyle(view, event) : null);
    }

    const extTag = typeof Symbol == "undefined" ? "__goto-line" : Symbol("goto-line");
    function createLineDialog(view) {
        let dom = document.createElement("form");
        dom.innerHTML = `<label>${view.state.phrase("Go to line:")} <input class=${themeClass("textfield")} name=line></label>
<button class=${themeClass("button")} type=submit>${view.state.phrase("go")}</button>`;
        let input = dom.querySelector("input");
        function go() {
            let n = parseInt(input.value, 10);
            view.dispatch({
                reconfigure: { [extTag]: [baseTheme$8] },
                selection: !isNaN(n) && n > 0 && n <= view.state.doc.lines ? EditorSelection.cursor(view.state.doc.line(n).from) : undefined,
                scrollIntoView: true
            });
            view.focus();
        }
        dom.addEventListener("keydown", event => {
            if (event.keyCode == 27) { // Escape
                event.preventDefault();
                view.dispatch({ reconfigure: { append: [baseTheme$8] } });
                view.focus();
            }
            else if (event.keyCode == 13) { // Enter
                event.preventDefault();
                go();
            }
        });
        dom.addEventListener("submit", go);
        return { dom, style: "gotoLine", pos: -10 };
    }
    /// Command that shows a dialog asking the user for a line number, and
    /// when a valid number is provided, moves the cursor to that line.
    ///
    /// The dialog can be styled with the `panel.gotoLine` theme
    /// selector.
    const gotoLine = view => {
        let panel = getPanel(view, createLineDialog);
        if (!panel) {
            view.dispatch({ reconfigure: { append: [panels(), showPanel.of(createLineDialog), baseTheme$8] } });
            panel = getPanel(view, createLineDialog);
        }
        if (panel)
            panel.dom.querySelector("input").focus();
        return true;
    };
    const baseTheme$8 = EditorView.baseTheme({
        "$panel.gotoLine": {
            padding: "2px 6px 4px",
            "& label": { fontSize: "80%" }
        }
    });
    /// Keymap that binds Alt-g to [`gotoLine`](#goto-line.gotoLine).
    const gotoLineKeymap = [
        { key: "Alt-g", run: gotoLine }
    ];

    /// Mark lines that have a cursor on them with the \`$activeLine\`
    /// theme class.
    function highlightActiveLine() {
        return [defaultTheme, activeLineHighlighter];
    }
    const lineDeco = Decoration.line({ attributes: { class: themeClass("activeLine") } });
    const activeLineHighlighter = ViewPlugin.fromClass(class {
        constructor(view) {
            this.decorations = this.getDeco(view);
        }
        update(update) {
            if (update.docChanged || update.selectionSet)
                this.decorations = this.getDeco(update.view);
        }
        getDeco(view) {
            let lastLineStart = -1, deco = [];
            for (let r of view.state.selection.ranges) {
                if (!r.empty)
                    continue;
                let line = view.visualLineAt(r.head);
                if (line.from > lastLineStart) {
                    deco.push(lineDeco.range(line.from));
                    lastLineStart = line.from;
                }
            }
            return Decoration.set(deco);
        }
    }, {
        decorations: v => v.decorations
    });
    const defaultHighlightOptions = {
        highlightWordAroundCursor: false,
        minSelectionLength: 1,
        maxMatches: 100
    };
    const highlightConfig = Facet.define({
        combine(options) {
            return combineConfig(options, defaultHighlightOptions, {
                highlightWordAroundCursor: (a, b) => a || b,
                minSelectionLength: Math.min,
                maxMatches: Math.min
            });
        }
    });
    /// This extension highlights text that matches the selection. It uses
    /// the `$selectionMatch` theme class for the highlighting. When
    /// `highlightWordAroundCursor` is enabled, the word at the cursor
    /// itself will be highlighted with `selectionMatch.main`.
    function highlightSelectionMatches(options) {
        let ext = [defaultTheme, matchHighlighter];
        if (options)
            ext.push(highlightConfig.of(options));
        return ext;
    }
    function wordAt(doc, pos, check) {
        let line = doc.lineAt(pos);
        let from = pos - line.from, to = pos - line.from;
        while (from > 0) {
            let prev = line.findClusterBreak(from, false);
            if (check(line.slice(prev, from)) != CharCategory.Word)
                break;
            from = prev;
        }
        while (to < line.length) {
            let next = line.findClusterBreak(to, true);
            if (check(line.slice(to, next)) != CharCategory.Word)
                break;
            to = next;
        }
        return from == to ? null : line.slice(from, to);
    }
    const matchDeco = Decoration.mark({ class: themeClass("selectionMatch") });
    const mainMatchDeco = Decoration.mark({ class: themeClass("selectionMatch.main") });
    const matchHighlighter = ViewPlugin.fromClass(class {
        constructor(view) {
            this.decorations = this.getDeco(view);
        }
        update(update) {
            if (update.selectionSet || update.docChanged || update.viewportChanged)
                this.decorations = this.getDeco(update.view);
        }
        getDeco(view) {
            let conf = view.state.facet(highlightConfig);
            let { state } = view, sel = state.selection;
            if (sel.ranges.length > 1)
                return Decoration.none;
            let range = sel.primary, query, check = null;
            if (range.empty) {
                if (!conf.highlightWordAroundCursor)
                    return Decoration.none;
                check = state.charCategorizer(range.head);
                query = wordAt(state.doc, range.head, check);
                if (!query)
                    return Decoration.none;
            }
            else {
                let len = range.to - range.from;
                if (len < conf.minSelectionLength || len > 200)
                    return Decoration.none;
                query = state.sliceDoc(range.from, range.to).trim();
                if (!query)
                    return Decoration.none;
            }
            let deco = [];
            for (let part of view.visibleRanges) {
                let cursor = new SearchCursor(state.doc, query, part.from, part.to);
                while (!cursor.next().done) {
                    let { from, to } = cursor.value;
                    if (!check || ((from == 0 || check(state.sliceDoc(from - 1, from)) != CharCategory.Word) &&
                        (to == state.doc.length || check(state.sliceDoc(to, to + 1)) != CharCategory.Word))) {
                        if (check && from <= range.from && to >= range.to)
                            deco.push(mainMatchDeco.range(from, to));
                        else if (from >= range.to || to <= range.from)
                            deco.push(matchDeco.range(from, to));
                        if (deco.length > conf.maxMatches)
                            return Decoration.none;
                    }
                }
            }
            return Decoration.set(deco);
        }
    }, {
        decorations: v => v.decorations
    });
    const defaultTheme = EditorView.baseTheme({
        "$$light $activeLine": { backgroundColor: "#f3f9ff" },
        "$$dark $activeLine": { backgroundColor: "#223039" },
        "$selectionMatch": { backgroundColor: "#99ff7780" },
        "$searchMatch $selectionMatch": { backgroundColor: "transparent" }
    });

    class SelectedDiagnostic {
        constructor(from, to, diagnostic) {
            this.from = from;
            this.to = to;
            this.diagnostic = diagnostic;
        }
    }
    class LintState {
        constructor(diagnostics, panel, selected) {
            this.diagnostics = diagnostics;
            this.panel = panel;
            this.selected = selected;
        }
    }
    function findDiagnostic(diagnostics, diagnostic = null, after = 0) {
        let found = null;
        diagnostics.between(after, diagnostics.length, (from, to, { spec }) => {
            if (diagnostic && spec.diagnostic != diagnostic)
                return;
            found = new SelectedDiagnostic(from, to, spec.diagnostic);
            return false;
        });
        return found;
    }
    function maybeEnableLint(state) {
        return state.field(lintState, false) ? undefined : { append: [
                lintState,
                EditorView.decorations.compute([lintState], state => {
                    let { selected, panel } = state.field(lintState);
                    return !selected || !panel || selected.from == selected.to ? Decoration.none : Decoration.set([
                        activeMark.range(selected.from, selected.to)
                    ]);
                }),
                panels(),
                hoverTooltip(lintTooltip),
                baseTheme$9
            ] };
    }
    const setDiagnosticsEffect = StateEffect.define();
    const togglePanel$1 = StateEffect.define();
    const movePanelSelection = StateEffect.define();
    const lintState = StateField.define({
        create() {
            return new LintState(Decoration.none, null, null);
        },
        update(value, tr) {
            if (tr.docChanged) {
                let mapped = value.diagnostics.map(tr.changes), selected = null;
                if (value.selected) {
                    let selPos = tr.changes.mapPos(value.selected.from, 1);
                    selected = findDiagnostic(mapped, value.selected.diagnostic, selPos) || findDiagnostic(mapped, null, selPos);
                }
                value = new LintState(mapped, value.panel, selected);
            }
            for (let effect of tr.effects) {
                if (effect.is(setDiagnosticsEffect)) {
                    let ranges = Decoration.set(effect.value.map((d) => {
                        return d.from < d.to
                            ? Decoration.mark({
                                attributes: { class: themeClass("lintRange." + d.severity) },
                                diagnostic: d
                            }).range(d.from, d.to)
                            : Decoration.widget({
                                widget: new DiagnosticWidget(d),
                                diagnostic: d
                            }).range(d.from);
                    }));
                    value = new LintState(ranges, value.panel, findDiagnostic(ranges));
                }
                else if (effect.is(togglePanel$1)) {
                    value = new LintState(value.diagnostics, effect.value ? LintPanel.open : null, value.selected);
                }
                else if (effect.is(movePanelSelection)) {
                    value = new LintState(value.diagnostics, value.panel, effect.value);
                }
            }
            return value;
        },
        provide: [showPanel.nFrom(s => s.panel ? [s.panel] : []),
            EditorView.decorations.from(s => s.diagnostics)]
    });
    const activeMark = Decoration.mark({ class: themeClass("lintRange.active") });
    function lintTooltip(view, check) {
        let { diagnostics } = view.state.field(lintState);
        let found = [], stackStart = 2e8, stackEnd = 0;
        diagnostics.between(0, view.state.doc.length, (start, end, { spec }) => {
            if (check(start, end)) {
                found.push(spec.diagnostic);
                stackStart = Math.min(start, stackStart);
                stackEnd = Math.max(end, stackEnd);
            }
        });
        if (!found.length)
            return null;
        return {
            pos: stackStart,
            end: stackEnd,
            above: view.state.doc.lineAt(stackStart).to < stackEnd,
            style: "lint",
            create() {
                let dom = document.createElement("ul");
                for (let d of found)
                    dom.appendChild(renderDiagnostic(view, d));
                return { dom };
            }
        };
    }
    /// Command to open and focus the lint panel.
    const openLintPanel = (view) => {
        let field = view.state.field(lintState, false);
        if (!field || !field.panel)
            view.dispatch({ effects: togglePanel$1.of(true),
                reconfigure: maybeEnableLint(view.state) });
        let panel = getPanel(view, LintPanel.open);
        if (panel)
            panel.dom.querySelector(".cm-panel-lint ul").focus();
        return true;
    };
    /// Command to close the lint panel, when open.
    const closeLintPanel = (view) => {
        let field = view.state.field(lintState, false);
        if (!field || !field.panel)
            return false;
        view.dispatch({ effects: togglePanel$1.of(false) });
        return true;
    };
    /// Move the selection to the next diagnostic.
    const nextDiagnostic = (view) => {
        let field = view.state.field(lintState, false);
        if (!field)
            return false;
        let sel = view.state.selection.primary, next = field.diagnostics.iter(sel.to + 1);
        if (!next.value) {
            next = field.diagnostics.iter(0);
            if (!next.value || next.from == sel.from && next.to == sel.to)
                return false;
        }
        view.dispatch({ selection: { anchor: next.from, head: next.to }, scrollIntoView: true });
        return true;
    };
    /// A set of default key bindings for the lint functionality.
    ///
    /// - Ctrl-Shift-m (Cmd-Shift-m on macOS): [`openLintPanel`](#lint.openLintPanel)
    /// - F8: [\`nextDiagnostic\`](#lint.nextDiagnostic)
    const lintKeymap = [
        { key: "Mod-Shift-m", run: openLintPanel },
        { key: "F8", run: nextDiagnostic }
    ];
    function renderDiagnostic(view, diagnostic) {
        let dom = document.createElement("li");
        dom.textContent = diagnostic.message;
        dom.className = themeClass("diagnostic." + diagnostic.severity);
        if (diagnostic.actions)
            for (let action of diagnostic.actions) {
                let button = dom.appendChild(document.createElement("button"));
                button.className = themeClass("diagnosticAction");
                button.textContent = action.name;
                button.onclick = button.onmousedown = e => {
                    e.preventDefault();
                    let found = findDiagnostic(view.state.field(lintState).diagnostics, diagnostic);
                    if (found)
                        action.apply(view, found.from, found.to);
                };
            }
        // FIXME render source?
        return dom;
    }
    class DiagnosticWidget extends WidgetType {
        constructor(diagnostic) {
            super();
            this.diagnostic = diagnostic;
        }
        eq(other) { return other.diagnostic == this.diagnostic; }
        toDOM() {
            let elt = document.createElement("span");
            elt.className = themeClass("lintPoint." + this.diagnostic.severity);
            return elt;
        }
    }
    class PanelItem {
        constructor(view, diagnostic) {
            this.diagnostic = diagnostic;
            this.id = "item_" + Math.floor(Math.random() * 0xffffffff).toString(16);
            this.dom = renderDiagnostic(view, diagnostic);
            this.dom.setAttribute("role", "option");
        }
    }
    class LintPanel {
        constructor(view) {
            this.view = view;
            this.items = [];
            this.dom = document.createElement("div");
            this.list = this.dom.appendChild(document.createElement("ul"));
            this.list.tabIndex = 0;
            this.list.setAttribute("role", "listbox");
            this.list.setAttribute("aria-label", this.view.state.phrase("Diagnostics"));
            this.list.addEventListener("keydown", event => {
                if (event.keyCode == 27) { // Escape
                    event.preventDefault();
                    closeLintPanel(this.view);
                    this.view.focus();
                }
                else if (event.keyCode == 38) { // ArrowUp
                    event.preventDefault();
                    this.moveSelection((this.selectedIndex - 1 + this.items.length) % this.items.length);
                }
                else if (event.keyCode == 40) { // ArrowDown
                    event.preventDefault();
                    this.moveSelection((this.selectedIndex + 1) % this.items.length);
                }
                else if (event.keyCode == 36) { // Home
                    event.preventDefault();
                    this.moveSelection(0);
                }
                else if (event.keyCode == 35) { // End
                    event.preventDefault();
                    this.moveSelection(this.items.length - 1);
                }
                else if (event.keyCode == 13) {
                    event.preventDefault();
                    this.view.focus();
                } // FIXME PageDown/PageUp
            });
            this.list.addEventListener("click", event => {
                for (let i = 0; i < this.items.length; i++) {
                    if (this.items[i].dom.contains(event.target))
                        this.moveSelection(i);
                }
            });
            let close = this.dom.appendChild(document.createElement("button"));
            close.setAttribute("name", "close");
            close.setAttribute("aria-label", this.view.state.phrase("close"));
            close.textContent = "×";
            close.addEventListener("click", () => closeLintPanel(this.view));
            this.update();
        }
        get selectedIndex() {
            let selected = this.view.state.field(lintState).selected;
            if (!selected)
                return -1;
            for (let i = 0; i < this.items.length; i++)
                if (this.items[i].diagnostic == selected.diagnostic)
                    return i;
            return -1;
        }
        update() {
            let { diagnostics, selected } = this.view.state.field(lintState);
            let i = 0, needsSync = false, newSelectedItem = null;
            diagnostics.between(0, this.view.state.doc.length, (_start, _end, { spec }) => {
                let found = -1, item;
                for (let j = i; j < this.items.length; j++)
                    if (this.items[j].diagnostic == spec.diagnostic) {
                        found = j;
                        break;
                    }
                if (found < 0) {
                    item = new PanelItem(this.view, spec.diagnostic);
                    this.items.splice(i, 0, item);
                    needsSync = true;
                }
                else {
                    item = this.items[found];
                    if (found > i) {
                        this.items.splice(i, found - i);
                        needsSync = true;
                    }
                }
                if (selected && item.diagnostic == selected.diagnostic) {
                    if (!item.dom.hasAttribute("aria-selected")) {
                        item.dom.setAttribute("aria-selected", "true");
                        newSelectedItem = item;
                    }
                }
                else if (item.dom.hasAttribute("aria-selected")) {
                    item.dom.removeAttribute("aria-selected");
                }
                i++;
            });
            while (i < this.items.length && !(this.items.length == 1 && this.items[0].diagnostic.from < 0)) {
                needsSync = true;
                this.items.pop();
            }
            if (this.items.length == 0) {
                this.items.push(new PanelItem(this.view, {
                    from: -1, to: -1,
                    severity: "info",
                    message: this.view.state.phrase("No diagnostics")
                }));
                needsSync = true;
            }
            if (newSelectedItem) {
                this.list.setAttribute("aria-activedescendant", newSelectedItem.id);
                this.view.requestMeasure({
                    key: this,
                    read: () => ({ sel: newSelectedItem.dom.getBoundingClientRect(), panel: this.list.getBoundingClientRect() }),
                    write: ({ sel, panel }) => {
                        if (sel.top < panel.top)
                            this.list.scrollTop -= panel.top - sel.top;
                        else if (sel.bottom > panel.bottom)
                            this.list.scrollTop += sel.bottom - panel.bottom;
                    }
                });
            }
            else if (!this.items.length) {
                this.list.removeAttribute("aria-activedescendant");
            }
            if (needsSync)
                this.sync();
        }
        sync() {
            let domPos = this.list.firstChild;
            function rm() {
                let prev = domPos;
                domPos = prev.nextSibling;
                prev.remove();
            }
            for (let item of this.items) {
                if (item.dom.parentNode == this.list) {
                    while (domPos != item.dom)
                        rm();
                    domPos = item.dom.nextSibling;
                }
                else {
                    this.list.insertBefore(item.dom, domPos);
                }
            }
            while (domPos)
                rm();
            if (!this.list.firstChild)
                this.list.appendChild(renderDiagnostic(this.view, {
                    severity: "info",
                    message: this.view.state.phrase("No diagnostics")
                }));
        }
        moveSelection(selectedIndex) {
            // FIXME make actions accessible
            if (this.items.length == 0)
                return;
            let field = this.view.state.field(lintState);
            let selection = findDiagnostic(field.diagnostics, this.items[selectedIndex].diagnostic);
            if (!selection)
                return;
            this.view.dispatch({
                selection: { anchor: selection.from, head: selection.to },
                scrollIntoView: true,
                effects: movePanelSelection.of(selection)
            });
        }
        get style() { return "lint"; }
        static open(view) { return new LintPanel(view); }
    }
    function underline(color) {
        if (typeof btoa != "function")
            return "none";
        let svg = `<svg xmlns="http://www.w3.org/2000/svg" width="6" height="3">
    <path d="m0 3 l2 -2 l1 0 l2 2 l1 0" stroke="${color}" fill="none" stroke-width=".7"/>
  </svg>`;
        return `url('data:image/svg+xml;base64,${btoa(svg)}')`;
    }
    const baseTheme$9 = EditorView.baseTheme({
        $diagnostic: {
            padding: "3px 6px 3px 8px",
            marginLeft: "-1px",
            display: "block"
        },
        "$diagnostic.error": { borderLeft: "5px solid #d11" },
        "$diagnostic.warning": { borderLeft: "5px solid orange" },
        "$diagnostic.info": { borderLeft: "5px solid #999" },
        $diagnosticAction: {
            font: "inherit",
            border: "none",
            padding: "2px 4px",
            backgroundColor: "#444",
            color: "white",
            borderRadius: "3px",
            marginLeft: "8px"
        },
        $lintRange: {
            backgroundPosition: "left bottom",
            backgroundRepeat: "repeat-x"
        },
        "$lintRange.error": { backgroundImage: underline("#d11") },
        "$lintRange.warning": { backgroundImage: underline("orange") },
        "$lintRange.info": { backgroundImage: underline("#999") },
        "$lintRange.active": { backgroundColor: "#ffdd9980" },
        $lintPoint: {
            position: "relative",
            "&:after": {
                content: '""',
                position: "absolute",
                bottom: 0,
                left: "-2px",
                borderLeft: "3px solid transparent",
                borderRight: "3px solid transparent",
                borderBottom: "4px solid #d11"
            }
        },
        "$lintPoint.warning": {
            "&:after": { borderBottomColor: "orange" }
        },
        "$lintPoint.info": {
            "&:after": { borderBottomColor: "#999" }
        },
        "$panel.lint": {
            position: "relative",
            "& ul": {
                maxHeight: "100px",
                overflowY: "auto",
                "& [aria-selected]": {
                    backgroundColor: "#ddd"
                },
                "&:focus [aria-selected]": {
                    background_fallback: "#bdf",
                    backgroundColor: "Highlight",
                    color_fallback: "white",
                    color: "HighlightText"
                },
                padding: 0,
                margin: 0
            },
            "& [name=close]": {
                position: "absolute",
                top: "0",
                right: "2px",
                background: "inherit",
                border: "none",
                font: "inherit",
                padding: 0,
                margin: 0
            }
        },
        "$tooltip.lint": {
            padding: 0,
            margin: 0
        }
    });

    /// This is an extension value that just pulls together a whole lot of
    /// extensions that you might want in a basic editor. It is meant as a
    /// convenient helper to quickly set up CodeMirror without installing
    /// and importing a lot of packages.
    ///
    /// Specifically, it includes...
    ///
    ///  - [the default command bindings](#commands.defaultKeymap)
    ///  - [line numbers](#gutter.lineNumbers)
    ///  - [special character highlighting](#view.highlightSpecialChars)
    ///  - [the undo history](#history.history)
    ///  - [a fold gutter](#fold.foldGutter)
    ///  - [custom selection drawing](#view.drawSelection)
    ///  - [multiple selections](#state.EditorState^allowMultipleSelections)
    ///  - [reindentation on input](#language.indentOnInput)
    ///  - [the default highlight style](#highlight.defaultHighlightStyle)
    ///  - [bracket matching](#matchbrackets.bracketMatching)
    ///  - [bracket closing](#closebrackets.closeBrackets)
    ///  - [autocompletion](#autocomplete.autocompletion)
    ///  - [rectangular selection](#rectangular-selection.rectangularSelection)
    ///  - [active line highlighting](#highlight-selection.highlightActiveLine)
    ///  - [selection match highlighting](#highlight-selection.highlightSelectionMatches)
    ///  - [search](#search.searchKeymap)
    ///  - [go to line](#goto-line.gotoLineKeymap)
    ///  - [commenting](#comment.commentKeymap)
    ///  - [linting](#lint.lintKeymap)
    ///
    /// (You'll probably want to add some language package to your setup
    /// too.)
    ///
    /// This package does not allow customization. The idea is that, once
    /// you decide you want to configure your editor more precisely, you
    /// take this package's source (which is just a bunch of imports and
    /// an array literal), copy it into your own code, and adjust it as
    /// desired.
    const basicSetup = [
        lineNumbers(),
        highlightSpecialChars(),
        history(),
        foldGutter(),
        drawSelection(),
        EditorState.allowMultipleSelections.of(true),
        indentOnInput(),
        defaultHighlightStyle,
        bracketMatching(),
        closeBrackets(),
        autocompletion(),
        rectangularSelection(),
        highlightActiveLine(),
        highlightSelectionMatches(),
        keymap([
            ...closeBracketsKeymap,
            ...defaultKeymap,
            ...searchKeymap,
            ...historyKeymap,
            ...foldKeymap,
            ...commentKeymap,
            ...gotoLineKeymap,
            ...completionKeymap,
            ...lintKeymap
        ])
    ];

    /// A parse stack. These are used internally by the parser to track
    /// parsing progress. They also provide some properties and methods
    /// that external code such as a tokenizer can use to get information
    /// about the parse state.
    class Stack {
        /// @internal
        constructor(
        /// A the parse that this stack is part of @internal
        p, 
        /// Holds state, pos, value stack pos (15 bits array index, 15 bits
        /// buffer index) triplets for all but the top state
        /// @internal
        stack, 
        /// The current parse state @internal
        state, 
        // The position at which the next reduce should take place. This
        // can be less than `this.pos` when skipped expressions have been
        // added to the stack (which should be moved outside of the next
        // reduction)
        /// @internal
        reducePos, 
        /// The input position up to which this stack has parsed.
        pos, 
        /// The dynamic score of the stack, including dynamic precedence
        /// and error-recovery penalties
        /// @internal
        score, 
        // The output buffer. Holds (type, start, end, size) quads
        // representing nodes created by the parser, where `size` is
        // amount of buffer array entries covered by this node.
        /// @internal
        buffer, 
        // The base offset of the buffer. When stacks are split, the split
        // instance shared the buffer history with its parent up to
        // `bufferBase`, which is the absolute offset (including the
        // offset of previous splits) into the buffer at which this stack
        // starts writing.
        /// @internal
        bufferBase, 
        /// @internal
        curContext, 
        // A parent stack from which this was split off, if any. This is
        // set up so that it always points to a stack that has some
        // additional buffer content, never to a stack with an equal
        // `bufferBase`.
        /// @internal
        parent) {
            this.p = p;
            this.stack = stack;
            this.state = state;
            this.reducePos = reducePos;
            this.pos = pos;
            this.score = score;
            this.buffer = buffer;
            this.bufferBase = bufferBase;
            this.curContext = curContext;
            this.parent = parent;
        }
        /// @internal
        toString() {
            return `[${this.stack.filter((_, i) => i % 3 == 0).concat(this.state)}]@${this.pos}${this.score ? "!" + this.score : ""}`;
        }
        // Start an empty stack
        /// @internal
        static start(p, state, pos = 0) {
            let cx = p.parser.context;
            return new Stack(p, [], state, pos, pos, 0, [], 0, cx ? new StackContext(cx, cx.start) : null, null);
        }
        /// The stack's current [context](#lezer.ContextTracker) value, if
        /// any. Its type will depend on the context tracker's type
        /// parameter, or it will be `null` if there is no context
        /// tracker.
        get context() { return this.curContext ? this.curContext.context : null; }
        // Push a state onto the stack, tracking its start position as well
        // as the buffer base at that point.
        /// @internal
        pushState(state, start) {
            this.stack.push(this.state, start, this.bufferBase + this.buffer.length);
            this.state = state;
        }
        // Apply a reduce action
        /// @internal
        reduce(action) {
            let depth = action >> 19 /* ReduceDepthShift */, type = action & 65535 /* ValueMask */;
            let { parser } = this.p;
            let dPrec = parser.dynamicPrecedence(type);
            if (dPrec)
                this.score += dPrec;
            if (depth == 0) {
                // Zero-depth reductions are a special case—they add stuff to
                // the stack without popping anything off.
                if (type < parser.minRepeatTerm)
                    this.storeNode(type, this.reducePos, this.reducePos, 4, true);
                this.pushState(parser.getGoto(this.state, type, true), this.reducePos);
                this.reduceContext(type);
                return;
            }
            // Find the base index into `this.stack`, content after which will
            // be dropped. Note that with `StayFlag` reductions we need to
            // consume two extra frames (the dummy parent node for the skipped
            // expression and the state that we'll be staying in, which should
            // be moved to `this.state`).
            let base = this.stack.length - ((depth - 1) * 3) - (action & 262144 /* StayFlag */ ? 6 : 0);
            let start = this.stack[base - 2];
            let bufferBase = this.stack[base - 1], count = this.bufferBase + this.buffer.length - bufferBase;
            // Store normal terms or `R -> R R` repeat reductions
            if (type < parser.minRepeatTerm || (action & 131072 /* RepeatFlag */)) {
                let pos = parser.stateFlag(this.state, 1 /* Skipped */) ? this.pos : this.reducePos;
                this.storeNode(type, start, pos, count + 4, true);
            }
            if (action & 262144 /* StayFlag */) {
                this.state = this.stack[base];
            }
            else {
                let baseStateID = this.stack[base - 3];
                this.state = parser.getGoto(baseStateID, type, true);
            }
            while (this.stack.length > base)
                this.stack.pop();
            this.reduceContext(type);
        }
        // Shift a value into the buffer
        /// @internal
        storeNode(term, start, end, size = 4, isReduce = false) {
            if (term == 0 /* Err */) { // Try to omit/merge adjacent error nodes
                let cur = this, top = this.buffer.length;
                if (top == 0 && cur.parent) {
                    top = cur.bufferBase - cur.parent.bufferBase;
                    cur = cur.parent;
                }
                if (top > 0 && cur.buffer[top - 4] == 0 /* Err */ && cur.buffer[top - 1] > -1) {
                    if (start == end)
                        return;
                    if (cur.buffer[top - 2] >= start) {
                        cur.buffer[top - 2] = end;
                        return;
                    }
                }
            }
            if (!isReduce || this.pos == end) { // Simple case, just append
                this.buffer.push(term, start, end, size);
            }
            else { // There may be skipped nodes that have to be moved forward
                let index = this.buffer.length;
                if (index > 0 && this.buffer[index - 4] != 0 /* Err */)
                    while (index > 0 && this.buffer[index - 2] > end) {
                        // Move this record forward
                        this.buffer[index] = this.buffer[index - 4];
                        this.buffer[index + 1] = this.buffer[index - 3];
                        this.buffer[index + 2] = this.buffer[index - 2];
                        this.buffer[index + 3] = this.buffer[index - 1];
                        index -= 4;
                        if (size > 4)
                            size -= 4;
                    }
                this.buffer[index] = term;
                this.buffer[index + 1] = start;
                this.buffer[index + 2] = end;
                this.buffer[index + 3] = size;
            }
        }
        // Apply a shift action
        /// @internal
        shift(action, next, nextEnd) {
            if (action & 131072 /* GotoFlag */) {
                this.pushState(action & 65535 /* ValueMask */, this.pos);
            }
            else if ((action & 262144 /* StayFlag */) == 0) { // Regular shift
                let start = this.pos, nextState = action, { parser } = this.p;
                if (nextEnd > this.pos || next <= parser.maxNode) {
                    this.pos = nextEnd;
                    if (!parser.stateFlag(nextState, 1 /* Skipped */))
                        this.reducePos = nextEnd;
                }
                this.pushState(nextState, start);
                if (next <= parser.maxNode)
                    this.buffer.push(next, start, nextEnd, 4);
                this.shiftContext(next);
            }
            else { // Shift-and-stay, which means this is a skipped token
                if (next <= this.p.parser.maxNode)
                    this.buffer.push(next, this.pos, nextEnd, 4);
                this.pos = nextEnd;
            }
        }
        // Apply an action
        /// @internal
        apply(action, next, nextEnd) {
            if (action & 65536 /* ReduceFlag */)
                this.reduce(action);
            else
                this.shift(action, next, nextEnd);
        }
        // Add a prebuilt node into the buffer. This may be a reused node or
        // the result of running a nested parser.
        /// @internal
        useNode(value, next) {
            let index = this.p.reused.length - 1;
            if (index < 0 || this.p.reused[index] != value) {
                this.p.reused.push(value);
                index++;
            }
            let start = this.pos;
            this.reducePos = this.pos = start + value.length;
            this.pushState(next, start);
            this.buffer.push(index, start, this.reducePos, -1 /* size < 0 means this is a reused value */);
            if (this.curContext)
                this.updateContext(this.curContext.tracker.reuse(this.curContext.context, value, this.p.input, this));
        }
        // Split the stack. Due to the buffer sharing and the fact
        // that `this.stack` tends to stay quite shallow, this isn't very
        // expensive.
        /// @internal
        split() {
            let parent = this;
            let off = parent.buffer.length;
            // Because the top of the buffer (after this.pos) may be mutated
            // to reorder reductions and skipped tokens, and shared buffers
            // should be immutable, this copies any outstanding skipped tokens
            // to the new buffer, and puts the base pointer before them.
            while (off > 0 && parent.buffer[off - 2] > parent.reducePos)
                off -= 4;
            let buffer = parent.buffer.slice(off), base = parent.bufferBase + off;
            // Make sure parent points to an actual parent with content, if there is such a parent.
            while (parent && base == parent.bufferBase)
                parent = parent.parent;
            return new Stack(this.p, this.stack.slice(), this.state, this.reducePos, this.pos, this.score, buffer, base, this.curContext, parent);
        }
        // Try to recover from an error by 'deleting' (ignoring) one token.
        /// @internal
        recoverByDelete(next, nextEnd) {
            let isNode = next <= this.p.parser.maxNode;
            if (isNode)
                this.storeNode(next, this.pos, nextEnd);
            this.storeNode(0 /* Err */, this.pos, nextEnd, isNode ? 8 : 4);
            this.pos = this.reducePos = nextEnd;
            this.score -= 200 /* Token */;
        }
        /// Check if the given term would be able to be shifted (optionally
        /// after some reductions) on this stack. This can be useful for
        /// external tokenizers that want to make sure they only provide a
        /// given token when it applies.
        canShift(term) {
            for (let sim = new SimulatedStack(this);;) {
                let action = this.p.parser.stateSlot(sim.top, 4 /* DefaultReduce */) || this.p.parser.hasAction(sim.top, term);
                if ((action & 65536 /* ReduceFlag */) == 0)
                    return true;
                if (action == 0)
                    return false;
                sim.reduce(action);
            }
        }
        /// Find the start position of the rule that is currently being parsed.
        get ruleStart() {
            for (let state = this.state, base = this.stack.length;;) {
                let force = this.p.parser.stateSlot(state, 5 /* ForcedReduce */);
                if (!(force & 65536 /* ReduceFlag */))
                    return 0;
                base -= 3 * (force >> 19 /* ReduceDepthShift */);
                if ((force & 65535 /* ValueMask */) < this.p.parser.minRepeatTerm)
                    return this.stack[base + 1];
                state = this.stack[base];
            }
        }
        /// Find the start position of an instance of any of the given term
        /// types, or return `null` when none of them are found.
        ///
        /// **Note:** this is only reliable when there is at least some
        /// state that unambiguously matches the given rule on the stack.
        /// I.e. if you have a grammar like this, where the difference
        /// between `a` and `b` is only apparent at the third token:
        ///
        ///     a { b | c }
        ///     b { "x" "y" "x" }
        ///     c { "x" "y" "z" }
        ///
        /// Then a parse state after `"x"` will not reliably tell you that
        /// `b` is on the stack. You _can_ pass `[b, c]` to reliably check
        /// for either of those two rules (assuming that `a` isn't part of
        /// some rule that includes other things starting with `"x"`).
        ///
        /// When `before` is given, this keeps scanning up the stack until
        /// it finds a match that starts before that position.
        ///
        /// Note that you have to be careful when using this in tokenizers,
        /// since it's relatively easy to introduce data dependencies that
        /// break incremental parsing by using this method.
        startOf(types, before) {
            let state = this.state, frame = this.stack.length, { parser } = this.p;
            for (;;) {
                let force = parser.stateSlot(state, 5 /* ForcedReduce */);
                let depth = force >> 19 /* ReduceDepthShift */, term = force & 65535 /* ValueMask */;
                if (types.indexOf(term) > -1) {
                    let base = frame - (3 * (force >> 19 /* ReduceDepthShift */)), pos = this.stack[base + 1];
                    if (before == null || before > pos)
                        return pos;
                }
                if (frame == 0)
                    return null;
                if (depth == 0) {
                    frame -= 3;
                    state = this.stack[frame];
                }
                else {
                    frame -= 3 * (depth - 1);
                    state = parser.getGoto(this.stack[frame - 3], term, true);
                }
            }
        }
        // Apply up to Recover.MaxNext recovery actions that conceptually
        // inserts some missing token or rule.
        /// @internal
        recoverByInsert(next) {
            if (this.stack.length >= 300 /* MaxInsertStackDepth */)
                return [];
            let nextStates = this.p.parser.nextStates(this.state);
            if (nextStates.length > 4 /* MaxNext */ << 1 || this.stack.length >= 120 /* DampenInsertStackDepth */) {
                let best = [];
                for (let i = 0, s; i < nextStates.length; i += 2) {
                    if ((s = nextStates[i + 1]) != this.state && this.p.parser.hasAction(s, next))
                        best.push(nextStates[i], s);
                }
                if (this.stack.length < 120 /* DampenInsertStackDepth */)
                    for (let i = 0; best.length < 4 /* MaxNext */ << 1 && i < nextStates.length; i += 2) {
                        let s = nextStates[i + 1];
                        if (!best.some((v, i) => (i & 1) && v == s))
                            best.push(nextStates[i], s);
                    }
                nextStates = best;
            }
            let result = [];
            for (let i = 0; i < nextStates.length && result.length < 4 /* MaxNext */; i += 2) {
                let s = nextStates[i + 1];
                if (s == this.state)
                    continue;
                let stack = this.split();
                stack.storeNode(0 /* Err */, stack.pos, stack.pos, 4, true);
                stack.pushState(s, this.pos);
                stack.shiftContext(nextStates[i]);
                stack.score -= 200 /* Token */;
                result.push(stack);
            }
            return result;
        }
        // Force a reduce, if possible. Return false if that can't
        // be done.
        /// @internal
        forceReduce() {
            let reduce = this.p.parser.stateSlot(this.state, 5 /* ForcedReduce */);
            if ((reduce & 65536 /* ReduceFlag */) == 0)
                return false;
            if (!this.p.parser.validAction(this.state, reduce)) {
                this.storeNode(0 /* Err */, this.reducePos, this.reducePos, 4, true);
                this.score -= 100 /* Reduce */;
            }
            this.reduce(reduce);
            return true;
        }
        /// @internal
        forceAll() {
            while (!this.p.parser.stateFlag(this.state, 2 /* Accepting */) && this.forceReduce()) { }
            return this;
        }
        /// Check whether this state has no further actions (assumed to be a direct descendant of the
        /// top state, since any other states must be able to continue
        /// somehow). @internal
        get deadEnd() {
            if (this.stack.length != 3)
                return false;
            let { parser } = this.p;
            return parser.data[parser.stateSlot(this.state, 1 /* Actions */)] == 65535 /* End */ &&
                !parser.stateSlot(this.state, 4 /* DefaultReduce */);
        }
        /// Restart the stack (put it back in its start state). Only safe
        /// when this.stack.length == 3 (state is directly below the top
        /// state). @internal
        restart() {
            this.state = this.stack[0];
            this.stack.length = 0;
        }
        /// @internal
        sameState(other) {
            if (this.state != other.state || this.stack.length != other.stack.length)
                return false;
            for (let i = 0; i < this.stack.length; i += 3)
                if (this.stack[i] != other.stack[i])
                    return false;
            return true;
        }
        /// Get the parser used by this stack.
        get parser() { return this.p.parser; }
        /// Test whether a given dialect (by numeric ID, as exported from
        /// the terms file) is enabled.
        dialectEnabled(dialectID) { return this.p.parser.dialect.flags[dialectID]; }
        shiftContext(term) {
            if (this.curContext)
                this.updateContext(this.curContext.tracker.shift(this.curContext.context, term, this.p.input, this));
        }
        reduceContext(term) {
            if (this.curContext)
                this.updateContext(this.curContext.tracker.reduce(this.curContext.context, term, this.p.input, this));
        }
        /// @internal
        emitContext() {
            let cx = this.curContext;
            if (!cx.tracker.strict)
                return;
            let last = this.buffer.length - 1;
            if (last < 0 || this.buffer[last] != -2)
                this.buffer.push(cx.hash, this.reducePos, this.reducePos, -2);
        }
        updateContext(context) {
            if (context != this.curContext.context) {
                let newCx = new StackContext(this.curContext.tracker, context);
                if (newCx.hash != this.curContext.hash)
                    this.emitContext();
                this.curContext = newCx;
            }
        }
    }
    class StackContext {
        constructor(tracker, context) {
            this.tracker = tracker;
            this.context = context;
            this.hash = tracker.hash(context);
        }
    }
    var Recover;
    (function (Recover) {
        Recover[Recover["Token"] = 200] = "Token";
        Recover[Recover["Reduce"] = 100] = "Reduce";
        Recover[Recover["MaxNext"] = 4] = "MaxNext";
        Recover[Recover["MaxInsertStackDepth"] = 300] = "MaxInsertStackDepth";
        Recover[Recover["DampenInsertStackDepth"] = 120] = "DampenInsertStackDepth";
    })(Recover || (Recover = {}));
    // Used to cheaply run some reductions to scan ahead without mutating
    // an entire stack
    class SimulatedStack {
        constructor(stack) {
            this.stack = stack;
            this.top = stack.state;
            this.rest = stack.stack;
            this.offset = this.rest.length;
        }
        reduce(action) {
            let term = action & 65535 /* ValueMask */, depth = action >> 19 /* ReduceDepthShift */;
            if (depth == 0) {
                if (this.rest == this.stack.stack)
                    this.rest = this.rest.slice();
                this.rest.push(this.top, 0, 0);
                this.offset += 3;
            }
            else {
                this.offset -= (depth - 1) * 3;
            }
            let goto = this.stack.p.parser.getGoto(this.rest[this.offset - 3], term, true);
            this.top = goto;
        }
    }
    // This is given to `Tree.build` to build a buffer, and encapsulates
    // the parent-stack-walking necessary to read the nodes.
    class StackBufferCursor {
        constructor(stack, pos, index) {
            this.stack = stack;
            this.pos = pos;
            this.index = index;
            this.buffer = stack.buffer;
            if (this.index == 0)
                this.maybeNext();
        }
        static create(stack) {
            return new StackBufferCursor(stack, stack.bufferBase + stack.buffer.length, stack.buffer.length);
        }
        maybeNext() {
            let next = this.stack.parent;
            if (next != null) {
                this.index = this.stack.bufferBase - next.bufferBase;
                this.stack = next;
                this.buffer = next.buffer;
            }
        }
        get id() { return this.buffer[this.index - 4]; }
        get start() { return this.buffer[this.index - 3]; }
        get end() { return this.buffer[this.index - 2]; }
        get size() { return this.buffer[this.index - 1]; }
        next() {
            this.index -= 4;
            this.pos -= 4;
            if (this.index == 0)
                this.maybeNext();
        }
        fork() {
            return new StackBufferCursor(this.stack, this.pos, this.index);
        }
    }

    /// Tokenizers write the tokens they read into instances of this class.
    class Token {
        constructor() {
            /// The start of the token. This is set by the parser, and should not
            /// be mutated by the tokenizer.
            this.start = -1;
            /// This starts at -1, and should be updated to a term id when a
            /// matching token is found.
            this.value = -1;
            /// When setting `.value`, you should also set `.end` to the end
            /// position of the token. (You'll usually want to use the `accept`
            /// method.)
            this.end = -1;
        }
        /// Accept a token, setting `value` and `end` to the given values.
        accept(value, end) {
            this.value = value;
            this.end = end;
        }
    }
    /// @internal
    class TokenGroup {
        constructor(data, id) {
            this.data = data;
            this.id = id;
        }
        token(input, token, stack) { readToken(this.data, input, token, stack, this.id); }
    }
    TokenGroup.prototype.contextual = TokenGroup.prototype.fallback = TokenGroup.prototype.extend = false;
    // Tokenizer data is stored a big uint16 array containing, for each
    // state:
    //
    //  - A group bitmask, indicating what token groups are reachable from
    //    this state, so that paths that can only lead to tokens not in
    //    any of the current groups can be cut off early.
    //
    //  - The position of the end of the state's sequence of accepting
    //    tokens
    //
    //  - The number of outgoing edges for the state
    //
    //  - The accepting tokens, as (token id, group mask) pairs
    //
    //  - The outgoing edges, as (start character, end character, state
    //    index) triples, with end character being exclusive
    //
    // This function interprets that data, running through a stream as
    // long as new states with the a matching group mask can be reached,
    // and updating `token` when it matches a token.
    function readToken(data, input, token, stack, group) {
        let state = 0, groupMask = 1 << group, dialect = stack.p.parser.dialect;
        scan: for (let pos = token.start;;) {
            if ((groupMask & data[state]) == 0)
                break;
            let accEnd = data[state + 1];
            // Check whether this state can lead to a token in the current group
            // Accept tokens in this state, possibly overwriting
            // lower-precedence / shorter tokens
            for (let i = state + 3; i < accEnd; i += 2)
                if ((data[i + 1] & groupMask) > 0) {
                    let term = data[i];
                    if (dialect.allows(term) &&
                        (token.value == -1 || token.value == term || stack.p.parser.overrides(term, token.value))) {
                        token.accept(term, pos);
                        break;
                    }
                }
            let next = input.get(pos++);
            // Do a binary search on the state's edges
            for (let low = 0, high = data[state + 2]; low < high;) {
                let mid = (low + high) >> 1;
                let index = accEnd + mid + (mid << 1);
                let from = data[index], to = data[index + 1];
                if (next < from)
                    high = mid;
                else if (next >= to)
                    low = mid + 1;
                else {
                    state = data[index + 2];
                    continue scan;
                }
            }
            break;
        }
    }

    // See lezer-generator/src/encode.ts for comments about the encoding
    // used here
    function decodeArray(input, Type = Uint16Array) {
        if (typeof input != "string")
            return input;
        let array = null;
        for (let pos = 0, out = 0; pos < input.length;) {
            let value = 0;
            for (;;) {
                let next = input.charCodeAt(pos++), stop = false;
                if (next == 126 /* BigValCode */) {
                    value = 65535 /* BigVal */;
                    break;
                }
                if (next >= 92 /* Gap2 */)
                    next--;
                if (next >= 34 /* Gap1 */)
                    next--;
                let digit = next - 32 /* Start */;
                if (digit >= 46 /* Base */) {
                    digit -= 46 /* Base */;
                    stop = true;
                }
                value += digit;
                if (stop)
                    break;
                value *= 46 /* Base */;
            }
            if (array)
                array[out++] = value;
            else
                array = new Type(value);
        }
        return array;
    }

    // FIXME find some way to reduce recovery work done when the input
    // doesn't match the grammar at all.
    // Environment variable used to control console output
    const verbose = typeof process != "undefined" && /\bparse\b/.test(process.env.LOG);
    let stackIDs = null;
    function cutAt(tree, pos, side) {
        let cursor = tree.cursor(pos);
        for (;;) {
            if (!(side < 0 ? cursor.childBefore(pos) : cursor.childAfter(pos)))
                for (;;) {
                    if ((side < 0 ? cursor.to < pos : cursor.from > pos) && !cursor.type.isError)
                        return side < 0 ? Math.max(0, Math.min(cursor.to - 1, pos - 5)) : Math.min(tree.length, Math.max(cursor.from + 1, pos + 5));
                    if (side < 0 ? cursor.prevSibling() : cursor.nextSibling())
                        break;
                    if (!cursor.parent())
                        return side < 0 ? 0 : tree.length;
                }
        }
    }
    class FragmentCursor {
        constructor(fragments) {
            this.fragments = fragments;
            this.i = 0;
            this.fragment = null;
            this.safeFrom = -1;
            this.safeTo = -1;
            this.trees = [];
            this.start = [];
            this.index = [];
            this.nextFragment();
        }
        nextFragment() {
            let fr = this.fragment = this.i == this.fragments.length ? null : this.fragments[this.i++];
            if (fr) {
                this.safeFrom = fr.openStart ? cutAt(fr.tree, fr.from + fr.offset, 1) - fr.offset : fr.from;
                this.safeTo = fr.openEnd ? cutAt(fr.tree, fr.to + fr.offset, -1) - fr.offset : fr.to;
                while (this.trees.length) {
                    this.trees.pop();
                    this.start.pop();
                    this.index.pop();
                }
                this.trees.push(fr.tree);
                this.start.push(-fr.offset);
                this.index.push(0);
                this.nextStart = this.safeFrom;
            }
            else {
                this.nextStart = 1e9;
            }
        }
        // `pos` must be >= any previously given `pos` for this cursor
        nodeAt(pos) {
            if (pos < this.nextStart)
                return null;
            while (this.fragment && this.safeTo <= pos)
                this.nextFragment();
            if (!this.fragment)
                return null;
            for (;;) {
                let last = this.trees.length - 1;
                if (last < 0) { // End of tree
                    this.nextFragment();
                    return null;
                }
                let top = this.trees[last], index = this.index[last];
                if (index == top.children.length) {
                    this.trees.pop();
                    this.start.pop();
                    this.index.pop();
                    continue;
                }
                let next = top.children[index];
                let start = this.start[last] + top.positions[index];
                if (start > pos) {
                    this.nextStart = start;
                    return null;
                }
                else if (start == pos && start + next.length <= this.safeTo) {
                    return start == pos && start >= this.safeFrom ? next : null;
                }
                if (next instanceof TreeBuffer) {
                    this.index[last]++;
                    this.nextStart = start + next.length;
                }
                else {
                    this.index[last]++;
                    if (start + next.length >= pos) { // Enter this node
                        this.trees.push(next);
                        this.start.push(start);
                        this.index.push(0);
                    }
                }
            }
        }
    }
    class CachedToken extends Token {
        constructor() {
            super(...arguments);
            this.extended = -1;
            this.mask = 0;
            this.context = 0;
        }
        clear(start) {
            this.start = start;
            this.value = this.extended = -1;
        }
    }
    const dummyToken = new Token;
    class TokenCache {
        constructor(parser) {
            this.tokens = [];
            this.mainToken = dummyToken;
            this.actions = [];
            this.tokens = parser.tokenizers.map(_ => new CachedToken);
        }
        getActions(stack, input) {
            let actionIndex = 0;
            let main = null;
            let { parser } = stack.p, { tokenizers } = parser;
            let mask = parser.stateSlot(stack.state, 3 /* TokenizerMask */);
            let context = stack.curContext ? stack.curContext.hash : 0;
            for (let i = 0; i < tokenizers.length; i++) {
                if (((1 << i) & mask) == 0)
                    continue;
                let tokenizer = tokenizers[i], token = this.tokens[i];
                if (main && !tokenizer.fallback)
                    continue;
                if (tokenizer.contextual || token.start != stack.pos || token.mask != mask || token.context != context) {
                    this.updateCachedToken(token, tokenizer, stack, input);
                    token.mask = mask;
                    token.context = context;
                }
                if (token.value != 0 /* Err */) {
                    let startIndex = actionIndex;
                    if (token.extended > -1)
                        actionIndex = this.addActions(stack, token.extended, token.end, actionIndex);
                    actionIndex = this.addActions(stack, token.value, token.end, actionIndex);
                    if (!tokenizer.extend) {
                        main = token;
                        if (actionIndex > startIndex)
                            break;
                    }
                }
            }
            while (this.actions.length > actionIndex)
                this.actions.pop();
            if (!main) {
                main = dummyToken;
                main.start = stack.pos;
                if (stack.pos == input.length)
                    main.accept(stack.p.parser.eofTerm, stack.pos);
                else
                    main.accept(0 /* Err */, stack.pos + 1);
            }
            this.mainToken = main;
            return this.actions;
        }
        updateCachedToken(token, tokenizer, stack, input) {
            token.clear(stack.pos);
            tokenizer.token(input, token, stack);
            if (token.value > -1) {
                let { parser } = stack.p;
                for (let i = 0; i < parser.specialized.length; i++)
                    if (parser.specialized[i] == token.value) {
                        let result = parser.specializers[i](input.read(token.start, token.end), stack);
                        if (result >= 0 && stack.p.parser.dialect.allows(result >> 1)) {
                            if ((result & 1) == 0 /* Specialize */)
                                token.value = result >> 1;
                            else
                                token.extended = result >> 1;
                            break;
                        }
                    }
            }
            else if (stack.pos == input.length) {
                token.accept(stack.p.parser.eofTerm, stack.pos);
            }
            else {
                token.accept(0 /* Err */, stack.pos + 1);
            }
        }
        putAction(action, token, end, index) {
            // Don't add duplicate actions
            for (let i = 0; i < index; i += 3)
                if (this.actions[i] == action)
                    return index;
            this.actions[index++] = action;
            this.actions[index++] = token;
            this.actions[index++] = end;
            return index;
        }
        addActions(stack, token, end, index) {
            let { state } = stack, { parser } = stack.p, { data } = parser;
            for (let set = 0; set < 2; set++) {
                for (let i = parser.stateSlot(state, set ? 2 /* Skip */ : 1 /* Actions */);; i += 3) {
                    if (data[i] == 65535 /* End */) {
                        if (data[i + 1] == 1 /* Next */) {
                            i = pair(data, i + 2);
                        }
                        else {
                            if (index == 0 && data[i + 1] == 2 /* Other */)
                                index = this.putAction(pair(data, i + 1), token, end, index);
                            break;
                        }
                    }
                    if (data[i] == token)
                        index = this.putAction(pair(data, i + 1), token, end, index);
                }
            }
            return index;
        }
    }
    var Rec;
    (function (Rec) {
        Rec[Rec["Distance"] = 5] = "Distance";
        Rec[Rec["MaxRemainingPerStep"] = 3] = "MaxRemainingPerStep";
        Rec[Rec["MinBufferLengthPrune"] = 200] = "MinBufferLengthPrune";
        Rec[Rec["ForceReduceLimit"] = 10] = "ForceReduceLimit";
    })(Rec || (Rec = {}));
    /// A parse context can be used for step-by-step parsing. After
    /// creating it, you repeatedly call `.advance()` until it returns a
    /// tree to indicate it has reached the end of the parse.
    class Parse {
        constructor(parser, input, startPos, context) {
            this.parser = parser;
            this.input = input;
            this.startPos = startPos;
            this.context = context;
            // The position to which the parse has advanced.
            this.pos = 0;
            this.recovering = 0;
            this.nextStackID = 0x2654;
            this.nested = null;
            this.nestEnd = 0;
            this.nestWrap = null;
            this.reused = [];
            this.tokens = new TokenCache(parser);
            this.topTerm = parser.top[1];
            this.stacks = [Stack.start(this, parser.top[0], this.startPos)];
            let fragments = context === null || context === void 0 ? void 0 : context.fragments;
            this.fragments = fragments && fragments.length ? new FragmentCursor(fragments) : null;
        }
        // Move the parser forward. This will process all parse stacks at
        // `this.pos` and try to advance them to a further position. If no
        // stack for such a position is found, it'll start error-recovery.
        //
        // When the parse is finished, this will return a syntax tree. When
        // not, it returns `null`.
        advance() {
            if (this.nested) {
                let result = this.nested.advance();
                this.pos = this.nested.pos;
                if (result) {
                    this.finishNested(this.stacks[0], result);
                    this.nested = null;
                }
                return null;
            }
            let stacks = this.stacks, pos = this.pos;
            // This will hold stacks beyond `pos`.
            let newStacks = this.stacks = [];
            let stopped, stoppedTokens;
            let maybeNest;
            // Keep advancing any stacks at `pos` until they either move
            // forward or can't be advanced. Gather stacks that can't be
            // advanced further in `stopped`.
            for (let i = 0; i < stacks.length; i++) {
                let stack = stacks[i], nest;
                for (;;) {
                    if (stack.pos > pos) {
                        newStacks.push(stack);
                    }
                    else if (nest = this.checkNest(stack)) {
                        if (!maybeNest || maybeNest.stack.score < stack.score)
                            maybeNest = nest;
                    }
                    else if (this.advanceStack(stack, newStacks, stacks)) {
                        continue;
                    }
                    else {
                        if (!stopped) {
                            stopped = [];
                            stoppedTokens = [];
                        }
                        stopped.push(stack);
                        let tok = this.tokens.mainToken;
                        stoppedTokens.push(tok.value, tok.end);
                    }
                    break;
                }
            }
            if (maybeNest) {
                this.startNested(maybeNest);
                return null;
            }
            if (!newStacks.length) {
                let finished = stopped && findFinished(stopped);
                if (finished)
                    return this.stackToTree(finished);
                if (this.parser.strict) {
                    if (verbose && stopped)
                        console.log("Stuck with token " + this.parser.getName(this.tokens.mainToken.value));
                    throw new SyntaxError("No parse at " + pos);
                }
                if (!this.recovering)
                    this.recovering = 5 /* Distance */;
            }
            if (this.recovering && stopped) {
                let finished = this.runRecovery(stopped, stoppedTokens, newStacks);
                if (finished)
                    return this.stackToTree(finished.forceAll());
            }
            if (this.recovering) {
                let maxRemaining = this.recovering == 1 ? 1 : this.recovering * 3 /* MaxRemainingPerStep */;
                if (newStacks.length > maxRemaining) {
                    newStacks.sort((a, b) => b.score - a.score);
                    while (newStacks.length > maxRemaining)
                        newStacks.pop();
                }
                if (newStacks.some(s => s.reducePos > pos))
                    this.recovering--;
            }
            else if (newStacks.length > 1) {
                // Prune stacks that are in the same state, or that have been
                // running without splitting for a while, to avoid getting stuck
                // with multiple successful stacks running endlessly on.
                outer: for (let i = 0; i < newStacks.length - 1; i++) {
                    let stack = newStacks[i];
                    for (let j = i + 1; j < newStacks.length; j++) {
                        let other = newStacks[j];
                        if (stack.sameState(other) ||
                            stack.buffer.length > 200 /* MinBufferLengthPrune */ && other.buffer.length > 200 /* MinBufferLengthPrune */) {
                            if (((stack.score - other.score) || (stack.buffer.length - other.buffer.length)) > 0) {
                                newStacks.splice(j--, 1);
                            }
                            else {
                                newStacks.splice(i--, 1);
                                continue outer;
                            }
                        }
                    }
                }
            }
            this.pos = newStacks[0].pos;
            for (let i = 1; i < newStacks.length; i++)
                if (newStacks[i].pos < this.pos)
                    this.pos = newStacks[i].pos;
            return null;
        }
        // Returns an updated version of the given stack, or null if the
        // stack can't advance normally. When `split` and `stacks` are
        // given, stacks split off by ambiguous operations will be pushed to
        // `split`, or added to `stacks` if they move `pos` forward.
        advanceStack(stack, stacks, split) {
            let start = stack.pos, { input, parser } = this;
            let base = verbose ? this.stackID(stack) + " -> " : "";
            if (this.fragments) {
                let strictCx = stack.curContext && stack.curContext.tracker.strict, cxHash = strictCx ? stack.curContext.hash : 0;
                for (let cached = this.fragments.nodeAt(start); cached;) {
                    let match = this.parser.nodeSet.types[cached.type.id] == cached.type ? parser.getGoto(stack.state, cached.type.id) : -1;
                    if (match > -1 && cached.length && (!strictCx || (cached.contextHash || 0) == cxHash)) {
                        stack.useNode(cached, match);
                        if (verbose)
                            console.log(base + this.stackID(stack) + ` (via reuse of ${parser.getName(cached.type.id)})`);
                        return true;
                    }
                    if (!(cached instanceof Tree) || cached.children.length == 0 || cached.positions[0] > 0)
                        break;
                    let inner = cached.children[0];
                    if (inner instanceof Tree)
                        cached = inner;
                    else
                        break;
                }
            }
            let defaultReduce = parser.stateSlot(stack.state, 4 /* DefaultReduce */);
            if (defaultReduce > 0) {
                stack.reduce(defaultReduce);
                if (verbose)
                    console.log(base + this.stackID(stack) + ` (via always-reduce ${parser.getName(defaultReduce & 65535 /* ValueMask */)})`);
                return true;
            }
            let actions = this.tokens.getActions(stack, input);
            for (let i = 0; i < actions.length;) {
                let action = actions[i++], term = actions[i++], end = actions[i++];
                let last = i == actions.length || !split;
                let localStack = last ? stack : stack.split();
                localStack.apply(action, term, end);
                if (verbose)
                    console.log(base + this.stackID(localStack) + ` (via ${(action & 65536 /* ReduceFlag */) == 0 ? "shift"
                    : `reduce of ${parser.getName(action & 65535 /* ValueMask */)}`} for ${parser.getName(term)} @ ${start}${localStack == stack ? "" : ", split"})`);
                if (last)
                    return true;
                else if (localStack.pos > start)
                    stacks.push(localStack);
                else
                    split.push(localStack);
            }
            return false;
        }
        // Advance a given stack forward as far as it will go. Returns the
        // (possibly updated) stack if it got stuck, or null if it moved
        // forward and was given to `pushStackDedup`.
        advanceFully(stack, newStacks) {
            let pos = stack.pos;
            for (;;) {
                let nest = this.checkNest(stack);
                if (nest)
                    return nest;
                if (!this.advanceStack(stack, null, null))
                    return false;
                if (stack.pos > pos) {
                    pushStackDedup(stack, newStacks);
                    return true;
                }
            }
        }
        runRecovery(stacks, tokens, newStacks) {
            let finished = null, restarted = false;
            let maybeNest;
            for (let i = 0; i < stacks.length; i++) {
                let stack = stacks[i], token = tokens[i << 1], tokenEnd = tokens[(i << 1) + 1];
                let base = verbose ? this.stackID(stack) + " -> " : "";
                if (stack.deadEnd) {
                    if (restarted)
                        continue;
                    restarted = true;
                    stack.restart();
                    if (verbose)
                        console.log(base + this.stackID(stack) + " (restarted)");
                    let done = this.advanceFully(stack, newStacks);
                    if (done) {
                        if (done !== true)
                            maybeNest = done;
                        continue;
                    }
                }
                let force = stack.split(), forceBase = base;
                for (let j = 0; force.forceReduce() && j < 10 /* ForceReduceLimit */; j++) {
                    if (verbose)
                        console.log(forceBase + this.stackID(force) + " (via force-reduce)");
                    let done = this.advanceFully(force, newStacks);
                    if (done) {
                        if (done !== true)
                            maybeNest = done;
                        break;
                    }
                    if (verbose)
                        forceBase = this.stackID(force) + " -> ";
                }
                for (let insert of stack.recoverByInsert(token)) {
                    if (verbose)
                        console.log(base + this.stackID(insert) + " (via recover-insert)");
                    this.advanceFully(insert, newStacks);
                }
                if (this.input.length > stack.pos) {
                    if (tokenEnd == stack.pos) {
                        tokenEnd++;
                        token = 0 /* Err */;
                    }
                    stack.recoverByDelete(token, tokenEnd);
                    if (verbose)
                        console.log(base + this.stackID(stack) + ` (via recover-delete ${this.parser.getName(token)})`);
                    pushStackDedup(stack, newStacks);
                }
                else if (!finished || finished.score < stack.score) {
                    finished = stack;
                }
            }
            if (finished)
                return finished;
            if (maybeNest)
                for (let s of this.stacks)
                    if (s.score > maybeNest.stack.score) {
                        maybeNest = undefined;
                        break;
                    }
            if (maybeNest)
                this.startNested(maybeNest);
            return null;
        }
        forceFinish() {
            let stack = this.stacks[0].split();
            if (this.nested)
                this.finishNested(stack, this.nested.forceFinish());
            return this.stackToTree(stack.forceAll());
        }
        // Convert the stack's buffer to a syntax tree.
        stackToTree(stack, pos = stack.pos) {
            if (this.parser.context)
                stack.emitContext();
            return Tree.build({ buffer: StackBufferCursor.create(stack),
                nodeSet: this.parser.nodeSet,
                topID: this.topTerm,
                maxBufferLength: this.parser.bufferLength,
                reused: this.reused,
                start: this.startPos,
                length: pos - this.startPos,
                minRepeatType: this.parser.minRepeatTerm });
        }
        checkNest(stack) {
            let info = this.parser.findNested(stack.state);
            if (!info)
                return null;
            let spec = info.value;
            if (typeof spec == "function")
                spec = spec(this.input, stack);
            return spec ? { stack, info, spec } : null;
        }
        startNested(nest) {
            let { stack, info, spec } = nest;
            this.stacks = [stack];
            this.nestEnd = this.scanForNestEnd(stack, info.end, spec.filterEnd);
            this.nestWrap = typeof spec.wrapType == "number" ? this.parser.nodeSet.types[spec.wrapType] : spec.wrapType || null;
            if (spec.startParse) {
                this.nested = spec.startParse(this.input.clip(this.nestEnd), stack.pos, this.context);
            }
            else {
                this.finishNested(stack);
            }
        }
        scanForNestEnd(stack, endToken, filter) {
            for (let pos = stack.pos; pos < this.input.length; pos++) {
                dummyToken.start = pos;
                dummyToken.value = -1;
                endToken.token(this.input, dummyToken, stack);
                if (dummyToken.value > -1 && (!filter || filter(this.input.read(pos, dummyToken.end))))
                    return pos;
            }
            return this.input.length;
        }
        finishNested(stack, tree) {
            if (this.nestWrap)
                tree = new Tree(this.nestWrap, tree ? [tree] : [], tree ? [0] : [], this.nestEnd - stack.pos);
            else if (!tree)
                tree = new Tree(NodeType.none, [], [], this.nestEnd - stack.pos);
            let info = this.parser.findNested(stack.state);
            stack.useNode(tree, this.parser.getGoto(stack.state, info.placeholder, true));
            if (verbose)
                console.log(this.stackID(stack) + ` (via unnest)`);
        }
        stackID(stack) {
            let id = (stackIDs || (stackIDs = new WeakMap)).get(stack);
            if (!id)
                stackIDs.set(stack, id = String.fromCodePoint(this.nextStackID++));
            return id + stack;
        }
    }
    function pushStackDedup(stack, newStacks) {
        for (let i = 0; i < newStacks.length; i++) {
            let other = newStacks[i];
            if (other.pos == stack.pos && other.sameState(stack)) {
                if (newStacks[i].score < stack.score)
                    newStacks[i] = stack;
                return;
            }
        }
        newStacks.push(stack);
    }
    class Dialect {
        constructor(source, flags, disabled) {
            this.source = source;
            this.flags = flags;
            this.disabled = disabled;
        }
        allows(term) { return !this.disabled || this.disabled[term] == 0; }
    }
    /// A parser holds the parse tables for a given grammar, as generated
    /// by `lezer-generator`.
    class Parser {
        /// @internal
        constructor(spec) {
            /// @internal
            this.bufferLength = DefaultBufferLength;
            /// @internal
            this.strict = false;
            this.cachedDialect = null;
            if (spec.version != 13 /* Version */)
                throw new RangeError(`Parser version (${spec.version}) doesn't match runtime version (${13 /* Version */})`);
            let tokenArray = decodeArray(spec.tokenData);
            let nodeNames = spec.nodeNames.split(" ");
            this.minRepeatTerm = nodeNames.length;
            this.context = spec.context;
            for (let i = 0; i < spec.repeatNodeCount; i++)
                nodeNames.push("");
            let nodeProps = [];
            for (let i = 0; i < nodeNames.length; i++)
                nodeProps.push([]);
            function setProp(nodeID, prop, value) {
                nodeProps[nodeID].push([prop, prop.deserialize(String(value))]);
            }
            if (spec.nodeProps)
                for (let propSpec of spec.nodeProps) {
                    let prop = propSpec[0];
                    for (let i = 1; i < propSpec.length;) {
                        let next = propSpec[i++];
                        if (next >= 0) {
                            setProp(next, prop, propSpec[i++]);
                        }
                        else {
                            let value = propSpec[i + -next];
                            for (let j = -next; j > 0; j--)
                                setProp(propSpec[i++], prop, value);
                            i++;
                        }
                    }
                }
            this.specialized = new Uint16Array(spec.specialized ? spec.specialized.length : 0);
            this.specializers = [];
            if (spec.specialized)
                for (let i = 0; i < spec.specialized.length; i++) {
                    this.specialized[i] = spec.specialized[i].term;
                    this.specializers[i] = spec.specialized[i].get;
                }
            this.states = decodeArray(spec.states, Uint32Array);
            this.data = decodeArray(spec.stateData);
            this.goto = decodeArray(spec.goto);
            let topTerms = Object.keys(spec.topRules).map(r => spec.topRules[r][1]);
            this.nodeSet = new NodeSet(nodeNames.map((name, i) => NodeType.define({
                name: i >= this.minRepeatTerm ? undefined : name,
                id: i,
                props: nodeProps[i],
                top: topTerms.indexOf(i) > -1,
                error: i == 0,
                skipped: spec.skippedNodes && spec.skippedNodes.indexOf(i) > -1
            })));
            this.maxTerm = spec.maxTerm;
            this.tokenizers = spec.tokenizers.map(value => typeof value == "number" ? new TokenGroup(tokenArray, value) : value);
            this.topRules = spec.topRules;
            this.nested = (spec.nested || []).map(([name, value, endToken, placeholder]) => {
                return { name, value, end: new TokenGroup(decodeArray(endToken), 0), placeholder };
            });
            this.dialects = spec.dialects || {};
            this.dynamicPrecedences = spec.dynamicPrecedences || null;
            this.tokenPrecTable = spec.tokenPrec;
            this.termNames = spec.termNames || null;
            this.maxNode = this.nodeSet.types.length - 1;
            this.dialect = this.parseDialect();
            this.top = this.topRules[Object.keys(this.topRules)[0]];
        }
        /// Parse a given string or stream.
        parse(input, startPos = 0, context = {}) {
            if (typeof input == "string")
                input = stringInput(input);
            let cx = new Parse(this, input, startPos, context);
            for (;;) {
                let done = cx.advance();
                if (done)
                    return done;
            }
        }
        /// Start an incremental parse.
        startParse(input, startPos = 0, context = {}) {
            if (typeof input == "string")
                input = stringInput(input);
            return new Parse(this, input, startPos, context);
        }
        /// Get a goto table entry @internal
        getGoto(state, term, loose = false) {
            let table = this.goto;
            if (term >= table[0])
                return -1;
            for (let pos = table[term + 1];;) {
                let groupTag = table[pos++], last = groupTag & 1;
                let target = table[pos++];
                if (last && loose)
                    return target;
                for (let end = pos + (groupTag >> 1); pos < end; pos++)
                    if (table[pos] == state)
                        return target;
                if (last)
                    return -1;
            }
        }
        /// Check if this state has an action for a given terminal @internal
        hasAction(state, terminal) {
            let data = this.data;
            for (let set = 0; set < 2; set++) {
                for (let i = this.stateSlot(state, set ? 2 /* Skip */ : 1 /* Actions */), next;; i += 3) {
                    if ((next = data[i]) == 65535 /* End */) {
                        if (data[i + 1] == 1 /* Next */)
                            next = data[i = pair(data, i + 2)];
                        else if (data[i + 1] == 2 /* Other */)
                            return pair(data, i + 2);
                        else
                            break;
                    }
                    if (next == terminal || next == 0 /* Err */)
                        return pair(data, i + 1);
                }
            }
            return 0;
        }
        /// @internal
        stateSlot(state, slot) {
            return this.states[(state * 6 /* Size */) + slot];
        }
        /// @internal
        stateFlag(state, flag) {
            return (this.stateSlot(state, 0 /* Flags */) & flag) > 0;
        }
        /// @internal
        findNested(state) {
            let flags = this.stateSlot(state, 0 /* Flags */);
            return flags & 4 /* StartNest */ ? this.nested[flags >> 10 /* NestShift */] : null;
        }
        /// @internal
        validAction(state, action) {
            if (action == this.stateSlot(state, 4 /* DefaultReduce */))
                return true;
            for (let i = this.stateSlot(state, 1 /* Actions */);; i += 3) {
                if (this.data[i] == 65535 /* End */) {
                    if (this.data[i + 1] == 1 /* Next */)
                        i = pair(this.data, i + 2);
                    else
                        return false;
                }
                if (action == pair(this.data, i + 1))
                    return true;
            }
        }
        /// Get the states that can follow this one through shift actions or
        /// goto jumps. @internal
        nextStates(state) {
            let result = [];
            for (let i = this.stateSlot(state, 1 /* Actions */);; i += 3) {
                if (this.data[i] == 65535 /* End */) {
                    if (this.data[i + 1] == 1 /* Next */)
                        i = pair(this.data, i + 2);
                    else
                        break;
                }
                if ((this.data[i + 2] & (65536 /* ReduceFlag */ >> 16)) == 0) {
                    let value = this.data[i + 1];
                    if (!result.some((v, i) => (i & 1) && v == value))
                        result.push(this.data[i], value);
                }
            }
            return result;
        }
        /// @internal
        overrides(token, prev) {
            let iPrev = findOffset(this.data, this.tokenPrecTable, prev);
            return iPrev < 0 || findOffset(this.data, this.tokenPrecTable, token) < iPrev;
        }
        /// Configure the parser. Returns a new parser instance that has the
        /// given settings modified. Settings not provided in `config` are
        /// kept from the original parser.
        configure(config) {
            // Hideous reflection-based kludge to make it easy to create a
            // slightly modified copy of a parser.
            let copy = Object.assign(Object.create(Parser.prototype), this);
            if (config.props)
                copy.nodeSet = this.nodeSet.extend(...config.props);
            if (config.top) {
                let info = this.topRules[config.top];
                if (!info)
                    throw new RangeError(`Invalid top rule name ${config.top}`);
                copy.top = info;
            }
            if (config.tokenizers)
                copy.tokenizers = this.tokenizers.map(t => {
                    let found = config.tokenizers.find(r => r.from == t);
                    return found ? found.to : t;
                });
            if (config.dialect)
                copy.dialect = this.parseDialect(config.dialect);
            if (config.nested)
                copy.nested = this.nested.map(obj => {
                    if (!Object.prototype.hasOwnProperty.call(config.nested, obj.name))
                        return obj;
                    return { name: obj.name, value: config.nested[obj.name], end: obj.end, placeholder: obj.placeholder };
                });
            if (config.strict != null)
                copy.strict = config.strict;
            if (config.bufferLength != null)
                copy.bufferLength = config.bufferLength;
            return copy;
        }
        /// Returns the name associated with a given term. This will only
        /// work for all terms when the parser was generated with the
        /// `--names` option. By default, only the names of tagged terms are
        /// stored.
        getName(term) {
            return this.termNames ? this.termNames[term] : String(term <= this.maxNode && this.nodeSet.types[term].name || term);
        }
        /// The eof term id is always allocated directly after the node
        /// types. @internal
        get eofTerm() { return this.maxNode + 1; }
        /// Tells you whether this grammar has any nested grammars.
        get hasNested() { return this.nested.length > 0; }
        /// The type of top node produced by the parser.
        get topNode() { return this.nodeSet.types[this.top[1]]; }
        /// @internal
        dynamicPrecedence(term) {
            let prec = this.dynamicPrecedences;
            return prec == null ? 0 : prec[term] || 0;
        }
        /// @internal
        parseDialect(dialect) {
            if (this.cachedDialect && this.cachedDialect.source == dialect)
                return this.cachedDialect;
            let values = Object.keys(this.dialects), flags = values.map(() => false);
            if (dialect)
                for (let part of dialect.split(" ")) {
                    let id = values.indexOf(part);
                    if (id >= 0)
                        flags[id] = true;
                }
            let disabled = null;
            for (let i = 0; i < values.length; i++)
                if (!flags[i]) {
                    for (let j = this.dialects[values[i]], id; (id = this.data[j++]) != 65535 /* End */;)
                        (disabled || (disabled = new Uint8Array(this.maxTerm + 1)))[id] = 1;
                }
            return this.cachedDialect = new Dialect(dialect, flags, disabled);
        }
        /// (used by the output of the parser generator) @internal
        static deserialize(spec) {
            return new Parser(spec);
        }
    }
    function pair(data, off) { return data[off] | (data[off + 1] << 16); }
    function findOffset(data, start, term) {
        for (let i = start, next; (next = data[i]) != 65535 /* End */; i++)
            if (next == term)
                return i - start;
        return -1;
    }
    function findFinished(stacks) {
        let best = null;
        for (let stack of stacks) {
            if (stack.pos == stack.p.input.length &&
                stack.p.parser.stateFlag(stack.state, 2 /* Accepting */) &&
                (!best || best.score < stack.score))
                best = stack;
        }
        return best;
    }

    // This file was generated by lezer-generator. You probably shouldn't edit it.
    const parser = Parser.deserialize({
      version: 13,
      states: "$OOVQPOOO_QPO'#CeQgQPOOOOQO'#Ck'#CkOoQPO'#CmOOQO,59P,59POtQPO,59PO|QPO'#CaO!UQPO,59VOOQO,59V,59VOOQO'#Cb'#CbOVQPO,59XO!ZQPO'#CgO!`QPO1G.kOOQO1G.k1G.kOOQO,58{,58{O!hQPO,58{OOQO1G.q1G.qO!mQPO1G.sOOQO,59R,59ROOQO-E6e-E6eOOQO7+$V7+$VOOQO1G.g1G.g",
      stateData: "#O~O^OS~OWPOYRO~OQSOVTO~OSVO`WO~ObYO~OV^Oc[O~OQSOR_O~OQaO~OQSO~OVeOc[O~ORfO~OSVO`WOVaicaiRai~O",
      goto: "!TbPPPPPcgPPjPnPPPtPzTXQbRZSTROZQ]URd]QQORbZQUPQ`VRc[",
      nodeNames: "⚠ PhiGrammar Attr ) ( Block Mapsto ] [ Block Locator",
      maxTerm: 19,
      nodeProps: [
        [NodeProp.openedBy, 3,"(",7,"["],
        [NodeProp.closedBy, 4,")",8,"]"]
      ],
      skippedNodes: [0],
      repeatNodeCount: 1,
      tokenData: "%R~RfXY!gYZ!gZ^!gpq!gxy#byz#g|}#l}!O#q!O!P#|!c!}$R!}#O$a#P#Q$f#Q#R$k#T#o$R#y#z!g$f$g!g#BY#BZ!g$IS$I_!g$I|$JO!g$JT$JU!g$KV$KW!g&FU&FV!g~!l[^~XY!gYZ!gZ^!gpq!g#y#z!g$f$g!g#BY#BZ!g$IS$I_!g$I|$JO!g$JT$JU!g$KV$KW!g&FU&FV!g~#gOS~~#lOR~~#qOc~~#tP!`!a#w~#|Ob~~$RO`~~$WRQ~!Q![$R!c!}$R#T#o$R~$fOW~~$kOV~~$nQ!Q!R$t!R![$y~$yOY~~%OPY~!Q![$y",
      tokenizers: [0],
      topRules: {"PhiGrammar":[0,1]},
      tokenPrec: 0
    });

    const lezerLanguage = LezerLanguage.define({
      parser: parser.configure({
        props: [
          foldNodeProp.add({

          }),
          styleTags({
            Locator: tags.keyword,
            Attr: tags.integer,
            Mapsto: tags.string,
          }),
        ],
      }),
      languageData: {
        closeBrackets: {brackets: ["(", "["]},
      },
    });

    function lezer() {
      return [lezerLanguage]
    }

    let code$1 = `[
  x -> ^0.y,
  p -> [y -> ^0.x, z -> [
    t -> ^3.p(
      d -> ^12
    )
  ]]
]`;

    const myTheme = EditorView.baseTheme({
      $:{
        maxHeight: '98vh',
        outline: '1px auto #ddd',
      },
      $scroller: {
        fontFamily: '"Fira Mono", monospace',
        fontSize: '30px',
      }
    });

    const initialState = EditorState.create({
      doc: code$1,
      extensions: [
        basicSetup,
        myTheme,
        lezer(),
      ]
    });

    const view = new EditorView({
      state: initialState,
      parent: document.body
    });

}());
//# sourceMappingURL=bundle.js.map
