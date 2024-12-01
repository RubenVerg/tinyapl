import * as tinyapl from './tinyapl.js';
import * as quads from './quads.js';
import 'https://cdn.plot.ly/plotly-2.34.0.min.js';
// @ts-ignore Import from web not supported
import { encode as _encodeGIF } from 'https://esm.run/modern-gif@2.0.3';
const encodeGIF = _encodeGIF;
const buttons = document.querySelector('#buttons');
const output = document.querySelector('#output');
const input = document.querySelector('#input');
const highlighted = document.querySelector('#highlighted');
const button = document.querySelector('#button');
const infobutton = document.querySelector('#infobutton');
const info = document.querySelector('#info');
const fancyarrays = document.querySelector('#fancyarrays');
function zip(as, bs) {
    return [...as, ...bs].slice(0, Math.min(as.length, bs.length)).map((_, idx) => [as[idx], bs[idx]]);
}
const prefix = { code: 'Backquote', sym: '`' };
const keyboard = [
    ['Backquote', '`', '~', undefined, '⍨', '⋄', '⌺'],
    ['Digit1', '1', '!', '¨', '↗', undefined, undefined],
    ['Digit2', '2', '@', '¯', undefined, undefined, undefined],
    ['Digit3', '3', '#', undefined, '⍒', undefined, undefined],
    ['Digit4', '4', '$', '≤', '⍋', '⊴', undefined],
    ['Digit5', '5', '%', undefined, undefined, undefined, undefined],
    ['Digit6', '6', '^', '≥', '⍉', '⊵', undefined],
    ['Digit7', '7', '&', undefined, '⊖', undefined, undefined],
    ['Digit8', '8', '*', '≠', '⍣', '⍟', '∞'],
    ['Digit9', '9', '(', '∨', '⍱', '∻', '⦋'],
    ['Digit0', '0', ')', '∧', '⍲', '⍬', '⦌'],
    ['Minus', '-', '_', '×', '⊗', '⸚', 'ⵧ'],
    ['Equal', '=', '+', '÷', '⊕', '⌹', '⧺'],
    ['KeyQ', 'q', 'Q', undefined, undefined, '⇾', '⇽'],
    ['KeyW', 'w', 'W', '⍵', '⍹', undefined, undefined],
    ['KeyE', 'e', 'E', '∊', '⍷', '⏨', '⋷'],
    ['KeyR', 'r', 'R', '⍴', '√', 'ϼ', 'ℜ'],
    ['KeyT', 't', 'T', '⊞', '⍨', '߹', '‥'],
    ['KeyY', 'y', 'Y', '↑', '↟', undefined, undefined],
    ['KeyU', 'u', 'U', '↓', '↡', undefined, undefined],
    ['KeyI', 'i', 'I', '⍳', '⍸', '…', 'ℑ'],
    ['KeyO', 'o', 'O', '○', '⍥', undefined, undefined],
    ['KeyP', 'p', 'P', '◡', '◠', undefined, '⌓'],
    ['BracketLeft', '[', '{', '←', '⟨', '⦅', '⦃'],
    ['BracketRight', ']', '}', '→', '⟩', '⦆', '⦄'],
    ['KeyA', 'a', 'A', '⍺', '⍶', 'ɛ', undefined],
    ['KeyS', 's', 'S', '⌈', '§', '↾', undefined],
    ['KeyD', 'd', 'D', '⌊', '⸠', '⇂', undefined],
    ['KeyF', 'f', 'F', '⍛', '∡', '∠', undefined],
    ['KeyG', 'g', 'G', '∇', '⍢', '⫇', undefined],
    ['KeyH', 'h', 'H', '∆', '⍙', '⊸', '⟜'],
    ['KeyJ', 'j', 'J', '∘', '⍤', 'ᴊ', undefined],
    ['KeyK', 'k', 'K', '⍆', '⌸', '⍅', undefined],
    ['KeyL', 'l', 'L', '⎕', '⌷', undefined, undefined],
    ['Semicolon', ';', ':', '⍎', '≡', '⍮', '⍠'],
    ['Quote', '\'', '"', '⍕', '≢', '⍘', '⍞'],
    ['Backslash', '\\', '|', '⊢', '⊣', '⊩', '⫣'],
    ['KeyZ', 'z', 'Z', '⊂', '⊆', '⊏', 'ᑣ'],
    ['KeyX', 'x', 'X', '⊃', '⊇', '⊐', 'ᑒ'],
    ['KeyC', 'c', 'C', '∩', '⍝', '⟃', '⟄'],
    ['KeyV', 'v', 'V', '∪', '⁖', '⫤', undefined],
    ['KeyB', 'b', 'B', '⊥', undefined, '⇇', undefined],
    ['KeyN', 'n', 'N', '⊤', undefined, '↚', undefined],
    ['KeyM', 'm', 'M', '«', '»', '↩', undefined],
    ['Comma', ',', '<', '⍪', 'ᑈ', '⊲', undefined],
    ['Period', '.', '>', '∙', 'ᐵ', '⊳', '■'],
    ['Slash', '/', '?', '⌿', undefined, undefined, '⍰'],
    ['Space', 'Space', 'Space', '`', '‿', undefined, undefined],
].map(([code, sym, symS, symP, symPS, symPP, symPPS]) => ({ code, sym, symS, symP, symPS, symPP, symPPS }));
const colors = {
    other: 'unset',
    syntax: 'unset',
    number: '#ea0027',
    string: '#0079d3',
    stringEscape: '#0266b3',
    arrayName: '#ff4500',
    primArray: '#cc3600',
    functionName: '#46d160',
    primFunction: '#349e48',
    adverbName: '#ff66ac',
    primAdverb: '#cc5289',
    conjunctionName: '#ffd635',
    primConjunction: '#ccac2b',
    comment: '#014980',
};
async function highlight(code, output) {
    const pairs = zip(await tinyapl.splitString(code), await tinyapl.highlight(code));
    output.innerHTML = '';
    for (const [t, c] of pairs) {
        if (t === '\n') {
            output.appendChild(document.createElement('br'));
            // Appending a zero-width non-joiner forces the line break to be rendered
            // and the highlighted text to expand properly
            output.appendChild(document.createTextNode('\u200c'));
            continue;
        }
        const span = document.createElement('span');
        span.className = 'char ' + tinyapl.colorsInv[c];
        span.style.color = colors[tinyapl.colorsInv[c]];
        span.innerText = t;
        output.appendChild(span);
    }
}
async function highlightInput() {
    highlight(input.value, highlighted);
    highlighted.scrollLeft = input.scrollLeft;
}
function insertText(str) {
    input.setRangeText(str, input.selectionStart ?? input.value.length - 1, input.selectionEnd ?? input.value.length - 1, "end");
    input.focus();
    highlightInput();
}
const io = new class IO {
    #input = [];
    #output = [];
    #error = [];
    rInput(l) { this.#input.push(l); }
    rOutput(l) { this.#output.push(l); }
    rError(l) { this.#error.push(l); }
    done() { this.#input = []; this.#output = []; this.#error = []; }
    async input() { const i = window.prompt('Input') ?? ''; for (const l of this.#input)
        await l(i); return i; }
    async output(what) { for (const l of this.#output)
        await l(what); }
    async error(what) { for (const l of this.#error)
        await l(what); }
};
for (const k of ['syntax', 'identifiers', 'arrays', 'functions', 'adverbs', 'conjunctions']) {
    for (const i of tinyapl.glyphs[k]) {
        let v, p;
        if (v = keyboard.find(k => k.symP === i))
            p = `${prefix.sym}${v.sym}`;
        else if (v = keyboard.find(k => k.symPS === i))
            p = `${prefix.sym}${v.symS}`;
        else if (v = keyboard.find(k => k.symPP === i))
            p = `${prefix.sym}${prefix.sym}${v.sym}`;
        else if (v = keyboard.find(k => k.symPPS === i))
            p = `${prefix.sym}${prefix.sym}${v.symS}`;
        const b = document.createElement('button');
        b.textContent = i;
        if (p !== undefined)
            b.title = `Input: ${p}`;
        b.addEventListener('click', () => { insertText(i); });
        buttons.appendChild(b);
    }
    buttons.appendChild(document.createElement('br'));
}
const context = await tinyapl.newContext(io.input.bind(io), io.output.bind(io), io.error.bind(io), {
    Debug: {
        type: 'function',
        repr: '⎕Debug',
        monad: async (y) => { console.log('monad call', y); return y; },
        dyad: async (x, y) => { console.log('dyad call', x, y); return y; },
    },
    CreateImage: quads.qCreateImage,
    DisplayImage: quads.qDisplayImage,
    PlayAnimation: quads.qPlayAnimation,
    ScatterPlot: quads.qScatterPlot,
    PlayAudio: quads.qPlayAudio,
    Fetch: quads.qFetch,
    _Graph: quads.qGraph,
});
function el(tag, cls, contents) {
    const el = document.createElement(tag);
    el.className = cls;
    if (typeof contents === 'string')
        el.textContent = contents;
    else if (Array.isArray(contents))
        for (const c of contents)
            el.appendChild(c);
    else
        el.appendChild(contents);
    return el;
}
const div = (cls, contents) => el('div', cls, contents);
const span = (cls, contents) => el('span', cls, contents);
function clickableEl(tag, cls, contents, clickedContents) {
    const e = el(tag, cls, contents);
    e.addEventListener('click', () => {
        if (input.value.trim() == '') {
            input.value = clickedContents;
            input.focus();
            highlightInput();
        }
    });
    return e;
}
const clickableDiv = (cls, contents, clickedContents) => clickableEl('div', cls, contents, clickedContents);
const clickableSpan = (cls, contents, clickedContents) => clickableEl('span', cls, contents, clickedContents);
const selfClickableDiv = (cls, contents) => clickableEl('div', cls, contents, contents);
const selfClickableSpan = (cls, contents) => clickableEl('span', cls, contents, contents);
const images = {};
const ranCode = [];
let lastIndex = 0;
async function fancyShow(result, depth = 0) {
    const fsScalar = (v) => {
        if (typeof v === 'object' && !Array.isArray(v) && v.type !== 'struct')
            return fancyShow(v, depth + 1);
        return fancyShow({ type: 'array', shape: [], contents: [v] }, depth + 1);
    };
    if (depth >= 5)
        return document.createTextNode(await tinyapl.show(result));
    if (result.type === 'array' && result.shape.length === 1 && result.contents.length !== 0 && !result.contents.every(x => typeof x === 'string')) {
        const table = document.createElement('table');
        table.className = 'vector';
        const tbody = document.createElement('tbody');
        table.appendChild(tbody);
        const tr = document.createElement('tr');
        tbody.appendChild(tr);
        for (let x = 0; x < result.shape[0]; x++) {
            const el = result.contents[x];
            const td = document.createElement('td');
            td.appendChild(await fsScalar(el));
            tr.appendChild(td);
        }
        return table;
    }
    else if (result.type === 'array' && result.shape.length === 2 && result.contents.length !== 0) {
        const table = document.createElement('table');
        table.className = 'matrix';
        const tbody = document.createElement('tbody');
        table.appendChild(tbody);
        for (let y = 0; y < result.shape[0]; y++) {
            const tr = document.createElement('tr');
            tbody.appendChild(tr);
            for (let x = 0; x < result.shape[1]; x++) {
                const el = result.contents[y * result.shape[1] + x];
                const td = document.createElement('td');
                td.appendChild(await fsScalar(el));
                tr.appendChild(td);
            }
        }
        return table;
    }
    else if (result.type === 'array' && result.shape.length === 0 && typeof result.contents[0] === 'object' && !Array.isArray(result.contents[0]) && result.contents[0].type === 'struct') {
        const struct = result.contents[0];
        const details = document.createElement('details');
        details.open = depth === 0;
        const summary = document.createElement('summary');
        summary.textContent = 'struct';
        details.appendChild(summary);
        const table = document.createElement('table');
        const tbody = document.createElement('tbody');
        table.appendChild(tbody);
        for (const [k, v] of Object.entries(struct.entries)) {
            if (v[0] === 'private')
                continue;
            const tr = document.createElement('tr');
            const tdName = document.createElement('td');
            tdName.textContent = k;
            tr.appendChild(tdName);
            const tdArrow = document.createElement('td');
            tdArrow.textContent = await tinyapl.varArrow(v[0]);
            tr.appendChild(tdArrow);
            const tdValue = document.createElement('td');
            tdValue.appendChild(await fsScalar(v[1]));
            tr.appendChild(tdValue);
            tbody.appendChild(tr);
        }
        details.appendChild(table);
        return details;
    }
    else if (result.type === 'dictionary' && result.entries.length !== 0) {
        const table = document.createElement('table');
        table.className = 'dictionary';
        const tbody = document.createElement('tbody');
        table.appendChild(tbody);
        for (const [k, v] of result.entries) {
            const tr = document.createElement('tr');
            const tdName = document.createElement('td');
            tdName.appendChild(await fsScalar(k));
            tr.appendChild(tdName);
            const tdColon = document.createElement('td');
            tdColon.textContent = ':';
            tr.appendChild(tdColon);
            const tdValue = document.createElement('td');
            tdValue.appendChild(await fsScalar(v));
            tr.appendChild(tdValue);
            tbody.appendChild(tr);
        }
        return table;
    }
    return document.createTextNode(await tinyapl.show(result));
}
async function runCode(code) {
    let d;
    const endDiv = () => {
        if (d.textContent.trim() === '')
            try {
                output.removeChild(d);
            }
            catch (_) { }
        ;
        if (d.textContent.at(-1) === '\n')
            d.textContent = d.textContent.slice(0, -1);
    };
    const newDiv = () => {
        d = div('quad', '');
        output.appendChild(d);
    };
    const createImage = (id, width, height) => {
        endDiv();
        const canvas = document.createElement('canvas');
        canvas.className = 'image';
        canvas.width = width;
        canvas.height = height;
        if (id !== undefined) {
            canvas.dataset.tinyaplId = id.toString();
            images[id] = canvas;
        }
        output.appendChild(canvas);
        newDiv();
        return canvas;
    };
    ranCode.push(code);
    lastIndex = ranCode.length;
    button.disabled = true;
    const in1 = div('executed', '');
    output.appendChild(in1);
    const pad = span('pad', '');
    const loader = div('loader', '');
    pad.appendChild(loader);
    in1.appendChild(pad);
    in1.appendChild(selfClickableSpan('code', code));
    newDiv();
    io.rInput(async (what) => { d.innerText += what + '\n'; });
    io.rOutput(async (what) => { d.innerText += what; });
    io.rError(async (what) => { d.innerText += what; });
    quads.rCreateImage(async (id, width, height) => { createImage(id, width, height); });
    quads.rDisplayImage(async (id, data) => {
        let canvas;
        if (id !== undefined)
            canvas = images[id];
        else
            canvas = createImage(id, data.width, data.height);
        const ctx = canvas.getContext('2d');
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        ctx.putImageData(data, 0, 0);
    });
    quads.rPlayAnimation(async (delay, data) => {
        endDiv();
        const gif = await encodeGIF({ width: data[0].width, height: data[0].height, frames: await Promise.all(data.map(async (d) => ({ data: await createImageBitmap(d), delay: delay * 1000 }))) });
        const img = document.createElement('img');
        img.className = 'image';
        img.src = URL.createObjectURL(new Blob([gif], { type: 'image/gif' }));
        output.appendChild(img);
        newDiv();
    });
    quads.rScatterPlot(async (xs, ys, mode) => {
        endDiv();
        const traces = xs.map((x, i) => ({ x, y: ys[i], mode: mode, type: 'scatter', line: { shape: 'spline' } }));
        const d = div('plot', '');
        output.appendChild(d);
        Plotly.newPlot(d, traces, { font: { family: 'var(--font-mono)' }, showlegend: false }, { responsive: true });
        newDiv();
    });
    quads.rGraph(async (data, labels) => {
        endDiv();
        const traces = zip((console.log(data), data), (console.log(labels), labels)).map(([ps, l]) => (console.log(ps), { x: ps.map(x => x[0]), y: ps.map(x => x[1]), mode: 'lines', line: { shape: 'spline' }, name: l }));
        console.log(traces);
        const d = div('plot', '');
        output.appendChild(d);
        Plotly.newPlot(d, traces, { font: { family: 'var(--font-mono)' }, showlegend: true }, { responsive: true });
        newDiv();
    });
    quads.rPlayAudio(async (buf) => {
        endDiv();
        const audio = document.createElement('audio');
        audio.controls = true;
        audio.autoplay = true;
        const blob = new Blob([buf], { type: 'audio/wav' });
        const url = URL.createObjectURL(blob);
        audio.src = url;
        output.appendChild(audio);
        newDiv();
    });
    const result = await tinyapl.runCode(context, code);
    io.done();
    quads.dCreateImage();
    quads.dDisplayImage();
    quads.dPlayAudio();
    quads.dPlayAnimation();
    quads.dScatterPlot();
    quads.dGraph();
    endDiv();
    if ('code' in result)
        output.appendChild(div('error', await tinyapl.show(result)));
    else if (fancyarrays.checked)
        output.appendChild(clickableDiv('result', await fancyShow(result), await tinyapl.repr(result)));
    else
        output.appendChild(clickableDiv('result', await tinyapl.show(result), await tinyapl.repr(result)));
    loader.remove();
    button.disabled = false;
}
async function run() {
    const v = input.value;
    input.value = '';
    highlightInput();
    await runCode(v);
}
let keyboardState = 0;
function loadLast() {
    if (lastIndex >= ranCode.length)
        lastIndex = ranCode.length;
    if (lastIndex < 0)
        lastIndex = 0;
    if (lastIndex === ranCode.length)
        input.value = '';
    else
        input.value = ranCode[lastIndex];
    highlightInput();
}
button.addEventListener('click', () => run());
input.addEventListener('keydown', evt => {
    if (keyboardState < 2 && evt.code === prefix.code && !evt.shiftKey) {
        keyboardState++;
        evt.preventDefault();
    }
    else if (keyboardState !== 0 && !evt.altKey && !evt.ctrlKey && !evt.metaKey) {
        const v = keyboard.find(k => k.code == evt.code);
        if (v) {
            const t = keyboardState === 2 ? (evt.shiftKey ? v.symPPS : v.symPP) : (evt.shiftKey ? v.symPS : v.symP);
            insertText(t ?? evt.key);
            keyboardState = 0;
            evt.preventDefault();
        }
    }
    else if (evt.key === 'Enter' && !evt.shiftKey) {
        evt.preventDefault();
        if (!button.disabled)
            return run();
    }
    else if (evt.key === 'ArrowUp' && evt.shiftKey && !evt.altKey && !evt.ctrlKey && !evt.metaKey) {
        lastIndex--;
        loadLast();
    }
    else if (evt.key === 'ArrowDown' && evt.shiftKey && !evt.altKey && !evt.ctrlKey && !evt.metaKey) {
        lastIndex++;
        loadLast();
    }
});
input.addEventListener('input', () => highlightInput());
input.addEventListener('scroll', () => highlightInput());
infobutton.addEventListener('click', () => {
    info.classList.toggle('shown');
});
document.querySelector('#prefixkey').textContent = prefix.sym;
for (const k of document.querySelectorAll('.hl'))
    highlight(k.textContent ?? '', k);
document.querySelector('#loading').remove();
button.disabled = false;
infobutton.disabled = false;
highlightInput(); // in case the input is pre-filled
const search = new URLSearchParams(window.location.search);
for (const line of search.getAll('run'))
    await runCode(decodeURIComponent(line));
