---
applyTo: "**/*.typ"
description: Typst syntax rules for SDU lecture notes
---

# Typst — not LaTeX

These are Typst (`.typ`) files, **not** LaTeX. Suggestions must use Typst syntax. Before suggesting unfamiliar constructs, look at the current file and at [`/temp/temp.typ`](../../temp/temp.typ) for the project's shorthands and conventions.

## Never emit LaTeX

Forbidden tokens (these are LaTeX, they will not compile):
- `\frac{a}{b}`, `\sqrt{x}`, `\sum_{...}^{...}`, `\int_{...}^{...}`
- `\begin{...} ... \end{...}` (align, equation, matrix, cases, itemize, enumerate, …)
- `\\` for line breaks, `\textbf{}`, `\textit{}`, `\mathbb{}`, `\mathcal{}`
- backslash-prefixed names: `\nabla`, `\alpha`, `\cdot`, `\to`, `\infty`, `\in`, `\forall`, `\exists`, `\partial`, `\top`, `\leq`, `\geq`, `\neq`, `\approx`, `\rightarrow`, `\Rightarrow`

In Typst these are bare identifiers in math mode: `nabla`, `alpha`, `cdot`, `arrow`, `infinity`, `in`, `forall`, `exists`, `diff`, `top`, `<=`, `>=`, `!=`, `approx`, `->`, `=>`.

## Math mode

- Block equation: `$ x^2 + y^2 = 1 $` — **spaces inside `$` make it a block**.
- Inline equation: `$x^2$` — no spaces inside `$` keeps it inline.
- Fraction: `(a + b) / c`, not `\frac{a+b}{c}`.
- Subscript / superscript: `x_1`, `x^2`, `x_(k+1)`, `x^(top)`.
- Matrix: `mat(1, 2; 3, 4)` (semicolons separate rows). Set delim via `set math.mat(delim: "[")` (already set in `temp.typ`).
- Norms: `norm(x)`, abs: `abs(x)`, floor/ceil: `floor(x)`, `ceil(x)`.
- Operators: `arg min`, `arg max`, `op("supremum", limits: #true)_(...)`.
- Multi-line align in math: separate lines with `\` and align with `&`. Example:
  ```typst
  $ a + b &= c \
      d &= e - f $
  ```
- Text inside math: `"some text"` (double quotes). Example: `$ x &= 0 wide "since x is min" $`.

## Markup

- Heading: `= H1`, `== H2`, `=== H3` — **never** `# Heading` (that's a code-mode prefix in Typst).
- Bold: `*strong*`. Italic: `_em_`. Inline code: `` `code` ``.
- Bullet list: `- item`. Numbered list: `+ item`.
- Code block: triple-backtick fence with language tag, e.g. ` ```py ... ``` `, ` ```hs ... ``` `.
- Page break: `#pagebreak()`.
- Image: `#figure(image("assets/foo.png", width: 20em), caption: [...])` — assets live in `assets/` next to the current file.

## Code mode

- Prefix: `#` switches from markup to code (`#let`, `#import`, `#show`, `#set`, `#if`, `#for`).
- Identifiers are **kebab-case**: `my-helper`, not `myHelper` or `my_helper`.
- Function call in markup: `#name(args)` or `#name[content block]`.

## This project's template (`temp/temp.typ`)

Every note file imports the shared template and applies it via `#show: note.with(...)`:

```typst
#let title = "Lecture N: ..."
#let author = "Simon Holm"
#let date = "Month - Year"

#import "../../../../temp/temp.typ": *  // count `..` to reach SDU root

#show: note.with(title: title, author: author, date: date)
```

**Use `#show: note.with(...)`, never `#note(...)`.** The relative `../` count depends on the file's depth — match what the file already does, do not invent paths.

### Shorthands defined in `temp.typ` — prefer these over inlining

Math:
- `nf` → `nabla f` (gradient). Don't write `nabla f` when `nf` is in scope.
- `bigo(x)` → `cal(O)(x)`, `smallo(x)` → `cal(o)(x)`
- `wrt`, `ie`, `eg`, `def`, `yes`, `no`, `absurd`
- `softmax(x)`, `ReLU(x)`, `GeLU(x)`, `supremum(x)`
- `evaluated(expr)` → `lr(expr|)`

Markup helpers (used as `#u(...)`, `#b(...)`, etc. in markup; bare in math):
- `u(x)` → underline, `b(x)` → bold, `i(x)` → italic emph
- `py("code")`, `hs("code")` → inline raw with language

Block environments — call with `[ ... ]`:
- `#theorem([ ... ])`
- `#example([ ... ])`

Before introducing a new helper, **grep `temp/temp.typ` for an existing one**. If you don't know whether a shorthand exists, look it up rather than guessing.

## Style conventions in this repo

- Math is the primary content; use display blocks (`$ ... $` with spaces) for derivations, inline (`$...$`) for short expressions in prose.
- Proofs go inside `#theorem([ ... #u("Proof:") ... ])` using `#align($ ... $)` for multi-line derivations.
- Code samples use fenced blocks with a language tag, not `raw(...)` calls.
- Image paths are **relative** to the current file: `assets/image-1.png`, not absolute paths.

## When unsure

Read the current file's existing patterns first, then `temp/temp.typ`. Match the style already in use rather than introducing a new convention.
