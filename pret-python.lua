-- This is inspired by Renaud Aubin's work on a C pretty printer
-- Here: https://github.com/r3n4ud/pretty-c/
if not modules then modules = { } end modules ['pret-python2'] = {
  version = 0.0,
  comment = "Student Exercise, use at your own risk!",
  author = "Jason Ross",
  copyright = "Jason Ross",
  license = "Use at your own risk"
}

local tohash = table.tohash
local P, S, V, R, patterns = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.patterns


-- Keywords
local control = tohash {
  "assert", "async", "await", "break", "class", "continue",
  "def", "del", "elif", "else", "except", "finally", "for",
  "from", "global", "if", "import", "nonlocal", "pass",
  "raise", "return", "try", "while", "with", "yield"
}
local singletons = tohash {
  "True", "False", "None"
}
local operators = tohash {
  "and", "as", "in", "is", "lambda", "not", "or"
}
local builtins = tohash {
  "abs", "all", "any", "ascii", "bin", "bool",
  "breakpoint", "bytearray", "bytes",
  "callable", "chr", "classmethod", "compile",
  "complex", "delattr", "dict", "dir", "divmod",
  "enumerate", "eval", "exec", "filter", "float",
  "format", "frozenset", "getattr", "globals",
  "hasatter", "hash", "help", "hex", "id", "input",
  "int", "isinstance", "issubclass", "iter", "len",
  "list", "locals", "map", "max", "memoryview", "min",
  "next", "object", "oct", "open", "ord", "pow", "print",
  "property", "range", "repr", "reversed", "round",
  "set", "setattr", "slice", "sorted", "staticmethod",
  "str", "sum", "super", "tuple", "type", "vars", "zip",
  "__import__"
}

local context = context
local verbatim = context.verbatim
local makepattern = visualizers.makepattern
local Snippet = context.PYTHONSnippet
local startSnippet = context.startPYTHONSnippet
local stopSnippet = context.stopPYTHONSnippet

local snippetBoundary = verbatim.PYTHONSnippetBoundary
local snippetKeyword = verbatim.PYTHONSnippetKeyword
local snippetSingleton = verbatim.PYTHONSnippetSingleton
local snippetOperator = verbatim.PYTHONSnippetOperator
local snippetComment = verbatim.PYTHONSnippetComment
local snippetString = verbatim.PYTHONSnippetString
local snippetVarName = verbatim.PYTHONSnippetVarName
local snippetNumber = verbatim.PYTHONSnippetNumber
local snippetBuiltin = verbatim.PYTHONSnippetBuiltin


local function visualizename(s)
  if control[s] then
    snippetKeyword(s)
  elseif singletons[s] then
    snippetSingleton(s)
  elseif operators[s] then
    snippetOperator(s)
  elseif builtins[s] then
    snippetBuiltin(s)
  else
    verbatim(s)
  end
end

local handler = visualizers.newhandler {
  startinline = function() Snippet(false, "{") end,
  stopinline = function() context("}") end,
  startdisplay = function() startSnippet() end,
  stopdisplay = function() stopSnippet() end,

  boundary = function(s) snippetBoundary(s) end,
  number = function(s) snippetNumber(s) end,
  keyword = function(s) snippetKeyword(s) end,
  singleton = function(s) snippetSingleton(s) end,
  operator = function(s) snippetOperator(s) end,
  comment = function(s) snippetComment(s) end,
  string = function(s) snippetString(s) end,
  varname = function(s) snippetVarName(s) end,
  name = visualizename,
}

local name = (patterns.letter + patterns.underscore)
  * (patterns.letter + patterns.underscore + patterns.digit)^0
local digit = R("09")
local number = {}
number.integer = digit ^ 1
number.fractional = P(".") * (digit ^ 1)
number.decimal = (number.integer * (number.fractional ^-1)) + number.fractional
number.scientific = number.decimal * S("Ee") * number.integer
number.number = number.scientific + number.decimal

local stringstart = S("rfb")^0
local tdquote = P("\"\"\"")
local tsquote = P("'''")

local grammar = visualizers.newgrammar(
  "default",
  {
    "visualizer",

    dstring =
      makepattern(handler, "string", stringstart) *
      makepattern(handler, "string", patterns.dquote) *
      (
        V("whitespace")
        + makepattern(handler, "string", (P("\\") * P(1)) + 1 - patterns.dquote)
      )^0 *
      makepattern(handler, "string", patterns.dquote),

    sstring =
      makepattern(handler, "string", stringstart) *
      makepattern(handler, "string", patterns.squote) *
      (
        V("whitespace") +
        makepattern(handler, "string", (P("\\") * P(1)) + 1 - patterns.squote)
      )^0 *
      makepattern(handler, "string", patterns.squote),

    multidstring =
      makepattern(handler, "string", stringstart) *
      makepattern(handler, "string", tdquote) *
      (
        makepattern(handler, "string", (P("\\") * P(1)) + 1 - tdquote)
      )^0 *
      makepattern(handler, "string", tdquote),
    multisstring =
      makepattern(handler, "string", stringstart) *
      makepattern(handler, "string", tsquote) *
      (
        makepattern(handler, "string", (P("\\") * P(1)) + 1 - tsquote)
      )^0 *
      makepattern(handler, "string", tsquote),


    comment = makepattern(handler, "comment", P("#") * patterns.space^0 * (1 - patterns.newline)^0),
    boundary = makepattern(handler, "boundary", S("{}[]()")),

    operator = makepattern(handler, "operator", S("+-/@%^&|*=")),
    name = makepattern(handler, "name", name),
    number = makepattern(handler, "number", number.number),
    pattern =
      V("comment")
      + V("multisstring")
      + V("multidstring")
      + V("dstring")
      + V("sstring")
      + V("name")
      + V("number")
      + V("operator")
      + V("boundary")
      + V("space")
      + V("line")
      + V("default"),

    visualizer = V("pattern")^1
  }
)

local parser = P(grammar)

visualizers.register("python", {parser = parser, handler = handler, grammar = grammar })
