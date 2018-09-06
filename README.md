# Schematic #
Basic toy Lisp interpreter in a few hundred lines of C++.

All functionality is contained in `main.cpp`. There's also a handy NatVis for natural human-readable viewing of Schematic Cells in Visual Studio, found in `CellViewer.natvis`. 

## Explanation of Source Code ##

```cpp
// ------------------------------- Basic Types ------------------------------ //
enum class Type {
	Void,
	I32,
	F32,
	Sym,
	Str,
	List,
	Cfp,
	Proc,
	Pair
};
```

The basic unit of manipulation in Schematic is a `Cell`. Cells can be one of several types:
* `Void`: a null or empty cell
* `I32`: a 32-bit signed integer
* `F32`: a 32-bit float
* `Sym`: a symbol
* `Str`: a string
* `List`: a list of Cells
* `Cfp`: a built-in function
* `Proc`: a callable procedure that is not built-in but is created at runtime
* `Pair`: a Scheme pair structure

```cpp
struct Err {
	I32 line, start, end;
	Err(I32 _line, I32 _start, I32 _end) : line(_line), start(_start), end(_end) {}
};
```

A structure for tracking the source of a cell: the line number, start character, and end character from which it was originally parsed. This source information is propagated through cell manipulation so that errors can be reported in a user-friendly way.

```cpp
using Cfp = Cell(*)(Err err, const List& args);
```
A callable Cfp takes an error source and a list of Cells as arguments, and returns a Cell.

```cpp
// ------------------------------- String Interning ------------------------------ //
```
Strings in Schematic are never manipulated directly during evaluation. Instead, they are _interned_. A global vector of strings, `g_pool`, is created with some pre-existing strings, which will never be moved or removed. At runtime, additional strings will be added to the pool as needed during parsing. This way, strings can be referred to and compared by manipulating an `InternedString`, which is simply an integer index into the `g_pool` array, without ever needing to manipulate an actual string during evaluation (only during parsing or unparsing).

```cpp
struct Cell {
	List list;
	union {
		I32 i32;
		InternedString iStr;
		F32 f32;
		Cfp cfp;
		I32 iEnv;
	};
	Type type;
	Err err;
```
A `Cell` consists of a type, an error source, and a union of basic data needed to represent all possible Cell types. The `list` member is not included in the union for two reasons: firstly, because a `List` is a `std::vector`, which is a nontrivial type, which complicates its inclusion in a union. And secondly, because types can potentially require a `List` as well as some additional data. In particular, the `Proc` type stores its body and arguments in `list` and its execution environment in `iEnv`.

```cpp
Vector<String> g_lines;
```
Each line is stored in this vector during execution for the purpose of user-friendly error handling. The `line` variable of an `Err` structure is used as an index into this vector to retrieve the source line responsible for that cell.

```cpp
enum class Color {
	Yellow = 6,
	White = 7,
	Blue = 9,
	Green = 10,
	Red = 12
};

void set_color(Color color) {
	SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), (WORD)color);
}

struct ColorJanitor {
	ColorJanitor(Color color) {
		set_color(color);
	}
	~ColorJanitor() {
		set_color(Color::White);
	}
};
```
Support for printing text in pretty colors on Windows. Feel free to disable or modify to compile on other platforms.

```cpp
void ERR_unhandled(const Cell& c) {
```
The `ERR_` functions report a specific type of error to the user.

```cpp
struct SchematicExc : public std::exception {
	I32 err_line;
	explicit SchematicExc(const I32 _err_line) : err_line(_err_line) {}
};
```
Exception handling is used to propagate an error up through the Scheme call stack via the C++ call stack, reporting useful source information at each level of Scheme execution. `SchematicExc` propagates current line information so that only the first error is reported per line as the stack is unwound.

```cpp
#define REQUIRE(x, f, c, ...) do {if (!(x)) {ColorJanitor jj(Color::Red); f(c, __VA_ARGS__); process_exc(SchematicExc(-1), c);}} while(0)
```
`REQUIRE`s are used to detect error conditions. If a requirement is not met, the error is reported, then `process_exc` is called to begin the stack unwinding and line printing process.

```cpp
// ------------------------- Execution Environment -------------------------- //
struct Env;
Vector<Env> g_env;

struct Env : public Map {
	I32 iEnv;
	Env() : iEnv(-1) {}
	Env(const List& _parms, const List& _args, const I32 _iEnv = -1) : iEnv(_iEnv) {
		for (size_t i = 0; i < _parms.size(); ++i) {
			(*this)[_parms[i].iStr] = _args[i];
		}
	}
	Cell* lookup(const I32 iVar) {
		const auto& it = find(iVar);
		return it != end() ? &it->second : iEnv == -1 ? nullptr : g_env[iEnv].lookup(iVar);
	}
};
```
A cell executes in an execution environment, which is a certain node within a hierarchy of environments. An environment can be thought of as a scope. At the start of execution there is a basic global environment, containing only built-ins. Additional symbols can be defined, or existing symbols can be redefined. If a new scope is started, symbols from the parent scope can be redefined, but their value is then restored when the scope is left (lexical scoping). This is implemented in Schematic via environments. When a new scope is created, it gains its own (initially empty) environment with the global environment as its parent in the hierarchy. Symbols defined within that scope go into that scope's environment. Symbol resolution within that scope checks first its own environment, then its parent environment, and so on all the way up the hierarchy until it resolves the symbol or reaches the top of the hierarchy. When the scope is exited, that environment is no longer considered. In this way, exiting a scope block naturally restores any clobbered symbol definitions to those of the parent scope, because the child environment, which is where the new definitions were stored, is no longer in use.

A convenience constructor is provided in which a new environment is created with a series of symbol definitions (mapping of parameter names, as `InternedString`s, to their values, as `Cell`s, are passed in, along with a parent environment. This is because one way an environment (new scope) can be created at runtime is upon execution of a Proc, to bind any incoming argument names to their passed-in values. 

`lookup` implements the hierarchy/tree structure, returning a pointer to the Cell that the symbol resolves to, if found, or recursively checking its parent environment if not found. If we make it all the way to the root of the hierarchy and still don't find it, `nullptr` is returned.

The vector `g_env` is used to store environments as they are created, so that an integer handle (index) can be used to uniquely identify an environment. Of course, this means environments stick around after they are no longer needed. If this became a problem, a shared pointer or other solution could be made to only keep them around as needed.

```cpp
// ----------------------------- Binding Helpers ---------------------------- //
template<class T> Cell binary_bool_func(Err err, T op, const List& args) {
	REQUIRE(args.size() == 2, ERR_arg_count_mismatch, Cell(err), false, 2, (I32)args.size());
	REQUIRE(args[0].is_i32() || args[0].is_f32(), ERR_type_mismatch, args[0], Type::I32, Type::F32);
	REQUIRE(args[1].is_i32() || args[1].is_f32(), ERR_type_mismatch, args[1], Type::I32, Type::F32);
	if (args[0].is_f32() || args[1].is_f32()) {
		return Cell(op(args[0].is_f32() ? args[0].f32 : (F32)args[0].i32, args[1].is_f32() ? args[1].f32 : (F32)args[1].i32), err);
	}
	else {
		return Cell(op(args[0].i32, args[1].i32), err);
	}
}

template<class T> Cell math_func(Err err, T op, const List& args) {
	REQUIRE(!args.empty(), ERR_arg_count_mismatch, Cell(err), true, 1, (I32)args.size());
	REQUIRE(args[0].is_i32() || args[0].is_f32(), ERR_type_mismatch, args[0], Type::I32, Type::F32);
	I32 iAccum = args[0].is_f32() ? (I32)args[0].f32 : args[0].i32;
	F32 fAccum = args[0].is_f32() ? args[0].f32 : (F32)args[0].i32;
	bool is_int = args[0].is_i32();
	for (size_t i = 1; i < args.size(); ++i) {
		const Cell& arg = args[i];
		REQUIRE(arg.is_i32() || arg.is_f32(), ERR_type_mismatch, arg, Type::I32, Type::F32);

		if (is_int && arg.is_i32()) {
			iAccum = op(iAccum, arg.i32);
		}
		else {
			if (is_int) fAccum = (F32)iAccum;
			is_int = false;
			fAccum = op(fAccum, arg.is_i32() ? (F32)arg.i32 : arg.f32);
		}
	}
	return is_int ? Cell(iAccum, err) : Cell(fAccum, err);
}
```
These are binding helpers that make it easier to define built-in functions that fit a certain pattern in terms of existing C/C++ functions. `binary-bool-func` is for functions which take two numbers (either floats or ints) and return a bool, i.e. comparison operators. It verifies the arguments are of one of the two permissible types, and also follows promotion rules - if both args are integers, it performs an ineteger comparison; otherwise, it performs a float comparison.

`math_func` is for functions which take an arbitrary number of numbers and return a number, i.e. basic math operations in Scheme like + or -. It verifies the arguments and similarly follows promotion rules.

```cpp
#define CFP(cfp) Cell([](Err err, const List& args)->Cell cfp);
```
A convenience wrapper that creates and returns a C++ lambda that takes an error source and a list of args and returns a Cell. This way we can easily package up basic operations, create lambdas out of them, and in turn, store those lambdas as Cfps in the global environment.

```cpp
// -------------------------------- Eval ------------------------------------ //
Cell eval(const Cell& x, const I32 iEnv) {
```
Evaluate a cell _in an environment_. It does not make sense to speak of evaluating a cell without an environment in which to evaluate it.
```cpp
try {
```
The whole function is wrapped in a `try` because reporting errors in a user-friendly way involves unwinding the Scheme stack to print useful error info for each line involved in evaluation, and this, in turn, is accomplished via the `SchematicExc` system discussed earlier, in which we use C++ exceptions to unwind the `eval` C++ stack as well.

```cpp
REQUIRE(!x.is_pair(), ERR_type_not_allowed, x);
```
You can't eval pairs!

```cpp
if (x.is_sym()) {
  const Cell* foundCell = g_env[iEnv].lookup(x.iStr);
  REQUIRE(foundCell, ERR_unresolved_symbol, x);
  Cell ret = *foundCell;
  ret.err = x.err;
  return ret;
}
```
If we're evaluating a symbol, look up the cell it points to, in the current execution environment. Return that cell but propagate the error source information from the current cell along with it.

```cpp
if (!x.is_list()) return x;
```
If `x` is not a list then it's a void, or a number, or a string, or a cfp, or a proc. In any of these cases we just return it as-is. The result of evaluating it is itself.

Otherwise, we are evaluating a non-empty list. If the first element of the list is a symbol we check for a few special cases:
```cpp
if (s == STR_quote) {
  REQUIRE(x.list.size() == 2, ERR_arg_count_mismatch, x, false, 2, (I32)x.list.size());
  return x.list[1];
}
```
If the symbol is "quote", we just return the second element of the list.

```cpp
if (s == STR_if) {
  REQUIRE(x.list.size() == 4, ERR_arg_count_mismatch, x, false, 4, (I32)x.list.size());
  const Cell& eval_res = eval(x.list[1], iEnv);
  REQUIRE(eval_res.is_sym(), ERR_type_mismatch, eval_res, Type::Sym);
  REQUIRE(eval_res.iStr == STR_hasht || eval_res.iStr == STR_hashf, ERR_not_bool, eval_res);
  const Cell& exp = x.list[2 + (eval_res.iStr == STR_hashf)];
  return eval(exp, iEnv);
}
```
If the symbol is "if", evaluate the condition recursively. Require that it returns either #t or #f. Evaluate and return the first or second alternative accordingly.

```cpp
if (s == STR_define) {
  REQUIRE(x.list.size() == 3, ERR_arg_count_mismatch, x, false, 3, (I32)x.list.size());
  REQUIRE(x.list[1].is_sym(), ERR_type_mismatch, x.list[1], Type::Sym);
  const InternedString var = x.list[1].iStr;
  const Cell& res = eval(x.list[2], iEnv);
  REQUIRE(!res.is_void(), ERR_type_not_allowed, res);
  g_env[iEnv][var] = res;
  return Cell(x.err);
}
```
If the symbol is "define", evaluate the third argument and bind the second argument's symbol to that result _in the current environment_.

```cpp
if (s == STR_setbang) {
  REQUIRE(x.list.size() == 3, ERR_arg_count_mismatch, x, false, 3, (I32)x.list.size());
  const Cell& var = x.list[1];
  const Cell& res = eval(x.list[2], iEnv);
  REQUIRE(!res.is_void(), ERR_type_not_allowed, res);
  Cell* foundCell = g_env[iEnv].lookup(var.iStr);
  REQUIRE(foundCell, ERR_unresolved_symbol, var);
  *foundCell = res;
  return Cell(x.err);
}
```
If the symbol is "set!", evaluate the third argument and bind the second argument's symbol to that result _in the innermost environment in which it's defined_.

```cpp
if (s == STR_lambda) {
  REQUIRE(x.list.size() == 3, ERR_arg_count_mismatch, x, false, 3, (I32)x.list.size());
  return Cell(x.list, iEnv, x.err);
}
```
If the symbol is "lambda", just return a Proc, which will consist of the entire list unmodified (which consists of the symbol "lambda", followed by a list of args, followed by a list which is the body of the lambda). We also package the current environment and error source along with it. That's all the information we need to store to later rehydrate and execute this Proc in future.

```cpp
const Cell& f = eval(x.list[0], iEnv);
REQUIRE(f.is_cfp() || f.is_proc(), ERR_type_mismatch, f, Type::Cfp, Type::Proc);
```
Otherwise, evaluate the first argument of the list. Require that it's either a Cfp or a Proc, as we now need to execute it.

```cpp
List args;
for (size_t i = 1; i < x.list.size(); ++i) args.emplace_back(eval(x.list[i], iEnv));
```
Evaluate the args in order and store the results of the evaluations into the `args` list.

```cpp
if (f.is_cfp()) return f.cfp(x.err, args);
```
If it's a Cfp, execute the Cfp with the arg list.

```cpp
REQUIRE(f.is_proc(), ERR_type_mismatch, f, Type::Proc);
REQUIRE(f.list[1].list.size() == args.size(), ERR_arg_count_mismatch, x, false, (I32)f.list[1].list.size(), (I32)args.size());
g_env.emplace_back(f.list[1].list, args, f.iEnv);
const Cell& res = eval(f.list[2], I32(g_env.size() - 1));
REQUIRE(!res.is_void(), ERR_type_not_allowed, res);
return res;
```
Otherwise, it must be a Proc, i.e. a lambda. Require that we got the expected number of args. Create a new environment whose parent is the environment that existed when the lambda was defined (not necessarily the same CONTENTS as when it was defined, just the same scope. It may have had symbols added/changed after the lambda definition but still within the same scope which we need to honor. Add to this new environment the mappings of passed-in arguments to formal parameters. Evaluate the body of the lambda in this new environment. Return the result.

```cpp
// -------------------------------- Parse ----------------------------------- //
bool make_i32(const String& S, I32& i) {
	char* end = nullptr;
	i = (I32)std::strtol(&S[0], &end, 10);
	return end == &S[S.size()];
}
```
Try to parse a string into an I32, and return true if successful.

```cpp
bool make_f32(const String& S, F32& f) {
	char* end = nullptr;
	f = std::strtof(&S[0], &end);
	return end == &S[S.size()];
}
```
Try to parse a string into an F32, and return true if successful. Note that you want to run `make_i32` first, because `make_f32` will successfully parse an integer.

```cpp
Cell parse_categorize(const String& S, const Err err) {
	I32 i;
	if (make_i32(S, i)) return Cell(i, err);

	F32 f;
	if (make_f32(S, f)) return Cell(f, err);

	return S.empty() || S[0] != '"' ? Cell(intern(S), err) : Cell(AsString(), intern(S.substr(1)), err);
}
```
Construct a Cell from a single String token. If it's an integer, make an integer cell. If it's a float, make a float cell. Otherwise, make a Symbol cell, or a String cell if it's the empty string or in double quotes.

```cpp
void parse_emplace(List& list, Cell c) {
	while (!list.empty() && list.back().is_sym() && list.back().iStr == STR_singlequote) {
		c = Cell(List{ Cell(STR_quote, list.back().err), c }, c.err);
		list.pop_back();
	}
	list.emplace_back(c);
}
```
Emplace cell `c` onto list `list`, but if we would have produced a single quote followed by an expression - say, 'x - replace it with (quote x). We do this recursively for as many single quotes are present, so ''x becomes (quote (quote x)), and so on.

```cpp
Cell& parse_finalize(Cell& c, I32 err_end) {
	if (c.is_list()) {
		c.err.end = err_end;
		for (size_t i = 0; i < c.list.size(); ++i) {
			REQUIRE((i == c.list.size() - 2 && i && i != c.list.size() - 1) || !c.list[i].is_sym() || c.list[i].iStr != STR_dot, ERR_bad_dot, c.list[i]);
		}
		if (c.list.size() >= 3 && c.list[c.list.size() - 2].is_sym() && c.list[c.list.size() - 2].iStr == STR_dot) {
			if (c.list.back().is_list()) {
				const List tmp = c.list.back().list;
				c.list.resize(c.list.size() - 2);
				c.list.insert(c.list.end(), tmp.begin(), tmp.end());
			}
			else {
				Cell ret(c.list[c.list.size() - 3], c.list[c.list.size() - 1], c.err);
				for (I32 i = I32(c.list.size()) - 4; i >= 0; --i) {
					ret = Cell(c.list[i], ret, ret.err);
				}
				c = ret;
			}
		}
	}
	return c;
}
```
Finalized a parsed cell. Patch in the end character of the cell for error source tracking since we didn't know the end character when we started parsing the cell. If it's a list, require that if it has a dot in it, the dot is in a legal place (second-from-last element, and not the first element). If it DOES have a dot there, then: if the last element is a list, we can collapse the dot and trailing list's elements flat into the original list:
(a . (b c d)) -> (a b c d)
Otherwise, we have to break the optimization we've used so far that a list is really stored flat as a list rather than as a pair of a value and a pair of a value and a pair..., because we need a way to properly reflect the now non-standard structure of this list. So we recursively break the list into a pair of a value and a pair of value and a pair..., culminating in a pair of _two values_, unlike the standard structure where we terminate in a pair of a value and a void.

```cpp
List parse(const String& S, const I32 err_line) {
	const char* s = &S[0];
	List ret{ Cell(AsList{}, {err_line, -1, -1}) };
```
Actual parsing of an input string! We parse an input string and return a List of Cells created from it.

To start out, we create a working list which we populate with an empty list. This empty list will be the implied list or implied scope of the incoming expression.

```cpp
for (;; ++s, ++i) {
  const char c = *s;
```
We now loop monotonically over the characters in the string. We never stagnate nor jump back, so it's singly linear in the number of characters.


```cpp
if (c == '(') {
  // a list has started
  REQUIRE(token.empty(), ERR_unexpected_char, Cell({err_line, i, i + 1 }));
  ret.emplace_back(AsList{}, Err{ err_line, i, -1 });
}
```
Upon encountering an openparen, we simply push a new empty list (scope) onto the working list. Note that we can push an error source consisting of the current line and start character, but we don't know the end character yet. That will be patched in later in `parse_finalize`.

```cpp
else if (c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\0') {
  // a token has ended
  if (!token.empty()) {
    parse_emplace(ret.back().list, parse_categorize(token, {err_line, tok_start, i }));
    token.clear();
  }
  if (c == '\0')
    break;
}
 ```
 If a token ends, categorize and emplace the token into the current scope.
 
 ```cpp
else if (c == ')') {
  // a token has ended
  if (!token.empty()) {
    parse_emplace(ret.back().list, parse_categorize(token, {err_line, tok_start, i }));
    token.clear();
  }

  // and the list it's in has ended
  REQUIRE(ret.size() > 1, ERR_unexpected_char, Cell({err_line, i, i + 1 }));
  parse_emplace(ret[ret.size() - 2].list, parse_finalize(ret.back(), i + 1));
  ret.pop_back();
}
```
If a list ends, categorize and emplace the current token, if there is one, into the current scope. Then, finalize the current scope, pop it out of the working set, and push the whole thing into its parent scope, since that's the scope we've now returned (back) to.

```cpp
else if (c == '"') {
  // a string has started
  REQUIRE(token.empty(), ERR_unexpected_char, Cell({err_line, i, i + 1 }));
  const char* close = strchr(s + 1, '"');
  REQUIRE(close, ERR_never_found_closing, Cell({err_line, i, I32(S.size()) }), "\"");
  REQUIRE(close[1] == ' ' || close[1] == '\t' || close[1] == '\r' || close[1] == '\n' || close[1] == '\0' || close[1] == ')', ERR_unexpected_char, Cell({err_line, I32(close - &S[0] + 1), I32(close - &S[0] + 2) }));
  tok_start = i;
  token.assign(s, close);
  i += I32(close - s);
  s = close;
}
 ```
 If a string has started, scan ahead to find the end of the string and make the whole string the current token.
 
 ```cpp
else if (c == '\'') {
  // a quote has started
  REQUIRE(token.empty(), ERR_unexpected_char, Cell({err_line, i, i + 1 }));
  ret.back().list.emplace_back(Cell(STR_singlequote, {err_line, i, i + 1 }));
}
```
If we encounter a quote, push it back as its own cell. It will be patched up in parse_emplace.

```cpp
// close all lists
while (ret.size() > 1) {
  parse_emplace(ret[ret.size() - 2].list, parse_finalize(ret.back(), i));
  ret.pop_back();
}
```
After we finish parsing, close all pending scopes, allowing the user to skip closing parens at the end of the expression if desired.

```cpp
return ret.back().list;
```
Return the (only remaining; top-level) scope.

```cpp
// ------------------------------ Unparse ----------------------------------- //
std::string to_hex(uint64_t w) {
	std::string ret;
	ret.reserve(16);
	for (size_t i = 0, j = 60; i < 16; ++i, j -= 4) {
		const size_t idx = (w >> j) & 15;
		ret.push_back((char)(idx + 48 + 7 * (idx > 9)));
	}
	return ret;
}
```
Quickly stringify a 64-bit unsigned integer to hex.

```cpp
String unparse_pair(const Cell& c) {
	REQUIRE(c.is_pair(), ERR_type_mismatch, c, Type::Pair);
	REQUIRE(c.list.size() == 2, ERR_arg_count_mismatch, c, false, 2, (I32)c.list.size());
	return unparse(c.list[0]) + (c.list[1].is_pair() ? ' ' + unparse_pair(c.list[1]) : " . " + unparse(c.list[1]));
}
```
Special rules for unparsing pairs, because Scheme is silly: you can collapse at print-time the expression (a . (b . c)) into (a b . c).

```cpp
String unparse(const Cell& c) {
	String s;
	if (c.is_list()) {
		s += '(';
		const List& L = c.list;
		for (size_t i = 0; i < L.size(); ++i) {
			s += unparse(L[i]);
			if (i != L.size() - 1) s += ' ';
		}
		s += ')';
	}
```
To unparse a list, recursively unparse its elements, separate them by spaces, and wrap the whole thing in parens.

```cpp
else if (c.is_i32()) {
  s += std::to_string(c.i32);
}
else if (c.is_f32()) {
  s.resize(s.size() + 128);
  const int sz = sprintf_s(&s[s.size() - 128], 128, fabs(c.f32) < 1e6f && round(c.f32) == c.f32 ? "%#.2g" : "%g", c.f32);
  s.resize(s.size() - 128 + sz);
}
```
To unparse a number, print it directly, except that small integral floats should be deliberately printed with a couple decimal digits (i.e. 6.00 instead of 6) to show that they're floats.

```cpp
// -------------------------------- Repl ------------------------------------ //
void repl() {
	set_global_env();
	for (;;) {
		set_color(Color::Yellow);
		printf("schematic> ");
		set_color(Color::White);
		char input[4096];
		gets_s(input, 4096);
		String S = input;
		String s;
		try {
			g_lines.emplace_back(S);
			List in = parse(input, I32(g_lines.size() - 1));
			if (!in.empty()) {
				s = unparse(eval(in.back(), 0));
			}
		}
		catch (...) {
			continue;
		}
		if (!s.empty()) {
			ColorJanitor jj(Color::Green);
			printf("%s\n", s.c_str());
		}
	}
}
```
The classic REPL. Prepare the global environment, then read, evaluate, print, and loop.

The rest is unittests that run on startup. Some of the unittests come from Peter's lis.py.

Hope this is useful and/or fun! It's missing tons of built-ins and language features like continuations and guaranteed tail recursion. But that's okay, it's just for fun and to explore how straightforward and simple it is possible to make a proper Scheme parser, unparser, and interpreter. If you see a cool way to add these features without losing sight of the goals of simplicitly, robustness, and good error reporting, I'd love to see them go in!
