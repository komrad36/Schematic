#define WIN32_LEAN_AND_MEAN
#define VC_EXTRALEAN

#include <cstdint>
#include <cstdio>
#include <exception>
#include <functional>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

#include <Windows.h>

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

const char* typestr(const Type type) {
	static const char* types[] = {
		"Void",
		"I32",
		"F32",
		"Sym",
		"Str",
		"List",
		"Cfp",
		"Proc",
		"Pair"
	};
	return types[(int)type];
}

using I32 = int32_t;
using F32 = float;
using String = std::string;
template<class T> using Vector = std::vector<T>;
struct Cell;
using List = Vector<Cell>;
struct Err {
	I32 line, start, end;
	Err(I32 _line, I32 _start, I32 _end) : line(_line), start(_start), end(_end) {}
};
using Cfp = Cell(*)(Err err, const List& args);
using Map = std::unordered_map<I32, Cell>;

// ------------------------------- String Interning ------------------------------ //
enum InternedString : I32 {
	STR_hashf,
	STR_hasht,
	STR_singlequote,
	STR_dot,
	STR_quote,
	STR_empty,
	STR_if,
	STR_define,
	STR_setbang,
	STR_lambda,
	STR_plus,
	STR_minus,
	STR_mul,
	STR_div,
	STR_le,
	STR_ge,
	STR_gt,
	STR_lt,
	STR_eq,
	STR_ne,
	STR_list,
	STR_begin,
	STR_cons,
	STR_nullq,
	STR_car,
	STR_cdr,
	STR_append,
	STR_length,
	STR_procq,
	STR_procedureq,
	STR_symbolq,
	STR_listq,
	STR_exit
};

Vector<String> g_pool{
	"#f",
	"#t",
	"'",
	".",
	"quote",
	"",
	"if",
	"define",
	"set!",
	"lambda",
	"+",
	"-",
	"*",
	"/",
	"<=",
	">=",
	">",
	"<",
	"==",
	"!=",
	"list",
	"begin",
	"cons",
	"null?",
	"car",
	"cdr",
	"append",
	"length",
	"proc?",
	"procedure?",
	"symbol?",
	"list?",
	"exit"
};

InternedString intern(const String& s) {
	const size_t i = std::find(g_pool.begin(), g_pool.end(), s) - g_pool.begin();
	if (i == g_pool.size()) g_pool.emplace_back(s);
	return InternedString(i);
}

const String& tern(const InternedString i) {
	return g_pool[i];
}

// ---------------------------------- Cell ---------------------------------- //
struct AsList {};
struct AsString {};
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

	Cell() : type(Type::Void), err{-1, -1, -1 } {}
	Cell(const Err _err) : type(Type::Void), err(_err) {}
	Cell(const bool b, const Err _err) : type(Type::Sym), iStr(b ? STR_hasht : STR_hashf), err(_err) {}
	Cell(const I32 i, const Err _err) : type(Type::I32), i32(i), err(_err) {}
	Cell(const F32 f, const Err _err) : type(Type::F32), f32(f), err(_err) {}
	Cell(AsString, const InternedString _iStr, const Err _err) : type(Type::Str), iStr(_iStr), err(_err) {}
	Cell(const InternedString _iSym, const Err _err) : type(Type::Sym), iStr(_iSym), err(_err) {}
	Cell(const List& _list, const Err _err) : type(Type::List), list(_list), err(_err) {}
	Cell(AsList, const Err _err) : type(Type::List), err(_err) {}
	Cell(const Cfp _cfp) : type(Type::Cfp), cfp(_cfp), err{ -1, -1, -1 } {}
	Cell(const List& _proc, const I32 _iEnv, const Err _err) : type(Type::Proc), list(_proc), iEnv(_iEnv), err(_err) {}
	Cell(const Cell& c1, const Cell& c2, const Err _err) : type(Type::Pair), list{ c1, c2 }, err(_err) {}

	bool is_void() const { return type == Type::Void; }
	bool is_i32() const { return type == Type::I32; }
	bool is_f32() const { return type == Type::F32; }
	bool is_sym() const { return type == Type::Sym; }
	bool is_str() const { return type == Type::Str; }
	bool is_list() const { return type == Type::List; }
	bool is_cfp() const { return type == Type::Cfp; }
	bool is_proc() const { return type == Type::Proc; }
	bool is_pair() const { return type == Type::Pair; }
};

// --------------------------------  HCI  ----------------------------------- //

Vector<String> g_lines;

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

#pragma warning( push )
#pragma warning( disable : 100 ) // unused formal parameter c
void ERR_unhandled(const Cell& c) {
	std::cout << "ERROR: Oops! You've triggered an error I recognize, but can't handle yet!" << std::endl;
}

void ERR_type_mismatch(const Cell& c, Type req1, Type req2 = Type::Void) {
	std::cout << "ERROR: wrong type! Expected " << typestr(req1) << (req2 == Type::Void ? String() : String(" or ") + typestr(req2)) << ", but got " << typestr(c.type);
	if (c.is_sym() || c.is_str()) std::cout << " (\"" << tern(c.iStr) << "\")";
	std::cout << '!' << std::endl;
}

void ERR_type_not_allowed(const Cell& c) {
	std::cout << "ERROR: cannot operate on type " << typestr(c.type) << '!' << std::endl;
}

void ERR_empty_list(const Cell& c) {
	std::cout << "ERROR: cannot operate on empty list!" << std::endl;
}

void ERR_arg_count_mismatch(const Cell& c, bool at_least, I32 req, I32 got) {
	std::cout << "ERROR: wrong number of args! Expected " << (at_least ? "at least " : "") << req << ", but got " << got << "!" << std::endl;
}

void ERR_bad_dot(const Cell& c) {
	std::cout << "ERROR: dot notation only permitted as the second-to-last element of a list!" << std::endl;
}

void ERR_unexpected_char(const Cell& c) {
	std::cout << "ERROR: unexpected token!" << std::endl;
}

void ERR_never_found_closing(const Cell& c, const String& s) {
	std::cout << "ERROR: never found closing " + s + "!" << std::endl;
}

void ERR_not_bool(const Cell& c) {
	std::cout << "ERROR: statement did not evaluate to #t/#f!";
	if (c.is_sym() || c.is_str()) std::cout << "(\"" << tern(c.iStr) << "\")";
	std::cout << std::endl;
}

void ERR_unresolved_symbol(const Cell& c) {
	std::cout << "ERROR: unresolved symbol \"" << tern(c.iStr) << "\"!" << std::endl;
}
#pragma warning( pop )

struct SchematicExc : public std::exception {
	I32 err_line;
	explicit SchematicExc(const I32 _err_line) : err_line(_err_line) {}
};

void process_exc(const SchematicExc& e, const Cell& c) {
	if (c.err.line != e.err_line && c.err.line != -1 && c.err.start != -1 && c.err.end != -1) {
		set_color(Color::Red);
		std::cout << (e.err_line == -1 ? "While evaluating:" : "Called from:") << std::endl;
		set_color(Color::White);
		std::cout << g_lines[c.err.line] << std::endl;
		std::cout << std::string(c.err.start, ' ') << std::string(c.err.end - c.err.start, '~') << std::endl;
	}
	throw SchematicExc(c.err.line);
}

#define REQUIRE(x, f, c, ...) do {if (!(x)) {ColorJanitor jj(Color::Red); f(c, __VA_ARGS__); process_exc(SchematicExc(-1), c);}} while(0)

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

// -------------------------- Global Environment ---------------------------- //
void set_global_env() {
	g_env.clear();
	g_env.emplace_back();
	Env& e = g_env.back();

#pragma warning(push)
#pragma warning( disable : 100 ) // unused formal parameter err
#define CFP(cfp) Cell([](Err err, const List& args)->Cell cfp);
	e[STR_hashf] = { false, {-1, -1, -1} };
	e[STR_hasht] = { true, {-1, -1, -1} };
	e[STR_plus] = CFP({ return math_func(err, std::plus(), args); });
	e[STR_minus] = CFP({ return math_func(err, std::minus(), args); });
	e[STR_mul] = CFP({ return math_func(err, std::multiplies(), args); });
	e[STR_div] = CFP({ return math_func(err, std::divides(), args); });
	e[STR_le] = CFP({ return binary_bool_func(err, std::less_equal(), args); });
	e[STR_ge] = CFP({ return binary_bool_func(err, std::greater_equal(), args); });
	e[STR_gt] = CFP({ return binary_bool_func(err, std::greater(), args); });
	e[STR_lt] = CFP({ return binary_bool_func(err, std::less(), args); });
	e[STR_eq] = CFP({ return binary_bool_func(err, std::equal_to(), args); });
	e[STR_ne] = CFP({ return binary_bool_func(err, std::not_equal_to(), args); });
	e[STR_list] = CFP({ return Cell(args, err); });
	e[STR_begin] = CFP({ REQUIRE(!args.empty(), ERR_arg_count_mismatch, Cell(err), true, 1, (I32)args.size()); return args.back(); });
	e[STR_cons] = CFP({
		REQUIRE(args.size() == 2, ERR_arg_count_mismatch, Cell(err), false, 2, (I32)args.size());
		if (args[1].is_list()) {
			List ret{ args[0] };
			ret.insert(ret.end(), args[1].list.begin(), args[1].list.end());
			return Cell(ret, err);
		}
		else return Cell(args[0], args[1], err);
	});
	e[STR_nullq] = CFP({
		REQUIRE(args.size() == 1, ERR_arg_count_mismatch, Cell(err), false, 1, (I32)args.size());
		REQUIRE(args[0].is_list(), ERR_type_mismatch, args[0], Type::List);
		return Cell(args[0].list.empty(), args[0].err);
	});
	e[STR_car] = CFP({
		REQUIRE(args.size() == 1, ERR_arg_count_mismatch, Cell(err), false, 1, (I32)args.size());
		REQUIRE(args[0].is_list() || args[0].is_pair(), ERR_type_mismatch, args[0], Type::List, Type::Pair);
		REQUIRE(!args[0].list.empty(), ERR_empty_list, args[0]);
		return args[0].list[0];
	});
	e[STR_cdr] = CFP({
		REQUIRE(args.size() == 1, ERR_arg_count_mismatch, Cell(err), false, 1, (I32)args.size());
		REQUIRE(args[0].is_list() || args[0].is_pair(), ERR_type_mismatch, args[0], Type::List, Type::Pair);
		REQUIRE(!args[0].list.empty(), ERR_empty_list, args[0]);
		return args[0].is_pair() ? args[0].list[1] : Cell(List(args[0].list.begin() + 1, args[0].list.end()), err);
	});
	e[STR_append] = CFP({
		REQUIRE(args.size() == 2, ERR_arg_count_mismatch, Cell(err), false, 1, (I32)args.size());
		REQUIRE(args[0].is_list(), ERR_type_mismatch, args[0], Type::List);
		REQUIRE(args[1].is_list(), ERR_type_mismatch, args[1], Type::List);
		List ret = args[0].list;
		ret.insert(ret.end(), args[1].list.begin(), args[1].list.end());
		return Cell(ret, err);
	});
	e[STR_length] = CFP({
		REQUIRE(args.size() == 1, ERR_arg_count_mismatch, Cell(err), false, 1, (I32)args.size());
		REQUIRE(args[0].is_list(), ERR_type_mismatch, args[0], Type::List);
		return Cell((I32)args[0].list.size(), args[0].err);
	});
	e[STR_procedureq] = e[STR_procq] = CFP({
		REQUIRE(args.size() == 1, ERR_arg_count_mismatch, Cell(err), false, 1, (I32)args.size());
		return Cell(args[0].is_proc() || args[0].is_cfp(), args[0].err);
	});
	e[STR_symbolq] = CFP({
		REQUIRE(args.size() == 1, ERR_arg_count_mismatch, Cell(err), false, 1, (I32)args.size());
		return Cell(args[0].is_sym(), args[0].err);
	});
	e[STR_listq] = CFP({
		REQUIRE(args.size() == 1, ERR_arg_count_mismatch, Cell(err), false, 1, (I32)args.size());
		return Cell(args[0].is_list(), args[0].err);
	});
	e[STR_exit] = CFP({
		REQUIRE(args.size() <= 1, ERR_arg_count_mismatch, Cell(err), false, 1, (I32)args.size());
		if (args.size() <= 1) exit(args.empty() || !args[0].is_i32() ? 1 : args[0].i32);
		return Cell({ -1, -1, -1 });
	});
#pragma warning( pop )
#undef CFP
}

// -------------------------------- Eval ------------------------------------ //
Cell eval(const Cell& x, const I32 iEnv) {
	try {
		REQUIRE(!x.is_pair(), ERR_type_not_allowed, x);
		if (x.is_sym()) {
			const Cell* foundCell = g_env[iEnv].lookup(x.iStr);
			REQUIRE(foundCell, ERR_unresolved_symbol, x);
			Cell ret = *foundCell;
			ret.err = x.err;
			return ret;
		}
		if (!x.is_list()) return x;
		REQUIRE(!x.list.empty(), ERR_empty_list, x);
		if (x.list[0].is_sym()) {
			const I32 s = x.list[0].iStr;
			if (s == STR_quote) {
				REQUIRE(x.list.size() == 2, ERR_arg_count_mismatch, x, false, 2, (I32)x.list.size());
				return x.list[1];
			}
			if (s == STR_if) {
				REQUIRE(x.list.size() == 4, ERR_arg_count_mismatch, x, false, 4, (I32)x.list.size());
				const Cell& eval_res = eval(x.list[1], iEnv);
				REQUIRE(eval_res.is_sym(), ERR_type_mismatch, eval_res, Type::Sym);
				REQUIRE(eval_res.iStr == STR_hasht || eval_res.iStr == STR_hashf, ERR_not_bool, eval_res);
				const Cell& exp = x.list[2 + (eval_res.iStr == STR_hashf)];
				return eval(exp, iEnv);
			}
			if (s == STR_define) {
				REQUIRE(x.list.size() == 3, ERR_arg_count_mismatch, x, false, 3, (I32)x.list.size());
				REQUIRE(x.list[1].is_sym(), ERR_type_mismatch, x.list[1], Type::Sym);
				const InternedString var = x.list[1].iStr;
				const Cell& res = eval(x.list[2], iEnv);
				REQUIRE(!res.is_void(), ERR_type_not_allowed, res);
				g_env[iEnv][var] = res;
				return Cell(x.err);
			}
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
			if (s == STR_lambda) {
				REQUIRE(x.list.size() == 3, ERR_arg_count_mismatch, x, false, 3, (I32)x.list.size());
				REQUIRE(x.list[1].is_list(), ERR_type_mismatch, x.list[1], Type::List);
				return Cell(x.list, iEnv, x.err);
			}
		}

		const Cell& f = eval(x.list[0], iEnv);
		REQUIRE(f.is_cfp() || f.is_proc(), ERR_type_mismatch, f, Type::Cfp, Type::Proc);
		List args;
		for (size_t i = 1; i < x.list.size(); ++i) args.emplace_back(eval(x.list[i], iEnv));
		if (f.is_cfp()) return f.cfp(x.err, args);

		REQUIRE(f.is_proc(), ERR_type_mismatch, f, Type::Proc);
		REQUIRE(f.list[1].is_list(), ERR_type_mismatch, f.list[1], Type::List);
		REQUIRE(f.list[1].list.size() == args.size(), ERR_arg_count_mismatch, x, false, (I32)f.list[1].list.size(), (I32)args.size());
		g_env.emplace_back(f.list[1].list, args, f.iEnv);
		const Cell& res = eval(f.list[2], I32(g_env.size() - 1));
		REQUIRE(!res.is_void(), ERR_type_not_allowed, res);
		return res;
	}
	catch (const SchematicExc& e) {
		process_exc(e, x);
		return Cell();
	}
}

// -------------------------------- Parse ----------------------------------- //
bool make_i32(const String& S, I32& i) {
	char* end = nullptr;
	i = (I32)std::strtol(&S[0], &end, 10);
	return end == &S[S.size()];
}

bool make_f32(const String& S, F32& f) {
	char* end = nullptr;
	f = std::strtof(&S[0], &end);
	return end == &S[S.size()];
}

Cell parse_categorize(const String& S, const Err err) {
	I32 i;
	if (make_i32(S, i)) return Cell(i, err);

	F32 f;
	if (make_f32(S, f)) return Cell(f, err);

	return S.empty() || S[0] != '"' ? Cell(intern(S), err) : Cell(AsString(), intern(S.substr(1)), err);
}

void parse_emplace(List& list, Cell c) {
	while (!list.empty() && list.back().is_sym() && list.back().iStr == STR_singlequote) {
		c = Cell(List{ Cell(STR_quote, list.back().err), c }, c.err);
		list.pop_back();
	}
	list.emplace_back(c);
}

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

List parse(const String& S, const I32 err_line) {
	const char* s = &S[0];
	List ret{ Cell(AsList{}, {err_line, -1, -1}) };
	String token;
	I32 tok_start = 0;
	I32 i = 0;
	for (;; ++s, ++i) {
		const char c = *s;
		if (c == '(') {
			// a list has started
			REQUIRE(token.empty(), ERR_unexpected_char, Cell({err_line, i, i + 1 }));
			ret.emplace_back(AsList{}, Err{ err_line, i, -1 });
		}
		else if (c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\0') {
			// a token has ended
			if (!token.empty()) {
				parse_emplace(ret.back().list, parse_categorize(token, {err_line, tok_start, i }));
				token.clear();
			}
			if (c == '\0')
				break;
		}
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
		else if (c == '\'') {
			// a quote has started
			REQUIRE(token.empty(), ERR_unexpected_char, Cell({err_line, i, i + 1 }));
			ret.back().list.emplace_back(Cell(STR_singlequote, {err_line, i, i + 1 }));
		}
		else {
			if (token.empty()) {
				tok_start = i;
			}
			// a token has continued
			token += c;
		}
	}

	// close all lists
	while (ret.size() > 1) {
		parse_emplace(ret[ret.size() - 2].list, parse_finalize(ret.back(), i));
		ret.pop_back();
	}

	return ret.back().list;
}

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

String unparse(const Cell& c);

String unparse_pair(const Cell& c) {
	REQUIRE(c.is_pair(), ERR_type_mismatch, c, Type::Pair);
	REQUIRE(c.list.size() == 2, ERR_arg_count_mismatch, c, false, 2, (I32)c.list.size());
	return unparse(c.list[0]) + (c.list[1].is_pair() ? ' ' + unparse_pair(c.list[1]) : " . " + unparse(c.list[1]));
}

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
	else if (c.is_i32()) {
		s += std::to_string(c.i32);
	}
	else if (c.is_f32()) {
		s.resize(s.size() + 128);
		const int sz = sprintf_s(&s[s.size() - 128], 128, fabs(c.f32) < 1e6f && round(c.f32) == c.f32 ? "%#.2g" : "%g", c.f32);
		s.resize(s.size() - 128 + sz);
	}
	else if (c.is_sym()) {
		s += tern(c.iStr);
	}
	else if (c.is_str()) {
		s += '"' + tern(c.iStr) + '"';
	}
	else if (c.is_pair()) {
		REQUIRE(c.list.size() == 2, ERR_arg_count_mismatch, c, false, 2, (I32)c.list.size());
		s += '(' + unparse_pair(c) + ')';
	}
	else if (c.is_cfp()) {
		s += "#<Built-in function at 0x" + to_hex((uint64_t)(uintptr_t)c.cfp) + '>';
	}
	else if (c.is_proc()) {
		const size_t num_args = c.list[1].list.size();
		s += "#<Proc taking " + std::to_string(num_args) + (num_args == 1 ? " arg>" : " args>");
	}
	return s;
}

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

// -------------------------------- Test ------------------------------------ //
void test(const char* s, const char* expected) {
	std::cout << std::string(79, '-') << std::endl;
	std::cout << "IN : " << s << std::endl;
	g_lines.emplace_back(s);
	String res;
	try {
		res = unparse(eval(parse(s, I32(g_lines.size() - 1)).back(), 0));
	}
	catch (...) { res = "<EXCEPTION!!!>"; };
	if (res == expected) {
		ColorJanitor jj(Color::Green);
		std::cout << "OUT: " << res << std::endl;
	}
	else {
		set_color(Color::Red);
		std::cout << "OUT: " << res << std::endl;
		std::cout << "ERROR: self-test failed! Expected " << expected << "! Aborting." << std::endl;
		set_color(Color::White);
		system("pause");
		exit(1);
	}
}

// -------------------------------- Main ------------------------------------ //
int main() {
	//repl();
	set_global_env();
	std::cout << "Running unit tests..." << std::endl;
	test("2.0", "2.0");
	test("-2.0", "-2.0");
	test("(quote (testing 1 (2.0) -3.14e+15))", "(testing 1 (2.0) -3.14e+15)");
	test("(+ 2 2)", "4");
	test("(+ (* 2 100) (* 1 10))", "210");
	test("(if (> 6 5) (+ 1 1) (+ 2 2))", "2");
	test("(if (< 6 5) (+ 1 1) (+ 2 2))", "4");
	test("(define x 3)", "");
	test("(define no-arg-lambda (lambda () 3))", "");
	test("(no-arg-lambda)", "3");
	test("x", "3");
	test("(+ x x)", "6");
	test("(+ 2 5)", "7");
	test("(+ 2.5 5)", "7.5");
	test("(+ 2 5.5)", "7.5");
	test("(+ 2.5 5.5)", "8.0");
	test("(- 2 5)", "-3");
	test("(- 2 3.5 -1)", "-0.5");
	test("(cons 1 2)", "(1 . 2)");
	test("(cons \"banana\" \"split\")", "(\"banana\" . \"split\")");
	test("(car (cons 1 2))", "1");
	test("(cdr (cons 1 2))", "2");
	test("(cons (list 2 3) 1)", "((2 3) . 1)");
	test("(cons 1 (list 2 3))", "(1 2 3)");
	test("(cons 0 (cons 1 2))", "(0 1 . 2)");
	test("'(0 . (1 . (2 . 3)))", "(0 1 2 . 3)");
	test("'(1 . (2 . (3 . ())))", "(1 2 3)");
	test("'(2 3 . 5)", "(2 3 . 5)");
	test("(list 0 (cons 1 2))", "(0 (1 . 2))");
	test("(quote (1 . 2))", "(1 . 2)");
	test("(quote (0 . (1 . 2)))", "(0 1 . 2)");
	test("(define f (cons 1 2))", "");
	test("f", "(1 . 2)");
	test("(cons f 2)", "((1 . 2) . 2)");
	test("(cons 2 f)", "(2 1 . 2)");
	test("(cons f f)", "((1 . 2) 1 . 2)");
	test("(+ 1 . (2))", "3");
	test("(list '(0 . 1))", "((0 . 1))");
	test("(begin (define x 1) (set! x (+ x 1)) (+ x 1))", "3");
	test("'hello", "hello");
	test("\"hello world\"", "\"hello world\"");
	test("'\"hello world\"", "\"hello world\"");
	test("''hello", "(quote hello)");
	test("((lambda (x) (+ x x)) 5)", "10");
	test("(define z (lambda (x y) (begin (define x y) x)))", "");
	test("(z 'b 25)", "25");
	test("(define twice (lambda (x) (* 2 x)))", "");
	test("twice", "#<Proc taking 1 arg>");
	test("(twice 5)", "10");
	test("(define compose (lambda (f g) (lambda (x) (f (g x)))))", "");
	test("(compose list twice)", "#<Proc taking 1 arg>");
	test("((compose list twice) 5)", "(10)");
	test("(define repeat (lambda (f) (compose f f)))", "");
	test("((repeat twice) 5)", "20");
	test("((repeat (repeat twice)) 5)", "80");
	test("(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))", "");
	test("(fact 3)", "6");
	test("(fact 12)", "479001600");
	test("(define abs (lambda (n) ((if (> n 0) + -) 0 n)))", "");
	test("(car (quote (2 3 5)))", "2");
	test("(cdr (quote (2 3 5)))", "(3 5)");
	test("(list (abs -3) (abs 0) (abs 3))", "(3 0 3)");
	test(R"((define combine (lambda (f)
		(lambda (x y)
		(if (null? x) (quote ())
		(f (list (car x) (car y))
		((combine f) (cdr x) (cdr y))))))))", "");
	test("(define zip (combine cons))", "");
	test("(zip (list 1 2 3 4) (list 5 6 7 8))", "((1 5) (2 6) (3 7) (4 8))");
	test(R"((define riff-shuffle (lambda (deck) (begin
		(define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq))))))
		(define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))
		(define mid (lambda (seq) (/ (length seq) 2)))
		((combine append) (take (mid deck) deck) (drop (mid deck) deck))))))", "");
	test("riff-shuffle", "#<Proc taking 1 arg>");
	test("(riff-shuffle (list 1 2 3 4 5 6 7 8))", "(1 5 2 6 3 7 4 8)");
	test("((repeat riff-shuffle) (list 1 2 3 4 5 6 7 8))", "(1 3 5 7 2 4 6 8)");
	test("(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))", "(1 2 3 4 5 6 7 8)");
	std::cout << std::string(79, '-') << std::endl;
	std::cout << std::endl << "Switching to interactive mode..." << std::endl << "Ready." << std::endl << std::endl;
	repl();
}
