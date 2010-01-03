// primitive datatypes
function Pair(car, cdr) {
    this.car = car;
    this.cdr = cdr;
}

// boxed so they can be mutable (JS strings are used as Scheme symbols)
function SchemeString(val) {
    this.val = val;
}

// JS has no character type!
function SchemeChar(val) {
    this.val = val;
}
// takes a JS length-1 string and returns the appropriate SchemeChar.
var interned_chars = new Object ();
function intern_char(c) {
    if (interned_chars[c]===undefined) {
	interned_chars[c] = new SchemeChar (c);
    }
    return interned_chars[c];
};

// ports
function SchemeInputPort(readfn, peekfn, readyfn, closefn) {
    this.readfn = readfn;
    this.peekfn = peekfn;
    this.readyfn = readyfn;
    this.closefn = closefn;
};

function SchemeOutputPort(writefn, closefn) {
    this.writefn = writefn;
    this.closefn = closefn;
};

function EOF () {};
var theEOF = new EOF ();

// multiple values are boxed as a list thus.
function MultipleValues(val) {
    this.val = val;
}
	
function Nil(){}
var theNil = new Nil();

// compiler support
// turn VEC (skipping N elements) into a list.
function sinjs_restify(vec,n) {
    var l = theNil, i;
    for (i = vec.length - 1; i >= n; i -= 1) {
	l = new Pair(vec[i],l);
    }
    return l;
}

// top level variables
var top_level_binding = new Object();

// stack management
//
// current stack depth
var sinjs_stack_depth;

// *rough* maximum stack depth
var sinjs_stack_max = 30;

// call PROC as the top of a sinjs stack.  A sinjs stack is
// really just a stack, but frames never return.
// When we have made a bunch of stack frames, we eventually throw
// a SINJSrestartstack exception, which simply continues here.  If
// we get #f as a restart procedure, we return the VAL member of the
// exception.
function sinjs_start_stack(proc) {
    var e = { restart: proc };
    while (true) {
	try {
	    sinjs_stack_depth = 0;
	    proc ();
	} catch (newe) {
	    if (newe.name === "SINJSrestartstack") {
		if (newe.proc === false) {
		    return newe.val;
		}
		else {
		    e = newe;
		}
	    } else {
		throw newe;
	    };
	};
    };
};

// top level execution
// used as a continuation for storing top-level procedures in the array
// of top-level forms to execute.
function top_level_return (value){return value;};

// execute the Nth top-level procedure and proceed to the N+1th after.
function top_level_run (n) {
    (scheme_top_level_table[n])(function (){top_level_run(n+1);});
}

function scheme_top_level() {
    try {
	sinjs_start_stack (function () {top_level_run(0);});
    } catch (e) {
	if (e.name === "SINJSreturn") {
	    return e.value;
	} else {
	    throw e;
	}
    }
}

function scheme_top_level_done () {
    throw { name: "SINJSreturn", 
	    value: "sinjs-top-level-undefined" };
}

// top level REPL support
// fun is a function, do it!
function sinjs_repl_execute(fun) {
    try {
	sinjs_start_stack (fun);
    } catch (e) {
	if (e.name === "SINJSreturn") {
	    return e.value;
	} else
	    throw e;
    };
};

// continuation for forms pumped to top level repl.  write the result;
// then a newline, then throw out to the top.
//  function sinjs_repl_k(val) {
//      print ("sinjs_repl_k " + val + "\n");
//      return (top_level_binding['write'])(sinjs_now_newline, val);
//  };
//  function sinjs_now_newline(val) {
//      return (top_level_binding['newline'])(scheme_top_level_done);
//  };
function sinjs_repl_k(fun) {
    sinjs_repl_execute(function () {fun(sinjs_repl_print_answer);});
};
function sinjs_repl_print_answer(answer) {
    rhino_write(answer);
    scheme_top_level_done ();
};

//
// Builtin procedures
//

// helper functions
function check_number(a) {
    if (typeof(a) !== "number")
	throw { name: "SINJStypeerror",
		message: a + " is not a number" };
}

function check_integer(a) {
    if ((typeof(a) !== "number") || (a !== Math.floor(a)))
	throw { name: "SINJStypeerror",
		message: a + " is not an integer" };
}

function check_pair(a) {
    if (a.constructor !== Pair)
	throw { name: "SINJStypeerror",
		message: a + " is not a pair" };
}

function check_symbol(a) {
    if (typeof(a) !== "string")
	throw { name: "SINJStypeerror",
		message: a + " is not a symbol" };
}

function check_string(a) {
    if (a.constructor !== SchemeString)
	throw { name: "SINJStypeerror",
		message: a + " is not a string" };
}

function check_string_and_len (a, n) {
    check_string (a);
    if (a.length >= n)
	throw { name: "SINJSlengtherror",
		message: n + " is out of bounds for access to " + a};
}

function check_char(a) {
    if (a.constructor !== SchemeChar)
	throw { name: "SINJStypeerror",
		message: a + " is not a char" };
}

function check_vector (a) {
    if (a.constructor !== Array) 
	throw { name: "SINJStypeerror",
		message: a + " is not a vector" };
}

function check_vector_and_len (a, n) {
    check_vector (a);
    if (a.length >= n)
	throw { name: "SINJSlengtherror",
		message: n + " is out of bounds for access to " + a};
}	

function check_procedure (a) {
    if (typeof(a) !== "function")
	throw { name: "SINJStypeerror",
		message: a + " is not a procedure" };
}

function check_pair_or_null (a) {
    if ((a.constructor !== Pair) && (a !== theNil))
	throw { name: "SINJStypeerror",
		message: a + " is not a list" };
}

function check_input_port (a) {
    if (a.constructor !== SchemeInputPort)
	throw { name: "SINJStypeerror",
		message: a + " is not an input port" };
}

function check_output_port (a) {
    if (a.constructor !== SchemeOutputPort)
	throw { name: "SINJStypeerror",
		message: a + " is not an output port" };
}


// R5RS 6.1
top_level_binding['eq?'] = function (k, a, b) {
    return k(a === b);
};

// R5RS 6.2.5
top_level_binding['number?'] = function (k, a) {
    return k(typeof(a)==="number");
};
top_level_binding['='] = function (k, a, b) {
    check_number(a);
    check_number(b);
    return k(a === b);
};
top_level_binding['<'] = function (k, a, b) {
    check_number(a);
    check_number(b);
    return k(a < b);
};
top_level_binding['>'] = function (k, a, b) {
    check_number(a);
    check_number(b);
    return k(a > b);
};
top_level_binding['<='] = function (k, a, b) {
    check_number(a);
    check_number(b);
    return k(a <= b);
};
top_level_binding['>='] = function (k, a, b) {
    check_number(a);
    check_number(b);
    return k(a >= b);
};
top_level_binding['+'] = function (k) {
    var total = 0, i;
    for (i = arguments.length - 1; i >= 1; i -= 1) {
	check_number (arguments[i]);
	total = total + arguments[i];
    }
    return k(total);
};
top_level_binding['*'] = function (k) {
    var total = 1, i;
    for (i = arguments.length - 1; i >= 1; i -= i) {
	check_number (arguments[i]);
	total = total * arguments[i];
    }
    return k(total);
};
top_level_binding['-'] = function (k, first) {
    var total, i;
    check_number (first);
    if (arguments.length = 2) {
	return k(- first);
    } else {
	total = first;
	for (i = arguments.length - 1; i >= 2; i -= 1) {
	    check_number (arguments[i]);
	    total = total - arguments[i];
	}
	return k(total);
    }
};
top_level_binding['/'] = function (k, first) {
    var total, i;
    check_number (first);
    if (arguments.length = 2) {
	return (1 / first);
    } else {
	total = first;
	for (i = arguments.length - 1; i >= 2; i -= 1) {
	    check_number (arguments [i]);
	    total = total / arguments[i];
	}
	return k(total);
    }
};
top_level_binding['abs'] = function (k, z) {
    check_number (z);
    return k(Math.abs(z));
};
top_level_binding['remainder'] = function (k, n1, n2) {
    check_integer (n1);
    check_integer (n2);
    return k(n1 % n2);
};
top_level_binding['floor'] = function (k, x) {
    check_number (x);
    return k(Math.floor(x));
};
top_level_binding['ceiling'] = function (k, x) {
    check_number (x);
    return k(Math.ceil(x));
};
top_level_binding['exp'] = function (k, x) {
    check_number (x);
    return k(Math.exp(x));
};
top_level_binding['log'] = function (k, x) {
    check_number (x);
    return k(Math.log(x));
};
top_level_binding['sin'] = function (k, x) {
    check_number (x);
    return k(Math.sin(x));
};
top_level_binding['cos'] = function (k, x) {
    check_number (x);
    return k(Math.cos(x));
};
top_level_binding['tan'] = function (k, x) {
    check_number (x);
    return k(Math.tan(x));
};
top_level_binding['asin'] = function (k, x) {
    check_number (x);
    return k(Math.asin(x));
};
top_level_binding['acos'] = function (k, x) {
    check_number (x);
    return k(Math.acos(x));
};
top_level_binding['atan'] = function (k, x) {
    check_number (x);
    return k(Math.atan(x));
};
top_level_binding['sqrt'] = function (k, x) {
    check_number (x);
    return k(Math.sqrt(x));
};
top_level_binding['expt'] = function (k, x, y) {
    check_number (x);
    check_number (y);
    return k(Math.pow(x, y));
};

//missing: number->string string->number

// R5RS 6.3.2
top_level_binding['pair?'] = function (k, obj) {
    return k(obj.constructor === Pair);
};
top_level_binding['cons'] = function (k, obj1, obj2) {
    return k(new Pair(obj1, obj2));
};
top_level_binding['car'] = function (k, pair) {
    check_pair (pair);
    return k(pair.car);
};
top_level_binding['cdr'] = function (k, pair) {
    check_pair (pair);
    return k(pair.cdr);
};
top_level_binding['set-car!'] = function (k, pair, obj) {
    check_pair (pair);
    pair.car = obj;
    return k("set-car! undefined value");
};
top_level_binding['set-cdr!'] = function (k, pair, obj) {
    check_pair (pair);
    pair.cdr = obj;
    return k("set-cdr! undefined value");
};

// 6.3.3 (symbols are just JS strings)
top_level_binding['symbol?'] = function (k, obj) {
    return k(typeof(obj)==="string");
};
top_level_binding['symbol->string'] = function (k, symbol) {
    check_symbol(symbol);
    return k(new SchemeString(symbol));
};
top_level_binding['string->symbol'] = function (k, string) {
    check_string(string);
    return k(string.val);
};

// R5RS 6.3.4
top_level_binding['char?'] = function (k, obj) {
    return k(obj.constructor === SchemeChar);
};
top_level_binding['char->integer'] = function (k, c) {
    check_char(c);
    return k(c.val.charCodeAt(0));
};
top_level_binding['integer->char'] = function (k, n) {
    check_integer(n);
    return k(intern_char(String.fromCharCode(n)));
};

// R5RS 6.3.5
top_level_binding['string?'] = function (k, obj) {
    return k(obj.constructor === SchemeString);
};
top_level_binding['make-string'] = function (k, n) {
    var init, s, i;
    check_integer(n);
    if (arguments.length > 2) {
	check_char (arguments[2]);
	init = arguments[2].val.charCodeAt(0);
    } else {
	init = "!".charCodeAt(0); // should stand out nicely
    };
    s = '';
    for (i = 0; i < n; i += 1) {
	s = s + init;
    };
    return k(new SchemeString(s));
};
top_level_binding['string-length'] = function (k, string) {
    check_string (string);
    return k(string.val.length);
};
top_level_binding['string-ref'] = function (k, string, n) {
    check_string_and_len (string, n);
    check_integer (n);
    return k(intern_char(string.val.charAt(n)));
};
top_level_binding['string-set!'] = function (k, string, n, c) {
    check_string_and_len (string, n);
    check_char (c);
    string.val = string.val.slice(0,n) + c + string.val.slice(n+1);
    return k("string-set! undefined value");
};
top_level_binding['substring'] = function (k, string, start, end) {
    check_integer(start);
    check_integer(end);
    check_string_and_len (string, end - 1);
    return k(new SchemeString (string.val.substring(start,end)));
}
top_level_binding['string-append'] = function (k) {
    var s = "", i;
    for (i = 1; i < arguments.length; i += 1) {
	check_string (arguments[i]);
	s = s + arguments[i].val;
    }
    return k(new SchemeString(s));
};

// 6.3.6 Scheme vectors are just JS arrays [from class Array]
top_level_binding['vector?'] = function (k, obj) {
    return k(obj.constructor===Array);
};
top_level_binding['make-vector'] = function (k, n) {
    var fill, a, i;
    check_integer (n);
    if (arguments.length > 2) {
	fill = arguments[2];
    } else {
	fill = "make-vector undefined value";
    };
    a = [];
    for (i = 0; i < n; i += 1) {
	a[i] = fill;
    };
    return k(a);
};
top_level_binding['vector-length'] = function (k, vector) {
    check_vector (vector);
    return k(vector.length);
};
top_level_binding['vector-ref'] = function (k, vector, n) {
    check_vector_and_len (vector, n);
    return k(vector[n]);
};
top_level_binding['vector-set!'] = function (k, vector, n, obj) {
    check_vector_and_len (vector, n);
    vector[n] = obj;
    return k("vector-set! undefined value");
};

// R5RS 6.4
top_level_binding['procedure?'] = function (k, obj) {
    return k(typeof(obj) === 'function');
};
sinjs_apply = 
top_level_binding['apply'] = function (k, proc) {
    var args = [], i, p;
    check_procedure (proc)
    args[0] = k;
    for (i = 1; i < (arguments.length - 2); i += 1) {
	args[i] = arguments[i + 1];
    }
    p = arguments[arguments.length - 1];
    check_pair_or_null (p);
    while (p !== theNil) {
	args[i] = p.car;
	i += 1;
	p = p.cdr;
	check_pair_or_null (p);
    };
    return proc.apply(null, args);
};
// dynamic wind not yet implemented xxx
top_level_binding['call-with-current-continuation'] = function (k, proc) {
    check_procedure (proc);
    // can't just pass K as the arg to PROC, because it's a naked
    // continuation.  Boxed continuations will be called as Scheme
    // procedures, which take an extra k (ignored in this case).
    return proc(k, 
		(function (kont) {
		    kont = false;	// in case some GC is looking at it?
		    if (arguments.length != 2)
			return k(new MultipleValues
				 (sinjs_restify(arguments, 1)));
		    else
			return k(arguments[1]);
		}));
};

// XXX use of top level APPLY here is subject to stray set!.
top_level_binding['call-with-values'] = function (k, producer, consumer) {
    check_procedure (producer);
    check_procedure (consumer);
    return producer (function (ret) {
	    if (ret.constructor===MultipleValues) {
		return (top_level_binding['apply'])(k, consumer, ret.val);
	    } else {
		return consumer(k, ret);
	    }
	})
};

// R5RS 6.6
//
// not implemented here (must be in platform-specific code):
//   open-input-file open-output-file
//
// magical global vars, must be initialized in platform-specific code.
var sinjs_current_input_port, sinjs_current_output_port;
top_level_binding['input-port?'] = function (k, obj) {
    return k(obj.constructor===SchemeInputPort);
};
top_level_binding['output-port?'] = function (k, obj) {
    return k(obj.constructor===SchemeOutputPort);
};
top_level_binding['current-input-port'] = function (k) {
    var pt = sinjs_current_input_port;
    if (arguments.length > 1) {
	check_input_port (arguments[1]);
	sinjs_current_input_port = arguments[1];
    };
    return k(pt);
};
top_level_binding['current-output-port'] = function (k) {
    var pt = sinjs_current_output_port;
    if (arguments.length > 1) {
	check_output_port (arguments[1]);
	sinjs_current_output_port = arguments[1];
    };
    return k(pt);
};
top_level_binding['close-input-port'] = function (k, port) {
    var close;
    check_input_port (port);
    close = port.closefn;
    if (close !== false) {
	port.closefn = false;
	port.readfn = false;
	port.peekfn = false;
	port.readyfn = false;
	(port.closefn)(port);
    };
    return k("close-input-port undefined value");
};
top_level_binding['close-output-port'] = function (k, port) {
    var close;
    check_output_port (port);
    close = port.closefn;
    if (close !== false) {
	port.closefn = false;
	port.writefn = false;
	(port.closefn)(port);
    };
    return k("close-output-port undefined value");
};
top_level_binding['read-char'] = function (k) {
    var port;
    if (arguments.length > 1) {
	port = arguments[1];
    } else {
	port = sinjs_current_input_port;
    };
    check_input_port (port);
    return k(port.readfn(port));
};
top_level_binding['peek-char'] = function (k) {
    var port;
    if (arguments.length > 1) {
	port = arguments[1];
    } else {
	port = sinjs_current_input_port;
    };
    check_input_port (port);
    return k(port.peekfn(port));
};
top_level_binding['eof-object?'] = function (k, obj) {
    return k(obj === theEOF);
};
top_level_binding['char-ready?'] = function (k) {
    var port;
    if (arguments.length > 1) {
	port = arguments[1];
    } else {
	port = sinjs_current_input_port;
    };
    check_input_port (port);
    return k(port.readyfn(port));
};
top_level_binding['write-char'] = function (k, c) {
    var port;
    check_char (c);
    if (arguments.length > 1) {
	port = arguments[1];
    } else {
	port = sinjs_current_output_port;
    };
    check_output_port (port);
    port.writefn(port, c);
    return k("write-char undefined value");
};
