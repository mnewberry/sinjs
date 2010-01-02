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

var top_level_binding = new Object();

// top level support
// used as a continuation for storing top-level procedures in the array
function top_level_return (value){return value;};

// execute the Nth top-level procedure and proceed to the N+1th after.
function top_level_run (n) {
    (scheme_top_level_table[n])(function (ignored){top_level_run(n+1);});
}


function scheme_top_level() {
    try {
	top_level_run(0);
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
    if (obj.constructor !== Pair)
	throw { name: "SINJStypeerror",
		message: a + " is not a pair" };
}

function check_symbol(a) {
    if (typeof(obj) !== "string")
	throw { name: "SINJStypeerror",
		message: a + " is not a symbol" };
}

function check_string(a) {
    if (obj.constructor !== SchemeString)
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
    if (obj.constructor !== SchemeChar)
	throw { name: "SINJStypeerror",
		message: a + " is not a char" };
}

function check_vector (a) {
    if (obj.constructor !== Array) 
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
    if ((obj.constructor !== Pair) && (obj !== theNil))
	throw { name: "SINJStypeerror",
		message: a + " is not a list" };
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
		    kont = nil;	// in case some GC is looking at it?
		    if (arguments.length != 2)
			return k(new MultipleValues
				 (sinjs_restify(arguments, 1)));
		    else
			return arguments[1];
		}));
};

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
