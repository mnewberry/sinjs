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
/*
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
*/

function sinjs_start_stack (proc) {
    while (true) {
	proc = proc ();
    };
}

// top level execution
// execute the Nth top-level procedure and proceed to the N+1th after.
function top_level_run (n) {
    return function () {return (scheme_top_level_table[n])(function (){return top_level_run(n+1);});};
}

function scheme_top_level() {
    try {
	sinjs_start_stack (function () {return top_level_run(0);});
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
	} else {
	    print ("sinjs exception [" + e.name + "]: " + e.obj + ": " + e.message);
	    return false;
	}
    };
};

// continuation for forms pumped to top level repl.  write the result;
// then a newline, then throw out to the top.

// k for top level functions in repl: print answer, then escape
function sinjs_repl_k(answer) {
    return function () {return (top_level_binding['write'])(sinjs_repl_print_newline, answer);};
}

function sinjs_repl_print_newline(ignored) {
    return function () {return (top_level_binding['newline'])(scheme_top_level_done);};
};

// execute the top-level function for library code in repl (don't print answer)
function sinjs_repl_noprint_k(answer) {
    return function () {scheme_top_level_done(answer)};
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
    check_integer (n);
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
    check_integer (n);
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


top_level_binding['make-string'] = function (k, n) {
    var init, s, i;
    check_integer(n);
    if (arguments.length > 2) {
	check_char (arguments[2]);
	init = arguments[2].val;
    } else {
	init = "!";		// should stand out nicely
    };
    s = '';
    for (i = 0; i < n; i += 1) {
	s = s + init;
    };
    print("make string returning " + s);
    return k(new SchemeString(s));
};
top_level_binding['string'] = function (k) {
    var s, i;
    s = '';
    for (i = 1; i < arguments.length; i += 1) {
	check_char(arguments[i]);
	s = s + arguments[i].val;
    };
    return k(new SchemeString(s));
};

top_level_binding['string-append'] = function (k) {
    var s = "", i;
    for (i = 1; i < arguments.length; i += 1) {
	check_string (arguments[i]);
	s = s + arguments[i].val;
    }
    return k(new SchemeString(s));
};
top_level_binding['list->string'] = function (k, lis) {
    var s = "";
    while (lis !== theNIL) {
	check_pair(lis);
	check_char(lis.car);
	s = s + lis.car.val;
	lis = lis.cdr;
    };
    return k(new SchemeString(s));
};
top_level_binding['string-fill!'] = function (k, string, c) {
    var s, i;
    check_string (string);
    check_char (c);
    s = '';
    for (i = 0; i < string.val.length; i += 1) {
	s = s + c.val;
    };
    string.val = s;
    return k("string-fill! undefined value");
};
// 6.3.6 Scheme vectors are just JS arrays [from class Array]

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
top_level_binding['vector'] = function (k) {
    var a, i;
    a = [];
    for (i = 1; i < arguments.length; i += 1) {
	a[i-1] = arguments[i];
    };
    return k(a);
};
top_level_binding['list->vector'] = function (k, lis) {
    var a, i;
    a = [];
    i = 0;
    while (lis !== theNIL) {
	check_pair(lis);
	a[i] = lis.car;
	i += 1;
	lis = lis.cdr;
    };
    return k(a);
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
		    if (arguments.length !== 2)
			return k(new MultipleValues
				 (sinjs_restify(arguments, 1)));
		    else
			return k(arguments[1]);
		}));
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
    if (arguments.length > 2) {
	port = arguments[2];
    } else {
	port = sinjs_current_output_port;
    };
    check_output_port (port);
    port.writefn(port, c);
    return k("write-char undefined value");
};
