// Rhino runtime support

importPackage(java.io, java.lang);

// a rhino input port is a Java InputStreamReader;
// an output port is a Java PrintWriter.

function rhino_read (port) {
    var c;
    if (port.peeked !== false) {
	c = port.peeked;
	port.peeked = false;
	return c;
    } else {
	c = port.isr.read ();
	if (c === null)
	    return theEOF;
	else
	    return intern_char (c);
    }
};

function rhino_peek (port) {
    var c;
    if (port.peeked === false) {
	c = port.isr.read ();
	if (c === null)
	    port.peeked = theEOF;
	else
	    port.peeked = intern_char(c);
    }
    return port.peeked;
};

function rhino_ready (port) {
    return ((port.peeked !== false) || port.isr.ready());
};

function rhino_read_close (port) {
    port.isr.close();
    port.isr = false;
};

function rhino_write_close (port) {
    port.osr.close();
    port.osr = false;
};

function rhino_write (port, c) {
    port.osr.write(c.val);
    port.osr.flush();
};

function make_rhino_input_port (stream) {
    var pt;
    pt = new SchemeInputPort (rhino_read, rhino_peek,
			      rhino_ready, rhino_read_close);
    pt.isr = stream;
    pt.peeked = false;
    return pt;
};

function make_rhino_output_port (stream) {
    var pt;
    pt = new SchemeOutputPort (rhino_write, rhino_write_close);
    pt.osr = stream;
    return pt;
};

top_level_binding['open-input-file'] = function (k, name) {
    check_string (name);
    return k(make_rhino_input_port
	     (new InputStreamReader
	      (new FileReader (name.val))));
};

top_level_binding['open-output-file'] = function (k, name) {
    check_string (name);
    return k(make_rhino_output_port
	     (new PrintWriter
	      (new OutputStreamWriter
	       (new FileWriter (name.val)))));
};

function rhino_cheating_write (answer) {
    sinjs_current_output_port.osr.println(answer);
    sinjs_current_output_port.osr.flush();
}

function rhino_initialize () {
    sinjs_current_input_port = 
	make_rhino_input_port (new InputStreamReader (System['in']));
    sinjs_current_output_port = 
	make_rhino_output_port (new PrintWriter
				(new OutputStreamWriter (System['out'])));
};

