// Rhino runtime support

importPackage(java.io, java.lang);

// a rhino input port is a Java InputStreamReader;
// an output port is a Java OutputStreamReader.
// we add members "isr" and "peeked".

function rhino_read (port) {
    var c;
    if (port.peeked !== false) {
	c = port.peeked;
	port.peeked = false;
	return c;
    } else
	return (intern_char(port.isr.read ()));
};

function rhino_peek (port) {
    if (port.peeked === false)
	port.peeked = intern_char(port.isr.read());
    return port.peeked;
};

function rhino_ready (port) {
    return ((port.peeked !== false) || port.isr.ready());
};

function rhino_close (port) {
    port.isr.close();
    port.isr = false;
};

function rhino_write (port, c) {
    port.isr.write(c.val);
};

function make_rhino_input_port (stream) {
    var pt;
    pt = new SchemeInputPort (rhino_read, rhino_peek, rhino_ready, rhino_close);
    pt.isr = stream;
    pt.peeked = false;
    return pt;
};

function make_rhino_output_port (stream) {
    var pt;
    pt = new SchemeOutputPort (rhino_write, rhino_close);
    pt.isr = stream;
    return pt;
};

top_level_binding['open-input-file'] = function (k, name) {
    check_string (name);
    return k(make_rhino_input_port
	     (new InputStreamReader
	      (new FileInputStream
	       (new File (name.val)))));
};

top_level_binding['open-output-file'] = function (k, name) {
    check_string (name);
    return k(make_rhino_output_port
	     (new OutputStreamWriter
	      (new FileOutputStream
	       (new File (name.val)))));
};

// but for output, use the Rhino print function.
function rhino_stdout_write(port, c) {
    print (c.val);
};

function rhino_stdout_close(port) {
};

function rhino_initialize () {
    sinjs_current_input_port = 
	make_rhino_input_port (new InputStreamReader (System['in']));
    sinjs_current_output_port =
	new SchemeOutputPort (rhino_stdout_write, rhino_stdout_close);
};

