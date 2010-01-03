importPackage(java.io, java.lang);
var inpt, outpt, line, more;
inp = new BufferedReader (new InputStreamReader (System['in']));
//outp = new PrintWriter (new OutputStreamWriter (System['out']));
more = true;
while (more) {
    line = inp.readLine ();
    if (line === null)
        more = false;
    else {
        val = eval(String(line));
    }
};
