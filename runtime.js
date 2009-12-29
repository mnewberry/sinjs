// primitive datatypes
function Pair(car, cdr) {
    this.car = car;
    this.cdr = cdr;
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



