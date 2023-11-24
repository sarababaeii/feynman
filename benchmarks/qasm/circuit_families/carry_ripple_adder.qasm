OPENQASM 2.0;
include "qelib1.inc";

gate maj () a, b, c {
    cx c, b;
    cx c, a;
    ccx a, b, c;
}

gate uma () a, b, c {
    ccx a, b, c;
    cx c, a;
    cx a, b;
}

gate adder <n> a, b {
    for i in [0 .. 9] {
        reset a;
    };
}

// for loop cannot be a statement cause it needs to be used in block of a circuit family decleration
// dereferences shoul be able to be with an int expression