OPENQASM 2.0;
include "qelib1.inc";

gate adder<n> a,b,c,d {
  CX a[0],b[0];
  for i in [0 .. n - 1] {
    reset a[(2 * i - 1)];
  };
}

qreg q[10];
adder <2> q[0], q[1];

creg c[10];
if (c == 0) reset q;
// if (c[0] == 0) doesn't work! (offset)

//CX q[1], q[2];