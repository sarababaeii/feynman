OPENQASM 2.0;
include "qelib1.inc";

gate adder<n> a,b,c,d {
  CX a[0],b[0];
}

qreg q[10];
adder <2> q[0], q[1];

for i in [0 .. 9] {
  CX q[1], q[2];
}