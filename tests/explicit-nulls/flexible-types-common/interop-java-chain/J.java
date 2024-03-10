class J1 {
  J2 getJ2() { return new J2(); }
}

class J2 {
  J1 getJ1() { return new J1(); }
}