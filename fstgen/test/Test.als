/**
 * An incomplete model of an address book.
 */

abstract sig Target {}
sig Name extends Target {}
sig Addr extends Target {}

sig Book { addr: Name -> Target }

pred init [b: Book] { no b.addr }

pred inv [b: Book] {
  let addr = b.addr | all n: Name {
    n not in n.^addr
    some addr.n => some n.addr
  }
}

fun lookup [b: Book, n: Name] : set Addr {
  n.^(b.addr) & Addr
}

assert namesResolve {
  all b: Book | inv[b] =>
    all n: Name | some b.addr[n] => some lookup[b, n]
}
check namesResolve for 4

pred add [b, b': Book, n: Name, t: Target] {
  b'.addr = b.addr + n->t
}
run add

