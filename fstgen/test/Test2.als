/*
 * A complete model of leader election in a ring.
 *
 * Copied from p.171 of
 * Software Abstractions, by Daniel Jackson
 */

open util/ordering[Time] as to
open util/ordering[Process] as po

sig Time {}

sig Process {
  succ: Process,
  toSend: Process -> Time,
  elected: set Time
}

fact ring {
  all p: Process | Process in p.^succ
}

fact defineElected {
  no elected.(to/first)
  all t: Time - to/first |
    elected.t = { p:Process | p in (p.toSend.t - p.toSend.(t.prev)) }
}

fact traces {
  init [to/first]
  all t: Time - to/last | let t' = t.next |
    all p: Process |
      step[t, t', p] || step[t, t', succ.p] || skip[t, t', p]
}

pred init [t: Time] {
  all p: Process | p.toSend.t = p
}

pred skip [t, t': Time, p: Process] {
  p.toSend.t = p.toSend.t'
}

pred step [t, t': Time, p: Process] {
  let from = p.toSend, to = p.succ.toSend |
    some id: from.t {
      from.t' = from.t - id
      to.t' = to.t + (id - p.succ.prevs)
    }
}

pred simulate() {}
run simulate for 3 Process, 7 Time

assert atMostOneElected {
  lone elected.Time
}
check atMostOneElected for 3 Process, 7 Time

pred progress {
  all t: Time - to/last | let t' = t.next |
    some Process.toSend.t =>
      some p: Process | not skip[t, t', p]
}

assert atLeastOneElected {
  progress => some elected.Time
}
check atLeastOneElected for 3 Process, 7 Time

pred looplessPath {
  no disj t, t': Time | toSend.t = toSend.t'
}
run looplessPath for 3 Process, 13 Time

// no counterexamples for 13 time instances
// so trace length of 12 is sufficient


