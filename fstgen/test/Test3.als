/*
 * A solution to courses model built in the Static Modeling lecture.
 */

abstract sig Course {
  dept: Dept
}

sig Introductory extends Course {}

sig Advanced extends Course {
  prereqs: some Course
}

sig Elective in Course {}

abstract sig Student {
  major: lone Dept,
  taken: set Course,
  grades: taken -> one Grade
}
sig Freshman, Sophomore, Junior, Senior extends Student {}

sig Dept {
  required: set Course,
  listing: set Course
}

abstract sig Grade {}
one sig A, B, C, D, F extends Grade {}

fact defineListing {
  listing = ~dept
}

fact prereqsAcyclic {
  all c: Course | c not in allPrereqs[c]
}

// sanity check
assert introPrecedesAdvanced {
  all c: Advanced | some Introductory & allPrereqs[c]
}
check introPrecedesAdvanced for 6

pred canTake [s: Student, c: Course] {
  c.prereqs in s.taken
  c !in s.taken
}

fun allPrereqs [cs: set Course] : set Course {
  cs.^prereqs
}

pred canGraduate [s: Student] {
  s in Senior
  some s.major
  some s.taken & Elective
  all c: s.major.required | some s.grades[c] & (A + B + C)
}

pred simulate {
  all d: Dept | some d.listing & Advanced
  some s: Student | canGraduate[s]
}
run simulate for 5

fact noRedundantPrereqs {
  no prereqs & prereqs.^prereqs
  // alternatively . . .
  // all c: Advanced | no c': c.prereqs | c' in c.prereqs.^prereqs
}

pred graduateImpliesAllPrereqs {
  all s: Student | canGraduate[s] =>
    allPrereqs[s.major.required] in s.taken
}

// fails
assert graduatesCorrect {
  graduateImpliesAllPrereqs
}
check graduatesCorrect for 6

pred validTaken {
  all s: Student | allPrereqs[s.taken] in s.taken
}

// passes
assert graduatesCorrect2 {
  validTaken => graduateImpliesAllPrereqs
}
check graduatesCorrect2 for 6

/*
allowing multiple sets of required courses would look something like . . .

sig RequiredSet {
  courses: set Course
}

fact canonicalize {
  no disj s1, s2: RequiredSet | s1.courses = s2.courses
}

sig Dept {
  required: set RequiredSet,
  listing: set Course
}
*/
