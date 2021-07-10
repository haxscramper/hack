# Copyright (c) 2017, the Dart project authors.  Please see the AUTHORS file
# for details. All rights reserved. Use of this source code is governed by a
# BSD-style license that can be found in the LICENSE file.

# @dart=2.10

import '../package_name.dart';
import 'assignment.dart';
import 'incompatibility.dart';
import 'set_relation.dart';
import 'term.dart';

type
  PartialSolution  = object
    ## A list of [Assignment]s that represent the solver's current best guess about
    ## what's true for the eventual set of package versions that will comprise the
    ## total solution.
    ##
    ## See https:#github.com/dart-lang/pub/tree/master/doc/solver.md#partial-solution.


    assignments: seq[Assignment] ## The assignments that have been made so
    ## far, in the order they were assigned.


    decisions*: Table[String, PackageId] ## The decisions made for each package.

    positive: Table[String, Term]     ## The intersection of all positive
    ## [Assignment]s for each package, minus any negative [Assignment]s
    ## that refer to that package.
    ##
    ## This is derived from [_assignments].


    negative: Table[string, Table[PackageRef, Term]] ## The union of
    ## all negative [Assignment]s for each package.
    ##
    ## If a package has any positive [Assignment]s, it doesn't appear in this
    ## map.
    ##
    ## This is derived from [_assignments].

    attemptedSolutions*: int ## The number of distinct solutions that have
                             ## been attempted so far.




iterator unsatisfied(): PackageRange =
  # ## Returns all the decisions that have been made in this partial
  # solution. Iterable<PackageId> get decisions => _decisions.values;

  ## Returns all [PackageRange]s that have been assigned but are not yet
  ## satisfied.
  for it in self.positive:
    if it.package.name in self.decistions:
      result.add it.package

proc decisionLevel(): int =
  ## The current decision level—that is, the length of [decisions].
  self.decisions.len()



  ## Whether the solver is currently backtracking.
  var _backtracking = false;

proc decide(PackageId package) =
  ## Adds an assignment of [package] as a decision and increments the
  ## [decisionLevel].

  # When we make a new decision after backtracking, count an additional
  # attempted solution. If we backtrack multiple times in a row, though, we
  # only want to count one, since we haven't actually started attempting a
  # new solution.
  if (backtracking): inc attemptedSolutions
  self.backtracking = false;
  self.decisions[package.name] = package;
  self.assign(Assignment.decision(package, decisionLevel, _assignments.length))

## Adds an assignment of [package] as a derivation.
proc derive(PackageName package, bool isPositive, Incompatibility cause) =
  assign(Assignment.derivation(
    package, isPositive, cause, decisionLevel, _assignments.length))

proc assign(Assignment assignment) =
  ## Adds [assignment] to [_assignments] and [_positive] or [_negative].
  self.assignments.add(assignment)
  self.register(assignment)

proc backtrack(self: PartialSolution, int decisionLevel) =
  ## Resets the current decision level to [decisionLevel], and removes all
  ## assignments made after that level.

  self.backtracking = true;

  var packages: seq[string]
  while (self.assignments.last.decisionLevel > decisionLevel):
    var removed = _assignments.removeLast()
    packages.add(removed.package.name)
    if (removed.isDecision):
      self.decisions.remove(removed.package.name)

  # Re-compute [_positive] and [_negative] for the packages that were removed.
  for package in packages:
    self.positive.remove(package)
    self.negative.remove(package)

  for assignment in self.assignments:
    if (packages.contains(assignment.package.name)):
      self.register(assignment)

proc register(Assignment assignment) =
  ## Registers [assignment] in [_positive] or [_negative].
  var
    name = assignment.package.name
    oldPositive = self.positive[name]

  if (oldPositive.isNil().not()):
    self.positive[name] = oldPositive.intersect(assignment)
    return

  var
    pref = assignment.package.toRef()
    negativeByRef = _negative[name]
    oldNegative = if negativeByRef.isNil(): nil else: negativeByRef[pref]

  var term =
    if oldNegative.isNil():
      assignment
    else:
      assignment.intersect(oldNegative)

  if (term.isPositive):
    self.negative.remove(name)
    self.positive[name] = term

  else:
    self.negative.mgetOrDefault(name)[pref] = term

proc satisfier(Term term): Assignment =
  ## Returns the first [Assignment] in this solution such that the sublist of
  ## assignments up to and including that entry collectively satisfies [term].
  ##
  ## Throws a [StateError] if [term] isn't satisfied by [this].
  var assignedTerm: Term
  for assignment in _assignments:
    if (assignment.package.name != term.package.name): continue;

    if (not assignment.package.isRoot and
        not assignment.package.samePackage(term.package)) {

      # not foo from hosted has no bearing on foo from git
      if (not assignment.isPositive): continue

      # foo from hosted satisfies not foo from git
      assert(not term.isPositive);
      return assignment;

    assignedTerm =
      if assignedTerm.isNil():
        assignment

      else:
        assignedTerm.intersect(assignment)

    # As soon as we have enough assignments to satisfy [term], return them.
    if (assignedTerm.satisfies(term)): return assignment

  raise newException(StateError, "[BUG] {term} is not satisfied.")

proc satisfies(Term term): bool =
  ## Returns whether [this] satisfies [other].
  ##
  ## That is, whether [other] must be true given the assignments in this
  ## partial solution.
  self.relation(term) == SetRelation.subset;

proc relation(Term term): SetRelation =
  ## Returns the relationship between the package versions allowed by all
  ## assignments in [this] and those allowed by [term].

  var positive = _positive[term.package.name];
  if (positive != null) return positive.relation(term);

  # If there are no assignments related to [term], that means the
  # assignments allow any version of any package, which is a superset of
  # [term].
  var byRef = _negative[term.package.name];
  if (byRef == null) return SetRelation.overlapping;

  # not foo from git is a superset of foo from hosted
  # not foo from git overlaps not foo from hosted
  var negative = byRef[term.package.toRef()]
  if (negative.isNil()): return SetRelation.overlapping

  return negative.relation(term)
