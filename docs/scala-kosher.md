# Kosher Scala

A summary of rules for the variant of Scala I'm using here.

## No variance

Ever. Of course, that's impossible to enforce at the *use* site, because

``` scala
class B
class C extends B
val x: B → C
val c: C
// will always work
val argh = x(c)
```

The dangers posed by this inherent bug are minimized if you adhere to the next rule:

## (Almost) No inheritance

Valid exceptions:

1. singleton objects
2. ADTs, do not expose it though
3. interfacing with external code requiring it (sad)
4. types containing polymorphic functions

In the particular case of 4. you should use an abstract class, and every implementing subclass should be concrete.

## No traits

Ever. There is no point in traits if you use composition instead of inheritance (which you should). They have downsides either way

1. worse generated bytecode (improving I admit, but in any case worse (≤) as a category theorist would understand it)
2. users *can* start using inheritance easier than with classes (which is *not* good, remember we want to *avoid* inheritance)
3. no way of "enforcing" immutability (abstract vals are a nightmare)
4. ...

## (Almost) always functions, (almost) never methods

The only use of methods is as polymorphic functions:

``` scala
// inside natural transformation
def η[X <: Objects]: TargetCat#C[F[X],G[X]]
```

There's no way of defining something equivalent with functions. Note that sadly this also forces you to use inheritance for providing instances of polymorphic functions: there's nothing like a polymorphic value.

This rule also applies backwards, of course: if you use a method, it **should** both

1. have type parameters
2. have *no* arguments

because otherwise you can use a function instead.

## No type parameters in classes

Use type members. The only legit context for type parameters is a method.

## Abstract values of type `X.is[Z]`

Ideally, I'd like Scala to require that for a value to conform to a type `X`, all their type members should be concrete (assigned to something). What we're doing is:

- For every type `X` with abstract type members, define an alias `X.is[X0 <: X]` in the companion object which forces all type members to correspond to their own projection
- whenever you want a *value* of type `X0 <: X`, require it to be of type `X.is[X0]`; note that `X.is[X0] <: X0`.

Writing this `X.is[X0 <: X]` type is boring; I'd like to generate it automatically (compiler plugin, whatever).

## No value-dependent types

Scala doesn't have dependent types, it only has a (seriously buggy) notion of type-dependent types (type members and projections).

Use projections, there's no use case for value-dependent types. The classical example of "guarantee that this comes from this value lalala" is seriously broken, as Scala does not (and will not) perform any sort of source analysis for values (trying to find if two values are provably equal), and, even if it will, for this to be usable you'd need to have a mechanism for writing proofs yourself, which looks even less likely.

## Implicits only for syntax

The only reasonable use of implicits is for providing syntax, more concretely creating infix operators so that one can write `(f ⊗ g) >=> μ`.

## Do no use Scala std library

and I mean for real. No tuples, functions, etc. From the point of view of Kosher Scala, they are broken without any possible remedy. Of course, we have substitutes for them here. This includes Scala collections: wrap better tested, scalable Java implementations (fastutil, Koloboke).

## Do not use equals

*Ever*. Types have no equality by default. If you want equality, build a category where objects are types *together with* an equality predicate. Eventually this should be available for a category with enough structure (a subobject classifier or something similar); right now this would be an ad-hoc extension to full subcategories of `Scala`.

### Impact on collections

Most "collections" like Maps, sets or bags *require* a notion of equality. They should only be available for categories supporting it.

<!--
  NOTE the way I see it now

  - free monoids will be wrapped arrays for Scala, something else in other cases (generic stacks if possible)
  - bags will be wrapped fastutil hashmaps with custom equality
  - sets will be wrapped fastutil hashsets with custom equality
  - maps will be *morphisms* in a category.
-->

### No case classes

An obvious consequence of the above.

## "But this goes against XYZ and ..."

I'm aware that the current Scala design and what's happening on Dotty goes against basically everything detailed here. I simply don't care.