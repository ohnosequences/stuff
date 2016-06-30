// package ohnosequences.stuff
//
// trait AnyTypeConstructor {
//
//   type Domain
//   type Codomain
//
//   /* this name is arguable */
//   type of[X <: Domain] <: Codomain
// }
//
// case object AnyTypeConstructor {
//
//   type is[T <: AnyTypeConstructor] = T with AnyTypeConstructor {
//
//     type Domain = T#Domain;
//     type Codomain = T#Codomain;
//     // type of[X <: T#Domain] = T#of[X];
//   }
// }
//
// trait AnyIdTC extends AnyTypeConstructor {
//
//   type Bound
//
//   type Domain   = Bound
//   type Codomain = Bound
//
//   type of[X <: Bound] = X
// }
//
// class IdTypeConstructor[On0] extends AnyIdTC { type Bound = On0 }
//
// trait AnyTypeConstructorComposition
//   extends
//     AnyTypeConstructor
// {
//
//   type First <: AnyTypeConstructor
//   val first: First
//   type Second <: AnyTypeConstructor { type Domain = First#Codomain }
//   val second: Second
//
//   type Domain = First#Domain
//   type Codomain = Second#Codomain
//
//   type of[V <: Domain] = Second#of[First#of[V]]
// }
//
// class TypeConstructorComposition[F0 <: AnyTypeConstructor, S0 <: AnyTypeConstructor { type Domain = F0#Codomain}](val first: F0, val second: S0)
//   extends
//     AnyTypeConstructorComposition
// {
//   type First = F0
//   type Second = S0
// }
