package std.data

abstract class Iso[A, B] { ab =>
  def to(a: A): B
  def from(b: B): A

  // to . from = id
  // from . to = id

  def andThen[C](bc: Iso[B, C]): Iso[A, C] = new Iso[A, C] {
    def to(a: A): C = bc.to(ab.to(a))
    def from(c: C): A = ab.from(bc.from(c))
  }

  def >>>[C](bc: Iso[B, C]): Iso[A, C] = ab andThen bc

  def compose[Z](za: Iso[Z, A]): Iso[Z, B] = za.andThen(ab)

  def flip: Iso[B, A] = new Iso[B, A] {
    def to(b: B): A = ab.from(b)
    def from(a: A): B = ab.to(a)
    override val flip: Iso[A, B] = ab
  }

  def and[I, J](ij: Iso[I, J]): Iso[(A, I), (B, J)] =
    Iso.unsafe(
      { case (a, i) => (ab.to(a), ij.to(i)) },
      { case (b, j) => (ab.from(b), ij.from(j)) })

  def or[I, J](ij: Iso[I, J]): Iso[Either[A, I], Either[B, J]] =
    Iso.unsafe({
      case Left(a) => Left(ab.to(a))
      case Right(i) => Right(ij.to(i))
    }, {
      case Left(b) => Left(ab.from(b))
      case Right(j) => Right(ij.from(j))
    })
}
object Iso {
  def apply[A, B](implicit ab: Iso[A, B]): Iso[A, B] = ab

  def unsafe[A, B](ab: A => B, ba: B => A): Iso[A, B] = new Iso[A, B] {
    def to(a: A): B = ab(a)
    def from(b: B): A = ba(b)
  }

  final case class Refl[A]() extends Iso[A, A] {
    def to(a: A): A = a
    def from(b: A): A = b
  }

  implicit def id[A]: Iso[A, A] = Refl[A]()

  implicit def flip[A, B](implicit ab: Iso[A, B]): Iso[B, A] = ab.flip

  object Product {
    type ⨂ [A, B] = (A, B)
    type Id = Unit

    final def associate[A, B, C]: Iso[A ⨂ (B ⨂ C), (A ⨂ B) ⨂ C] = unsafe(
      { case (a, (b, c)) => ((a, b), c) },
      { case ((a, b), c) => (a, (b, c)) })

    final def commute[A, B]: Iso[A ⨂ B, B ⨂ A] = unsafe(
      { case (a, b) => (b, a) },
      { case (b, a) => (a, b) })

    final def unitR[A]: Iso[A, A ⨂ Id] = unsafe(
      { case a => (a, ()) },
      { case (a, ()) => a })

    final def unitL[A]: Iso[A, Id ⨂ A] = unsafe(
      { case a => ((), a) },
      { case ((), a) => a })

    final def first[A, B, C](iso: Iso[A, C]): Iso[A ⨂ B, C ⨂ B] =
      iso and id

    final def second[A, B, C](iso: Iso[B, C]): Iso[A ⨂ B, A ⨂ C] =
      id and iso
  }

  object Coproduct {
    type ⨂ [A, B] = Either[A, B]
    type Id = Nothing

    final def associate[A, B, C]: Iso[A ⨂ (B ⨂ C), (A ⨂ B) ⨂ C] =
      unsafe({
        case Left(a) => Left(Left(a))
        case Right(Left(b)) => Left(Right(b))
        case Right(Right(c)) => Right(c)
      }, {
        case Left(Left(a)) => Left(a)
        case Left(Right(b)) => Right(Left(b))
        case Right(c) => Right(Right(c))
      })

    final def commute[A, B]: Iso[A ⨂ B, B ⨂ A] =
      unsafe({
        case Left(a) => Right(a)
        case Right(b) => Left(b)
      },
        {
          case Right(a) => Left(a)
          case Left(b) => Right(b)
        })

    final def unit[A]: Iso[A, A ⨂ Id] = unsafe(
      { case a => Left(a) },
      {
        case Left(a) => a
        case Right(n) => n
      })

    final def first[A, B, C](iso: Iso[A, C]): Iso[A ⨂ B, C ⨂ B] =
      iso or id

    final def second[A, B, C](iso: Iso[B, C]): Iso[A ⨂ B, A ⨂ C] =
      id or iso
  }
}