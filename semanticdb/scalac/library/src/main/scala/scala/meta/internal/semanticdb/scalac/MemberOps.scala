package scala.meta.internal.semanticdb.scalac

import scala.{meta => m}

trait MemberOps { self: DatabaseOps =>
  private[this] lazy val ignoreName: Set[g.Name] = Set(
    g.nme.CONSTRUCTOR,
    g.nme.MIXIN_CONSTRUCTOR,
    g.nme.asInstanceOf_,
    g.nme.asInstanceOf_Ob,
    g.nme.isInstanceOf_,
    g.nme.isInstanceOf_Ob,
    g.nme.hashCode_,
    g.nme.HASHHASH,
    g.nme.ne,
    g.nme.eq,
    g.nme.finalize_,
    g.nme.wait_,
    g.nme.EQ,
    g.nme.NE,
    g.nme.synchronized_,
    g.nme.notify_,
    g.nme.notifyAll_,
    g.nme.clone_,
    g.nme.equals_,
    g.nme.toString_,
    g.nme.getClass_
  )

  implicit class XtensionGTypeMSignatures(tpe: g.Type) {
    def lookupMembers: List[m.Signature] = {
      val buffer = List.newBuilder[m.Signature]
      tpe.members.iterator.filterNot(s => ignoreName(s.name)).foreach { s =>
        buffer += (
          if (s.name.isTermName) m.Signature.Term(s.decodedName)
          else m.Signature.Type(s.decodedName)
        )
      }
      buffer.result()
    }
  }
}
