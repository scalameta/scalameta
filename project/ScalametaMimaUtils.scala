package com.typesafe.tools.mima.core

object ScalametaMimaUtils {

  /* scopedPrivatePrefix is what mima prints, not a test: it reads "private[..] " for every member,
   * so asking whether it is empty said no member is public and dropped every problem. */
  def isPublic(obj: MemberInfo): Boolean = null != obj && !obj.nonAccessible &&
    !obj.scopedPrivate && isPublic(obj.owner, null)

  def isPublic(obj: ClassInfo, ref: AnyRef): Boolean = obj == ref || NoClass == obj ||
    null != obj && {
      obj.isPublic && !obj.isScopedPrivate && !obj.isPrivate && !obj.isProtected
    } && isPublic(obj.moduleClass, obj) && isPublic(obj.outer, obj)

}
