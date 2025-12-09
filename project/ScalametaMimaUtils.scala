package com.typesafe.tools.mima.core

object ScalametaMimaUtils {

  def isPublic(obj: MemberInfo): Boolean = null != obj && !obj.nonAccessible &&
    isPublic(obj.owner, null)

  def isPublic(obj: ClassInfo, ref: AnyRef): Boolean = obj == ref || NoClass == obj ||
    null != obj && {
      obj.isPublic && !obj.isScopedPrivate && !obj.isPrivate && !obj.isProtected
    } && isPublic(obj.moduleClass, obj) && isPublic(obj.outer, obj)

}
