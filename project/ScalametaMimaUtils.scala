package com.typesafe.tools.mima.core

object ScalametaMimaUtils {

  def isPublic(obj: MemberInfo): Boolean = null != obj && !obj.nonAccessible &&
    isPublic(obj.owner, null)

  def isPublic(obj: ClassInfo, ref: AnyRef): Boolean = (obj == ref) ||
    null != obj && obj.isPublic && isPublic(obj.module, obj) && isPublic(obj.outer, obj)

}
