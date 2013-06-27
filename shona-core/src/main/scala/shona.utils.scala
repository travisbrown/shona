package shona

trait ReflectionUtils {
  def constructor(u: scala.reflect.api.Universe)(args: List[u.Tree]) = {
    import u._
 
    DefDef(
      Modifiers(),
      nme.CONSTRUCTOR,
      Nil,
      Nil :: Nil,
      TypeTree(),
      Block(
        Apply(
          Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR),
          args
        )
      )
    )
  }
}

