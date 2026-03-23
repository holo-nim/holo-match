import ./impl, std/[options, macros]

type
  AssignOptionError* = object of AssignError
    ## error for failed contains checks in assignments

template assignCheckOption*(a): untyped =
  ## template for equality checks in assignments
  if not a.isSome:
    assignCheckFail: # this will generate `break` for `tap`
      raise newException(AssignOptionError, "option " & astToStr(a) & " was not Some")

macro assign*[T](lhs; rhs: Option[T], kind: static AssignKind = akLet): untyped =
  ## The library's builtin overload of `assign` for `Option[T]`.
  if lhs.kind in {nnkCall, nnkCommand} and lhs.len == 2 and (lhs[0].eqIdent"some" or lhs[0].eqIdent"Some"):
    let tmp = genSym(nskLet, "tmpOption")
    result = newStmtList(
      newLetStmt(tmp, rhs),
      newCall(bindSym"assignCheckOption", tmp),
      openAssign(lhs[1], newCall(bindSym"unsafeGet", tmp), kind))
  else:
    result = defaultAssign(lhs, rhs, kind)
