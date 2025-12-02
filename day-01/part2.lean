open scoped Fin.NatCast

def main (args: List String) : IO UInt32 := do
  let [filename] := args | throw <| IO.userError "missing input file"
  let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
  let mut dial_unrestricted: Int := 50
  let mut final: Int := 0

  while true do
    let line <- (handle.getLine)
    if line.isEmpty then
      break
    let it := line.iter

    match it.curr with
      | 'L'  =>
        let rotation := it.next.remainingToString.trim.toNat!
        for _ in [0:rotation] do
          if dial_unrestricted == 0 then
            final := final + 1
          dial_unrestricted := dial_unrestricted - 1
          if dial_unrestricted == -1 then
            dial_unrestricted := 99

      | 'R' =>
        let rotation := it.next.remainingToString.trim.toNat!
        for _ in [0:rotation] do
          if dial_unrestricted == 0 then
            final := final + 1
          dial_unrestricted := dial_unrestricted + 1
          if dial_unrestricted == 100 then
            dial_unrestricted := 0

      | _ => throw <| IO.userError "invalid input"

  IO.println s!"Final: {final}"
  return 0
