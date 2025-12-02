open scoped Fin.NatCast

def main (args: List String) : IO UInt32 := do
  -- let mut a: Fin 100 := 50
  let [filename] := args | throw <| IO.userError "missing input file"
  let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
  -- let a := (-8 : Fin 10)
  let mut dial: Fin 100 := 50
  let mut final := 0
  while true do
    let line <- (handle.getLine)
    if line.isEmpty then
      break
    let it := line.iter

    match it.curr with
      | 'L'  =>
        let rotation := it.next.remainingToString.trim.toNat!
        dial := dial - rotation

      | 'R' =>
        let rotation := it.next.remainingToString.trim.toNat!
        dial := dial + rotation
      | _ => throw <| IO.userError "invalid input"
    if dial == 0 then
      final := final + 1


  IO.println s!"Final: {final}"
  return 0
