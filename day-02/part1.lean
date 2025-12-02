def listSlice (l : List α) (start_index end_index : Nat) : List α :=
  (l.drop start_index).take (end_index - start_index)

def main(args: List String) : IO UInt32 := do
  let [filename] := args | throw <| IO.userError "missing input file"
  let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
  let input <- handle.getLine
  let input := input.splitOn ","
  let mut total := 0
  for pair in input do
    let [up, low] := pair.splitOn "-" | throw <| IO.userError "invalid input"
    let up := up.trim.toNat!
    let low := low.trim.toNat!
    for n in [up:low + 1] do
      let n_str := s!"{n}"
      let chars := n_str.toList
      let mid_point := chars.length / 2
      let x := listSlice chars 0 mid_point
      let y := listSlice chars mid_point chars.length
      if x == y then
        total := total + n

  IO.println s!"total: {total}"

  return 0
