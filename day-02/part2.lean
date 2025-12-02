
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
      let char_count := chars.length
      -- drop diving 0 times and 1 times
      for x in ((List.range (char_count + 1) ) |>.drop 2).reverse do
        let is_divisible := char_count % x == 0
        let division_size := char_count / x
        if is_divisible then
          let mut prev := listSlice chars (0) (division_size)
          let mut is_equal_to_prev := true
          for i in [1:x] do
            let new := listSlice chars (i * division_size) (i * division_size + division_size)
            if new != prev then
              is_equal_to_prev := false
              break
            prev := new
          if is_equal_to_prev then
            total := total + n
            break

  IO.println s!"total: {total}"

  return 0
