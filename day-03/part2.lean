def listSlice (l : Array α) (start_index end_index : Nat) : Array α :=
  (l.drop start_index).take (end_index - start_index)

def get_values_from_positions (positions: Array Nat) (values: Array Nat) : Nat :=
  let (val, _) := values.foldl (fun (values, i) val =>
        if positions.contains i then
          (s!"{values}{val}", i + 1)
          else
          (values, i + 1)
      ) ("", 0)
  val.toNat!

def main(args: List String) : IO UInt32 := do
  let [filename] := args | throw <| IO.userError "missing input file"
  let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
  let mut total := 0
  let battery_count := 12
  while true do
    let line <- (handle.getLine)
    if line.isEmpty then
      break

    let (values, _) := line.trim.foldl (fun (values, idx) char => (values.push ((char.toString.toNat!, idx)), idx +1)) (Array.empty, 0)

    let mut positions := Array.empty

    let line_length := values.size

    let mut values_left := line_length
    let mut best_possible_pos := 0

    for i in [0:battery_count] do
      let possible_moves := (values_left - battery_count) + 1
      let possible_positions := listSlice values (best_possible_pos) (best_possible_pos + possible_moves) |>.insertionSort (fun (a, _) (b, _) => a > b)

      best_possible_pos := possible_positions[0]!.2
      positions := positions.push best_possible_pos

      best_possible_pos := best_possible_pos + 1
      values_left := line_length - best_possible_pos + i + 1

    total := total + get_values_from_positions positions (values.map fun (x, _) => x)

  IO.println s!"total: {total}"

  return 0
