def main(args: List String) : IO UInt32 := do
  let [filename] := args | throw <| IO.userError "missing input file"
  let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
  let mut total := 0
  while true do
    let line <- (handle.getLine)
    if line.isEmpty then
      break

    let mut (numbers , _line_count) := line.trim.foldl (fun (tuples, i) char => (tuples.push (char.toString.toNat!, i), i+1)) (Array.empty, 0)

    numbers := numbers.insertionSort (fun (a, _) (b, _) => a > b)
    let res := numbers.foldl (init := #[]) fun acc x =>
      numbers.foldl (init := acc) fun acc y =>
        let (x_val, x_pos) := x
        let (y_val, y_pos) := y
        if x_pos < y_pos then
          acc.push s!"{x_val}{y_val}"
        else
          acc

    total := total + res[0]!.toNat!

  IO.println s!"total: {total}"

  return 0
