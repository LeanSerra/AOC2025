def main(args: List String) : IO UInt32 := do
  let [filename] := args | throw <| IO.userError "missing input file"
  let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
  let mut total := 0

  let mut operators := Array.empty

  let mut lines := Array.empty
  let mut prev_line <- (handle.getLine)

  while true do
    let line <- (handle.getLine)
    if line.isEmpty then
      operators := prev_line.splitOn " " |>.filter (fun s => !s.trim.isEmpty) |>.toArray.reverse
      break
    lines := lines.push (prev_line.toList.reverse |>.filter (fun s => s != '\n')).asString
    prev_line := line

  let mut iterators := lines.map fun line => line.iter

  for op in operators do
    let mut values := Array.empty

    while iterators.foldl (fun ac iter => iter.curr != ' ' || ac) false do
      let iters_at_end := iterators.foldl (fun ac iter => iter.atEnd && ac) true
      if iters_at_end then
        break
      let val_array := iterators.foldl (fun ac iter => ac.push iter.curr) Array.empty
      values := values.push val_array.toList.asString.trim.toNat!
      iterators := iterators.map fun iter => iter.next

    let mut ac := values[0]!

    for i in [1:values.size] do
      match op with
          | "+" => ac:= ac + values[i]!
          | "*" => ac:= ac * values[i]!
          | _ => throw <| IO.userError "missing input file"

    total := total + ac
    iterators := iterators.map fun iter => iter.next

  IO.println s!"total: {total}"

  return 0
