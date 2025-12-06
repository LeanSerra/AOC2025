def main(args: List String) : IO UInt32 := do
  let [filename] := args | throw <| IO.userError "missing input file"
  let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
  let mut total := 0
  let mut ranges := Array.empty
  let mut values := Array.empty
  while true do
    let line <- (handle.getLine)
    if line.isEmpty then
      break
    if line == "\n" then
      continue
    match line.splitOn "-" with
      | [low, up] => ranges := ranges.push (low.trim.toNat!, up.trim.toNat!)
      | [num] => values := values.push num.trim.toNat!
      | _ => continue

  for value in values do
    for (low, up) in ranges do
      if value >= low && value <= up then
        total := total + 1
        break

  IO.println s!"total: {total}"

  return 0
