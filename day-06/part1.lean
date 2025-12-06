def main(args: List String) : IO UInt32 := do
  let [filename] := args | throw <| IO.userError "missing input file"
  let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
  let mut total := 0

  let mut nums := Array.empty
  let mut operators := Array.empty

  let mut line <- (handle.getLine)
  let mut possible_values := line.splitOn " " |>.filter (fun s => !s.trim.isEmpty)

  while true do
    line <- (handle.getLine)
    if line.isEmpty then
      operators := possible_values.toArray
      break
    nums := nums.push (possible_values.map fun nat => nat.trim.toNat!)
    possible_values := line.splitOn " " |>.filter (fun s => !s.trim.isEmpty)

  let mut j := 0
  for op in operators do
    let mut ac := nums[0]![j]!
    for i in [1:nums.size] do
      match op with
        | "+" => ac:= ac + nums[i]![j]!
        | "*" => ac:= ac * nums[i]![j]!
        | _ => throw <| IO.userError "missing input file"

    total := total + ac
    j := j +1

  IO.println s!"total: {total}"

  return 0
