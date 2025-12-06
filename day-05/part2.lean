def main(args: List String) : IO UInt32 := do
  let [filename] := args | throw <| IO.userError "missing input file"
  let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
  let mut total := 0
  let mut ranges := Array.empty
  while true do
    let line <- (handle.getLine)
    if line.isEmpty then
      break
    if line == "\n" then
      continue
    match line.splitOn "-" with
      | [low, up] => ranges := ranges.push (low.trim.toNat!, up.trim.toNat!)
      | [_] => break
      | _ => continue

  ranges := ranges.insertionSort fun (low_a, _) (low_b, _) => low_a < low_b
  let (low, up) := ranges[0]!
  let mut unified_ranges := #[low, up]
  for (low, up) in ranges do
    let mut is_in_any_range := false
    let mut i := 0
    while i + 1 < unified_ranges.size do
      let (range_low, range_up) := (unified_ranges[i]!, unified_ranges[i+1]!)
      -- is in range
      if low >= range_low && up <= range_up then
        is_in_any_range := true
        break
      -- contains this range
      if low <= range_low && up >= range_up then
        is_in_any_range := true
        unified_ranges := unified_ranges.set! i low
        unified_ranges := unified_ranges.set! (i + 1) up
        break
      -- extends the range in the low bound
      if low < range_low && up <= range_up then
        is_in_any_range := true
        unified_ranges := unified_ranges.set! i low
        break
      -- extends the range in the upper bound
      if low <= range_up && up >= range_up then
        is_in_any_range := true
        unified_ranges := unified_ranges.set! (i + 1) up
        break
      i := i + 2

    if !is_in_any_range then
      let first := unified_ranges[0]!
      let last := unified_ranges[unified_ranges.size - 1]!
      if up < first then
        unified_ranges := unified_ranges.insertIdx 0 up
        unified_ranges := unified_ranges.insertIdx 0 low
      if up > last then
        unified_ranges := unified_ranges.push low
        unified_ranges := unified_ranges.push up

  let mut i := 0
  while i + 1 < unified_ranges.size do
    let (range_low, range_up) := (unified_ranges[i]!, unified_ranges[i+1]!)
    total := total + range_up - range_low + 1
    i := i + 2

  IO.println s!"total: {total}"

  return 0
