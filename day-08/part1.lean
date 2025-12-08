import Std.Data.HashSet

def allUnorderedPairs : List α → List (α × α)
| []      => []
| x :: xs => (xs.map fun y => (x, y)) ++ allUnorderedPairs xs

def main(args: List String) : IO UInt32 := do
  let [filename] := args | throw <| IO.userError "missing input file"
  let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
  let mut total := 1
  let mut boxes := Array.empty
  while true do
    let line <- (handle.getLine)
    if line.isEmpty then
      break
    let [x, y ,z] := line.splitOn "," | throw <| IO.userError "invalid input"
    let x := x.trim.toInt!
    let y := y.trim.toInt!
    let z := z.trim.toInt!
    boxes := boxes.push (x, y, z)

  let pairs := (allUnorderedPairs boxes.toList).toArray |>.qsort (
    fun ((x1, y1, z1), (x2, y2, z2)) ((x3, y3, z3), (x4, y4, z4)) =>
    let distance_a := (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2
    let distance_b :=(x3 - x4) ^ 2 + (y3 - y4) ^ 2 + (z3 - z4) ^ 2
    distance_a < distance_b
  )

  let mut connections: Array (Std.HashSet (Int × Int × Int)) := Array.empty
  let mut box_count := 0

  for (a, b) in pairs do
    -- For test data use 10
    if box_count == 1000 then break
    let mut inserted := false

    for i in [0:connections.size] do
      let set := connections[i]!
      if set.contains a then
        let mut merged := false
        for j in [0:connections.size] do
          let set_2 := connections[j]!
          if set_2.contains b then
            connections := connections.set! i (connections[i]!.insertMany set_2)
            merged := true
            break
        if !merged then
          connections := connections.set! i (connections[i]!.insert b)
        inserted := true
        break
      if set.contains b then
        let mut merged := false
        for j in [0:connections.size] do
          let set_2 := connections[j]!
          if set_2.contains a then
            connections := connections.set! i (connections[i]!.insertMany set_2)
            merged := true
            break
        if !merged then
          connections := connections.set! i (connections[i]!.insert a)
        inserted := true
        break
      if inserted then break

    if !inserted then
      let set := Std.HashSet.emptyWithCapacity 8
      connections := connections.push (set.insert a |>.insert b)

    box_count := box_count + 1

  let big_connections := connections.map (fun set => set.toArray)
    |>.map (fun a => a.size)
    |>.insertionSort fun a b => a > b

  total := total * big_connections[0]!
  total := total * big_connections[1]!
  total := total * big_connections[2]!

  IO.println s!"total: {total}"

  return 0
