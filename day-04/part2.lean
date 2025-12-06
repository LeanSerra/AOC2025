def get_removed_count (grid : Array (List Char)) : IO (Array (List Char) × Nat) :=  do
  let mut i := 0
  let mut total := 0
  let mut new_grid := Array.empty
  for line in grid do
    let mut j := 0
    let mut new_line := line
    for col in line do
      let positions := [
        -- top left
        if i > 0 && j > 0 then (grid[i - 1]?.map (fun line => line[j - 1]?)).join else none,
        -- top
        if i > 0 then (grid[i - 1]?.map (fun line => line[j]?)).join else none,
        -- top right
        if i > 0 && j < line.length - 1 then (grid[i - 1]?.map (fun line => line[j + 1]?)).join else none,
        -- left
        if j > 0 then (line[j - 1]?) else none,
        -- right
        if j < line.length - 1 then (line[j + 1]?) else none,
        -- bottom left
        if i < grid.size - 1 && j > 0 then (grid[i + 1]?.map (fun line => line[j - 1]?)).join else none,
        -- bottom
        if i < grid.size - 1 then (grid[i + 1]?.map (fun line => line[j]?)).join else none,
        -- bottom right
        if i < grid.size - 1 && j < line.length - 1 then (grid[i + 1]?.map (fun line => line[j + 1]?)).join else none,
      ]

      let sorrounding_rolls := positions.foldl (fun acc pos =>
        match pos with
        | some '@' => acc + 1
        | some _ => acc
        | none => acc
        ) 0

      if col == '@' && sorrounding_rolls < 4 then
        new_line := new_line.set j '.'
        total := total + 1

      j := j + 1
    new_grid := new_grid.push new_line
    i := i + 1
  return (new_grid, total)


def main(args: List String) : IO UInt32 := do
  let [filename] := args | throw <| IO.userError "missing input file"
  let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
  let mut total := 0
  let mut grid := Array.empty
  while true do
    let line <- (handle.getLine)
    if line.isEmpty then
      break
    grid := grid.push line.toList

  let mut prev := 1
  while prev != 0 do
    (grid, prev) <- get_removed_count grid
    total := total + prev

  IO.println s!"total: {total}"

  return 0
