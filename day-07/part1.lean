import Std.Data.HashMap

partial def count_splits (cache_ref: IO.Ref (Std.HashMap (Nat × Nat) (Array (Array Char))))(level: Nat) (pos: Nat) (grid: Array (Array Char)) : IO (Array (Array Char)) := do
  let cache ← cache_ref.get
  match cache.get? (level, pos) with
   | some result => return result
   | none => pure ()

  let mut new_grid := grid
  for i in [level:grid.size] do
    match grid[i]![pos]! with
      | '^' =>
        new_grid := new_grid.set! i (new_grid[i]!.set! (pos - 1) '|')
        new_grid := new_grid.set! i (new_grid[i]!.set! (pos + 1) '|')
        let left <- count_splits cache_ref i (pos - 1) new_grid
        new_grid := new_grid.zip left
          |> .map fun (new, l) =>
            new.zip l
              |> .map fun (n_val, l_val) =>
                match n_val, l_val with
                  | '|', '|' => '|'
                  | '.', '|' => '|'
                  | '|', '.' => '|'
                  | '^', '^' => '^'
                  | 'S', 'S' => 'S'
                  | '.', '.' => '.'
                  | _, _ => '.'
        let right <- count_splits cache_ref i (pos + 1) new_grid
        new_grid := new_grid.zip right
        |> .map fun (new, r) =>
          new.zip r
            |> .map fun (n_val, l_val) =>
              match n_val, l_val with
                | '|', '|' => '|'
                | '.', '|' => '|'
                | '|', '.' => '|'
                | '^', '^' => '^'
                | 'S', 'S' => 'S'
                | '.', '.' => '.'
                | _, _ => '.'
        break
      | _ => new_grid := new_grid.set! i (new_grid[i]!.set! pos '|')
  cache_ref.modify fun cache => cache.insert (level, pos) new_grid
  return new_grid

def main(args: List String) : IO UInt32 := do
  let [filename] := args | throw <| IO.userError "missing input file"
  let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
  let mut total := 0
  let mut grid := Array.empty
  while true do
    let line <- (handle.getLine)
    if line.isEmpty then
      break
    grid := grid.push line.toList.toArray

  let start := (grid[0]!.findIdx? fun s => s == 'S').get!
  let cache ← IO.mkRef ({} : Std.HashMap (Nat × Nat) (Array (Array Char)))
  let grid_expanded <- count_splits cache 1 start grid

  for i in [1:grid_expanded.size] do
    let mut j := 0
    for char in grid_expanded[i]! do
      if char == '^' then
        if grid_expanded[i - 1]![j]! == '|' then
          total := total + 1
      j := j + 1

  IO.println s!"total: {total}"

  return 0
