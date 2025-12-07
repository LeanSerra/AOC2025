import Std.Data.HashMap

partial def count_splits (cache_ref: IO.Ref (Std.HashMap (Nat × Nat) (Nat)))(level: Nat) (pos: Nat)  (grid: Array (Array Char)) : IO (Nat) := do
  let cache ← cache_ref.get
  match cache.get? (level, pos) with
   | some result => return result
   | none => pure ()

  let mut level_mut := level

  while level_mut < grid.size do
    if grid[level_mut]![pos]! == '^' then
      let left <- count_splits cache_ref level_mut (pos - 1) grid
      let right <- count_splits cache_ref level_mut (pos + 1) grid
      cache_ref.modify fun cache => cache.insert (level, pos) (left + right)
      return left + right
    level_mut := level_mut + 1

  cache_ref.modify fun cache => cache.insert (level_mut, pos) 1
  return 1

def main(args: List String) : IO UInt32 := do
  let [filename] := args | throw <| IO.userError "missing input file"
  let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read

  let mut grid := Array.empty
  while true do
    let line <- (handle.getLine)
    if line.isEmpty then
      break
    grid := grid.push line.toList.toArray

  let start := (grid[0]!.findIdx? fun s => s == 'S').get!
  let cache ← IO.mkRef ({} : Std.HashMap (Nat × Nat) (Nat))
  let total <- count_splits cache 1 start grid

  IO.println s!"total: {total}"

  return 0
