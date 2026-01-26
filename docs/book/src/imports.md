# Imports

Glyph uses file-based modules. Each `.glyph` file is a module, and you import
modules by their path relative to the entry file's directory.

## Module Paths

- Use `/` as the separator in import paths.
- Omit the `.glyph` extension.
- Parent directory segments (`..`) are not allowed.
- Path segments must be valid identifiers.

If your entry file is `src/main.glyph`, then these files map to module paths:

```
src/utils.glyph          -> utils
src/math/geometry.glyph  -> math/geometry
src/math/algebra.glyph   -> math/algebra
```

## Selective Imports

Use `from <path> import <name>` to bring specific symbols into scope:

```glyph
from std import println
from math/geometry import Point, distance
from utils import helper

fn main() -> i32 {
  let p = Point { x: 3, y: 4 }
  let d = distance(p)
  println("d = ")
  ret helper() + d
}
```

You can also use the `import ... from` form with aliases:

```glyph
import Point as GeoPoint, distance as dist from math/geometry

fn main() -> i32 {
  let p = GeoPoint { x: 1, y: 2 }
  ret dist(p)
}
```

## Wildcard (Qualified) Imports

Use `import <path>` to make a module available for qualified access. This does
not bring any names into scope; you access symbols with `::`.

```glyph
import math/geometry

fn main() -> i32 {
  let p = math::geometry::Point { x: 2, y: 3 }
  ret math::geometry::distance(p)
}
```

## Import Order and Collisions

- Imports must appear before any `fn`, `struct`, `enum`, or other items.
- If two selective imports introduce the same name, use an alias to avoid
  collisions.

## Current Limitations

- There is no `pub` visibility filtering yet; all items are importable.
- Parent directory imports (`..`) are rejected.
