## Module Text.Parsing.Parser.Pos

#### `Position`

``` purescript
data Position
  = Position { line :: Int, column :: Int }
```

`Position` represents the position of the parser in the input.
- `line` is the current line in the input
- `column` is the column of the next character in the current line that will be parsed

##### Instances
``` purescript
instance showPosition :: Show Position
instance eqPosition :: Eq Position
```

#### `initialPos`

``` purescript
initialPos :: Position
```

The `Position` before any input has been parsed.

#### `updatePosString`

``` purescript
updatePosString :: Position -> String -> Position
```

Updates a `Position` by adding the columns and lines in `String`.


