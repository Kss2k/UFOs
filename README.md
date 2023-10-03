# UFOs
UseFull Operators in R

This package aims to add some usefull operators for selecting and subsetting arrays and lists in R.

Selecting variables by prefixes: 
```
iris %pre% Sepal
iris %!pre% Sepal
```

Selecting variables by names containing pattern:
```
iris %match% Len
iris %!match% Len
```

Selecting variables by suffix (names ending with pattern)
```
iris %suf% Length
iris %!suf% Length
```

Selecting variables by regulare expressions (regex)
```
iris %rgx% "(Sepal|Petal).(Length)"
iris %!rgx% "^Petal"
```

Splitting datasets by variablenames
```
iris %by% Spe
iris %#by% Spe %by% Petal.Len
```
Splitting datasets by variablename-prefixes
```
iris %pby% Spe
iris %pby% Spe %pby% Petal.Len
```
