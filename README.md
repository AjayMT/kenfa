
# kenfa
Kenfa ("Ken F A") is a regular expression tool+library based on [Thompson's algorithm](https://en.wikipedia.org/wiki/Thompson%27s_construction#The_algorithm) and [this excellent article](https://swtch.com/~rsc/regexp/regexp1.html) by Russ Cox.

Kenfa currently supports the following operators:
- `*`: zero or more matched strings (ex: `a*` matches `"", "a", "aa", ...`)
- `+`: one or more matched strings (ex: `a+` matches `"a", "aa", "aaa", ...`)
- `?`: zero or one matched strings (ex: `a?` matches `"", "a"`)
- `|`: alternation / union; this operator has lower precedence than concatenation (ex: `ab|cd` matches `"ab", "cd"`)
- `()`: grouping (ex: "a(b|c)d" matches `"abd", "acd"`)

Operators can be escaped with a backslash: `\*` matches a literal `*` character.

Kenfa can match regular expressions against strings and can produce DOT diagrams of the DFAs it creates when compiling regular expressions.

## Build
You will need:
- the OCaml toolchain
- the dune build system

```
dune build
dune install
```

## Usage
```
kenfa <regexp> <input>
kenfa -d <regexp> [<input>]

Options:
  -d 	Output the generated DFA as a DOT diagram.
  -help  Display this list of options
  --help  Display this list of options
```

Kenfa will match the given regular expression against the input and output `true` or `false`.
When the `-d` flag is provided, it outputs a DOT diagram:
```
$ kenfa -d "a(b|c)d+"
digraph DFA {
3 -> 3 [ label="d" ];
2 -> 3 [ label="d" ];
1 -> 2 [ label="c" ];
1 -> 2 [ label="b" ];
0 -> 1 [ label="a" ];
3 [ peripheries=2 ];
}
```

Compiling the DOT diagram with `dot -Tsvg -O` produces the following SVG:
![](https://github.com/AjayMT/kenfa/blob/master/example.svg?raw=true)