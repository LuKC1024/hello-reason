type elm =
  | Line(string)
  | Quoteblock(doc)
  | Codeblock(string)
and doc = list(elm);

type block = list(string);

let rec subblock_of = (term: string, lines: list(string)) => {
  switch (lines) {
  | [] => ([], [])
  | [line, ...lines] when line == term => ([], lines)
  | [line, ...lines] =>
    let (subblock, lines) = subblock_of(term, lines);
    ([line, ...subblock], lines);
  };
};

let rec doc_of_block = (lines: block) => {
  switch (lines) {
  | [] => []
  | [">>>", ...lines] =>
    let (subblock, lines) = subblock_of("<<<", lines);
    [Quoteblock(doc_of_block(subblock)), ...doc_of_block(lines)];
  | ["```", ...lines] =>
    let (subblock, lines) = subblock_of("```", lines);
    [Codeblock(String.concat("\n", subblock)), ...doc_of_block(lines)];
  | [line, ...lines] => [Line(line), ...doc_of_block(lines)]
  };
};

let doc_of_string = (str: string): doc => {
  let lines = String.split(Str.regexp("\n"), str);
  doc_of_block(lines);
};

let rec string_of_elm = (elm: elm): string => {
  switch elm {
  | Line(line) => "<p>" ++ line ++ "</p>"
  | Quoteblock(doc) => "<blockquote>" ++ string_of_doc(doc) ++ "</blockquote>"
  | Codeblock(src) => "<pre>" ++ src ++ "</pre>"
  };
}
and string_of_doc = (elms: doc): string => {
  String.concat("\n", List.map(string_of_elm, elms))
}

let rec read_all = () => {
  read_line() ++ "\n" ++ read_all()
}

print_endline("Running Test Program:");
// let () = print_endline(Lib.Util.hello());
let () = print_endline(read_all())