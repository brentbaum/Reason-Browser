open Parse;

open Css;

let test = {|
  h1.test, .test, h3 { margin: auto; color: #cc0000; }
  div.note { margin-bottom: 20px; padding: 10px; }
  #answer { display: none; }
|};

/* Ripped from https://github.com/ocaml/ocaml/blob/trunk/stdlib/string.ml#L206-L21 */
let split_on_chars = (seps, s) => {
  let r = ref([]);
  let j = ref(String.length(s));
  for (i in String.length(s) - 1 downto 0) {
    if (Belt.List.some(seps, (sep) => sep == String.unsafe_get(s, i))) {
      r := [String.sub(s, i + 1, j^ - i - 1), ...r^];
      j := i
    }
  };
  [String.sub(s, 0, j^), ...r^]
};

let validateTag = (tag) =>
  switch tag {
  | "div" => Some(Div)
  | "p" => Some(P)
  | "html" => Some(Html)
  | "body" => Some(Body)
  | "h1" => Some(H1)
  | "h2" => Some(H2)
  | "h3" => Some(H3)
  | _ => None
  };

let parseSimpleSelector: string => Css.selector =
  (str) => {
    let classParts = str |> split_on_chars(['.']) |> Array.of_list;
    let classes = Belt.Array.sliceToEnd(classParts, 1);
    let selectorRest = classParts[0];
    if (Array.length(classParts) > 0 && classParts[0] == "") {
      Simple({tagName: None, id: None, classes: Array.to_list(classes)})
    } else {
      let rest = split_on_chars(['#'], selectorRest) |> Array.of_list;
      let (tagName, id: option(string)) =
        if (Array.length(rest) == 1) {
          (validateTag(rest[0]), None)
        } else if (Array.length(rest) == 2 && rest[0] == "") {
          (None, Some(rest[1]))
        } else {
          (None, None)
        };
      Simple({tagName, id, classes: Array.to_list(classes)})
    }
  };

let parseSelector = (str) =>
  str |> split_on_chars([',']) |> List.map(String.trim) |> List.map(parseSimpleSelector);

let validateValue = (str) =>
  if (String.length(str) == 0) {
    Keyword("")
  } else if (str.[0] == '#') {
    Color(str)
  } else if (String.sub(str, String.length(str) - 2, 2) == "px") {
    Length(float_of_string(String.sub(str, 0, String.length(str) - 2)), Px)
  } else if (String.sub(str, String.length(str) - 2, 2) == "px") {
    Length(float_of_string(String.sub(str, 0, String.length(str) - 2)), Px)
  } else {
    Keyword("")
  };

let parseDeclaration = (str) => {
  let parts = split_on_chars([':'], str);
  if (List.length(parts) != 2) {
    None
  } else {
    let name = List.hd(parts);
    let value = parts |> List.tl |> List.hd |> String.trim |> validateValue;
    Some({name, value})
  }
};

let parseDeclarations = (str) =>
  str
  |> split_on_chars([';'])
  |> List.map(String.trim)
  |> List.map(parseDeclaration)
  |> ((a) => Belt.List.keep(a, Belt.Option.isSome))
  |> List.map(Belt.Option.getExn);

let parseNextRule = (head) =>
  switch (seekUpTo(head, "{")) {
  | None => None
  | Some((selHead, selectorString)) =>
    let selectors: list(Css.selector) = parseSelector(selectorString);
    let (declarationHead, declarations) =
      switch (seekUpTo(incHead(selHead), "}")) {
      | None => (selHead, []) /* Replace with exception Unclosed declarations */
      | Some((dhead, declarationString)) => (dhead, parseDeclarations(declarationString))
      };
    Some(({selectors, declarations}, incHead(declarationHead)))
  };

let rec parse = (head) => {
  let ruleOption = parseNextRule(head);
  switch ruleOption {
  | None => []
  | Some((rule, nextHead)) => List.append([rule], parse(nextHead))
  }
};

let head = {body: test, pos: 0};

Js.log(parse(head));