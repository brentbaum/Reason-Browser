module StringMap =
  Map.Make(
    {
      type t = string;
      let compare = compare;
    }
  );

module StyleMap =
  Map.Make(
    {
      type t = Css.name;
      let compare = compare;
    }
  );

type attrMap = StringMap.t(string);

type styleMap = StyleMap.t(Css.value);

type tag =
  | Div
  | P
  | Html
  | Body
  | H1
  | H2
  | H3;

type elementData = {
  tagName: string,
  attributes: attrMap
};

type nodeType =
  | Text(string)
  | Element(elementData)
  | Comment(string);

type node = {
  children: list(node),
  specifiedValues: StyleMap.t(Css.value),
  nodeType
};

let text = (text) => {children: [], specifiedValues: StyleMap.empty, nodeType: Text(text)};

let comment = (text) => {children: [], specifiedValues: StyleMap.empty, nodeType: Comment(text)};

let element = (tagName, attributes, children) => {
  children,
  specifiedValues: StyleMap.empty,
  nodeType: Element({tagName, attributes})
};

let getTagName = (node) =>
  switch node.nodeType {
  | Element(elementData) => elementData.tagName
  | Text(_) => "text"
  | Comment(_) => "comment"
  };

let getAttrStr = (elementData) =>
  StringMap.fold(
    (name, value, acc) => acc ++ " " ++ name ++ "=" ++ value,
    elementData.attributes,
    ""
  );

let empty = StringMap.empty;

let getStyleString = (node) =>
  switch (StyleMap.cardinal(node.specifiedValues)) {
  | 0 => ""
  | _ =>
    let s =
      StyleMap.fold((key, value, list) => List.append(list, [key]), node.specifiedValues, [])
      |> Array.of_list
      |> Js.Array.joinWith(",");
    " style=" ++ s
  };

let rec printTree = (~level=0, node) => {
  let (o, c) =
    switch node.nodeType {
    | Element(data) => (
        "<" ++ data.tagName ++ getAttrStr(data) ++ getStyleString(node) ++ ">\n",
        "</" ++ data.tagName ++ ">"
      )
    | Text(text) => ("" ++ text, "")
    | Comment(text) => (text, "// " ++ text)
    };
  let indent = String.make(2 * level, ' ');
  let childrenStr = node.children |> List.map(printTree(~level=level + 1)) |> String.concat("");
  indent ++ o ++ childrenStr ++ indent ++ c ++ "\n"
};
/* Js.log(printTree(element("p", StringMap.empty, [text("hi")]))); */