module StringMap =
  Map.Make(
    {
      type t = string;
      let compare = compare;
    }
  );

type attrMap = StringMap.t(string);

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
  specifiedValues: attrMap,
  nodeType
};

let text = (text) => {children: [], specifiedValues: StringMap.empty, nodeType: Text(text)};

let comment = (text) => {children: [], specifiedValues: StringMap.empty, nodeType: Comment(text)};

let element = (tagName, attributes, children) => {
  children,
  specifiedValues: StringMap.empty,
  nodeType: Element({tagName, attributes})
};

let getAttrStr = (elementData) =>
  StringMap.fold(
    (name, value, acc) => acc ++ " " ++ name ++ "=" ++ value,
    elementData.attributes,
    ""
  );

let rec printTree = (~level=0, node) => {
  let (o, c) =
    switch node.nodeType {
    | Element(data) => (
        "<" ++ data.tagName ++ getAttrStr(data) ++ ">\n",
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