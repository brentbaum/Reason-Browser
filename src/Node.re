module StringMap =
  Map.Make(
    {
      type t = string;
      let compare = compare;
    }
  );

type attrMap = StringMap.t(string);

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
  nodeType
};

let text = (text) => {children: [], nodeType: Text(text)};

let comment = (text) => {children: [], nodeType: Comment(text)};

let element = (tagName, attributes, children) => {
  children,
  nodeType: Element({tagName, attributes})
};

let rec printTree = (~level=0, node) => {
  let str =
    switch node.nodeType {
    | Element(data) => "<" ++ data.tagName ++ ">"
    | Text(text) => text
    | Comment(text) => "// " ++ text
    };
  let levelStr = String.make(2 * level, ' ') ++ str ++ "\n";
  let childrenStr = node.children |> List.map(printTree(~level=level + 1)) |> String.concat("");
  levelStr ++ childrenStr
};
/* Js.log(printTree(element("p", StringMap.empty, [text("hi")]))); */