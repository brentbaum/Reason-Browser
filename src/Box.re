open Node;

type rect = {
  x: float,
  y: float,
  width: float,
  height: float
};

type edges = {
  left: float,
  right: float,
  bottom: float,
  top: float
};

type dimensions = {
  content: rect,
  padding: edges,
  border: edges,
  margin: edges
};

let defaultDimensions = {
  content: {x: 0., y: 0., width: 0., height: 0.},
  padding: {left: 0., right: 0., bottom: 0., top: 0.},
  border: {left: 0., right: 0., bottom: 0., top: 0.},
  margin: {left: 0., right: 0., bottom: 0., top: 0.}
};

type boxType =
  | BlockNode(Node.node)
  | InlineNode(Node.node)
  | AnonymousBlock
  | None;

type layoutBox = {
  dimensions,
  boxType,
  children: list(layoutBox)
};

let blockTags = ["div"];

let getBoxType = (node) => {
  let display = "display";
  let displayType =
    if (StyleMap.mem(display, node.specifiedValues)) {
      StyleMap.find(display, node.specifiedValues)
    } else {
      switch node.nodeType {
      | Text(_) => Keyword("inline")
      | _ => Keyword("block")
      }
    };
  if (displayType == Keyword("none")) {
    None
  } else if (displayType == Keyword("block")
             || Belt.List.some(blockTags, (tag) => Node.getTagName(node) == tag)) {
    BlockNode(node)
  } else {
    InlineNode(node)
  }
};

/*
 Filter out display: none.
   fold_left with {container: [], children: []}. container collects inline anonymous.
   0. If next is inline, add to container.
   1. If next is block, then add Anonymous(container) to children (if not children empty). Then add block directly to children.
   2. After gone through all, add container to children if not empty.
   */
type groupChildrenCollector = {
  container: list(layoutBox),
  kids: list(layoutBox)
};

let make_anonymous = (children) => {
  boxType: AnonymousBlock,
  dimensions: defaultDimensions,
  children
};

let addAnonymous = (collected) =>
  List.length(collected.container) == 0 ?
    collected.kids : List.append(collected.kids, [make_anonymous(collected.container)]);

let groupChildren = (nodes) => {
  let collected =
    nodes
    |> List.filter(
         (n) =>
           switch n.boxType {
           | None => false
           | _ => true
           }
       )
    |> List.fold_left(
         (acc, node) =>
           switch node.boxType {
           | InlineNode(_) => {kids: acc.kids, container: List.append(acc.container, [node])}
           | _ => {container: [], kids: List.append(addAnonymous(acc), [node])}
           },
         {container: [], kids: []}
       );
  addAnonymous(collected)
};

let rec getLayoutTree = (node) => {
  let boxType = getBoxType(node);
  let children = boxType == None ? [] : List.map(getLayoutTree, node.children);
  /* If a block node contains an inline child, create an anonymous block box to contain it. If there are several inline children in a row, put them all in the same anonymous container. */
  let groupedChildren = groupChildren(children);
  {boxType, dimensions: defaultDimensions, children: groupedChildren}
};
/* let layoutTree = getLayoutTree(StyleTree.styledNode); */