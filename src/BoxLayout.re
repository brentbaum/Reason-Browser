open Box;

open Node;

open Css;

exception NodeType(string);

let getBlockNode = (box) =>
  switch box.boxType {
  | BlockNode(node) => node
  | _ => raise(NodeType("Tried to call block method on non-block box."))
  };

let styleLookup = (node, accessors, default) => {
  let value =
    Belt.List.head(
      Belt.List.keepMap(
        accessors,
        (accessor) =>
          Node.StyleMap.mem(accessor, node.specifiedValues) ?
            Some(Node.StyleMap.find(accessor, node.specifiedValues)) : None
      )
    );
  Belt.Option.getWithDefault(value, default)
};

let toPx = (containing, value) =>
  switch value {
  | Length(percent, Percent) => containing.content.width *. percent
  | Length(pixels, Px) => pixels
  | _ => 0.0
  };

/* Mutability trigger warning */
/* Follows: https://www.w3.org/TR/CSS2/visudet.html#blockwidth */
let calculateBlockWidth = (containing, blockBox) => {
  let node = getBlockNode(blockBox);
  /* TODO: Need to add support for % width. will be interpreted 0 now. */
  let width = ref(styleLookup(node, ["width"], Keyword("auto")));
  let zero = Length(0.0, Px);
  let auto = Keyword("auto");
  let marginLeft = ref(styleLookup(node, ["margin-left", "margin"], zero));
  let marginRight = ref(styleLookup(node, ["margin-right", "margin"], zero));
  let paddingLeft = styleLookup(node, ["padding-left", "padding"], zero);
  let paddingRight = styleLookup(node, ["padding-right", "padding"], zero);
  let borderLeft = styleLookup(node, ["border-left", "border"], zero);
  let borderRight = styleLookup(node, ["border-right", "border"], zero);
  let total =
    List.fold_left(
      (t, v) => t +. toPx(containing, v),
      0.0,
      [marginLeft^, marginRight^, paddingLeft, paddingRight, borderLeft, borderRight, width^]
    );
  /* If we're overflowing, then ignore any auto's for margins. */
  if (width^ != auto && total > containing.content.width) {
    if (marginLeft^ == auto) {
      marginLeft := Length(0.0, Px)
    };
    if (marginRight^ == auto) {
      marginRight := Length(0.0, Px)
    }
  };
  let underflow = containing.content.width -. total;
  switch (width^ == auto, marginLeft^ == auto, marginRight^ == auto) {
  | (false, false, _) => marginRight := Length(toPx(containing, marginRight^) +. underflow, Px)
  | (false, true, false) => marginLeft := Length(underflow, Px)
  | (true, _, _) =>
    if (marginLeft^ == auto) {
      marginLeft := Length(0.0, Px)
    };
    if (marginRight^ == auto) {
      marginRight := Length(0.0, Px)
    };
    if (underflow >= 0.0) {
      width := Length(underflow, Px)
    } else {
      width := Length(0.0, Px);
      marginRight := Length(toPx(containing, marginRight^) +. underflow, Px)
    }
  | (false, true, true) =>
    marginLeft := Length(underflow /. 2.0, Px);
    marginRight := Length(underflow /. 2.0, Px)
  };
  {
    ...blockBox,
    dimensions: {
      ...blockBox.dimensions,
      content: {...blockBox.dimensions.content, width: toPx(containing, width^)},
      margin: {
        ...blockBox.dimensions.margin,
        left: toPx(containing, marginLeft^),
        right: toPx(containing, marginRight^)
      }
    }
  }
};

let expandedBy = (edge, rect) => {
  x: rect.x -. edge.left,
  y: rect.y -. edge.top,
  width: rect.width +. edge.left +. edge.right,
  height: rect.height +. edge.top +. edge.bottom
};

let getMarginBox = (dimensions) =>
  dimensions.content
  |> expandedBy(dimensions.padding)
  |> expandedBy(dimensions.border)
  |> expandedBy(dimensions.margin);

let calculateBlockPosition = (containing, blockBox) => {
  let node = getBlockNode(blockBox);
  /* TODO: Need to add support for % width. will be interpreted 0 now. */
  let zero = Length(0.0, Px);
  let marginTop = toPx(containing, styleLookup(node, ["margin-top", "margin"], zero));
  let marginBottom = toPx(containing, styleLookup(node, ["margin-bottom", "margin"], zero));
  let borderTop = toPx(containing, styleLookup(node, ["border-width-top", "border-width"], zero));
  let borderBottom =
    toPx(containing, styleLookup(node, ["border-width-bottom", "border-width"], zero));
  let paddingTop = toPx(containing, styleLookup(node, ["padding-top", "padding"], zero));
  let paddingBottom = toPx(containing, styleLookup(node, ["padding-bottom", "padding"], zero));
  let contentX =
    containing.content.x
    +. blockBox.dimensions.margin.left
    +. blockBox.dimensions.border.left
    +. blockBox.dimensions.padding.left;
  let contentY =
    containing.content.height
    +. containing.content.y
    +. blockBox.dimensions.margin.top
    +. blockBox.dimensions.border.top
    +. blockBox.dimensions.padding.top;
  {
    ...blockBox,
    dimensions: {
      margin: {...blockBox.dimensions.margin, top: marginTop, bottom: marginBottom},
      border: {...blockBox.dimensions.margin, top: borderTop, bottom: borderBottom},
      padding: {...blockBox.dimensions.padding, top: paddingTop, bottom: paddingBottom},
      content: {...blockBox.dimensions.content, x: contentX, y: contentY}
    }
  }
};

let calculateBlockHeight = (containing, block) => {
  let node = getBlockNode(block);
  let height = styleLookup(node, ["height"], Length(block.dimensions.content.height, Px));
  {
    ...block,
    dimensions: {
      ...block.dimensions,
      content: {...block.dimensions.content, height: toPx(containing, height)}
    }
  }
};

type childrenCollector = {
  height: float,
  children: layoutBox
};

/*
 1. Calculate my width, based off parent's width.
 2. Calculate my position, based off parent's position.
 3. Layout my children
 4. Calculate my height, based off children + parent height.
 */
let rec getBoxLayout = (containing_dimensions: Box.dimensions, box) =>
  switch box.boxType {
  | BlockNode(_) =>
    Js.log("Block");
    layoutBlock(containing_dimensions, box)
  | InlineNode(_) =>
    Js.log("Inline");
    box
  | AnonymousBlock =>
    Js.log("Anon");
    box
  | _ => box
  }
and layoutChildren = (block: Box.layoutBox) => {
  let childrenTotalHeight =
    List.fold_left(
      (a, b) => a +. getMarginBox(b.dimensions).height,
      block.dimensions.content.height
    );
  let children =
    List.fold_left(
      (acc, child) => {
        let height = childrenTotalHeight(acc);
        let nextChild =
          getBoxLayout(
            {...block.dimensions, content: {...block.dimensions.content, height}},
            child
          );
        List.append(acc, [nextChild])
      },
      [],
      block.children
    );
  let height = childrenTotalHeight(children);
  {
    ...block,
    dimensions: {...block.dimensions, content: {...block.dimensions.content, height}},
    children
  }
}
and layoutBlock = (containing, block) =>
  block
  |> calculateBlockWidth(containing)
  |> calculateBlockPosition(containing)
  |> layoutChildren
  |> calculateBlockHeight(containing);
/* let boxLayoutTree = getBoxLayout(Box.defaultDimensions, Box.layoutTree); */
/* Js.log(layoutTree); */