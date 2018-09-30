open Parse;

let testHtml = {|<html>
    <body>
        <h1>Title</h1>
        <div id="main" class="test" style="display:flex">
            <p>Hello <em id="answer">world</em>!</p>
        </div>
        <div class="note">It's me</div>
    </body>
</html>|};

let parseAttribute = (tagStr) => {
  Js.log(tagStr);
  let parts = Js.String.split("=", tagStr);
  if (Array.length(parts) == 2) {
    (parts[0], String.sub(parts[1], 1, String.length(parts[1]) - 2))
  } else {
    ("", "")
  }
};

/* TODO: spliting by space breaks display: flex, so we have to do display:flex */
let parseTagContents = (contents) => {
  let innerContents = String.sub(contents, 1, String.length(contents) - 2);
  let tags = Js.String.split(" ", String.trim(innerContents));
  let attrMap =
    Belt.Array.sliceToEnd(tags, 1)
    |> Array.map(parseAttribute)
    |. Belt.Array.keep(((name, _value)) => name != "")
    |> Array.fold_left(
         (map, (name, value)) => Node.StringMap.add(name, value, map),
         Node.StringMap.empty
       );
  Node.element(tags[0], attrMap, [])
};

let getElementTagName = (node: Node.node) =>
  switch node.nodeType {
  | Node.Element({tagName}) => tagName
  | _ => ""
  };

let rec step = (head) =>
  if (Parse.headComplete(head)) {
    []
  } else {
    let (nextHead, node) =
      switch (readHead(head)) {
      | '<' =>
        /* Have to check for close without open - skip in that case */
        let tagOption = seekUntil(head, ">");
        switch tagOption {
        | None => (incHead(head), Node.comment("null"))
        | Some((nextHead, contents)) =>
          let element = parseTagContents(contents);
          let tagName = getElementTagName(element);
          let closeStart = seekUntil(head, "</" ++ tagName);
          switch closeStart {
          | None => (nextHead, Node.text(contents))
          | Some((closeStartHead, _)) =>
            let closeEnd = seekUntil(closeStartHead, ">");
            switch closeEnd {
            | None => (nextHead, Node.text(contents))
            | Some((closeEndHead, _)) =>
              let childrenContents =
                String.sub(nextHead.body, nextHead.pos, closeStartHead.pos - nextHead.pos);
              let childHead = {pos: 0, body: childrenContents};
              let elementWithChildren = {...element, children: step(childHead)};
              (closeEndHead, elementWithChildren)
            }
          }
        }
      | x =>
        switch (seekUntil(head, "<")) {
        | None => (incHead(head), Node.text(String.make(1, x)))
        | Some((nextHead, contents)) => (
            decHead(nextHead),
            Node.text(String.trim(String.sub(contents, 0, String.length(contents) - 1)))
          )
        }
      };
    /* hacky - will break for trailing whitespace*/
    let nextWithoutWhiteSpaceHead = {...nextHead, pos: headSkipWhitespace(nextHead)};
    let nodeList =
      nextWithoutWhiteSpaceHead.pos + 1 == String.length(nextHead.body) ?
        [node] : List.append([node], step(nextWithoutWhiteSpaceHead));
    Belt.List.keep(
      nodeList,
      (n) =>
        switch n.nodeType {
        | Node.Text(t) when String.trim(t) == "" => false
        | _ => true
        }
    )
  };

let parseHtml = (str) => {
  let head = {body: str, pos: 0};
  Node.element("Root", Node.StringMap.empty, step(head))
};

Js.log("Results\n\n\n\n");

Js.log(Node.printTree(parseHtml(testHtml)));