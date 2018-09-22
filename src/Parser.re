let test = {|<html>
    <body>
        <h1>Title</h1>
        <div id="main" class="test">
            <p>Hello <em>world</em>!</p>
        </div>
    </body>
</html>|};

type parseHead = {
  pos: int,
  body: string
};

let headComplete = (head) => head.pos == String.length(head.body);

let incHead = (head) => {...head, pos: head.pos + 1};

let decHead = (head) => {...head, pos: head.pos - 1};

let readHead = (head) => head.body.[head.pos];

let readHeadRest = (head) => String.sub(head.body, head.pos, String.length(head.body) - head.pos);

let rec toCharList = (ch) =>
  switch ch {
  | "" => []
  | ch => [ch.[0], ...toCharList(String.sub(ch, 1, String.length(ch) - 1))]
  };

let headPatternEq = (str, pattern) => {
  /* Js.log3("eq?", str, pattern); */
  let pairs = Belt.List.zip(toCharList(str), toCharList(pattern));
  ! Belt.List.some(pairs, ((c1, c2)) => c1 != c2)
};

let rec seekUntilHelper = (head, pattern) =>
  if (headComplete(head)) {
    None
  } else if (headPatternEq(readHeadRest(head), pattern)) {
    Some(head.pos)
  } else {
    let res = seekUntilHelper(incHead(head), pattern);
    Js.log(res);
    switch res {
    | None => None
    | Some(i) => Some(i)
    }
  };

let seekUntil = (head, pattern) => {
  let indexOption = seekUntilHelper(head, pattern);
  switch indexOption {
  | None => None
  | Some(pos) =>
    let nextHead = {...head, pos: pos + 1};
    Some((nextHead, String.sub(nextHead.body, head.pos, nextHead.pos - head.pos)))
  }
};

let headSkipWhitespace = (head) =>
  if (String.length(head.body) == head.pos) {
    head.pos
  } else {
    let p = ref(head.pos);
    while (Belt.List.has([' ', '\n', '\t', '\r'], head.body.[p^], (a, b) => a == b)) {
      p := p^ + 1
    };
    p^
  };

let parseTagContents = (contents) => {
  let tag = Js.String.split(" ", String.trim(contents));
  Js.log(tag);
  Node.element(tag[0], Node.StringMap.empty, [])
};

let rec step = (head) =>
  if (headComplete(head)) {
    []
  } else {
    Js.log("switch");
    let (nextHead, node) =
      switch (readHead(head)) {
      | '<' =>
        /* Have to check for close without open - skip in that case */
        Js.log("Hey");
        let tagOption = seekUntil(head, ">");
        switch tagOption {
        | None => (incHead(head), Node.comment("null"))
        | Some((nextHead, contents)) => (nextHead, parseTagContents(contents))
        }
      | x =>
        switch (seekUntil(head, "<")) {
        | None => (incHead(head), Node.text(String.make(1, x)))
        | Some((nextHead, contents)) => (
            decHead(nextHead),
            Node.text(String.sub(contents, 0, String.length(contents) - 1))
          )
        }
      };
    /* hacky - will break for trailing whitespace*/
    let nextWithoutWhiteSpaceHead = {...nextHead, pos: headSkipWhitespace(nextHead)};
    if (nextWithoutWhiteSpaceHead.pos + 1 == String.length(nextHead.body)) {
      [node]
    } else {
      List.append([node], step(nextWithoutWhiteSpaceHead))
    }
  };

let head = {body: test, pos: 0};

let nodes = step(head);

Js.log("Results\n\n\n\n");

Js.log(Node.printTree(Node.element("Root", Node.StringMap.empty, nodes)));
/*


 seekUntil = (readHead, char) => (readHead, string)
    1. inc until encounter <
    2. Get contents of tag by seekUntil(">"). tagName = String.split(contents, " ")[0]. attrs = parseAttrs([1])
    3. Child text is seekUntil("<" ++ tagName)
    4. Iterate on child
  */