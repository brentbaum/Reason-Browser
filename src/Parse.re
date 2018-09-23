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

let rec seekUntilHelper = (head, pattern) =>
  if (headComplete(head)) {
    None
  } else if (headPatternEq(readHeadRest(head), pattern)) {
    Some(head.pos)
  } else {
    let res = seekUntilHelper(incHead(head), pattern);
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

let seekUpTo = (head, pattern) =>
  switch (seekUntil(head, pattern)) {
  | None => None
  | Some((nextHead, contents)) =>
    Some((decHead(nextHead), String.sub(contents, 0, String.length(contents) - 1)))
  };

let seekPast = (head, pattern) =>
  switch (seekUntil(head, pattern)) {
  | None => None
  | Some((nextHead, contents)) =>
    Some((incHead(nextHead), String.sub(contents, 1, String.length(contents))))
  };