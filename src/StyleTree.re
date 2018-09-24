open Node;

let node_has_class = (node, className) =>
  if (! StringMap.mem("class", node.attributes)) {
    false
  } else {
    StringMap.find("class", node.attributes)
    |> CssParser.split_on_chars([' '])
    |. Belt.List.some((s) => className == s)
  };

let selector_match = (node, selector) =>
  switch selector {
  | Css.Simple(simpleSelector) =>
    let tagMatch =
      switch simpleSelector.tagName {
      | None => true
      | Some(tagName) => tagName == node.tagName
      };
    let classMatch =
      switch simpleSelector.classes {
      | [] => true
      | _ => Belt.List.every(simpleSelector.classes, node_has_class(node))
      };
    let idMatch =
      switch simpleSelector.id {
      | None => true
      | Some(id) =>
        StringMap.mem("id", node.attributes) && id == StringMap.find("id", node.attributes)
      };
    tagMatch && classMatch && idMatch
  | Universal => true
  | Attribute(_attributeSelector) => false
  | PseudoClass(_pseudoClassSelector) => false
  | DescentCombinator(_descentlist) => false
  | ChildCombinator(_childlist) => false
  };

let get_matching_rules = (rules, node) =>
  switch node.nodeType {
  | Node.Element(elementData) =>
    Belt.List.keep(
      rules,
      (rule: Css.rule) => Belt.List.some(rule.selectors, selector_match(elementData))
    )
  | Text(_) => []
  | Comment(_) => []
  };

let rec styleNode = (rules, node) => {
  let matching_rules = get_matching_rules(rules, node);
  let declarations =
    matching_rules |> List.map((rule: Css.rule) => rule.declarations) |> List.concat;
  let inlineDeclarations =
    switch node.nodeType {
    | Element(elementData) =>
      Js.log("inline found");
      if (StringMap.mem("style", elementData.attributes)) {
        Js.log("Style really found");
        Js.log(StringMap.find("style", elementData.attributes));
        CssParser.parseDeclarations(StringMap.find("style", elementData.attributes))
      } else {
        []
      }
    | _ => []
    };
  let style =
    List.fold_left(
      (map, dec: Css.declaration) => StyleMap.add(dec.name, dec.value, map),
      StyleMap.empty,
      List.append(declarations, inlineDeclarations)
    );
  {...node, specifiedValues: style, children: List.map(styleNode(rules), node.children)}
};

let rules = CssParser.parseCss(CssParser.testCss);

let node = HtmlParser.parseHtml(HtmlParser.testHtml);

Js.log(printTree(styleNode(rules, node)));