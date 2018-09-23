type tag =
  | Div
  | P
  | Html
  | Body
  | H1
  | H2
  | H3;

type simpleSelector = {
  tagName: option(string),
  id: option(string),
  classes: list(string)
};

type attributeSelector = {
  tagName: option(string),
  attributeName: string,
  value: option(string)
};

type pseudoClassSelector = {classes: list(string)};

type selector =
  | Simple(simpleSelector)
  | Universal
  | Attribute(attributeSelector)
  | PseudoClass(pseudoClassSelector)
  | DescentCombinator(list(simpleSelector))
  | ChildCombinator(list(simpleSelector));

type unit =
  | Px
  | Percent;

type name = string;

type value =
  | Keyword(string)
  | Length(float, unit)
  | Color(string);

type declaration = {
  name,
  value
};

type rule = {
  selectors: list(selector),
  declarations: list(declaration)
};