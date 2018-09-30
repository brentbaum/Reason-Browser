open Reprocessing;

open Box;

let translucent: Reprocessing.colorT = {r: 0.9, g: 0.9, a: 0.2, b: 0.9};

let translucentDark: Reprocessing.colorT = {r: 0.9, g: 0.9, a: 0.4, b: 0.9};

let drawNode = (env, box) => {
  Draw.fill(translucent, env);
  Draw.stroke(translucentDark, env);
  Draw.strokeWeight(1, env);
  Js.log(box.dimensions.content);
  Draw.rectf(
    ~pos=(box.dimensions.content.x, box.dimensions.content.y),
    ~width=box.dimensions.content.width +. 50.0,
    ~height=box.dimensions.content.height +. 30.0,
    env
  )
};

let rec drawBoxLayout = (_state, env, node) => {
  switch node.boxType {
  | BlockNode(_node) => drawNode(env, node)
  | InlineNode(_node) => drawNode(env, node)
  | _ => ()
  };
  Belt.List.forEach(node.children, drawBoxLayout(_state, env))
};

let setup = (env) => Env.size(~width=600, ~height=400, env);

let draw = (tree, _state, env) => {
  Draw.background({r: 0.9, g: 0.9, a: 0.9, b: 0.9}, env);
  drawBoxLayout(_state, env, tree)
};

let getScreenDimensions = (width, height) => {
  ...Box.defaultDimensions,
  content: {x: 0.0, y: 0.0, height, width}
};

let parseAndDraw = (html, css) => {
  let rootNode = HtmlParser.parseHtml(html);
  let rules = CssParser.parseCss(css);
  let styledNode = StyleTree.getStyleTree(rules, rootNode);
  let layoutTree = getLayoutTree(styledNode);
  let boxLayoutTree = BoxLayout.getBoxLayout(getScreenDimensions(600.0, 400.0), layoutTree);
  run(~setup, ~draw=draw(boxLayoutTree), ())
};

let html = {|<div class="a">
<div class="b">
  <div class="c">
    <div class="d">
      <div class="e">
        <div class="f">
          <div class="g">
          </div>
        </div>
      </div>
    </div>
  </div>
</div>
</div>|};

let css = {|* { display: block; padding: 12px; }
.a { background: #ff0000; }
.b { background: #ffa500; }
.c { background: #ffff00; }
.d { background: #008000; }
.e { background: #0000ff; }
.f { background: #4b0082; }
.g { background: #800080; }|};

parseAndDraw(html, css);