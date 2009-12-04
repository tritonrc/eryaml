-module(yaml).
-author("Brian McKinney (brian@realitycontainment.com)").
-include("yaml.hrl").
-export([parse_file/1, find/2, find_by_path/2, key/1, value/1, children/1]).


key(Yaml) when is_record(Yaml, yamlnode) -> Yaml#yamlnode.key.

value(Yaml) when is_record(Yaml, yamlnode) -> Yaml#yamlnode.value.

children(Yaml) when is_record(Yaml, yamlnode) -> Yaml#yamlnode.children.

find_by_path(Path, Yaml) ->
  case Path of
    [] -> Yaml;
    [Segment|Rest] ->
      case find(Segment, Yaml) of
        not_found -> not_found;
        SubYaml -> find_by_path(Rest, SubYaml)
      end
  end.

find(SearchKey, Yaml) when is_record(Yaml, yamlnode) -> 
  case SearchKey =:= key(Yaml) of 
    true -> Yaml;
    false -> find(SearchKey, children(Yaml))
  end;
find(SearchKey, Yaml) when is_list(Yaml) ->
   Nodes = [Node || Node <- Yaml, SearchKey =:= key(Node)],
   case length(Nodes) of 
     0 -> not_found;
     _ -> hd(Nodes)
   end.

load_file(Filename) -> 
  case file:read_file(Filename) of 
    {ok,Binary} -> string:tokens(binary_to_list(Binary),"\n");
    {error,enoent} -> []
  end.

parse_file(Path) ->
  Lines = lists:reverse(load_file(Path)),
  parse(Lines,[],nan).

parse([],Yaml,_LastIndent) ->
  Yaml;
parse(Lines,Yaml,LastIndent) ->
  [Line|Rest] = Lines,
  case parse_line(Line) of
    comment -> parse(Rest,Yaml,LastIndent);
    Node ->
      #yamlnode{indent=Indent} = Node,
      if
        LastIndent =:= nan ->
          parse(Rest, [Node|Yaml],Indent);
        LastIndent =:= Indent -> % These two nodes are siblings
          parse(Rest,[Node|Yaml],Indent);
        LastIndent > Indent -> % This node is our parent
          {Children,Other} = lists:partition(fun(N) -> N#yamlnode.indent > Indent end, Yaml),
        ParentNode = Node#yamlnode{children=Children},
          parse(Rest,[ParentNode|Other],Indent);
        LastIndent < Indent -> % This node begins a new branch
          parse(Rest,[Node|Yaml],Indent)
      end
  end.

parse_line(Line) ->
  Indent = count_indent(Line,0),
  SLine = string:strip(Line,left),
  parse_line(SLine,#yamlnode{indent=Indent}).

parse_line([$#|_Rest], _Node) -> comment;
parse_line([$-|Rest], Node) -> parse_value(Rest,Node#yamlnode{key=scalar});
parse_line(Data, Node) -> parse_key(Data, Node).

parse_key(Data, Node) -> 
  case re:run(Data,"([\-_\:[:alnum:]]+):(\s)?") of
    {match, [_,{KeyStart,KeyLength}]} -> Node#yamlnode{key=string:substr(Data,KeyStart+1,KeyLength)};
    {match, [_,{KeyStart,KeyLength},{SpaceStart,SpaceEnd}]} ->
      parse_value(
        string:substr(Data,SpaceStart+SpaceEnd+1),
        Node#yamlnode{key=string:substr(Data,KeyStart+1,KeyLength)})
  end.

parse_value([$"|Value], Node) ->
  {match, [{QuoteStart,_}]} = re:run(Value,"(?<!\\\\)\""),
  Node#yamlnode{value=string:substr(Value,1,QuoteStart)};
parse_value([$'|Value], Node) ->
  {match, [{QuoteStart,_}]} = re:run(Value,"(?<!\\\\)'"),
  Node#yamlnode{value=string:substr(Value,1,QuoteStart)};
parse_value(Value, Node) ->
  Node#yamlnode{value=string:substr(Value,1,string:cspan(Value,"#"))}.

count_indent([],_Ctr) -> 0;
count_indent(Line,Ctr) ->
  case Line of 
    [32|Rest] -> count_indent(Rest,Ctr+1);
    _ -> Ctr
  end.
