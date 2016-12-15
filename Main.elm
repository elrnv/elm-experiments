import Html exposing (..)
import Html.Lazy exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse exposing (Position)
import Keyboard exposing (KeyCode)
import Color exposing (..)

import ListUtils exposing (..)
import Keys exposing (..)

-- import hen needed
import Debug exposing (log)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

-- Making this a type instead of an alias allows the compiler to prevent us from using it incorrectly
type NodeIndex = NodeIndex Int
nodeIndexToInt : NodeIndex -> Int
nodeIndexToInt (NodeIndex i) = i
incrementIndex : NodeIndex -> NodeIndex
incrementIndex (NodeIndex idx) = NodeIndex (idx + 1)
mapIndex : (Int -> Int) -> NodeIndex -> NodeIndex
mapIndex f (NodeIndex idx) = NodeIndex (f idx)


-- The underlying object being manipulated. This can be a point, line, box, etc.
-- for now its just a string representing the name of the object
type alias Object = String


-- Object Node
type alias ObjectNode =
  { object   : Object
  , id       : NodeIndex
  , width    : Int
  , height   : Int
  , position : Position
  , color    : Color
  , selected : Bool
  }


-- Object Tree
type alias ObjectTree =
  { nodes       : List ObjectNode
  , position    : Position
  , maxid       : NodeIndex
  , multiselect : Bool
  -- drag state
  , drag        : Maybe Drag
  }


type alias Drag =
  { nodeid   : NodeIndex -- id of the node being dragged
  , start    : Position
  , current  : Position
  , dragging : Bool -- False if mouse hasn't moved enough to start dragging
  }


init : ( ObjectTree, Cmd Msg )
init =
  let
    node id = 
      { object   = ("New Node " ++ (toString id))
      , id       = (NodeIndex id)
      , width    = 200
      , height   = 50
      , position = Position 10 10
      , color    = rgb 0 (50*id % 256) 0
      , selected = False
    }
    maxId = 3
  in
    ( { nodes = reposition (List.map node (List.range 0 maxId))
      , position = Position 0 0
      , maxid = NodeIndex maxId
      , multiselect = False
      , drag = Nothing
      }, Cmd.none )


-- UPDATE

type Msg
    = NoOp
    -- Drag events
    | DragStart NodeIndex Position
    | DragAt NodeIndex (Maybe NodeIndex) Position
    | DragEnd NodeIndex (Maybe NodeIndex) Position
    -- New node creation event
    | NewNode String
    -- Multiple selection modes
    | MultipleSelect Bool
    | UnselectAll

reposition : List ObjectNode -> List ObjectNode
reposition list = repositionHelp list 0 0

repositionHelp : List ObjectNode -> Int -> Int -> List ObjectNode
repositionHelp list x y =
  case list of
    [] -> []
    node :: rest ->
      { node | position = Position x y } :: repositionHelp rest x (y+70)

-- select only the given node index
selectOne : NodeIndex -> List ObjectNode -> List ObjectNode
selectOne targetid =
  List.map (\node -> { node | selected = node.id == targetid })

-- toggle selection of the target Node
selectToggle : NodeIndex -> List ObjectNode -> List ObjectNode
selectToggle targetid =
  List.map (\node -> 
    { node | selected = node.selected |> xor (node.id == targetid) })

unselectAll : List ObjectNode -> List ObjectNode
unselectAll = List.map (\node -> { node | selected = False })

update : Msg -> ObjectTree -> ( ObjectTree, Cmd Msg )
update msg tree =
  let
    newtree =
      case msg of
        DragStart id xy ->
          { tree | drag = Just (Drag id xy xy False) }

        DragAt src Nothing xy ->
          { tree 
          | drag = (Maybe.map 
              (\{start,dragging} -> Drag src start xy (dragging || (distSquared xy start) > 1))
              tree.drag)
          }

        DragEnd src Nothing xy ->
          let
            updateNodes =
              case tree.drag of
                Nothing         -> tree.nodes
                Just {dragging} -> 
                  if dragging then 
                    tree.nodes
                  else if tree.multiselect then
                    selectToggle src tree.nodes
                  else
                    selectOne src tree.nodes
          in
            { tree | nodes = updateNodes, drag = Nothing }

        DragAt src (Just dest) xy ->
          let
            disp =
              case computeDisplacement src dest tree.nodes of
                Nothing -> Position 0 0
                Just d  -> d
          in
            { tree
            | nodes = reposition (reinsert .id src dest tree.nodes)
            , drag = (Maybe.map (\{start,dragging} -> Drag src (start +|+ disp) xy dragging) tree.drag)
            }

        DragEnd src (Just dest) xy ->
          { tree | drag = Nothing }

        NewNode title ->
          let
            newMaxId = incrementIndex tree.maxid
          in
            { tree
              | nodes =
                reposition
                  ({ object  = title ++ " " ++ toString (nodeIndexToInt newMaxId)
                  , id       = newMaxId
                  , width    = 200
                  , height   = 50
                  , position = Position 0 0
                  , color    = rgb 0 (50 * (nodeIndexToInt newMaxId) % 256) 0
                  , selected = False
                  } :: tree.nodes)
              , maxid = newMaxId
            }

        MultipleSelect enable -> -- toggle multiple selection
          { tree | multiselect = enable }

        UnselectAll ->
          { tree | nodes = unselectAll tree.nodes }

        NoOp -> tree
  in
    ( newtree, Cmd.none )

-- return Position 0 0 if not found
findNodePosition : NodeIndex -> List ObjectNode -> Maybe Position
findNodePosition targetid nodes =
  case nodes of
    [] -> Nothing
    node :: rest ->
      if node.id == targetid then
        Just node.position
      else
        findNodePosition targetid rest

computeDisplacement : NodeIndex -> NodeIndex -> List ObjectNode -> Maybe Position
computeDisplacement from to nodes =
  case nodes of
    [] -> Nothing
    node :: rest ->
      if node.id == to then
        Maybe.map (\posn -> node.position -|- posn) (findNodePosition from rest)
      else if node.id == from then
        Maybe.map (\posn -> posn -|- node.position) (findNodePosition to rest)
      else
        computeDisplacement from to rest


-- SUBSCRIPTIONS

subscriptions : ObjectTree -> Sub Msg
subscriptions tree =
  case tree.drag of
    Nothing ->
      Sub.batch [ Mouse.clicks (\_ -> UnselectAll), Keyboard.presses onKeyPress, Keyboard.downs onKeyDown, Keyboard.ups onKeyUp  ]

    -- subscribe to drag events that don't end up on top of another node
    Just {nodeid} ->
      Sub.batch [ Mouse.moves (DragAt nodeid Nothing), Mouse.ups (DragEnd nodeid Nothing) ]


-- VIEW


(=>) = (,)

zipWithPos : (Int -> Int -> Int) -> Position -> Position -> Position
zipWithPos f a b = { x = f a.x b.x, y = f a.y b.y }

(+|+) = zipWithPos (+)
(-|-) = zipWithPos (-)

dist : Position -> Position -> Float
dist a b = length (a -|- b)

distSquared : Position -> Position -> Int
distSquared a b = lengthSquared (a -|- b)

length : Position -> Float
length p = p |> lengthSquared |> toFloat |> sqrt

lengthSquared : Position -> Int
lengthSquared {x,y} = x*x + y*y

view : ObjectTree -> Html Msg
view tree =
    lazy layout tree

px : Int -> String
px number =
  toString number ++ "px"


layout : ObjectTree -> Html Msg
layout tree =
  div [] (layoutIter tree)


layoutIter : ObjectTree -> List (Html Msg)
layoutIter tree =
  case tree.nodes of
    [] -> []
    node :: rest ->
      let
        nodeView =
          lazy2 buildNodeView { node | position = tree.position +|+ node.position } tree.drag
      in
        nodeView :: layoutIter { tree | nodes = rest }


getDraggedPosition : ObjectNode -> Maybe Drag -> Position
getDraggedPosition node drag =
  case drag of
    Nothing -> -- not being dragged
      node.position

    Just {nodeid,start,current} -> -- being dragged
      if node.id == nodeid then
        node.position +|+ current -|- start
      else
        node.position


-- given a node box and whether something is currently being dragged, create a div for the node
buildNodeView : ObjectNode -> Maybe Drag -> Html Msg
buildNodeView node drag =
  let
    mouseEvents =
      case drag of
        Nothing ->
          [onMouseDown node.id]
        Just {nodeid} ->
          if node.id /= nodeid then
            [onMouseUp nodeid node.id, onMouseOver nodeid node.id]
          else []

    draggedPosition = getDraggedPosition node drag

    zindex =
      case drag of
        Nothing -> "1"
        Just {nodeid} ->
          if node.id /= nodeid then "1" else "5"

    rgba = toRgb node.color

    rgbaString =
      "rgba("
        ++ (toString rgba.red) ++ ", "
        ++ (toString rgba.green) ++ ", "
        ++ (toString rgba.blue) ++ ", "
        ++ (toString rgba.alpha) ++ ")"

    selectedRgbaString = "rgba(255,0,0,255)"

    rotation =
      case drag of
        Nothing -> []
        Just {nodeid,dragging} ->
          if node.id == nodeid && dragging then
            [ "transform" => "rotate(7deg)" ]
          else
            []
  in
    div []
      -- Invisible placeholder to catch mouse events
      [ div
          (mouseEvents ++
          [ class "nodeghost"
          , style
            [ "width" => px node.width
            , "height" => px node.height
            , "left" => px node.position.x
            , "top" => px node.position.y
            ]
          ])
          []
      -- The actual viewable box
      , div
        [ class "nodebox"
        , style
          (rotation ++
          [ "background-color" => if node.selected then selectedRgbaString else rgbaString
          , "width" => px node.width
          , "height" => px node.height
          , "left" => px draggedPosition.x
          , "top" => px draggedPosition.y
          , "z-index" => zindex
          ])
        ] [ text node.object ]
      ]

onKeyDown : KeyCode -> Msg
onKeyDown code =
  case decode code of
    Just Shift -> MultipleSelect True -- shift key
    _ -> NoOp

onKeyUp : KeyCode -> Msg
onKeyUp code =
  case decode code of
    Just Shift -> MultipleSelect False -- shift key
    _ -> NoOp

onKeyPress : KeyCode -> Msg
onKeyPress key =
  case decode key of
    Just CharN -> NewNode "New Node"
    _   -> NoOp

onMouseDown : NodeIndex -> Attribute Msg
onMouseDown id =
  on "mousedown" (Decode.map (DragStart id) Mouse.position)

onMouseOver : NodeIndex -> NodeIndex -> Attribute Msg
onMouseOver srcid destid =
  on "mouseover" (Decode.map (DragAt srcid (Just destid)) Mouse.position)

onMouseUp : NodeIndex -> NodeIndex -> Attribute Msg
onMouseUp srcid destid =
  on "mouseup" (Decode.map (DragEnd srcid (Just destid)) Mouse.position)
