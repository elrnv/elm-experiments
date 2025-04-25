import Html exposing (..)
import Html.Lazy exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse exposing (Position)
import Keyboard exposing (KeyCode)
import Color exposing (..)
import Random

import ListUtils
import TreeUtils exposing (..)
import Keys exposing (..)
import MultiwayTree as Tree exposing (..)

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
type alias Object
  = (Identified (Positioned (Boxed
      { name     : String
      , color    : Color
      , selected : Bool
      })))

type alias Identified a
  = { a | id : NodeIndex }

type alias Positioned a
  = { a | position : Position }

type alias Boxed a
  = { a | width : Int, height : Int }

type alias ObjectTree =
    { root        : ObjectNode
    , maxid       : NodeIndex
    , multiselect : Bool
    -- drag state
    , drag        : Maybe Drag
    }

type alias ObjectNode = Tree Object

-- Object Handle (used for dragging)
type alias ObjectHandle = Identified (Positioned (Boxed {}))

toHandle : ObjectNode -> ObjectHandle
toHandle node = 
  let
    obj = datum node
  in
    { id       = obj.id
    , position = obj.position
    , width    = obj.width
    , height   = obj.height
    }

type alias Drag =
  { draggedHandle : ObjectHandle
  , start         : Position
  , current       : Position
  , dragging      : Bool -- False if mouse hasn't moved enough to start dragging
  }


-- INIT

init : ( ObjectTree, Cmd Msg )
init =
  let
    node id name children = 
      let
        (h,s) = Random.step (Random.int 10 100) (Random.initialSeed id)
      in
        Tree
          { id       = (NodeIndex id)
          , position = Position 0 0 -- dummy position
          , width    = 200
          , height   = h
          , name     = (str ++ (toString id))
          , color    = rgb 0 (50*id % 256) 0
          , selected = False
          } children
    maxId = 8
  in
    ( { root 
        = reposition 
          ( node 0 "alpha"
            [ node 1 "beta" []
            , node 2 "gamma" 
              [ node 3 "gamma.a"
              , node 4 "gamma.b"
              , node 5 "gamma.c"
              ]
            , node 6 "delta" 
              [ node 7 "delta.a"
              , node 8 "delta.b"
              ]
            ] )
      , maxid = NodeIndex maxId
      , multiselect = False
      , drag = Nothing
      }, Cmd.none )


-- UPDATE

type Msg
    = NoOp
    -- Drag events
    | DragStart ObjectHandle Position
    | DragAt ObjectHandle (Maybe ObjectHandle) Position
    | DragEnd ObjectHandle (Maybe ObjectHandle) Position
    -- New node creation event
    --| NewNode String
    -- Multiple selection modes
    | MultipleSelect Bool
    | UnselectAll

indent = 10

reposition : ObjectNode -> ObjectNode
reposition tree = repositionHelp 0 0 tree

repositionTree : Int -> Int -> ObjectNode -> ObjectNode
repositionTree x y (Tree object children) =
  Tree { object | position = Position x y } (repositionList (x + indent) (y + object.height) children)

repositionList : Int -> Int -> List ObjectNode -> List ObjectNode
repositionList x y list =
  case list of
    [] -> []
    node :: rest ->
      let
        obj = datum node
      in
        repositionTree x y node :: repositionList x (y + obj.height) rest

-- select only the given node index
selectOne : NodeIndex -> ObjectNode -> ObjectNode
selectOne targetid =
  Tree.map (\obj -> { obj | selected = obj.id == targetid })

-- toggle selection of the target Node
selectToggle : NodeIndex -> ObjectNode -> ObjectNode
selectToggle targetid =
  Tree.map (\obj -> 
    { obj | selected = obj.selected |> xor (obj.id == targetid) })

unselectAll : ObjectNode -> ObjectNode
unselectAll = Tree.map (\obj -> { obj | selected = False })

update : Msg -> ObjectTree -> ( ObjectTree, Cmd Msg )
update msg objtree =
  let
    newtree =
      case msg of
        DragStart handle xy ->
          { objtree | drag = Just (Drag handle xy xy False) }

        DragAt src Nothing xy ->
          { objtree 
          | drag = (Maybe.map 
              (\{start,dragging} -> Drag src start xy (dragging || (distSquared xy start) > 1))
              objtree.drag)
          }

        DragAt src (Just dest) xy ->
          let
            newRoot = reposition (reinsert (.id << datum) src.id dest.id objtree.root)
            updateHandlePos handle (Tree obj children) =
              let
                updateHandlePosInList handle list =
                  case list of
                    [] -> Nothing
                    node :: rest ->
                      case updateHandlePos handle node of
                        Nothing -> updateHandlePos handle 
                        Just updatedHandle -> Just updatedHandle
              in
                if obj.id == handle.id then
                  Just { handle | position = node.position }
                else
                  updateHandlePosInList handle children
          in
            { objtree
            | root = newRoot
            , drag = (Maybe.map (\{start,dragging} ->
              let
                unwrapHandleUpdate maybeHandle =
                  case maybeHandle of
                    Nothing -> handle
                    Just handle -> handle
                  
                Drag (updateHandlePos src newRoot) start xy dragging) objtree.drag)
            }

        DragEnd src Nothing xy ->
          let
            updateNodes =
              case objtree.drag of
                Nothing         -> objtree.nodes
                Just {dragging} -> 
                  if dragging then 
                    objtree.nodes
                  else if objtree.multiselect then
                    selectToggle src.id objtree.nodes
                  else
                    selectOne src.id objtree.nodes
          in
            { objtree | nodes = updateNodes, drag = Nothing }

        DragEnd _ (Just _) _ ->
          { objtree | drag = Nothing }

        {- NewNode title ->
          let
            newMaxId = incrementIndex objtree.maxid
          in
            { objtree
              | nodes =
                reposition
                  ({ name    = title ++ " " ++ toString (nodeIndexToInt newMaxId)
                  , id       = newMaxId
                  , width    = 200
                  , height   = 50
                  , position = Position 0 0
                  , color    = rgb 0 (50 * (nodeIndexToInt newMaxId) % 256) 0
                  , selected = False
                  } :: objtree.nodes)
              , maxid = newMaxId
            }-}

        MultipleSelect enable -> -- toggle multiple selection
          { objtree | multiselect = enable }

        UnselectAll ->
          { objtree | nodes = unselectAll objtree.nodes }

        NoOp -> objtree
  in
    ( newtree, Cmd.none )


-- SUBSCRIPTIONS

subscriptions : ObjectTree -> Sub Msg
subscriptions tree =
  case tree.drag of
    Nothing ->
      Sub.batch [ Mouse.clicks (\_ -> UnselectAll), Keyboard.presses onKeyPress, Keyboard.downs onKeyDown, Keyboard.ups onKeyUp  ]

    -- subscribe to drag events that don't end up on top of another node
    Just {draggedHandle} ->
      Sub.batch [ Mouse.moves (DragAt draggedHandle Nothing << subHandlePos draggedHandle),
                  Mouse.ups (DragEnd draggedHandle Nothing << subHandlePos draggedHandle) ]


-- VIEW

view : ObjectTree -> Html Msg
view tree =
    lazy layout tree

(=>) = (,)

zipWithPos : (Int -> Int -> Int) -> Position -> Position -> Position
zipWithPos f a b = { x = f a.x b.x, y = f a.y b.y }

(+|+) = zipWithPos (+)
(-|-) = zipWithPos (-)

mapPos : (Int -> Int) -> Position -> Position
mapPos f p = { x = f p.x, y = f p.y }

dist : Position -> Position -> Float
dist a b = length (a -|- b)

distSquared : Position -> Position -> Int
distSquared a b = lengthSquared (a -|- b)

length : Position -> Float
length p = p |> lengthSquared |> toFloat |> sqrt

lengthSquared : Position -> Int
lengthSquared {x,y} = x*x + y*y

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

    Just {draggedHandle, start, current} -> -- being dragged
      if node.id == draggedHandle.id then
        node.position +|+ current -|- start
      else
        node.position


-- given a node box and whether something is currently being dragged, create a div for the node
buildNodeView : ObjectNode -> Maybe Drag -> Html Msg
buildNodeView node drag =
  let
    nodeHandle = toHandle node
  in let
    (ghostHeight,ghostY) =
      case drag of
        Nothing ->
          (node.height, node.position.y)
        Just {draggedHandle} ->
          if node.position.y > draggedHandle.position.y then
            (Basics.min node.height draggedHandle.height, node.position.y + (Basics.max 0 (node.height - draggedHandle.height)))
          else if node.position.y < draggedHandle.position.y then
            (Basics.min node.height draggedHandle.height, node.position.y)
          else
            (node.height, node.position.y)

    mouseEventsUpper =
      case drag of
        Nothing ->
          [onMouseDown nodeHandle]
        Just {draggedHandle} ->
          if node.id /= draggedHandle.id then
            [onMouseUp draggedHandle nodeHandle, onMouseOver draggedHandle nodeHandle]
          else []

    mouseEventsLower =
      case drag of
        Nothing ->
          [onMouseDown nodeHandle]
        Just {draggedHandle} ->
          if node.id /= draggedHandle.id then
            [onMouseUp draggedHandle nodeHandle, onMouseOver draggedHandle nodeHandle]
          else []

    draggedPosition = getDraggedPosition node drag

    zindex =
      case drag of
        Nothing -> "1"
        Just {draggedHandle} ->
          if node.id /= draggedHandle.id then "1" else "5"

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
        Just {draggedHandle, dragging} ->
          if node.id == draggedHandle.id && dragging then
            [ "transform" => "rotate(7deg)" ]
          else
            []
  in
    div []
      -- Invisible placeholder to catch mouse events
      [ div -- on mouse over, inserts before the current element
          (mouseEventsUpper ++
          [ class "nodeghost"
          , style
            [ "width" => px node.width
            , "height" => px ghostHeight
            , "left" => px node.position.x
            , "top" => px ghostY
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
        ] [ text node.name   ]
      ]

onKeyDown : KeyCode -> Msg
onKeyDown code =
  case decode code of
    Just Shift -> MultipleSelect True -- shift key
    _          -> NoOp

onKeyUp : KeyCode -> Msg
onKeyUp code =
  case decode code of
    Just Shift -> MultipleSelect False -- shift key
    _          -> NoOp

onKeyPress : KeyCode -> Msg
onKeyPress key =
  case decode key of
    Just CharN -> NewNode "New Node"
    _          -> NoOp


-- Helper function to subtract out node position
subHandlePos : ObjectHandle -> Position -> Position
subHandlePos handle p = p -|- handle.position

onMouseDown : ObjectHandle -> Attribute Msg
onMouseDown node =
  on "mousedown" (Decode.map (DragStart node << subHandlePos node) Mouse.position)

onMouseOver : ObjectHandle -> ObjectHandle -> Attribute Msg
onMouseOver src dest =
  on "mouseover" (Decode.map (DragAt src (Just dest) << subHandlePos dest) Mouse.position)

onMouseUp : ObjectHandle -> ObjectHandle -> Attribute Msg
onMouseUp src dest =
  on  "mouseup"  (Decode.map (DragEnd src (Just dest) << subHandlePos src) Mouse.position)
