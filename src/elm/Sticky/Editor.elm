module Sticky.Editor exposing
    ( Model, Msg, init, update, view
    , Style(..), BlockStyle(..)
    , undo, redo
    , toggleStyle, toggleBlockStyle
    , unwrapBlock, wrapAsBlockQuote, wrapAsOrderedListBlock, wrapAsUnorderedListBlock
    , insertHorizontalRule
    , ControlContext, getControlContext
    )

{-|


# TEA model.

@docs Model, Msg, init, update, view


# Available inline or block styles .

@docs Style, BlockStyle


# Control actions.

@docs undo, redo
@docs toggleStyle, toggleBlockStyle
@docs unwrapBlock, wrapAsBlockQuote, wrapAsOrderedListBlock, wrapAsUnorderedListBlock
@docs insertHorizontalRule


# Context to assist with correctly rendering and applying control actions.

@docs ControlContext, getControlContext

-}

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes
import Markdown.Block as Block
import Markdown.Inline as Inline
import RichText.Commands as Commands
import RichText.Config.Command as Command exposing (CommandMap)
import RichText.Config.Decorations as Decorations exposing (Decorations)
import RichText.Config.Keys as Keys
import RichText.Config.Spec exposing (Spec)
import RichText.Definitions as Definitions
import RichText.Editor as Editor exposing (Config, Editor, Message)
import RichText.List as RTList exposing (ListType)
import RichText.Model.Attribute as Attribute
import RichText.Model.Element as Element exposing (Element)
import RichText.Model.History as History
import RichText.Model.InlineElement as InlineElement
import RichText.Model.Mark as Mark exposing (Mark, MarkOrder)
import RichText.Model.Node as ModelNode exposing (Block, Children, Inline, InlineTree, Path)
import RichText.Model.Selection as Selection
import RichText.Model.State as State exposing (State)
import RichText.Model.Text as Text
import RichText.Node as Node exposing (Node)
import Set exposing (Set)
import Update2


type alias Model =
    { editor : Editor
    , styles : List Style
    , textMarkdown : String
    , markdownError : Maybe String
    }


type Msg
    = InternalMsg Message


type Style
    = Bold
    | Italic
    | Code


type BlockStyle
    = CodeBlock
    | Heading Int


config : Config Msg
config =
    Editor.config
        { decorations = decorations
        , commandMap = commandBindings Definitions.markdown
        , spec = Definitions.markdown
        , toMsg = InternalMsg
        }


docInitNode : Block
docInitNode =
    ModelNode.block
        (Element.element Definitions.doc [])
        (ModelNode.blockChildren (Array.fromList [ initialEditorNode ]))


initialEditorNode : Block
initialEditorNode =
    ModelNode.block
        (Element.element Definitions.paragraph [])
        (ModelNode.inlineChildren (Array.fromList [ ModelNode.plainText "This is some sample text" ]))


initialState : State
initialState =
    State.state docInitNode Nothing


listCommandBindings : CommandMap
listCommandBindings =
    RTList.defaultCommandMap RTList.defaultListDefinition


emptyParagraph : Block
emptyParagraph =
    ModelNode.block
        (Element.element Definitions.paragraph [])
        (Array.fromList [ ModelNode.plainText "" ] |> ModelNode.inlineChildren)


commandBindings : Spec -> CommandMap
commandBindings spec =
    let
        markOrder =
            Mark.markOrderFromSpec spec
    in
    Command.combine
        listCommandBindings
        (Commands.defaultCommandMap
            |> Command.set [ Command.inputEvent "insertParagraph", Command.key [ Keys.enter ], Command.key [ Keys.return ] ]
                [ ( "insertNewline"
                  , Commands.insertNewline [ "code_block" ]
                        |> Command.transform
                  )
                , ( "liftEmpty"
                  , Commands.liftEmpty
                        |> Command.transform
                  )
                , ( "splitBlockHeaderToNewParagraph"
                  , Commands.splitBlockHeaderToNewParagraph [ "heading" ] (Element.element Definitions.paragraph [])
                        |> Command.transform
                  )
                , ( "insertEmptyParagraph"
                  , Commands.insertAfterBlockLeaf emptyParagraph
                        |> Command.transform
                  )
                ]
            |> Command.set [ Command.inputEvent "formatBold", Command.key [ Keys.short, "b" ] ]
                [ ( "toggleStyle"
                  , Commands.toggleMark markOrder (Mark.mark Definitions.bold []) Mark.Flip
                        |> Command.transform
                  )
                ]
            |> Command.set [ Command.inputEvent "formatItalic", Command.key [ Keys.short, "i" ] ]
                [ ( "toggleStyle"
                  , Commands.toggleMark markOrder (Mark.mark Definitions.italic []) Mark.Flip
                        |> Command.transform
                  )
                ]
            |> Command.set [ Command.inputEvent "formatHeading", Command.key [ Keys.short, "y" ] ]
                [ ( "toggleBlock"
                  , Commands.toggleTextBlock
                        (Element.element Definitions.heading [ Attribute.IntegerAttribute "level" 1 ])
                        (Element.element Definitions.paragraph [])
                        False
                        |> Command.transform
                  )
                ]
        )


decorations : Decorations Msg
decorations =
    Decorations.emptyDecorations
        |> Decorations.addElementDecoration Definitions.image (Decorations.selectableDecoration InternalMsg)
        |> Decorations.addElementDecoration Definitions.horizontalRule (Decorations.selectableDecoration InternalMsg)
        -- Disable the grammarly plugin as it breaks the virtual dom.
        |> Decorations.withTopLevelAttributes [ Html.Attributes.attribute "data-gramm_editor" "false" ]


initEditor : State -> Editor
initEditor iState =
    Editor.init iState


init : Model
init =
    let
        markdownNodes =
            rootToMarkdown (State.root initialState)

        ( result, error ) =
            case Result.andThen markdownToString markdownNodes of
                Err e ->
                    ( "", Just e )

                Ok m ->
                    ( m, Nothing )
    in
    { editor = initEditor initialState
    , styles = [ Bold, Italic ]
    , textMarkdown = result
    , markdownError = error
    }


editorUpdate : Config msg -> Editor.Message -> Editor -> ( Editor, Cmd Editor.Message )
editorUpdate cfg msg ed =
    ( Editor.update cfg msg ed, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InternalMsg internalEditorMsg ->
            model
                |> Update2.lift .editor (\x m -> { m | editor = x }) InternalMsg (editorUpdate config) internalEditorMsg
                |> Update2.andThen extractMarkdown


extractMarkdown : Model -> ( Model, Cmd Msg )
extractMarkdown model =
    let
        markdownNodes =
            rootToMarkdown (State.root (Editor.state model.editor))

        ( result, error ) =
            case Result.andThen markdownToString markdownNodes of
                Err e ->
                    ( model.textMarkdown, Just e )

                Ok m ->
                    ( m, Nothing )
    in
    case error of
        Just e ->
            ( { model | markdownError = Just e }, Cmd.none )

        Nothing ->
            ( { model
                | textMarkdown = result
                , markdownError = Nothing
              }
            , Cmd.none
            )


unwrapBlock : Model -> Model
unwrapBlock model =
    let
        spec =
            Editor.spec config
    in
    { model
        | editor =
            Result.withDefault model.editor
                (Editor.applyList
                    [ ( "liftList"
                      , RTList.lift RTList.defaultListDefinition
                            |> Command.transform
                      )
                    , ( "lift"
                      , Command.transform Commands.lift
                      )
                    ]
                    spec
                    model.editor
                )
    }


wrapAsUnorderedListBlock : Model -> Model
wrapAsUnorderedListBlock model =
    wrapAsListBlock RTList.Unordered model


wrapAsOrderedListBlock : Model -> Model
wrapAsOrderedListBlock model =
    wrapAsListBlock RTList.Ordered model


wrapAsListBlock : ListType -> Model -> Model
wrapAsListBlock listType model =
    let
        spec =
            Editor.spec config
    in
    { model
        | editor =
            Result.withDefault model.editor
                (Editor.apply
                    ( "wrapList"
                    , RTList.wrap RTList.defaultListDefinition listType |> Command.transform
                    )
                    spec
                    model.editor
                )
    }


redo : Model -> Model
redo model =
    let
        spec =
            Editor.spec config
    in
    { model
        | editor =
            Result.withDefault model.editor
                (Editor.apply ( "redo", Command.internal Command.Redo ) spec model.editor)
    }


undo : Model -> Model
undo model =
    let
        spec =
            Editor.spec config
    in
    { model
        | editor =
            Result.withDefault model.editor
                (Editor.apply ( "undo", Command.internal Command.Undo ) spec model.editor)
    }


toggleStyle : Style -> Model -> Model
toggleStyle style model =
    let
        spec =
            Editor.spec config

        markDef =
            case style of
                Bold ->
                    Definitions.bold

                Italic ->
                    Definitions.italic

                Code ->
                    Definitions.code

        markOrder =
            Mark.markOrderFromSpec spec
    in
    { model
        | editor =
            Result.withDefault model.editor
                (Editor.apply
                    ( "toggleStyle"
                    , Commands.toggleMark markOrder (Mark.mark markDef []) Mark.Flip
                        |> Command.transform
                    )
                    spec
                    model.editor
                )
    }


toggleBlockStyle : BlockStyle -> Model -> Model
toggleBlockStyle blockStyle model =
    let
        spec =
            Editor.spec config

        onParams =
            case blockStyle of
                CodeBlock ->
                    Element.element
                        Definitions.codeBlock
                        []

                Heading level ->
                    Element.element
                        Definitions.heading
                        [ Attribute.IntegerAttribute
                            "level"
                            level
                        ]

        offParams =
            Element.element Definitions.paragraph []

        convertToPlainText =
            blockStyle == CodeBlock
    in
    { model
        | editor =
            Result.withDefault model.editor
                (Editor.apply
                    ( "toggleBlock"
                    , Commands.toggleTextBlock onParams offParams convertToPlainText |> Command.transform
                    )
                    spec
                    model.editor
                )
    }


wrapAsBlockQuote : Model -> Model
wrapAsBlockQuote model =
    let
        spec =
            Editor.spec config
    in
    { model
        | editor =
            Result.withDefault model.editor
                (Editor.apply
                    ( "wrapBlockquote"
                    , Commands.wrap
                        (\n -> n)
                        (Element.element Definitions.blockquote [])
                        |> Command.transform
                    )
                    spec
                    model.editor
                )
    }


insertHorizontalRule : Model -> Model
insertHorizontalRule model =
    let
        spec =
            Editor.spec config
    in
    { model
        | editor =
            Result.withDefault model.editor
                (Editor.apply
                    ( "insertHR"
                    , Commands.insertBlock
                        (ModelNode.block
                            (Element.element
                                Definitions.horizontalRule
                                []
                            )
                            ModelNode.Leaf
                        )
                        |> Command.transform
                    )
                    spec
                    model.editor
                )
    }


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.class "editor-container"
        , Html.Attributes.attribute "data-gramm_editor" "false"
        ]
        [ Editor.view config model.editor ]



--- Controls


type alias ControlContext =
    { hasInline : Bool
    , hasSelection : Bool
    , hasUndo : Bool
    , hasRedo : Bool
    , nodes : Set String
    , marks : Set String
    , canLift : Bool
    }


emptyControlState : ControlContext
emptyControlState =
    { hasUndo = False, hasRedo = False, hasInline = False, hasSelection = False, nodes = Set.empty, marks = Set.empty, canLift = False }


accumulateControlState : Node -> ControlContext -> ControlContext
accumulateControlState node controlState =
    case node of
        Node.Block n ->
            { controlState
                | nodes =
                    Set.insert (Element.name (ModelNode.element n)) controlState.nodes
            }

        Node.Inline inline ->
            let
                names =
                    List.map Mark.name (ModelNode.marks inline)
            in
            { controlState | hasInline = True, marks = Set.union (Set.fromList names) controlState.marks }


accumulateControlStateWithRanges : List ( Path, Path ) -> Block -> ControlContext -> ControlContext
accumulateControlStateWithRanges ranges root controlState =
    List.foldl
        (\( start, end ) cs ->
            Node.foldlRange start
                end
                accumulateControlState
                cs
                root
        )
        controlState
        ranges


getControlContext : Model -> ControlContext
getControlContext model =
    let
        state_ =
            Editor.state model.editor

        history_ =
            Editor.history model.editor
    in
    case State.selection state_ of
        Nothing ->
            emptyControlState

        Just selection ->
            let
                hasUndo =
                    History.peek history_ /= Nothing

                hasRedo =
                    List.isEmpty (History.redoList history_) |> not

                normalizedSelection =
                    Selection.normalize selection

                parentFocus =
                    ModelNode.parent (Selection.focusNode normalizedSelection)

                parentAnchor =
                    ModelNode.parent (Selection.anchorNode normalizedSelection)

                controlState =
                    accumulateControlStateWithRanges
                        [ ( Selection.anchorNode normalizedSelection, Selection.focusNode normalizedSelection )
                        , ( parentFocus, parentFocus )
                        , ( parentAnchor, parentAnchor )
                        ]
                        (State.root state_)
                        { emptyControlState | hasSelection = True }
            in
            { controlState
                | canLift =
                    -- This is hacky, but we'll assume we can lift anything that's nested
                    -- three or more nodes deep.
                    (List.length (Selection.anchorNode normalizedSelection) > 2)
                        || (List.length (Selection.focusNode normalizedSelection) > 2)
                        || Set.member "blockquote" controlState.nodes
                        || Set.member "li" controlState.nodes
                , hasUndo = hasUndo
                , hasRedo = hasRedo
            }



-- Markdown AST


type alias CustomInline =
    {}


type alias CustomBlock =
    {}


type alias MBlock =
    Block.Block CustomBlock CustomInline


type alias MInline =
    Inline.Inline CustomInline


markdownMarkOrder : MarkOrder
markdownMarkOrder =
    Mark.markOrderFromSpec Definitions.markdown



-- Convert markdown AST to RTE Toolkit blocks.


unwrapAndFilterChildNodes : List (Result String a) -> Result String (List a)
unwrapAndFilterChildNodes results =
    let
        unwrappedResults =
            List.filterMap
                (\x ->
                    case x of
                        Ok v ->
                            Just v

                        _ ->
                            Nothing
                )
                results
    in
    if List.length unwrappedResults == List.length results then
        Ok unwrappedResults

    else
        Err <|
            String.join "\n" <|
                List.filterMap
                    (\x ->
                        case x of
                            Err s ->
                                Just s

                            _ ->
                                Nothing
                    )
                    results


blockChildrenToMarkdown : Children -> Result String (List MBlock)
blockChildrenToMarkdown cn =
    case cn of
        ModelNode.BlockChildren a ->
            let
                results =
                    List.map blockToMarkdown (Array.toList (ModelNode.toBlockArray a))
            in
            unwrapAndFilterChildNodes results

        ModelNode.InlineChildren _ ->
            Err "Invalid child nodes, received inline, expected block"

        ModelNode.Leaf ->
            Err "Invalid child nodes, received leaf, expected block"


inlineChildrenToMarkdown : Children -> Result String (List MInline)
inlineChildrenToMarkdown cn =
    case cn of
        ModelNode.InlineChildren a ->
            let
                results =
                    List.map (inlineToMarkdown (ModelNode.toInlineArray a)) (Array.toList (ModelNode.toInlineTree a))
            in
            Result.map (List.concatMap identity) (unwrapAndFilterChildNodes results)

        ModelNode.BlockChildren _ ->
            Err "Invalid child nodes, was expected inline, received block"

        ModelNode.Leaf ->
            Err "Invalid child nodes, was expected inline, received leaf"


rootToMarkdown : Block -> Result String (List MBlock)
rootToMarkdown node =
    let
        children =
            ModelNode.childNodes node
    in
    blockChildrenToMarkdown children


imageToMarkdown : Element -> Result String MInline
imageToMarkdown parameters =
    let
        attributes =
            Element.attributes parameters

        alt =
            Attribute.findStringAttribute "alt" attributes
    in
    case Attribute.findStringAttribute "src" attributes of
        Nothing ->
            Err "No src attribute found"

        Just src ->
            Ok <| Inline.Image src alt []


inlineToMarkdown : Array Inline -> InlineTree -> Result String (List MInline)
inlineToMarkdown leaves tree =
    case tree of
        ModelNode.LeafNode i ->
            case Array.get i leaves of
                Nothing ->
                    Err "Invalid leaf tree"

                Just inlineLeaf ->
                    case inlineLeaf of
                        ModelNode.Text p ->
                            Ok <| [ Inline.Text (Text.text p) ]

                        ModelNode.InlineElement il ->
                            let
                                parameters =
                                    InlineElement.element il
                            in
                            case Element.name parameters of
                                "image" ->
                                    Result.map List.singleton (imageToMarkdown parameters)

                                "hard_break" ->
                                    Ok <| [ Inline.HardLineBreak ]

                                name ->
                                    Err <| "Unsupported inline leaf :" ++ name

        ModelNode.MarkNode m ->
            case unwrapAndFilterChildNodes <| List.map (inlineToMarkdown leaves) (Array.toList m.children) of
                Err s ->
                    Err s

                Ok children ->
                    let
                        flattenedChildren =
                            List.concatMap identity children
                    in
                    case Mark.name m.mark of
                        "bold" ->
                            Ok <| [ Inline.Emphasis 2 flattenedChildren ]

                        "italic" ->
                            Ok <| [ Inline.Emphasis 1 flattenedChildren ]

                        "code" ->
                            Ok <|
                                List.map
                                    (\x ->
                                        case x of
                                            Inline.Text s ->
                                                Inline.CodeInline s

                                            _ ->
                                                x
                                    )
                                    flattenedChildren

                        "link" ->
                            let
                                attributes =
                                    Mark.attributes m.mark

                                title =
                                    Attribute.findStringAttribute "title" attributes
                            in
                            case Attribute.findStringAttribute "href" attributes of
                                Nothing ->
                                    Err "Invalid link mark"

                                Just href ->
                                    Ok <| [ Inline.Link href title flattenedChildren ]

                        name ->
                            Err <| "Unsupported mark: " ++ name


textFromChildNodes : Children -> String
textFromChildNodes cn =
    case cn of
        ModelNode.InlineChildren il ->
            String.join "" <|
                Array.toList <|
                    Array.map
                        (\l ->
                            case l of
                                ModelNode.Text tl ->
                                    Text.text tl

                                ModelNode.InlineElement p ->
                                    if
                                        Element.name
                                            (InlineElement.element p)
                                            == "hard_break"
                                    then
                                        "\n"

                                    else
                                        ""
                        )
                        (ModelNode.toInlineArray il)

        _ ->
            ""


headingToMarkdown : Element -> Children -> Result String MBlock
headingToMarkdown p cn =
    let
        attributes =
            Element.attributes p

        level =
            Maybe.withDefault 1 (Attribute.findIntegerAttribute "level" attributes)
    in
    Result.map (Block.Heading "" level) (inlineChildrenToMarkdown cn)


codeBlockToMarkdown : Children -> Result String MBlock
codeBlockToMarkdown cn =
    let
        t =
            textFromChildNodes cn
    in
    Ok <| Block.CodeBlock Block.Indented t


listToMarkdown : Block.ListType -> Element -> Children -> Result String MBlock
listToMarkdown type_ parameters cn =
    let
        defaultDelimiter =
            case type_ of
                Block.Unordered ->
                    "*"

                Block.Ordered _ ->
                    "."

        delimiter =
            Maybe.withDefault defaultDelimiter <|
                Attribute.findStringAttribute
                    "delimiter"
                    (Element.attributes parameters)

        listItems =
            case cn of
                ModelNode.BlockChildren a ->
                    let
                        children =
                            Array.toList <| ModelNode.toBlockArray a
                    in
                    unwrapAndFilterChildNodes <|
                        List.map
                            (\x ->
                                blockChildrenToMarkdown (ModelNode.childNodes x)
                            )
                            children

                _ ->
                    Err <| "Invalid list items"
    in
    case listItems of
        Err s ->
            Err s

        Ok lis ->
            Ok <|
                Block.List
                    { type_ = type_
                    , indentLength = 3
                    , delimiter = delimiter
                    , isLoose = False
                    }
                    lis


blockToMarkdown : Block -> Result String MBlock
blockToMarkdown node =
    let
        parameters =
            ModelNode.element node

        children =
            ModelNode.childNodes node
    in
    case Element.name parameters of
        "paragraph" ->
            Result.map (Block.Paragraph "") (inlineChildrenToMarkdown children)

        "blockquote" ->
            Result.map Block.BlockQuote (blockChildrenToMarkdown children)

        "horizontal_rule" ->
            Ok Block.ThematicBreak

        "heading" ->
            headingToMarkdown parameters children

        "code_block" ->
            codeBlockToMarkdown children

        "unordered_list" ->
            listToMarkdown Block.Unordered parameters children

        "ordered_list" ->
            listToMarkdown (Block.Ordered 1) parameters children

        name ->
            Err ("Unexpected element: " ++ name)



-- Convert RTE Toolkit blocks to string formatted markdown.


markdownToString : List MBlock -> Result String String
markdownToString =
    blockMarkdownChildrenToString


escapeForMarkdown : String -> String
escapeForMarkdown s =
    s


inlineMarkdownToString : MInline -> Result String String
inlineMarkdownToString inline =
    case inline of
        Inline.Text s ->
            Ok <| escapeForMarkdown s

        Inline.HardLineBreak ->
            Ok "  \n"

        Inline.CodeInline s ->
            Ok <| "`" ++ s ++ "`"

        Inline.Link href title children ->
            Result.map
                (\c ->
                    let
                        t =
                            Maybe.withDefault "" <| Maybe.map (\m -> " \"" ++ m ++ "\"") title
                    in
                    "[" ++ c ++ "](" ++ href ++ t ++ ")"
                )
                (inlineMarkdownChildrenToString children)

        Inline.Image url alt children ->
            Result.map
                (\c ->
                    let
                        a =
                            Maybe.withDefault "" <| Maybe.map (\m -> " \"" ++ m ++ "\"") alt
                    in
                    "![" ++ c ++ "](" ++ url ++ a ++ ")"
                )
                (inlineMarkdownChildrenToString children)

        Inline.Emphasis length children ->
            let
                e =
                    String.repeat length "*"
            in
            Result.map (\c -> e ++ c ++ e) (inlineMarkdownChildrenToString children)

        Inline.HtmlInline _ _ _ ->
            Err "Html inline is not implemented."

        Inline.Custom _ _ ->
            Err "Custom elements are not implemented"


inlineMarkdownChildrenToString : List MInline -> Result String String
inlineMarkdownChildrenToString inlines =
    Result.map (String.join "") <|
        unwrapAndFilterChildNodes <|
            List.map inlineMarkdownToString inlines


blockMarkdownChildrenToString : List MBlock -> Result String String
blockMarkdownChildrenToString blocks =
    Result.map (String.join "\n") <|
        unwrapAndFilterChildNodes (List.map markdownBlockToString blocks)


indentEverythingButFirstLine : Int -> String -> String
indentEverythingButFirstLine n s =
    String.join "\n" <|
        List.indexedMap
            (\i x ->
                if i == 0 then
                    x

                else
                    String.repeat n " " ++ x
            )
            (String.split "\n" s)


listMarkdownToString : Block.ListBlock -> List (List MBlock) -> Result String String
listMarkdownToString listBlock listItems =
    Result.map
        (\children ->
            String.join "\n"
                (List.indexedMap
                    (\i z ->
                        let
                            prefix =
                                case listBlock.type_ of
                                    Block.Unordered ->
                                        listBlock.delimiter ++ " "

                                    Block.Ordered startIndex ->
                                        String.fromInt (startIndex + i) ++ listBlock.delimiter ++ " "
                        in
                        prefix ++ indentEverythingButFirstLine (String.length prefix) z
                    )
                    children
                )
        )
        (unwrapAndFilterChildNodes <|
            List.map blockMarkdownChildrenToString listItems
        )


markdownCodeBlockToString : Block.CodeBlock -> String -> Result String String
markdownCodeBlockToString cb s =
    case cb of
        Block.Fenced _ fence ->
            let
                delimeter =
                    String.repeat fence.fenceLength fence.fenceChar
            in
            Ok <|
                (delimeter ++ "\n")
                    ++ String.join "\n" (List.map (\v -> String.repeat fence.indentLength " " ++ v) (String.split "\n" s))
                    ++ ("\n" ++ delimeter)

        Block.Indented ->
            Ok <| String.join "\n" <| List.map (\v -> "    " ++ v) (String.split "\n" s)


markdownBlockToString : MBlock -> Result String String
markdownBlockToString block =
    case block of
        Block.BlankLine s ->
            Ok <| s

        Block.ThematicBreak ->
            Ok <| "---"

        Block.Heading _ i children ->
            Result.map
                (\x -> String.repeat i "#" ++ " " ++ x)
                (inlineMarkdownChildrenToString children)

        Block.CodeBlock cb s ->
            markdownCodeBlockToString cb s

        Block.Paragraph _ children ->
            Result.map (\x -> x ++ "\n") <|
                inlineMarkdownChildrenToString children

        Block.BlockQuote children ->
            Result.map
                (\x ->
                    String.join "\n" (List.map (\m -> "> " ++ m) (String.split "\n" x))
                )
                (blockMarkdownChildrenToString children)

        Block.List lb listItems ->
            listMarkdownToString lb listItems

        Block.PlainInlines children ->
            inlineMarkdownChildrenToString children

        Block.Custom _ _ ->
            Err "Custom element are not implemented"


markdownToBlock : List MBlock -> Result String Block
markdownToBlock md =
    Result.map
        (\children ->
            ModelNode.block
                (Element.element Definitions.doc [])
                children
        )
        (markdownBlockListToBlockChildNodes md)


markdownBlockListToBlockChildNodes : List MBlock -> Result String Children
markdownBlockListToBlockChildNodes blocks =
    Result.map
        (\items -> ModelNode.blockChildren (Array.fromList items))
        (markdownBlockListToBlockLeaves blocks)


markdownBlockListToBlockLeaves : List MBlock -> Result String (List Block)
markdownBlockListToBlockLeaves blocks =
    unwrapAndFilterChildNodes (List.map markdownBlockToEditorBlock blocks)


markdownInlineListToInlineChildNodes : List MInline -> Result String Children
markdownInlineListToInlineChildNodes inlines =
    Result.map
        (\items -> ModelNode.inlineChildren (Array.fromList items))
        (markdownInlineListToInlineLeaves [] inlines)


markdownInlineListToInlineLeaves : List Mark -> List MInline -> Result String (List Inline)
markdownInlineListToInlineLeaves marks inlines =
    Result.map
        (\items -> List.concatMap identity items)
        (unwrapAndFilterChildNodes (List.map (markdownInlineToInlineLeaves marks) inlines))


markdownInlineToInlineLeaves : List Mark -> MInline -> Result String (List Inline)
markdownInlineToInlineLeaves marks inline =
    case inline of
        Inline.Text s ->
            Ok <|
                [ ModelNode.markedText s (Mark.sort markdownMarkOrder marks) ]

        Inline.HardLineBreak ->
            Ok <|
                [ ModelNode.inlineElement (Element.element Definitions.hardBreak []) [] ]

        Inline.CodeInline s ->
            let
                codeMark =
                    Mark.mark Definitions.code []
            in
            Ok <| [ ModelNode.markedText s (Mark.sort markdownMarkOrder (codeMark :: marks)) ]

        Inline.Link href title children ->
            let
                linkMark =
                    Mark.mark Definitions.link
                        (List.filterMap identity
                            [ Just <| Attribute.StringAttribute "href" href
                            , Maybe.map (\t -> Attribute.StringAttribute "title" t) title
                            ]
                        )
            in
            markdownInlineListToInlineLeaves (linkMark :: marks) children

        Inline.Image src alt _ ->
            let
                inlineImage =
                    ModelNode.inlineElement
                        (Element.element Definitions.image
                            (List.filterMap identity
                                [ Just <| Attribute.StringAttribute "src" src
                                , Maybe.map (\t -> Attribute.StringAttribute "alt" t) alt
                                ]
                            )
                        )
                        (Mark.sort markdownMarkOrder marks)
            in
            Ok <| [ inlineImage ]

        Inline.Emphasis i children ->
            let
                emphasis =
                    case i of
                        1 ->
                            [ Mark.mark Definitions.italic [] ]

                        2 ->
                            [ Mark.mark Definitions.bold [] ]

                        3 ->
                            [ Mark.mark Definitions.bold [], Mark.mark Definitions.italic [] ]

                        _ ->
                            []
            in
            markdownInlineListToInlineLeaves (emphasis ++ marks) children

        Inline.HtmlInline _ _ _ ->
            Err "Not implemented"

        Inline.Custom _ _ ->
            Err "Not implemented"


markdownCodeBlockToEditorBlock : Block.CodeBlock -> String -> Result String Block
markdownCodeBlockToEditorBlock cb s =
    let
        attributes =
            case cb of
                Block.Indented ->
                    [ Attribute.StringAttribute "type" "indented" ]

                Block.Fenced b f ->
                    List.filterMap identity
                        [ Just <| Attribute.BoolAttribute "open" b
                        , Just <| Attribute.StringAttribute "type" "fenced"
                        , Just <| Attribute.IntegerAttribute "indentLength" f.indentLength
                        , Just <| Attribute.IntegerAttribute "fenceLength" f.fenceLength
                        , Maybe.map (\m -> Attribute.StringAttribute "language" m) f.language
                        ]
    in
    Ok <|
        ModelNode.block
            (Element.element Definitions.codeBlock attributes)
            (ModelNode.inlineChildren <| Array.fromList [ ModelNode.plainText s ])


markdownListToEditorBlock : Block.ListBlock -> List (List MBlock) -> Result String Block
markdownListToEditorBlock lb children =
    let
        ( node, typeAttributes ) =
            case lb.type_ of
                Block.Ordered i ->
                    ( Definitions.orderedList, [ Attribute.IntegerAttribute "startIndex" i ] )

                Block.Unordered ->
                    ( Definitions.unorderedList, [] )

        attributes =
            [ Attribute.IntegerAttribute "indentLength" lb.indentLength
            , Attribute.StringAttribute "delimiter" lb.delimiter
            ]
                ++ typeAttributes
    in
    Result.map
        (\listItems ->
            ModelNode.block
                (Element.element node attributes)
                (ModelNode.blockChildren
                    (Array.fromList
                        (List.map
                            (\cn ->
                                ModelNode.block
                                    (Element.element Definitions.listItem [])
                                    cn
                            )
                            listItems
                        )
                    )
                )
        )
        (unwrapAndFilterChildNodes
            (List.map
                (\x -> markdownBlockListToBlockChildNodes x)
                children
            )
        )


markdownInlineToParagraphBlock : List MInline -> Result String Block
markdownInlineToParagraphBlock children =
    Result.map
        (\c ->
            ModelNode.block
                (Element.element Definitions.paragraph [])
                c
        )
        (markdownInlineListToInlineChildNodes children)


markdownBlockToEditorBlock : MBlock -> Result String Block
markdownBlockToEditorBlock mblock =
    case mblock of
        Block.BlankLine s ->
            Ok <|
                ModelNode.block
                    (Element.element Definitions.paragraph [])
                    (ModelNode.inlineChildren <| Array.fromList [ ModelNode.plainText s ])

        Block.ThematicBreak ->
            Ok <|
                ModelNode.block
                    (Element.element Definitions.horizontalRule [])
                    ModelNode.Leaf

        Block.Heading _ i children ->
            Result.map
                (\c ->
                    ModelNode.block
                        (Element.element
                            Definitions.heading
                            [ Attribute.IntegerAttribute "level" i ]
                        )
                        c
                )
                (markdownInlineListToInlineChildNodes children)

        Block.CodeBlock cb s ->
            markdownCodeBlockToEditorBlock cb s

        Block.Paragraph _ children ->
            markdownInlineToParagraphBlock children

        Block.BlockQuote children ->
            Result.map
                (\c ->
                    ModelNode.block
                        (Element.element Definitions.blockquote [])
                        c
                )
                (markdownBlockListToBlockChildNodes children)

        Block.List lb listItems ->
            markdownListToEditorBlock lb listItems

        Block.PlainInlines children ->
            markdownInlineToParagraphBlock children

        Block.Custom _ _ ->
            Err "Custom elements are not implemented"
