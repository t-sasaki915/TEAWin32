module TEAWin32.Core.VirtualDOM () where

import           Data.Maybe                     (fromJust)
import           Data.Typeable                  (cast)
import qualified TEAWin32.Core.Native.Constants as Native
import           TEAWin32.Core.Types

getTargetUniqueId :: CCallRequest -> Maybe UniqueId
getTargetUniqueId (CreateWindowRequest _)        = Nothing
getTargetUniqueId (CreateButtonRequest _)        = Nothing
getTargetUniqueId (DestroyComponentRequest u)    = Just u
getTargetUniqueId (UpdateTextRequest u _)        = Just u
getTargetUniqueId (UpdatePosRequest u _)         = Just u
getTargetUniqueId (UpdateFontRequest u _)        = Just u
getTargetUniqueId (UpdateIconRequest u _)        = Just u
getTargetUniqueId (UpdateCursorRequest u _)      = Just u
getTargetUniqueId (InvalidateRectFullyRequest u) = Just u
getTargetUniqueId (ShowWindowRequest u)          = Just u

optimiseCCallRequests :: [CCallRequest] -> [CCallRequest]
optimiseCCallRequests requests = []

diffGUIComponent :: Maybe GUIComponent -> Maybe GUIComponent -> Maybe UniqueId -> [CCallRequest]
diffGUIComponent (Just newComponent) Nothing maybeParent             = renderGUIComponent newComponent maybeParent
diffGUIComponent Nothing (Just oldComponent) _                       = destroyGUIComponent oldComponent
diffGUIComponent (Just newComponent) (Just oldComponent) maybeParent = []
diffGUIComponent Nothing Nothing _                                   = []

diffGUIComponentProperty :: Maybe GUIComponentProperty -> Maybe GUIComponentProperty -> UniqueId -> [CCallRequest]
diffGUIComponentProperty (Just newProperty) Nothing parent            = applyGUIComponentProperty newProperty parent
diffGUIComponentProperty Nothing (Just oldProperty) parent            = unapplyGUIComponentProperty oldProperty parent
diffGUIComponentProperty (Just newProperty) (Just oldProperty) parent = []
diffGUIComponentProperty Nothing Nothing _                            = []

renderGUIComponent :: GUIComponent -> Maybe UniqueId -> [CCallRequest]
renderGUIComponent (GUIComponent c) maybeParent
    | Just (Window uniqueId className windowStyles properties children) <- cast c = do
        let createWindowReq = CreateWindowRequest $ CreateWindowReq
                { newWindowUniqueId       = uniqueId
                , newWindowClassName      = className
                , newWindowExStyles       = 0 -- TODO
                , newWindowStyles         = 0 -- TODO
                , newWindowParentUniqueId = maybeParent
                }

            propertyReqs = concatMap ((`applyGUIComponentProperty` uniqueId) . GUIComponentProperty) properties

            childrenReqs = concatMap (`renderGUIComponent` Just uniqueId) children

        createWindowReq : propertyReqs ++ childrenReqs

    | Just (Button uniqueId properties) <- cast c = do
        let createButtonReq = CreateButtonRequest $ CreateButtonReq
                { newButtonUniqueId       = uniqueId
                , newButtonParentUniqueId = fromJust maybeParent -- TODO
                }

            propertyReqs = concatMap ((`applyGUIComponentProperty` uniqueId) . GUIComponentProperty) properties

        createButtonReq : propertyReqs

    | otherwise =
        []

destroyGUIComponent :: GUIComponent -> [CCallRequest]
destroyGUIComponent component = [DestroyComponentRequest (getUniqueId component)]

applyGUIComponentProperty :: GUIComponentProperty -> UniqueId -> [CCallRequest]
applyGUIComponentProperty (GUIComponentProperty p) componentUniqueId
    | Just (ComponentTitle title) <- cast p =
        [UpdateTextRequest componentUniqueId title]

    | Just (ComponentSize size) <- cast p =
        let req = UpdatePosReq
                { newLocation           = Nothing
                , newSize               = Just size
                , bringComponentToFront = False
                }
        in
        [UpdatePosRequest componentUniqueId req]

    | Just (ComponentPosition pos) <- cast p =
        let req = UpdatePosReq
                { newLocation           = Just pos
                , newSize               = Nothing
                , bringComponentToFront = False
                }
        in
        [UpdatePosRequest componentUniqueId req]

    | Just (ComponentFont font) <- cast p =
        [UpdateFontRequest componentUniqueId font]

    | Just (WindowIcon icon) <- cast p =
        [UpdateIconRequest componentUniqueId icon]

    | Just (WindowCursor cursor) <- cast p =
        [UpdateCursorRequest componentUniqueId cursor]

    | Just (WindowBackgroundColour colour) <- cast p =
        [] -- TODO

    | otherwise =
        []

unapplyGUIComponentProperty :: GUIComponentProperty -> UniqueId -> [CCallRequest]
unapplyGUIComponentProperty (GUIComponentProperty p) componentUniqueId
    | Just (ComponentTitle _) <- cast p =
        [UpdateTextRequest componentUniqueId ""]

    | Just (ComponentSize _) <- cast p =
        let req = UpdatePosReq
                { newLocation           = Nothing
                , newSize               = Just (raw Native.const_CW_USEDEFAULT, raw Native.const_CW_USEDEFAULT)
                , bringComponentToFront = False
                }
        in
        [UpdatePosRequest componentUniqueId req]

    | Just (ComponentPosition _) <- cast p =
        let req = UpdatePosReq
                { newLocation           = Just (raw Native.const_CW_USEDEFAULT, raw Native.const_CW_USEDEFAULT)
                , newSize               = Nothing
                , bringComponentToFront = False
                }
        in
        [UpdatePosRequest componentUniqueId req]

    | Just (ComponentFont font) <- cast p =
        [UpdateFontRequest componentUniqueId font] -- TODO

    | Just (WindowIcon _) <- cast p =
        [UpdateIconRequest componentUniqueId IconApplication]

    | Just (WindowCursor _) <- cast p =
        [UpdateCursorRequest componentUniqueId CursorArrow]

    | Just (WindowBackgroundColour _) <- cast p =
        [] -- TODO

    | otherwise =
        []
