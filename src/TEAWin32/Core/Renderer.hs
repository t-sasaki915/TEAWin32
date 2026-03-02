module TEAWin32.Core.Renderer () where

import           Data.Maybe          (fromJust)
import           Data.Typeable       (cast)
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

            childrenReqs = concatMap (`renderGUIComponent` Just uniqueId) children

        -- TODO apply properties

        createWindowReq : childrenReqs

    | Just (Button uniqueId properties) <- cast c = do
        let createButtonReq = CreateButtonRequest $ CreateButtonReq
                { newButtonUniqueId       = uniqueId
                , newButtonParentUniqueId = fromJust maybeParent -- TODO
                }

        -- TODO apply properties

        [createButtonReq]

    | otherwise =
        error "" -- TODO
