module TEAWin32.Core.VirtualDOM (optimiseRenderProcedures) where

import           Control.Applicative ((<|>))
import           Data.Maybe          (listToMaybe, mapMaybe)
import           TEAWin32.Core.Types

combineRenderProcedure :: (UniqueId, RenderProcedure) -> (UniqueId, RenderProcedure) -> Maybe (UniqueId, RenderProcedure)
combineRenderProcedure newProc@(uniqueId1, SetComponentText _) (uniqueId2, SetComponentText _)
    | uniqueId1 == uniqueId2 = Just newProc
    | otherwise              = Nothing
combineRenderProcedure (uniqueId1, SetComponentPos newPos newSize newBringCompFrt) (uniqueId2, SetComponentPos oldPos oldSize oldBringCompFrt)
    | uniqueId1 == uniqueId2 = Just (uniqueId1, SetComponentPos (newPos <|> oldPos) (newSize <|> oldSize) (newBringCompFrt || oldBringCompFrt))
    | otherwise              = Nothing
combineRenderProcedure newProc@(uniqueId1, SetComponentFont _) (uniqueId2, SetComponentFont _)
    | uniqueId1 == uniqueId2 = Just newProc
    | otherwise              = Nothing
combineRenderProcedure newProc@(uniqueId1, SetComponentIcon _) (uniqueId2, SetComponentIcon _)
    | uniqueId1 == uniqueId2 = Just newProc
    | otherwise              = Nothing
combineRenderProcedure newProc@(uniqueId1, SetComponentCursor _) (uniqueId2, SetComponentCursor _)
    | uniqueId1 == uniqueId2 = Just newProc
    | otherwise              = Nothing
combineRenderProcedure newProc@(uniqueId1, SetComponentBackgroundColour _) (uniqueId2, SetComponentBackgroundColour _)
    | uniqueId1 == uniqueId2 = Just newProc
    | otherwise              = Nothing
combineRenderProcedure _ _   = Nothing

-- TODO VERY SLOW
optimiseRenderProcedures :: [(UniqueId, RenderProcedure)] -> [(UniqueId, RenderProcedure)]
optimiseRenderProcedures = reverse . foldl append []
    where
        append :: [(UniqueId, RenderProcedure)] -> (UniqueId, RenderProcedure) -> [(UniqueId, RenderProcedure)]
        append lst newItem =
            case listToMaybe (mapMaybe (\item ->  (,) item <$> combineRenderProcedure newItem item) lst) of
                Just (oldItem, combined) -> combined : filter (/= oldItem) lst
                Nothing                  -> newItem : lst
