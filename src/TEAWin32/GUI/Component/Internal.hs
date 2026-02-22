module TEAWin32.GUI.Component.Internal
    ( sortComponentsWithZIndex
    , resolveScalableValueForHWND
    , updateComponentDPI
    , getRelativeRectFromHWNDUsingWin32
    , setComponentTitle
    , setComponentFont
    , useDefaultFont
    , setWindowIcon
    , setWindowCursor
    , requestRedraw
    , destroyChildren
    , destroyComponent
    ) where

import           Control.Concurrent                       (modifyMVar)
import           Control.Monad                            (filterM, forM,
                                                           unless, when)
import           Data.Functor                             (void, (<&>))
import qualified Data.List                                as List
import           Data.Map.Strict                          ((!))
import qualified Data.Map.Strict                          as Map
import           Data.Text                                (Text)
import qualified Data.Text                                as Text
import           Foreign                                  hiding (new, void)
import           GHC.Stack                                (HasCallStack)
import qualified Graphics.Win32                           as Win32
import           TEAWin32.Exception                       (TEAWin32Error (..),
                                                           errorTEAWin32)
import           TEAWin32.GUI
import           TEAWin32.GUI.Component                   (ComponentType (..),
                                                           GUIComponent,
                                                           IsGUIComponent (..),
                                                           ZIndex (..))
import           TEAWin32.GUI.Component.ComponentRegistry
import           TEAWin32.GUI.Component.Property
import qualified TEAWin32.GUI.Internal                    as GUIInternal
import qualified TEAWin32.Internal.Foreign                as Win32

sortComponentsWithZIndex :: HasCallStack => [GUIComponent] -> Maybe Win32.HWND -> IO [GUIComponent]
sortComponentsWithZIndex guiComponents maybeParent = do
    children <- case maybeParent of
        Just parent' -> GUIInternal.withImmediateChildWindows parent' pure
        Nothing      -> GUIInternal.withTopLevelWindows pure >>= filterM isComponentManaged

    uniqueIdsWithZIndex <- Map.fromList <$> forM (zip [1..] children) (\(i, hwnd) ->
        getComponentRegistryEntryValue ComponentUniqueIdRegKey hwnd >>= \uniqueId ->
            pure (uniqueId, i))

    let guiComponentMap = Map.fromList [ (getUniqueId comp, comp) | comp <- guiComponents ]

    componentsUniqueIdWithZIndex <- forM (zip [1..] guiComponents) $ \(listIndex, guiComponent) -> do
        let uniqueId      = getUniqueId guiComponent
            maybeSysIndex = Map.lookup uniqueId uniqueIdsWithZIndex
            maybeUsrIndex = case [ usrIndex | Just (ComponentZIndex usrIndex) <- map getZIndexProperty (getProperties guiComponent) ] of
                [ usrIndex ] -> Just usrIndex
                []           -> Nothing
                x            -> errorTEAWin32 $ InternalTEAWin32Error $ "Illegal ComponentZIndex state: " <> Text.show x

        case (maybeUsrIndex, maybeSysIndex) of
            (Just usrIndex, Just sysIndex) -> pure (ZIndexWithUserSpecification usrIndex sysIndex, uniqueId)
            (Nothing      , Just sysIndex) -> pure (SystemCalculatedZIndex sysIndex, uniqueId)
            (Just usrIndex, Nothing      ) -> pure (NewlyCreatedZIndexWithUserSpecification usrIndex listIndex, uniqueId)
            (Nothing      , Nothing      ) -> pure (NewlyCreatedZIndex listIndex, uniqueId)

    pure [ guiComponentMap ! uid | (_, uid) <- List.sort componentsUniqueIdWithZIndex ]

resolveScalableValueForHWND :: HasCallStack => Win32.HWND -> ScalableValue -> IO Int
resolveScalableValueForHWND _ (RawValue x) = pure (round x)
resolveScalableValueForHWND hwnd (ScalableValue x) =
    getComponentRegistryEntryValue ComponentCurrentDPIRegKey hwnd >>= \currentDpi ->
        pure (round (x * fromIntegral currentDpi / 96.0))

updateComponentDPI :: HasCallStack => Win32.HWND -> Int -> IO ()
updateComponentDPI hwnd newDPI = do
    updateComponentRegistryEntry ComponentCurrentDPIRegKey (ComponentCurrentDPIReg newDPI) hwnd

    GUIInternal.withImmediateChildWindows hwnd $ mapM_ $ \child -> do
        isManaged <- isComponentManaged child

        when isManaged $ do
            whenComponentHasRegistryKey ComponentFontRegKey child $ \font ->
                updateProperty (ComponentFont font) (ComponentFont font) child

            getComponentRegistryEntryValue ComponentTypeRegKey child >>= \case
                ComponentWindow ->
                    pure ()

                _ -> do
                    whenComponentHasRegistryKey ComponentSizeRegKey child $ \size ->
                        updateProperty (ComponentSize size) (ComponentSize size) child

                    whenComponentHasRegistryKey ComponentPositionRegKey child $ \position ->
                        updateProperty (ComponentPosition position) (ComponentPosition position) child

getRelativeRectFromHWNDUsingWin32 :: Win32.HWND -> IO (Int, Int, Int, Int)
getRelativeRectFromHWNDUsingWin32 hwnd = do
    (l', t', r', b') <- Win32.getWindowRect hwnd
    let (l, t, r, b) = (fromIntegral l', fromIntegral t', fromIntegral r', fromIntegral b')

    GUIInternal.isTopLevelWindow hwnd >>= \case
        True  -> pure (l, t, r - l, b - t)
        False -> do
            parentHWND <- Win32.getParent hwnd
            (x, y) <- Win32.screenToClient parentHWND (fromIntegral l, fromIntegral t)

            pure (fromIntegral x, fromIntegral y, r - l, b - t)

setComponentTitle :: Text -> Win32.HWND -> IO ()
setComponentTitle title hwnd = Win32.setWindowText hwnd (Text.unpack title)

setComponentFont :: HasCallStack => Font -> Win32.HWND -> IO ()
setComponentFont DefaultGUIFont hwnd =
    Win32.getStockFont Win32.dEFAULT_GUI_FONT >>= \font ->
        setComponentFont' font hwnd

setComponentFont SystemFont hwnd =
    Win32.getStockFont Win32.sYSTEM_FONT >>= \font ->
        setComponentFont' font hwnd

setComponentFont font@(Font fontName fontSize) hwnd =
    modifyMVar GUIInternal.fontCacheRef $ \fontCache ->
        case Map.lookup font fontCache of
            Just hndl ->
                setComponentFont' hndl hwnd >>
                    pure (fontCache, ())
            Nothing ->
                resolveScalableValueForHWND hwnd fontSize >>= \fontSize' ->
                    Win32.createFont (fromIntegral fontSize') 0 0 0 Win32.fW_NORMAL False False False Win32.dEFAULT_CHARSET
                        Win32.oUT_DEFAULT_PRECIS Win32.cLIP_DEFAULT_PRECIS Win32.dEFAULT_QUALITY
                            (Win32.fIXED_PITCH .|. Win32.fF_DONTCARE) (Text.unpack fontName) >>= \fontHandle ->
                                setComponentFont' fontHandle hwnd >>
                                    pure (Map.insert font fontHandle fontCache, ())

setComponentFont' :: Win32.HANDLE -> Win32.HWND -> IO ()
setComponentFont' font hwnd =
    void $ Win32.sendMessage hwnd Win32.wM_SETFONT (fromIntegral $ ptrToWordPtr font) 1

useDefaultFont :: HasCallStack => Win32.HWND -> IO ()
useDefaultFont = setComponentFont DefaultGUIFont

setWindowIcon :: Icon -> Win32.HWND -> IO ()
setWindowIcon icon hwnd =
    toWin32Icon icon >>= \icon' ->
        void $ Win32.c_SetClassLongPtr hwnd Win32.gCLP_HICON icon'

setWindowCursor :: Cursor -> Win32.HWND -> IO ()
setWindowCursor cursor hwnd =
    toWin32Cursor cursor >>= \cursor' ->
        void $ Win32.c_SetClassLongPtr hwnd Win32.gCLP_HCURSOR cursor'

requestRedraw :: Win32.HWND -> IO ()
requestRedraw hwnd =
    Win32.invalidateRect (Just hwnd) Nothing True

destroyChildren :: Win32.HWND -> IO ()
destroyChildren hwnd =
    GUIInternal.withImmediateChildWindows hwnd (mapM_ destroyComponent)

destroyComponent :: Win32.HWND -> IO ()
destroyComponent hwnd = do
    isWindow <- getComponentRegistryEntryValue ComponentTypeRegKey hwnd <&> (== ComponentWindow)

    unless isWindow $
        unregisterComponentFromRegistry hwnd

    Win32.destroyWindow hwnd
