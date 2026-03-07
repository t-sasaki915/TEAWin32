#include "VirtualDOM.h"
#include "Cache.h"
#include "DPIAware.h"
#include "Event.h"
#include "Registry.h"
#include "TEAWin32.h"

#include <commctrl.h>
#include <stdio.h>
#include <windows.h>

void ExecuteRenderProcedure(RenderProcedure *procedure, HDWP hdwp)
{
    DEBUG_LOG(
        L"Processing RenderProcedure for UniqueId %d. ProcedureType: %d.",
        procedure->targetUniqueId,
        procedure->procType);

    HWND targetHWND = NULL;
    if (procedure->procType != RENDER_PROC_TYPE_CREATE_WINDOW && procedure->procType != RENDER_PROC_TYPE_CREATE_BUTTON)
    {
        targetHWND = GetHWNDFromUniqueId(procedure->targetUniqueId);
    }

    switch (procedure->procType)
    {
        case RENDER_PROC_TYPE_CREATE_WINDOW: {
            CreateWindowData data = procedure->procData.createWindowData;

            LPCWSTR className = GetCachedClassName(data.newWindowClassName);

            HWND parentHWND = NULL;
            if (data.newWindowParentUniqueId != 0)
            {
                parentHWND = GetHWNDFromUniqueId(data.newWindowParentUniqueId);
            }

            HWND newWindow = CreateWindowExW(
                data.newWindowExStyles,
                className,
                L"",
                WS_CLIPCHILDREN | WS_CLIPSIBLINGS | data.newWindowStyles,
                CW_USEDEFAULT,
                CW_USEDEFAULT,
                CW_USEDEFAULT,
                CW_USEDEFAULT,
                parentHWND,
                NULL,
                TEAWIN32_MAIN_INSTANCE,
                0);

            if (newWindow == NULL)
            {
                NotifyFatalError(L"CreateWindowExW failed.", L"ExecuteRenderProcedure (VirtualDOM.c)");
                return;
            }

            RegisterHWNDToRegistry(newWindow, procedure->targetUniqueId);

            TEAWIN32_ACTIVE_WINDOW_COUNT++;

            ShowWindow(newWindow, SW_SHOW);
            UpdateWindow(newWindow);

            DEBUG_LOG(
                L"Created Window. Class: %ls, Parent: %d, ExStyles: %d, Styles: %d.",
                data.newWindowClassName,
                data.newWindowParentUniqueId,
                data.newWindowExStyles,
                data.newWindowStyles);

            break;
        }
        case RENDER_PROC_TYPE_CREATE_BUTTON: {
            HWND newButton = CreateWindowW(
                L"BUTTON",
                L"",
                WS_TABSTOP | WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON | WS_CLIPSIBLINGS,
                CW_USEDEFAULT,
                CW_USEDEFAULT,
                CW_USEDEFAULT,
                CW_USEDEFAULT,
                GetHWNDFromUniqueId(procedure->procData.newButtonParentUniqueId),
                NULL,
                TEAWIN32_MAIN_INSTANCE,
                0);

            if (newButton == NULL)
            {
                NotifyFatalError(L"CreateWindowW failed.", L"ExecuteRenderProcedure (VirtualDOM.c)");
                return;
            }

            SetWindowSubclass(newButton, SubclassWndProc, (UINT_PTR)procedure->targetUniqueId, 0);

            RegisterHWNDToRegistry(newButton, procedure->targetUniqueId);

            DEBUG_LOG(L"Created Button. Parent: %d", procedure->procData.newButtonParentUniqueId);

            break;
        }
        case RENDER_PROC_TYPE_UPDATE_TEXT: {
            if (targetHWND == NULL)
            {
                NotifyFatalError(L"targetHWND was NULL", L"ExecuteRenderProcedure (VirtualDOM.c)");
                break;
            }

            SetWindowTextW(targetHWND, procedure->procData.newComponentText);

            DEBUG_LOG(
                L"Updated Window Text of HWND %p (UniqueId %d): %ls",
                (void *)targetHWND,
                procedure->targetUniqueId,
                procedure->procData.newComponentText);

            break;
        }
        case RENDER_PROC_TYPE_UPDATE_POS: {
            if (targetHWND == NULL)
            {
                NotifyFatalError(L"targetHWND was NULL", L"ExecuteRenderProcedure (VirtualDOM.c)");
                break;
            }

            UpdatePosData updatePosData = procedure->procData.updatePosData;

            int x = updatePosData.hasNewLocation ? ResolvePixelForHWND(updatePosData.newX, targetHWND) : 0;
            int y = updatePosData.hasNewLocation ? ResolvePixelForHWND(updatePosData.newY, targetHWND) : 0;
            int w = updatePosData.hasNewSize ? ResolvePixelForHWND(updatePosData.newWidth, targetHWND) : 0;
            int h = updatePosData.hasNewSize ? ResolvePixelForHWND(updatePosData.newHeight, targetHWND) : 0;

            DWORD flags = SWP_NOACTIVATE;
            if (!updatePosData.hasNewLocation)
            {
                flags = flags | SWP_NOMOVE;
            }
            if (!updatePosData.hasNewSize)
            {
                flags = flags | SWP_NOSIZE;
            }
            if (!updatePosData.bringComponentToFront)
            {
                flags = flags | SWP_NOZORDER;
            }

            if (hdwp == NULL)
            {
                NotifyFatalError(L"hdwp was NULL", L"ExecuteRenderProcedure (VirtualDOM.c)");
                break;
            }

            hdwp = DeferWindowPos(hdwp, targetHWND, HWND_TOP, x, y, w, h, flags);

            DEBUG_LOG(
                L"Updated Window Pos of HWND %p (UniqueId %d): X: %d, Y: %d, W: %d, H: %d, Front: %d",
                (void *)targetHWND,
                procedure->targetUniqueId,
                x,
                y,
                w,
                h,
                updatePosData.bringComponentToFront);

            break;
        }
        case RENDER_PROC_TYPE_UPDATE_FONT: {
            if (targetHWND == NULL)
            {
                NotifyFatalError(L"targetHWND was NULL", L"ExecuteRenderProcedure (VirtualDOM.c)");
                break;
            }

            CachedFont cacheKey = procedure->procData.newFontCacheKey;
            cacheKey.absoluteFontSize = ResolvePointForHWND(cacheKey.fontSize, targetHWND);

            SendMessageW(targetHWND, WM_SETFONT, (WPARAM)GetCachedFont(&cacheKey), 1);

            DEBUG_LOG(
                L"Updated Font of HWND %p (UniqueId %d): Size %d, Italic %d, Underline %d, StrikeOut %d",
                (void *)targetHWND,
                procedure->targetUniqueId,
                cacheKey.fontName,
                cacheKey.absoluteFontSize,
                cacheKey.isItalic,
                cacheKey.isUnderline,
                cacheKey.isStrikeOut);

            break;
        }
        case RENDER_PROC_TYPE_UPDATE_ICON: {
            if (targetHWND == NULL)
            {
                NotifyFatalError(L"targetHWND was NULL", L"ExecuteRenderProcedure (VirtualDOM.c)");
                break;
            }

            CachedIcon cacheKey = procedure->procData.newIconCacheKey;
            cacheKey.dpi = GetDPI(targetHWND);

            SendMessageW(targetHWND, WM_SETICON, 1, (LPARAM)GetCachedIcon(&cacheKey));

#ifdef TEAWIN32_DEBUG_MODE
            if (cacheKey.iconType == STOCK_ICON)
            {
                DEBUG_LOG(
                    L"Updated Icon of HWND %p (UniqueId %d): Stock Icon Id: %d, DPI: %d",
                    (void *)targetHWND,
                    procedure->targetUniqueId,
                    cacheKey.iconId.stockIconId,
                    cacheKey.dpi);
            }
            else
            {
                if (IS_INTRESOURCE(cacheKey.iconId.stockIconId))
                {
                    DEBUG_LOG(
                        L"Updated Icon of HWND %p (UniqueId %d): Resource Icon Id: %d, DPI: %d",
                        (void *)targetHWND,
                        procedure->targetUniqueId,
                        cacheKey.iconId.stockIconId,
                        cacheKey.dpi);
                }
                else
                {
                    DEBUG_LOG(
                        L"Updated Icon of HWND %p (UniqueId %d): Resource Icon Id: %ls, DPI: %d",
                        (void *)targetHWND,
                        procedure->targetUniqueId,
                        cacheKey.iconId.stockIconId,
                        cacheKey.dpi);
                }
            }
#endif

            break;
        }
        case RENDER_PROC_TYPE_UPDATE_CURSOR: {
            if (targetHWND == NULL)
            {
                NotifyFatalError(L"targetHWND was NULL", L"ExecuteRenderProcedure (VirtualDOM.c)");
                break;
            }

            SetClassLongPtrW(
                targetHWND,
                GCLP_HCURSOR,
                (LONG_PTR)GetCachedCursor(&procedure->procData.newCursorCacheKey));

#ifdef TEAWIN32_DEBUG_MODE
            if (IS_INTRESOURCE(procedure->procData.newCursorCacheKey.cursorKey))
            {
                DEBUG_LOG(
                    L"Updated Cursor of HWND %p (UniqueId %d): %d",
                    (void *)targetHWND,
                    procedure->targetUniqueId,
                    procedure->procData.newCursorCacheKey);
            }
            else
            {
                DEBUG_LOG(
                    L"Updated Cursor of HWND %p (UniqueId %d): %ls",
                    (void *)targetHWND,
                    procedure->targetUniqueId,
                    procedure->procData.newCursorCacheKey);
            }
#endif

            break;
        }
        case RENDER_PROC_TYPE_UPDATE_BACKGROUND_COLOUR: {
            if (targetHWND == NULL)
            {
                NotifyFatalError(L"targetHWND was NULL", L"ExecuteRenderProcedure (VirtualDOM.c)");
                break;
            }

            DEBUG_LOG(L"TODO: implement RENDER_PROC_TYPE_UPDATE_BACKGROUND_COLOUR.");

            break;
        }
        case RENDER_PROC_TYPE_DESTROY_COMPONENT: {
            if (targetHWND == NULL)
            {
                NotifyFatalError(L"targetHWND was NULL", L"ExecuteRenderProcedure (VirtualDOM.c)");
                break;
            }

            DestroyWindow(targetHWND);

            DEBUG_LOG(L"Destroyed HWND %p (UniqueId %d)", (void *)targetHWND, procedure->targetUniqueId);

            break;
        }
    }
}

void ExecuteRenderProcedures(RenderProcedure *procedures, int procedureCount, int updatePosNumber)
{
    DEBUG_LOG(L"Executing RenderProcedures.");

    HDWP hdwp = NULL;
    if (updatePosNumber > 0)
    {
        DEBUG_LOG(L"updatePosNumber > 0, using DeferWindowPos.");

        hdwp = BeginDeferWindowPos(updatePosNumber);
    }

    for (int i = 0; i < procedureCount; i++)
    {
        ExecuteRenderProcedure(&procedures[i], hdwp);
    }

    if (updatePosNumber > 0)
    {
        if (hdwp == NULL)
        {
            NotifyFatalError(L"hdwp is NULL", L"ExecuteRenderProcedures (VirtualDOM.c)");
        }

        EndDeferWindowPos(hdwp);
    }

    DEBUG_LOG(L"Executed RenderProcedures.");
}

void RequestRender(RenderProcedure *procedures, int procedureCount, int updatePosNumber)
{
    DEBUG_LOG(L"Received RenderProcedures from Haskell. Requesting render.");

    int size = procedureCount * sizeof(RenderProcedure);

    RenderProcedure *permanentProcedures = (RenderProcedure *)malloc(size);
    if (permanentProcedures == NULL)
    {
        NotifyFatalError(L"malloc Failed.", L"RequestRender (VirtualDOM.c)");
        return;
    }

    memcpy(permanentProcedures, procedures, size);

    LPARAM lParam = ((LPARAM)updatePosNumber << 32) | (unsigned int)procedureCount;

    if (!PostMessageW(TEAWIN32_MANAGEMENT_HWND, WM_TEAWIN32_RENDER_REQUEST, (WPARAM)permanentProcedures, lParam))
    {
        free(permanentProcedures);
        NotifyFatalError(L"Failed to post WM_TEAWIN32_RENDER_REQUEST", L"RenderRequest (VirtualDOM.c)");
        return;
    }

    DEBUG_LOG(L"Requested render.");
}
