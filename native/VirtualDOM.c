#include "VirtualDOM.h"
#include "Cache.h"
#include "DPIAware.h"
#include "Event.h"
#include "Registry.h"
#include "TEAWin32.h"

#include <commctrl.h>
#include <stdio.h>
#include <windows.h>

void ExecuteRenderProcedure(RenderProcedure *procedure, HDWP *hdwp)
{
    HWND targetHWND = GetHWNDFromUniqueId(procedure->targetUniqueId);

    DEBUG_LOG(
        L"Processing RenderProcedure for HWND %p (UniqueId %d). ProcedureType: %d.",
        (void *)targetHWND,
        procedure->targetUniqueId,
        procedure->procType);

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

            SetWindowSubclass(targetHWND, SubclassWndProc, (UINT_PTR)procedure->targetUniqueId, 0);

            RegisterHWNDToRegistry(newButton, procedure->targetUniqueId);

            DEBUG_LOG(L"Created Button. Parent: %d", procedure->procData.newButtonParentUniqueId);

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

            HWND hwndInsertAfter = NULL;
            if (updatePosData.bringComponentToFront)
            {
                hwndInsertAfter = HWND_TOP;
            }

            if (hdwp == NULL)
            {
                NotifyFatalError(L"hdwp was NULL", L"ExecuteRenderProcedure (VirtualDOM.c)");
                break;
            }

            *hdwp = DeferWindowPos(*hdwp, targetHWND, hwndInsertAfter, x, y, w, h, flags);

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
                DEBUG_LOG(
                    L"Updated Icon of HWND %p (UniqueId %d): Resource Icon Id: %ls, DPI: %d",
                    (void *)targetHWND,
                    procedure->targetUniqueId,
                    cacheKey.iconId.stockIconId,
                    cacheKey.dpi);
            }

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

            DEBUG_LOG(
                L"Updated Cursor of HWND %p (UniqueId %d): %ls",
                (void *)targetHWND,
                procedure->targetUniqueId,
                procedure->procData.newCursorCacheKey);

            break;
        }
    }
}

void ExecuteRenderProcedures(RenderProcedure **procedures, int procedureCount, int updatePosNumber)
{
    DEBUG_LOG(L"RenderProcedure received.");

    HDWP hdwp = NULL;
    if (updatePosNumber > 0)
    {
        DEBUG_LOG(L"updatePosNumber > 0, using DeferWindowPos.");

        hdwp = BeginDeferWindowPos(updatePosNumber);
    }

    for (int i = 0; i < procedureCount; i++)
    {
        RenderProcedure *proc = procedures[i];
        ExecuteRenderProcedure(proc, &hdwp);
    }

    if (hdwp != NULL)
    {
        EndDeferWindowPos(hdwp);
    }
}
