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

    printf("%d, %d\n", procedure->procType, procedure->targetUniqueId);

    switch (procedure->procType)
    {
        case RENDER_PROC_TYPE_CREATE_WINDOW: {
            CreateWindowData data = procedure->procData.createWindowData;

            wchar_t *className = CreateTEAWin32WindowClassName(data.newWindowClassName);

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

            break;
        }
        case RENDER_PROC_TYPE_DESTROY_COMPONENT: {
            if (targetHWND == NULL)
            {
                NotifyFatalError(L"targetHWND was NULL", L"ExecuteCCallRequest (VirtualDOM.c)");
                break;
            }

            DestroyWindow(targetHWND);

            break;
        }
        case RENDER_PROC_TYPE_UPDATE_TEXT: {
            if (targetHWND == NULL)
            {
                NotifyFatalError(L"targetHWND was NULL", L"ExecuteCCallRequest (VirtualDOM.c)");
                break;
            }

            SetWindowTextW(targetHWND, procedure->procData.newComponentText);

            break;
        }
        case RENDER_PROC_TYPE_UPDATE_POS: {
            if (targetHWND == NULL)
            {
                NotifyFatalError(L"targetHWND was NULL", L"ExecuteCCallRequest (VirtualDOM.c)");
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
                NotifyFatalError(L"hdwp was NULL", L"ExecuteCCallRequest (VirtualDOM.c)");
                break;
            }

            *hdwp = DeferWindowPos(*hdwp, targetHWND, hwndInsertAfter, x, y, w, h, flags);

            break;
        }
        case RENDER_PROC_TYPE_UPDATE_FONT: {
            if (targetHWND == NULL)
            {
                NotifyFatalError(L"targetHWND was NULL", L"ExecuteCCallRequest (VirtualDOM.c)");
                break;
            }

            CachedFont cacheKey = procedure->procData.newFontCacheKey;
            cacheKey.dpi = GetDPI(targetHWND);

            SendMessageW(targetHWND, WM_SETFONT, (WPARAM)GetCachedFont(&cacheKey), 1);

            break;
        }
        case RENDER_PROC_TYPE_UPDATE_ICON: {
            if (targetHWND == NULL)
            {
                NotifyFatalError(L"targetHWND was NULL", L"ExecuteCCallRequest (VirtualDOM.c)");
                break;
            }

            CachedIcon cacheKey = procedure->procData.newIconCacheKey;
            cacheKey.dpi = GetDPI(targetHWND);

            SendMessageW(targetHWND, WM_SETICON, 1, (LPARAM)GetCachedIcon(&cacheKey));

            break;
        }
        case RENDER_PROC_TYPE_UPDATE_CURSOR: {
            if (targetHWND == NULL)
            {
                NotifyFatalError(L"targetHWND was NULL", L"ExecuteCCallRequest (VirtualDOM.c)");
                break;
            }

            SetClassLongPtrW(
                targetHWND,
                GCLP_HCURSOR,
                (LONG_PTR)GetCachedCursor(&procedure->procData.newCursorCacheKey));

            break;
        }
    }
}

void ExecuteRenderProcedures(RenderProcedure **procedures, int procedureCount, int updatePosNumber)
{
    HDWP hdwp = NULL;
    if (updatePosNumber > 0)
    {
        hdwp = BeginDeferWindowPos(updatePosNumber);
    }

    for (int i = 0; i < procedureCount; i++)
    {
        RenderProcedure proc = *procedures[i];
        ExecuteRenderProcedure(&proc, &hdwp);
    }

    if (hdwp != NULL)
    {
        EndDeferWindowPos(hdwp);
    }
}
