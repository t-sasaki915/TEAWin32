#include "VirtualDOM.h"
#include "Cache.h"
#include "DPIAware.h"
#include "Event.h"
#include "Registry.h"
#include "TEAWin32.h"

#include <commctrl.h>
#include <stdio.h> // IWYU pragma: keep
#include <windows.h>

typedef struct
{
    HWND parentHWND;
    HDWP hdwp;
} DeferWindowPosContext;

typedef struct
{
    RenderProcedure *renderProcedure;
    DeferWindowPosContext *deferWindowPosContexts;
    int *deferWindowPosContextsCount;
    HWND *hwndsPendingToShow;
    int *hwndsPendingToShowCount;
    int procedureCount;
} RenderSession;

void ExecuteRenderProcedure(RenderSession *session)
{
    RenderProcedure *procedure = session->renderProcedure;

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

            LPCWSTR className;
            if (!GetCachedClassName(data.newWindowClassName, &className))
            {
                return;
            }

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

            session->hwndsPendingToShow[*session->hwndsPendingToShowCount] = newWindow;
            (*session->hwndsPendingToShowCount)++;

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
                return;
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
                return;
            }

            UpdatePosData updatePosData = procedure->procData.updatePosData;

            int dpi;
            if (!GetCachedDpi(targetHWND, &dpi))
            {
                return;
            }

            int x = updatePosData.hasNewLocation ? ResolvePixel(updatePosData.newX, dpi) : 0;
            int y = updatePosData.hasNewLocation ? ResolvePixel(updatePosData.newY, dpi) : 0;
            int w = updatePosData.hasNewSize ? ResolvePixel(updatePosData.newWidth, dpi) : 0;
            int h = updatePosData.hasNewSize ? ResolvePixel(updatePosData.newHeight, dpi) : 0;

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

            HWND parentOfTargetHWND = GetParent(targetHWND);
            HDWP hdwp = NULL;

            DEBUG_LOG(L"Looking for HDWP for HWND %p.", (void *)parentOfTargetHWND);
            for (int i = 0; i < *session->deferWindowPosContextsCount; i++)
            {
                if (session->deferWindowPosContexts[i].parentHWND == parentOfTargetHWND)
                {
                    hdwp = session->deferWindowPosContexts[i].hdwp;

                    DEBUG_LOG(L"Found HDWP for HWND %p: %p", (void *)parentOfTargetHWND, (void *)hdwp);

                    break;
                }
            }

            if (hdwp == NULL)
            {
                DEBUG_LOG(L"Creating HDWP for HWND %p.", (void *)parentOfTargetHWND);

                hdwp = BeginDeferWindowPos(session->procedureCount);

                if (hdwp == NULL)
                {
                    NotifyFatalError(L"BeginDeferWindowPos failed", L"ExecuteRenderProcedure (VirtualDOM.c)");
                    return;
                }

                int *contextsCountPtr = session->deferWindowPosContextsCount;
                session->deferWindowPosContexts[*contextsCountPtr].parentHWND = parentOfTargetHWND;
                session->deferWindowPosContexts[*contextsCountPtr].hdwp = hdwp;
                (*contextsCountPtr)++;
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
                return;
            }

            int dpi;
            if (!GetCachedDpi(targetHWND, &dpi))
            {
                return;
            }

            CachedFont cacheKey = procedure->procData.newFontCacheKey;
            cacheKey.absoluteFontSize = ResolvePoint(cacheKey.fontSize, dpi);

            HFONT font;
            if (!GetCachedFont(&cacheKey, &font))
            {
                return;
            }

            SendMessageW(targetHWND, WM_SETFONT, (WPARAM)font, 1);

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
                return;
            }

            CachedIcon cacheKey = procedure->procData.newIconCacheKey;
            int dpi;
            if (!GetCachedDpi(targetHWND, &dpi))
            {
                return;
            }
            cacheKey.dpi = dpi;

            HICON icon;
            if (!GetCachedIcon(&cacheKey, &icon))
            {
                return;
            }

            SendMessageW(targetHWND, WM_SETICON, 1, (LPARAM)icon);

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
                return;
            }

            HCURSOR cursor;
            if (!GetCachedCursor(&procedure->procData.newCursorCacheKey, &cursor))
            {
                return;
            }

            SetClassLongPtrW(targetHWND, GCLP_HCURSOR, (LONG_PTR)cursor);

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
                return;
            }

            HWNDRegistryEntry *regEntry = GetHWNDRegistryEntry(targetHWND);
            if (regEntry == NULL)
            {
                NotifyFatalError(L"GetHWNDRegistryEntry returned NULL", L"ExecuteRenderProcedure (VirtualDOM.c)");
                return;
            }

            regEntry->hasBackgroundColour = TRUE;
            regEntry->backgroundColour = procedure->procData.newBackgroundColour;

            InvalidateRect(targetHWND, NULL, TRUE);

            DEBUG_LOG(
                L"Updated Background Colour of HWND%p (UniqueId %d): %d",
                (void *)targetHWND,
                procedure->targetUniqueId,
                procedure->procData.newBackgroundColour);

            break;
        }
        case RENDER_PROC_TYPE_DESTROY_COMPONENT: {
            if (targetHWND == NULL)
            {
                NotifyFatalError(L"targetHWND was NULL", L"ExecuteRenderProcedure (VirtualDOM.c)");
                return;
            }

            DestroyWindow(targetHWND);

            DEBUG_LOG(L"Destroyed HWND %p (UniqueId %d)", (void *)targetHWND, procedure->targetUniqueId);

            break;
        }
    }
}

void ExecuteRenderProcedures(RenderProcedure *procedures, int procedureCount)
{
    DEBUG_LOG(L"Executing RenderProcedures.");

    DeferWindowPosContext *deferWindowPosContexts = malloc(procedureCount * sizeof(DeferWindowPosContext));
    int deferWindowPosContextsCount = 0;

    if (deferWindowPosContexts == NULL)
    {
        NotifyFatalError(L"deferWindowPosContexts malloc Failed", L"ExecuteRenderProcedures (VirtualDOM.c)");
        return;
    }

    HWND *hwndsPendingToShow = malloc(procedureCount * sizeof(HWND));
    int hwndsPendingToShowCount = 0;

    if (hwndsPendingToShow == NULL)
    {
        free(deferWindowPosContexts);

        NotifyFatalError(L"hwndsPendingToShow malloc Failed", L"ExecuteRenderProcedures (VirtualDOM.c)");
        return;
    }

    for (int i = 0; i < procedureCount; i++)
    {
        RenderSession session;
        ZeroMemory(&session, sizeof(session));
        session.renderProcedure = &procedures[i];
        session.deferWindowPosContexts = deferWindowPosContexts;
        session.deferWindowPosContextsCount = &deferWindowPosContextsCount;
        session.hwndsPendingToShow = hwndsPendingToShow;
        session.hwndsPendingToShowCount = &hwndsPendingToShowCount;
        session.procedureCount = procedureCount;

        ExecuteRenderProcedure(&session);
    }

    for (int i = 0; i < deferWindowPosContextsCount; i++)
    {
        HDWP hdwp = deferWindowPosContexts[i].hdwp;

        if (hdwp != NULL)
        {
            DEBUG_LOG(L"Ending DeferWindowPos: %p.", (void *)hdwp);

            EndDeferWindowPos(hdwp);
        }
    }

    for (int i = 0; i < hwndsPendingToShowCount; i++)
    {
        HWND hwnd = hwndsPendingToShow[i];

        if (hwnd != NULL)
        {
            DEBUG_LOG(L"Showing HWND %p.", (void *)hwnd);

            ShowWindow(hwnd, SW_SHOW);
            UpdateWindow(hwnd);
        }
    }

    free(deferWindowPosContexts);
    free(hwndsPendingToShow);

    DEBUG_LOG(L"Executed RenderProcedures.");
}

void RequestRender(RenderProcedure *procedures, int procedureCount)
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

    LPARAM lParam = (LPARAM)procedureCount;

    if (!PostMessageW(TEAWIN32_MANAGEMENT_HWND, WM_TEAWIN32_RENDER_REQUEST, (WPARAM)permanentProcedures, lParam))
    {
        free(permanentProcedures);
        NotifyFatalError(L"Failed to post WM_TEAWIN32_RENDER_REQUEST", L"RenderRequest (VirtualDOM.c)");
        return;
    }

    DEBUG_LOG(L"Requested render.");
}
