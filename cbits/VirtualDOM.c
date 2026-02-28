#include "VirtualDOM.h"
#include "Cache.h"
#include "DPIAware.h"
#include "TEAWin32.h"

#include <stdio.h>
#include <windows.h>

void ExecuteCCallRequest(CCallRequest *request, HDWP *hdwp)
{
    HWND targetHWND = GetHWNDFromUniqueId(request->targetUniqueId);

    printf("%d, %d\n", request->reqType, request->targetUniqueId);

    switch (request->reqType)
    {
        case REQ_CREATE_WINDOW: {
            CreateWindowReq req = request->reqData.createWindowReq;

            wchar_t *className = CreateTEAWin32WindowClassName(req.newWindowClassName);

            HWND parentHWND = NULL;
            if (req.newWindowParentUniqueId != 0)
            {
                parentHWND = GetHWNDFromUniqueId(req.newWindowParentUniqueId);
            }

            HWND newWindow = CreateWindowExW(
                req.newWindowExStyles,
                className,
                L"",
                WS_CLIPCHILDREN | WS_CLIPSIBLINGS | req.newWindowStyles,
                CW_USEDEFAULT,
                CW_USEDEFAULT,
                0,
                0,
                parentHWND,
                NULL,
                TEAWIN32_MAIN_INSTANCE,
                0);

            RegisterHWNDUniqueId(newWindow, request->targetUniqueId);

            break;
        }
        case REQ_CREATE_BUTTON: {
            HWND newButton = CreateWindowW(
                L"BUTTON",
                L"",
                WS_TABSTOP | WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON | WS_CLIPSIBLINGS,
                0,
                0,
                0,
                0,
                GetHWNDFromUniqueId(request->reqData.newButtonParentUniqueId),
                NULL,
                TEAWIN32_MAIN_INSTANCE,
                0);

            RegisterHWNDUniqueId(newButton, request->targetUniqueId);

            break;
        }
        case REQ_DESTROY_COMPONENT: {
            if (targetHWND == NULL)
            {
                break;
            }

            DestroyWindow(targetHWND);
            UnregisterHWNDUniqueId(targetHWND);

            break;
        }
        case REQ_UPDATE_TEXT: {
            if (targetHWND == NULL)
            {
                break;
            }

            SetWindowTextW(targetHWND, request->reqData.newComponentText);

            break;
        }
        case REQ_UPDATE_POS: {
            if (targetHWND == NULL)
            {
                break;
            }

            UpdatePosReq updatePosReq = request->reqData.updatePosReq;

            int x = updatePosReq.hasNewLocation ? ResolveScalableValueForHWND(updatePosReq.newX, targetHWND) : 0;
            int y = updatePosReq.hasNewLocation ? ResolveScalableValueForHWND(updatePosReq.newY, targetHWND) : 0;
            int w = updatePosReq.hasNewSize ? ResolveScalableValueForHWND(updatePosReq.newWidth, targetHWND) : 0;
            int h = updatePosReq.hasNewSize ? ResolveScalableValueForHWND(updatePosReq.newHeight, targetHWND) : 0;

            DWORD flags = SWP_NOACTIVATE;
            if (!updatePosReq.hasNewLocation)
            {
                flags = flags | SWP_NOMOVE;
            }
            if (!updatePosReq.hasNewSize)
            {
                flags = flags | SWP_NOSIZE;
            }
            if (!updatePosReq.bringComponentToFront)
            {
                flags = flags | SWP_NOZORDER;
            }

            HWND hwndInsertAfter = NULL;
            if (updatePosReq.bringComponentToFront)
            {
                hwndInsertAfter = HWND_TOP;
            }

            if (hdwp != NULL)
            {
                *hdwp = DeferWindowPos(*hdwp, targetHWND, hwndInsertAfter, x, y, w, h, flags);
            }
            else
            {
                SetWindowPos(targetHWND, hwndInsertAfter, x, y, w, h, flags);
            }

            break;
        }
        case REQ_UPDATE_FONT: {
            if (targetHWND == NULL)
            {
                break;
            }

            CachedFont cacheKey = request->reqData.newFontCacheKey;
            cacheKey.scaleRatio = GetScaleFactorForHWND(targetHWND);

            SendMessageW(targetHWND, WM_SETFONT, (WPARAM)GetCachedFont(&cacheKey), 1);

            break;
        }
        case REQ_UPDATE_ICON: {
            if (targetHWND == NULL)
            {
                break;
            }

            CachedIcon cacheKey = request->reqData.newIconCacheKey;
            cacheKey.scaleRatio = GetScaleFactorForHWND(targetHWND);

            SendMessageW(targetHWND, WM_SETICON, 1, (LPARAM)GetCachedIcon(&cacheKey));

            break;
        }
        case REQ_UPDATE_CURSOR: {
            if (targetHWND == NULL)
            {
                break;
            }

            SetClassLongPtrW(targetHWND, GCLP_HCURSOR, (LONG_PTR)GetCachedCursor(&request->reqData.newCursorCacheKey));

            break;
        }
        case REQ_INVALIDATE_RECT_FULLY: {
            if (targetHWND == NULL)
            {
                break;
            }

            InvalidateRect(targetHWND, NULL, TRUE);

            break;
        }
        case REQ_SHOW_WINDOW: {
            if (targetHWND == NULL)
            {
                break;
            }

            ShowWindow(targetHWND, SW_SHOW);
            UpdateWindow(targetHWND);
        }
    }
}

void ExecuteCCallRequests(CCallRequest requests[], int requestSize, int updatePosNumber)
{
    HDWP hdwp = NULL;
    if (updatePosNumber > 0)
    {
        hdwp = BeginDeferWindowPos(updatePosNumber);
    }

    for (int i = 0; i < requestSize; i++)
    {
        CCallRequest *req = &requests[i];
        ExecuteCCallRequest(req, &hdwp);
    }

    if (hdwp != NULL)
    {
        EndDeferWindowPos(hdwp);
    }
}
