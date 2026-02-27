#include "VirtualDOM.h"
#include "Cache.h"

#include <windows.h>

void ExecuteCCallRequest(CCallRequest *request, HDWP *hdwp)
{
    HWND target = request->targetHWND;

    switch (request->reqType)
    {
        case REQ_CREATE_WINDOW: {
            break;
        }
        case REQ_CREATE_BUTTON: {
            break;
        }
        case REQ_DESTROY_COMPONENT: {
            break;
        }
        case REQ_UPDATE_TEXT: {
            SetWindowTextW(target, request->reqData.newComponentText);

            break;
        }
        case REQ_UPDATE_POS: {
            UpdatePosReq updatePosReq = request->reqData.updatePosReq;

            int x = updatePosReq.hasNewLocation ? updatePosReq.newX : 0;
            int y = updatePosReq.hasNewLocation ? updatePosReq.newY : 0;
            int w = updatePosReq.hasNewSize ? updatePosReq.newWidth : 0;
            int h = updatePosReq.hasNewSize ? updatePosReq.newHeight : 0;

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
                *hdwp = DeferWindowPos(*hdwp, target, hwndInsertAfter, x, y, w, h, flags);
            }
            else
            {
                SetWindowPos(target, hwndInsertAfter, x, y, w, h, flags);
            }

            break;
        }
        case REQ_UPDATE_FONT: {
            SendMessageW(target, WM_SETFONT, (WPARAM)GetCachedFont(request->reqData.newFontCacheKey), 1);

            break;
        }
        case REQ_UPDATE_ICON: {
            SendMessageW(target, WM_SETICON, 1, (LPARAM)GetCachedIcon(request->reqData.newIconCacheKey));

            break;
        }
        case REQ_UPDATE_CURSOR: {
            SetClassLongPtrW(target, GCLP_HCURSOR, (LONG_PTR)GetCachedCursor(request->reqData.newCursorCacheKey));

            break;
        }
        case REQ_INVALIDATE_RECT_FULLY: {
            InvalidateRect(target, NULL, TRUE);

            break;
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
