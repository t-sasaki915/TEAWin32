#include "Cache.h"

#include <windows.h>

typedef enum
{
    REQ_CREATE_WINDOW = 0,
    REQ_CREATE_BUTTON,
    REQ_DESTROY_COMPONENT,
    REQ_UPDATE_TEXT,
    REQ_UPDATE_POS,
    REQ_UPDATE_FONT,
    REQ_UPDATE_ICON,
    REQ_UPDATE_CURSOR,
    REQ_INVALIDATE_RECT_FULLY
} RequestType;

typedef struct
{
    LPCWSTR newWindowClassName;
    DWORD newWindowExStyles;
    DWORD newWindowStyles;
} CreateWindowReq;

typedef struct
{
    BOOL hasNewLocation;
    BOOL hasNewSize;
    BOOL bringComponentToFront;
    int newX;
    int newY;
    int newWidth;
    int newHeight;
} UpdatePosReq;

typedef struct
{
    RequestType reqType;
    HWND targetHWND; // Parent HWND if request of creation
    union {
        CreateWindowReq createWindowReq;
        LPCWSTR newComponentText;
        UpdatePosReq updatePosReq;
        CachedFont newFontCacheKey;
        CachedIcon newIconCacheKey;
        CachedCursor newCursorCacheKey;
    } reqData;
} CCallRequest;

void ExecuteCCallRequest(CCallRequest *request)
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

            SetWindowPos(target, NULL, x, y, w, h, flags);

            break;
        }
        case REQ_UPDATE_FONT: {
            break;
        }
        case REQ_UPDATE_ICON: {
            break;
        }
        case REQ_UPDATE_CURSOR: {
            break;
        }
        case REQ_INVALIDATE_RECT_FULLY: {
            break;
        }
    }
}
