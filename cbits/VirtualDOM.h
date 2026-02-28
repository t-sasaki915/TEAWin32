#ifndef VIRTUALDOM_H
#define VIRTUALDOM_H

#include "Cache.h"
#include "DPIAware.h"

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
    REQ_INVALIDATE_RECT_FULLY,
    REQ_SHOW_WINDOW
} RequestType;

typedef struct
{
    int newWindowParentUniqueId;
    LPCWSTR newWindowClassName;
    DWORD newWindowExStyles;
    DWORD newWindowStyles;
} CreateWindowReq;

typedef struct
{
    BOOL hasNewLocation;
    BOOL hasNewSize;
    BOOL bringComponentToFront;
    CScalableValue newX;
    CScalableValue newY;
    CScalableValue newWidth;
    CScalableValue newHeight;
} UpdatePosReq;

typedef struct
{
    RequestType reqType;
    int targetUniqueId; // New uniqueId if request of creation
    union {
        CreateWindowReq createWindowReq;
        int newButtonParentUniqueId;
        LPCWSTR newComponentText;
        UpdatePosReq updatePosReq;
        CachedFont newFontCacheKey;
        CachedIcon newIconCacheKey;
        CachedCursor newCursorCacheKey;
    } reqData;
} CCallRequest;

#endif
