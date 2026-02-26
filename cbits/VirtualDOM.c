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
    RequestType requestType;
    HWND targetHWND; // Parent HWND if CreateRequest
    union {
        CreateWindowReq createWindowReq;
        LPCWSTR newComponentText;
        UpdatePosReq updatePosReq;
        CachedFont newFontCacheKey;
        int newIconCacheId;
        int newCursorCacheId;
    } data;
} CCallRequest;
