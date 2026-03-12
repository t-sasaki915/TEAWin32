#ifndef VIRTUALDOM_H
#define VIRTUALDOM_H

#include "Cache.h"
#include "DPIAware.h"

#include <windows.h>

typedef enum
{
    RENDER_PROC_TYPE_CREATE_WINDOW = 0,
    RENDER_PROC_TYPE_CREATE_BUTTON,
    RENDER_PROC_TYPE_UPDATE_TEXT,
    RENDER_PROC_TYPE_UPDATE_POS,
    RENDER_PROC_TYPE_UPDATE_FONT,
    RENDER_PROC_TYPE_UPDATE_ICON,
    RENDER_PROC_TYPE_UPDATE_CURSOR,
    RENDER_PROC_TYPE_UPDATE_BACKGROUND_COLOUR,
    RENDER_PROC_TYPE_DESTROY_COMPONENT,
    RENDER_PROC_TYPE_SET_COMPONENT_CLICK_EVENT,
    RENDER_PROC_TYPE_UNSET_COMPONENT_CLICK_EVENT
} RenderProcedureType;

typedef struct
{
    int newWindowParentUniqueId;
    LPCWSTR newWindowClassName;
    DWORD newWindowExStyles;
    DWORD newWindowStyles;
} CreateWindowData;

typedef struct
{
    BOOL hasNewLocation;
    BOOL hasNewSize;
    BOOL bringComponentToFront;
    ScalableValue newX;
    ScalableValue newY;
    ScalableValue newWidth;
    ScalableValue newHeight;
} UpdatePosData;

typedef struct
{
    RenderProcedureType procType;
    int targetUniqueId; // New uniqueId if request of creation
    union {
        CreateWindowData createWindowData;
        int newButtonParentUniqueId;
        LPCWSTR newComponentText;
        UpdatePosData updatePosData;
        CachedFont newFontCacheKey;
        CachedIcon newIconCacheKey;
        CachedCursor newCursorCacheKey;
        DWORD newBackgroundColour;
    } procData;
} RenderProcedure;

void ExecuteRenderProcedures(RenderProcedure *procedures, int procedureCount);

#endif
