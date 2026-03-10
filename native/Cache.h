#ifndef CACHE_H
#define CACHE_H

#include "DPIAware.h"

#include <windows.h>

typedef struct
{
    LPCWSTR fontName;
    int absoluteFontSize;
    ScalableValue fontSize;
    BOOL isItalic;
    BOOL isUnderline;
    BOOL isStrikeOut;
} CachedFont;

typedef struct
{
    LPCWSTR cursorKey;
    BOOL isKeyId;
} CachedCursor;

typedef enum
{
    RESOURCE_ICON = 0,
    STOCK_ICON = 1
} CachedIconType;

typedef struct
{
    CachedIconType iconType;
    int dpi;
    union {
        SHSTOCKICONID stockIconId;
        LPCWSTR resourceId;
    } iconId;
    BOOL isKeyId;
} CachedIcon;

BOOL GetCachedClassName(LPCWSTR className, void *resultPtr);

BOOL GetCachedFont(CachedFont *fontKey, HFONT *resultPtr);

BOOL GetCachedCursor(CachedCursor *cacheKey, HCURSOR *resultPtr);

BOOL GetCachedIcon(CachedIcon *cacheKey, HICON *resultPtr);

void FinaliseClassCache(void);

void FinaliseFontCache(void);

void FinaliseCursorCache(void);

void FinaliseIconCache(void);

#endif
