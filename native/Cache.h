#ifndef CACHE_H
#define CACHE_H

#include "DPIAware.h"

#include <windows.h>

typedef struct
{
    LPCWSTR fontName;
    ScalableValue fontSize;
    int dpi;
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

wchar_t *CreateTEAWin32WindowClassName(LPCWSTR userClass);

HFONT GetCachedFont(CachedFont *fontKey);

HCURSOR GetCachedCursor(CachedCursor *cacheKey);

HICON GetCachedIcon(CachedIcon *cacheKey);

void FinaliseClassCache(void);

void FinaliseFontCache(void);

void FinaliseCursorCache(void);

void FinaliseIconCache(void);

#endif
