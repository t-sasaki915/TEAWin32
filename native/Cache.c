#include "Cache.h"
#include "DPIAware.h"
#include "Event.h"
#include "TEAWin32.h"

#include <stdio.h> // IWYU pragma: keep
#include <windows.h>

#define CLASS_CACHE_MAX 1024
#define FONT_CACHE_MAX 1024
#define CURSOR_CACHE_MAX 1024
#define ICON_CACHE_MAX 1024

typedef struct
{
    LPCWSTR className;
} ClassCacheEntry;

typedef struct
{
    CachedFont fontCacheKey;
    HFONT fontCacheHandle;
} FontCacheEntry;

typedef struct
{
    CachedCursor cursorCacheKey;
    HCURSOR cursorCacheHandle;
} CursorCacheEntry;

typedef struct
{
    CachedIcon iconCacheKey;
    HICON iconCacheHandle;
} IconCacheEntry;

static ClassCacheEntry CLASS_CACHE[CLASS_CACHE_MAX];
static int CLASS_CACHE_COUNT = 0;
static FontCacheEntry FONT_CACHE[FONT_CACHE_MAX];
static int FONT_CACHE_COUNT = 0;
static CursorCacheEntry CURSOR_CACHE[CURSOR_CACHE_MAX];
static int CURSOR_CACHE_COUNT = 0;
static IconCacheEntry ICON_CACHE[ICON_CACHE_MAX];
static int ICON_CACHE_COUNT = 0;

BOOL GetCachedClassName(LPCWSTR className, void *resultPtr)
{
    DEBUG_LOG(L"Searching Class %ls from CLASS_CACHE.", className);

    for (int i = 0; i < CLASS_CACHE_COUNT; i++)
    {
        if (wcscmp(CLASS_CACHE[i].className, className) == 0)
        {
            DEBUG_LOG(L"Class %ls was cached in CLASS_CACHE. Reusing.", className);

            *(LPCWSTR *)resultPtr = className;

            return TRUE;
        }
    }

    LPCWSTR permanentClassName = _wcsdup(className);

    if (CLASS_CACHE_COUNT >= CLASS_CACHE_MAX)
    {
        NotifyFatalError(L"CLASS_CACHE Overflow", L"CreateTEAWin32WindowClassName (Cache.c)");
        return FALSE;
    }

    CLASS_CACHE[CLASS_CACHE_COUNT].className = permanentClassName;
    CLASS_CACHE_COUNT++;

    WNDCLASSEXW wndClass;
    ZeroMemory(&wndClass, sizeof(wndClass));
    wndClass.cbSize = sizeof(wndClass);
    wndClass.lpszClassName = permanentClassName;
    wndClass.style = CS_VREDRAW | CS_HREDRAW;
    wndClass.hInstance = TEAWIN32_MAIN_INSTANCE;
    wndClass.lpfnWndProc = TEAWin32WndProc;

    if (!RegisterClassExW(&wndClass))
    {
        NotifyFatalError(L"RegisterClassExW Failed", L"CreateTEAWin32WindowClassName (Cache.c)");
        return FALSE;
    }

    DEBUG_LOG(L"Class %ls is added to CLASS_CACHE.", className);

    *(LPCWSTR *)resultPtr = className;

    return TRUE;
}

BOOL GetCachedFont(CachedFont *fontKey, HFONT *resultPtr)
{
    DEBUG_LOG(
        L"Searching Font %ls (Size %d, Italic %d, Underline %d, StrikeOut %d) from FONT_CACHE.",
        fontKey->fontName,
        fontKey->absoluteFontSize,
        fontKey->isItalic,
        fontKey->isUnderline,
        fontKey->isStrikeOut);

    for (int i = 0; i < FONT_CACHE_COUNT; i++)
    {
        CachedFont entry = FONT_CACHE[i].fontCacheKey;

        BOOL matchFontName = fontKey->fontName == entry.fontName || wcscmp(fontKey->fontName, entry.fontName) == 0;
        BOOL matchFontSize = fontKey->absoluteFontSize == entry.absoluteFontSize;
        BOOL matchIsItalic = fontKey->isItalic == entry.isItalic;
        BOOL matchIsUnderline = fontKey->isUnderline == entry.isUnderline;
        BOOL matchIsStrikeOut = fontKey->isStrikeOut == entry.isStrikeOut;

        if (matchFontName && matchFontSize && matchIsItalic && matchIsUnderline && matchIsStrikeOut)
        {
            DEBUG_LOG(
                L"Font %ls (Size %d, Italic %d, Underline %d, StrikeOut %d) was cached in FONT_CACHE. Reusing.",
                fontKey->fontName,
                fontKey->absoluteFontSize,
                fontKey->isItalic,
                fontKey->isUnderline,
                fontKey->isStrikeOut);

            *resultPtr = FONT_CACHE[i].fontCacheHandle;

            return TRUE;
        }
    }

    if (FONT_CACHE_COUNT >= FONT_CACHE_MAX)
    {
        NotifyFatalError(L"FONT_CACHE Overflow", L"GetCachedFont (Cache.c)");
        return FALSE;
    }

    wchar_t *permanentFontName = _wcsdup(fontKey->fontName);

    HFONT newFont = CreateFontW(
        fontKey->absoluteFontSize,
        0,
        0,
        0,
        FW_NORMAL,
        fontKey->isItalic,
        fontKey->isUnderline,
        fontKey->isStrikeOut,
        DEFAULT_CHARSET,
        OUT_DEFAULT_PRECIS,
        CLIP_DEFAULT_PRECIS,
        CLEARTYPE_QUALITY,
        DEFAULT_PITCH | FF_DONTCARE,
        permanentFontName);

    if (newFont == NULL)
    {
        NotifyFatalError(L"CreateFontW Failed", L"GetCachedFont (Cache.c)");
        return FALSE;
    }

    FONT_CACHE[FONT_CACHE_COUNT].fontCacheHandle = newFont;
    FONT_CACHE[FONT_CACHE_COUNT].fontCacheKey.fontName = permanentFontName;
    FONT_CACHE[FONT_CACHE_COUNT].fontCacheKey.absoluteFontSize = fontKey->absoluteFontSize;
    FONT_CACHE[FONT_CACHE_COUNT].fontCacheKey.isItalic = fontKey->isItalic;
    FONT_CACHE[FONT_CACHE_COUNT].fontCacheKey.isUnderline = fontKey->isUnderline;
    FONT_CACHE[FONT_CACHE_COUNT].fontCacheKey.isStrikeOut = fontKey->isStrikeOut;

    FONT_CACHE_COUNT++;

    DEBUG_LOG(
        L"Font %ls (Size %d, Italic %d, Underline %d, StrikeOut %d) is added to FONT_CACHE.",
        fontKey->fontName,
        fontKey->absoluteFontSize,
        fontKey->isItalic,
        fontKey->isUnderline,
        fontKey->isStrikeOut);

    *resultPtr = newFont;

    return TRUE;
}

BOOL GetCachedCursor(CachedCursor *cacheKey, HCURSOR *resultPtr)
{
    BOOL isIdGiven = IS_INTRESOURCE(cacheKey->cursorKey);

    if (isIdGiven)
    {
        DEBUG_LOG(L"Searching for Cursor %d from CURSOR_CACHE.", cacheKey->cursorKey);
    }
    else
    {
        DEBUG_LOG(L"Searching for Cursor %ls from CURSOR_CACHE.", cacheKey->cursorKey);
    }

    for (int i = 0; i < CURSOR_CACHE_COUNT; i++)
    {
        CursorCacheEntry *entry = &CURSOR_CACHE[i];

        if (isIdGiven && entry->cursorCacheKey.isKeyId)
        {
            if (cacheKey->cursorKey == entry->cursorCacheKey.cursorKey)
            {
                DEBUG_LOG(L"Cursor %d was cached in CURSOR_CACHE. Reusing.", cacheKey->cursorKey);

                *resultPtr = CURSOR_CACHE[i].cursorCacheHandle;

                return TRUE;
            }
        }

        if (!isIdGiven && !entry->cursorCacheKey.isKeyId)
        {
            if (wcscmp(cacheKey->cursorKey, entry->cursorCacheKey.cursorKey) == 0)
            {
                DEBUG_LOG(L"Cursor %ls was cached in CURSOR_CACHE. Reusing.", cacheKey->cursorKey);

                *resultPtr = CURSOR_CACHE[i].cursorCacheHandle;

                return TRUE;
            }
        }
    }

    if (CURSOR_CACHE_COUNT >= CURSOR_CACHE_MAX)
    {
        NotifyFatalError(L"CURSOR_CACHE Overflow", L"GetCachedCursor (Cache.c)");
        return FALSE;
    }

    LPCWSTR permanentNameOrId = isIdGiven ? cacheKey->cursorKey : _wcsdup(cacheKey->cursorKey);
    HINSTANCE instanceToUse = isIdGiven ? NULL : TEAWIN32_MAIN_INSTANCE;

    HCURSOR newCursor = LoadCursorW(instanceToUse, permanentNameOrId);

    if (newCursor == NULL)
    {
        if (!isIdGiven)
        {
            free((void *)permanentNameOrId);
        }

        NotifyFatalError(L"LoadCursorW Failed", L"GetCachedCursor (Cache.c)");
        return FALSE;
    }

    CURSOR_CACHE[CURSOR_CACHE_COUNT].cursorCacheHandle = newCursor;
    CURSOR_CACHE[CURSOR_CACHE_COUNT].cursorCacheKey.cursorKey = permanentNameOrId;
    CURSOR_CACHE[CURSOR_CACHE_COUNT].cursorCacheKey.isKeyId = isIdGiven;

    CURSOR_CACHE_COUNT++;

    if (isIdGiven)
    {
        DEBUG_LOG(L"Cursor %d is added to CURSOR_CACHE.", cacheKey->cursorKey);
    }
    else
    {
        DEBUG_LOG(L"Cursor %d is added to CURSOR_CACHE.", cacheKey->cursorKey);
    }

    *resultPtr = newCursor;

    return TRUE;
}

BOOL GetCachedIcon(CachedIcon *cacheKey, HICON *resultPtr)
{
    if (cacheKey->iconType == STOCK_ICON)
    {
        DEBUG_LOG(
            L"Searching for Stock Icon %d (DPI: %d) from ICON_CACHE.",
            cacheKey->iconId.stockIconId,
            cacheKey->dpi);
    }
    else
    {
        DEBUG_LOG(
            L"Searching for Resource Icon %ls (DPI: %d) from ICON_CACHE.",
            cacheKey->iconId.resourceId,
            cacheKey->dpi);
    }

    BOOL isIdGiven = FALSE;
    if (cacheKey->iconType == RESOURCE_ICON)
    {
        isIdGiven = IS_INTRESOURCE(cacheKey->iconId.resourceId);
    }

    for (int i = 0; i < ICON_CACHE_COUNT; i++)
    {
        IconCacheEntry *entry = &ICON_CACHE[i];

        if (entry->iconCacheKey.iconType != cacheKey->iconType || entry->iconCacheKey.dpi != cacheKey->dpi)
        {
            continue;
        }

        if (cacheKey->iconType == STOCK_ICON)
        {
            if (entry->iconCacheKey.iconId.stockIconId == cacheKey->iconId.stockIconId)
            {
                DEBUG_LOG(
                    L"Stock Icon %d (DPI: %d) was found in ICON_CACHE. Reusing.",
                    cacheKey->iconId.stockIconId,
                    cacheKey->dpi);

                *resultPtr = entry->iconCacheHandle;

                return TRUE;
            }
        }
        else
        {
            LPCWSTR entryKey = entry->iconCacheKey.iconId.resourceId;
            LPCWSTR givenKey = cacheKey->iconId.resourceId;

            if (entryKey == givenKey || (!isIdGiven && !entry->iconCacheKey.isKeyId && wcscmp(entryKey, givenKey) == 0))
            {
                DEBUG_LOG(
                    L"Resource Icon %ls (DPI: %d) was found in ICON_CACHE. Reusing.",
                    cacheKey->iconId.resourceId,
                    cacheKey->dpi);

                *resultPtr = entry->iconCacheHandle;

                return TRUE;
            }
        }
    }

    if (ICON_CACHE_COUNT >= ICON_CACHE_MAX)
    {
        NotifyFatalError(L"ICON_CACHE Overflow", L"GetCachedIcon (Cache.c)");
        return FALSE;
    }

    HICON newIcon;
    LPCWSTR permanentResourceId;

    if (cacheKey->iconType == STOCK_ICON)
    {
        newIcon = GetHighDPIIcon(cacheKey->iconId.stockIconId);

        if (newIcon == NULL)
        {
            NotifyFatalError(L"GetHighDPIIcon Failed", L"GetCachedIcon (Cache.c)");
            return FALSE;
        }
    }
    else
    {
        permanentResourceId = isIdGiven ? cacheKey->iconId.resourceId : _wcsdup(cacheKey->iconId.resourceId);

        newIcon = LoadIconW(TEAWIN32_MAIN_INSTANCE, permanentResourceId);

        if (newIcon == NULL)
        {
            free((void *)permanentResourceId);

            NotifyFatalError(L"LoadIconW Failed", L"GetCachedIcon (Cache.c)");
            return FALSE;
        }
    }

    ICON_CACHE[ICON_CACHE_COUNT].iconCacheHandle = newIcon;
    ICON_CACHE[ICON_CACHE_COUNT].iconCacheKey.iconType = cacheKey->iconType;
    ICON_CACHE[ICON_CACHE_COUNT].iconCacheKey.dpi = cacheKey->dpi;
    if (cacheKey->iconType == STOCK_ICON)
    {
        ICON_CACHE[ICON_CACHE_COUNT].iconCacheKey.iconId.stockIconId = cacheKey->iconId.stockIconId;
    }
    else
    {
        ICON_CACHE[ICON_CACHE_COUNT].iconCacheKey.iconId.resourceId = permanentResourceId;
        ICON_CACHE[ICON_CACHE_COUNT].iconCacheKey.isKeyId = isIdGiven;
    }

    ICON_CACHE_COUNT++;

    if (cacheKey->iconType == STOCK_ICON)
    {
        DEBUG_LOG(L"Stock Icon %d (DPI: %d) is added to ICON_CACHE.", cacheKey->iconId.stockIconId, cacheKey->dpi);
    }
    else
    {
        if (isIdGiven)
        {
            DEBUG_LOG(
                L"Resource Icon %d (DPI: %d) is added to ICON_CACHE.",
                cacheKey->iconId.resourceId,
                cacheKey->dpi);
        }
        else
        {
            DEBUG_LOG(
                L"Resource Icon %ls (DPI: %d) is added to ICON_CACHE.",
                cacheKey->iconId.resourceId,
                cacheKey->dpi);
        }
    }

    *resultPtr = newIcon;

    return TRUE;
}

void FinaliseClassCache(void)
{
    DEBUG_LOG(L"Finalising CLASS_CACHE.");

    for (int i = 0; i < CLASS_CACHE_COUNT; i++)
    {
        ClassCacheEntry entry = CLASS_CACHE[i];

        DEBUG_LOG(L"Unregistering Class %ls.", entry.className);

        UnregisterClassW(entry.className, TEAWIN32_MAIN_INSTANCE);

        free((void *)entry.className);
    }

    CLASS_CACHE_COUNT = 0;

    DEBUG_LOG(L"Finalised CLASS_CACHE.");
}

void FinaliseFontCache(void)
{
    DEBUG_LOG(L"Finalising FONT_CACHE.");

    for (int i = 0; i < FONT_CACHE_COUNT; i++)
    {
        FontCacheEntry entry = FONT_CACHE[i];

        DEBUG_LOG(
            L"Deleting Font %ls (Size %d, Italic %d, Underline %d, StrikeOut %d).",
            entry.fontCacheKey.fontName,
            entry.fontCacheKey.absoluteFontSize,
            entry.fontCacheKey.isItalic,
            entry.fontCacheKey.isUnderline,
            entry.fontCacheKey.isStrikeOut);

        DeleteObject((HGDIOBJ)entry.fontCacheHandle);

        free((void *)entry.fontCacheKey.fontName);
    }

    FONT_CACHE_COUNT = 0;

    DEBUG_LOG(L"Finalised FONT_CACHE.");
}

void FinaliseCursorCache(void)
{
    DEBUG_LOG(L"Finalising CURSOR_CACHE.");

    for (int i = 0; i < CURSOR_CACHE_COUNT; i++)
    {
        CursorCacheEntry entry = CURSOR_CACHE[i];

        if (!entry.cursorCacheKey.isKeyId)
        {
            DEBUG_LOG(L"Releasing cursorKey pointer %ls.", entry.cursorCacheKey.cursorKey);

            free((void *)entry.cursorCacheKey.cursorKey);
        }
    }

    CURSOR_CACHE_COUNT = 0;

    DEBUG_LOG(L"Finalised CURSOR_CACHE.");
}

void FinaliseIconCache(void)
{
    DEBUG_LOG(L"Finalising ICON_CACHE.");

    for (int i = 0; i < ICON_CACHE_COUNT; i++)
    {
        IconCacheEntry entry = ICON_CACHE[i];

        if (entry.iconCacheKey.iconType == STOCK_ICON)
        {
            DEBUG_LOG(
                L"Destroying Stock Icon %d (DPI: %d).",
                entry.iconCacheKey.iconId.stockIconId,
                entry.iconCacheKey.dpi);

            DestroyIcon(entry.iconCacheHandle);
        }
        else
        {
            if (!entry.iconCacheKey.isKeyId)
            {
                DEBUG_LOG(
                    L"Releasing resourceId pointer %ls (DPI: %d).",
                    entry.iconCacheKey.iconId.resourceId,
                    entry.iconCacheKey.dpi);

                free((void *)entry.iconCacheKey.iconId.resourceId);
            }
        }
    }

    ICON_CACHE_COUNT = 0;

    DEBUG_LOG(L"Finalised ICON_CACHE.");
}
