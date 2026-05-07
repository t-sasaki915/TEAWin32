#include "Cache.h"
#include "DPIAware.h"
#include "Error.h"
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

static ClassCacheEntry g_classCache[CLASS_CACHE_MAX];
static int g_classCacheCount = 0;
static FontCacheEntry g_fontCache[FONT_CACHE_MAX];
static int g_fontCacheCount = 0;
static CursorCacheEntry g_cursorCache[CURSOR_CACHE_MAX];
static int g_cursorCacheCount = 0;
static IconCacheEntry g_iconCache[ICON_CACHE_MAX];
static int g_iconCacheCount = 0;

BOOL GetCachedClassName(LPCWSTR className, void *resultPtr)
{
    DEBUG_LOG(L"Searching Class %ls from CLASS_CACHE.", className);

    for (int i = 0; i < g_classCacheCount; i++)
    {
        if (wcscmp(g_classCache[i].className, className) == 0)
        {
            DEBUG_LOG(L"Class %ls was cached in CLASS_CACHE. Reusing.", className);

            *(LPCWSTR *)resultPtr = className;

            return TRUE;
        }
    }

    LPCWSTR permanentClassName = _wcsdup(className);

    if (g_classCacheCount >= CLASS_CACHE_MAX)
    {
        TEAWIN32_ERROR(L"CLASS_CACHE Overflow");
        return FALSE;
    }

    g_classCache[g_classCacheCount].className = permanentClassName;
    g_classCacheCount++;

    WNDCLASSEXW wndClass;
    ZeroMemory(&wndClass, sizeof(wndClass));
    wndClass.cbSize = sizeof(wndClass);
    wndClass.lpszClassName = permanentClassName;
    wndClass.style = CS_VREDRAW | CS_HREDRAW;
    wndClass.hInstance = g_teaWin32MainInstance;
    wndClass.lpfnWndProc = TEAWin32WndProc;

    if (!RegisterClassExW(&wndClass))
    {
        WIN32_ERROR(L"RegisterClassExW Failed");

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

    for (int i = 0; i < g_fontCacheCount; i++)
    {
        CachedFont entry = g_fontCache[i].fontCacheKey;

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

            *resultPtr = g_fontCache[i].fontCacheHandle;

            return TRUE;
        }
    }

    if (g_fontCacheCount >= FONT_CACHE_MAX)
    {
        TEAWIN32_ERROR(L"FONT_CACHE Overflow");
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
        WIN32_ERROR(L"CreateFontW Failed");

        return FALSE;
    }

    g_fontCache[g_fontCacheCount].fontCacheHandle = newFont;
    g_fontCache[g_fontCacheCount].fontCacheKey.fontName = permanentFontName;
    g_fontCache[g_fontCacheCount].fontCacheKey.absoluteFontSize = fontKey->absoluteFontSize;
    g_fontCache[g_fontCacheCount].fontCacheKey.isItalic = fontKey->isItalic;
    g_fontCache[g_fontCacheCount].fontCacheKey.isUnderline = fontKey->isUnderline;
    g_fontCache[g_fontCacheCount].fontCacheKey.isStrikeOut = fontKey->isStrikeOut;

    g_fontCacheCount++;

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

    for (int i = 0; i < g_cursorCacheCount; i++)
    {
        CursorCacheEntry *entry = &g_cursorCache[i];

        if (isIdGiven && entry->cursorCacheKey.isKeyId)
        {
            if (cacheKey->cursorKey == entry->cursorCacheKey.cursorKey)
            {
                DEBUG_LOG(L"Cursor %d was cached in CURSOR_CACHE. Reusing.", cacheKey->cursorKey);

                *resultPtr = g_cursorCache[i].cursorCacheHandle;

                return TRUE;
            }
        }

        if (!isIdGiven && !entry->cursorCacheKey.isKeyId)
        {
            if (wcscmp(cacheKey->cursorKey, entry->cursorCacheKey.cursorKey) == 0)
            {
                DEBUG_LOG(L"Cursor %ls was cached in CURSOR_CACHE. Reusing.", cacheKey->cursorKey);

                *resultPtr = g_cursorCache[i].cursorCacheHandle;

                return TRUE;
            }
        }
    }

    if (g_cursorCacheCount >= CURSOR_CACHE_MAX)
    {
        TEAWIN32_ERROR(L"CURSOR_CACHE Overflow");
        return FALSE;
    }

    LPCWSTR permanentNameOrId = isIdGiven ? cacheKey->cursorKey : _wcsdup(cacheKey->cursorKey);
    HINSTANCE instanceToUse = isIdGiven ? NULL : g_teaWin32MainInstance;

    HCURSOR newCursor = LoadCursorW(instanceToUse, permanentNameOrId);

    if (newCursor == NULL)
    {
        if (!isIdGiven)
        {
            free((void *)permanentNameOrId);
        }

        WIN32_ERROR(L"LoadCursorW Failed");

        return FALSE;
    }

    g_cursorCache[g_cursorCacheCount].cursorCacheHandle = newCursor;
    g_cursorCache[g_cursorCacheCount].cursorCacheKey.cursorKey = permanentNameOrId;
    g_cursorCache[g_cursorCacheCount].cursorCacheKey.isKeyId = isIdGiven;

    g_cursorCacheCount++;

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

    for (int i = 0; i < g_iconCacheCount; i++)
    {
        IconCacheEntry *entry = &g_iconCache[i];

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

    if (g_iconCacheCount >= ICON_CACHE_MAX)
    {
        TEAWIN32_ERROR(L"ICON_CACHE Overflow");
        return FALSE;
    }

    HICON newIcon;
    LPCWSTR permanentResourceId;

    if (cacheKey->iconType == STOCK_ICON)
    {
        newIcon = GetHighDPIIcon(cacheKey->iconId.stockIconId);

        if (newIcon == NULL)
        {
            WIN32_ERROR(L"GetHighDPIIcon Failed");
            return FALSE;
        }
    }
    else
    {
        permanentResourceId = isIdGiven ? cacheKey->iconId.resourceId : _wcsdup(cacheKey->iconId.resourceId);

        newIcon = LoadIconW(g_teaWin32MainInstance, permanentResourceId);

        if (newIcon == NULL)
        {
            free((void *)permanentResourceId);

            WIN32_ERROR(L"LoadIconW Failed");
            return FALSE;
        }
    }

    g_iconCache[g_iconCacheCount].iconCacheHandle = newIcon;
    g_iconCache[g_iconCacheCount].iconCacheKey.iconType = cacheKey->iconType;
    g_iconCache[g_iconCacheCount].iconCacheKey.dpi = cacheKey->dpi;
    if (cacheKey->iconType == STOCK_ICON)
    {
        g_iconCache[g_iconCacheCount].iconCacheKey.iconId.stockIconId = cacheKey->iconId.stockIconId;
    }
    else
    {
        g_iconCache[g_iconCacheCount].iconCacheKey.iconId.resourceId = permanentResourceId;
        g_iconCache[g_iconCacheCount].iconCacheKey.isKeyId = isIdGiven;
    }

    g_iconCacheCount++;

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

    for (int i = 0; i < g_classCacheCount; i++)
    {
        ClassCacheEntry entry = g_classCache[i];

        DEBUG_LOG(L"Unregistering Class %ls.", entry.className);

        UnregisterClassW(entry.className, g_teaWin32MainInstance);

        free((void *)entry.className);
    }

    g_classCacheCount = 0;

    DEBUG_LOG(L"Finalised CLASS_CACHE.");
}

void FinaliseFontCache(void)
{
    DEBUG_LOG(L"Finalising FONT_CACHE.");

    for (int i = 0; i < g_fontCacheCount; i++)
    {
        FontCacheEntry entry = g_fontCache[i];

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

    g_fontCacheCount = 0;

    DEBUG_LOG(L"Finalised FONT_CACHE.");
}

void FinaliseCursorCache(void)
{
    DEBUG_LOG(L"Finalising CURSOR_CACHE.");

    for (int i = 0; i < g_cursorCacheCount; i++)
    {
        CursorCacheEntry entry = g_cursorCache[i];

        if (!entry.cursorCacheKey.isKeyId)
        {
            DEBUG_LOG(L"Releasing cursorKey pointer %ls.", entry.cursorCacheKey.cursorKey);

            free((void *)entry.cursorCacheKey.cursorKey);
        }
    }

    g_cursorCacheCount = 0;

    DEBUG_LOG(L"Finalised CURSOR_CACHE.");
}

void FinaliseIconCache(void)
{
    DEBUG_LOG(L"Finalising ICON_CACHE.");

    for (int i = 0; i < g_iconCacheCount; i++)
    {
        IconCacheEntry entry = g_iconCache[i];

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

    g_iconCacheCount = 0;

    DEBUG_LOG(L"Finalised ICON_CACHE.");
}
