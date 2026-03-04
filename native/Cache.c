#include "Cache.h"
#include "DPIAware.h"
#include "Event.h"
#include "TEAWin32.h"

#include <windows.h>

#define CLASS_CACHE_MAX 1024
#define FONT_CACHE_MAX 1024
#define CURSOR_CACHE_MAX 1024
#define ICON_CACHE_MAX 1024

typedef struct
{
    LPCWSTR userClassName;
    wchar_t *fullClassNamePtr;
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

wchar_t *CreateTEAWin32WindowClassName(LPCWSTR userClass)
{
    for (int i = 0; i < CLASS_CACHE_COUNT; i++)
    {
        if (wcscmp(CLASS_CACHE[i].userClassName, userClass) == 0)
        {
            return CLASS_CACHE[i].fullClassNamePtr;
        }
    }

    wchar_t fullClassName[256];
    wchar_t *p = fullClassName;

    memcpy(p, TEAWIN32_INSTANCE_PID_STR, 8 * sizeof(wchar_t));
    p += 8;

    memcpy(p, TEAWIN32_WINDOW_CLASS_IDENTIFIER, TEAWIN32_WINDOW_CLASS_IDENTIFIER_LENGTH * sizeof(wchar_t));
    p += TEAWIN32_WINDOW_CLASS_IDENTIFIER_LENGTH;

    size_t userClassLength = wcslen(userClass);
    memcpy(p, userClass, (userClassLength + 1) * sizeof(wchar_t));

    wchar_t *permanentUserClassName = _wcsdup(userClass);
    wchar_t *permanentFullClassName = _wcsdup(fullClassName);

    if (CLASS_CACHE_COUNT >= CLASS_CACHE_MAX)
    {
        NotifyFatalError(L"CLASS_CACHE Overflow", L"CreateTEAWin32WindowClassName (Cache.c)");
        return NULL;
    }

    CLASS_CACHE[CLASS_CACHE_COUNT].userClassName = permanentUserClassName;
    CLASS_CACHE[CLASS_CACHE_COUNT].fullClassNamePtr = permanentFullClassName;
    CLASS_CACHE_COUNT++;

    WNDCLASSEXW wndClass;
    ZeroMemory(&wndClass, sizeof(wndClass));
    wndClass.cbSize = sizeof(wndClass);
    wndClass.lpszClassName = permanentFullClassName;
    wndClass.style = CS_VREDRAW | CS_HREDRAW;
    wndClass.hInstance = TEAWIN32_MAIN_INSTANCE;
    wndClass.lpfnWndProc = TEAWin32WndProc;

    if (!RegisterClassExW(&wndClass))
    {
        NotifyFatalError(L"RegisterClassExW Failed", L"CreateTEAWin32WindowClassName (Cache.c)");
        return NULL;
    }

    return permanentFullClassName;
}

HFONT GetCachedFont(CachedFont *fontKey)
{
    for (int i = 0; i < FONT_CACHE_COUNT; i++)
    {
        CachedFont entry = FONT_CACHE[i].fontCacheKey;

        BOOL matchFontName = fontKey->fontName == entry.fontName || wcscmp(fontKey->fontName, entry.fontName) == 0;
        BOOL matchFontSize = ScalableValue_Equals(fontKey->fontSize, entry.fontSize);
        BOOL matchIsItalic = fontKey->isItalic == entry.isItalic;
        BOOL matchIsUnderline = fontKey->isUnderline == entry.isUnderline;
        BOOL matchIsStrikeOut = fontKey->isStrikeOut == entry.isStrikeOut;
        BOOL matchDpi = fontKey->dpi == entry.dpi;
        if (!fontKey->fontSize.isScalable && !entry.fontSize.isScalable)
        {
            matchDpi = TRUE;
        }

        if (matchFontName && matchFontSize && matchIsItalic && matchIsUnderline && matchIsStrikeOut && matchDpi)
        {
            return FONT_CACHE[i].fontCacheHandle;
        }
    }

    if (FONT_CACHE_COUNT >= FONT_CACHE_MAX)
    {
        NotifyFatalError(L"FONT_CACHE Overflow", L"GetCachedFont (Cache.c)");
        return (HFONT)GetStockObject(DEFAULT_GUI_FONT);
    }

    wchar_t *permanentFontName = _wcsdup(fontKey->fontName);

    HFONT newFont = CreateFontW(
        ResolvePointForDpi(fontKey->fontSize, fontKey->dpi),
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
        return (HFONT)GetStockObject(DEFAULT_GUI_FONT);
    }

    FONT_CACHE[FONT_CACHE_COUNT].fontCacheHandle = newFont;
    FONT_CACHE[FONT_CACHE_COUNT].fontCacheKey.fontName = permanentFontName;
    FONT_CACHE[FONT_CACHE_COUNT].fontCacheKey.fontSize = fontKey->fontSize;
    FONT_CACHE[FONT_CACHE_COUNT].fontCacheKey.isItalic = fontKey->isItalic;
    FONT_CACHE[FONT_CACHE_COUNT].fontCacheKey.isUnderline = fontKey->isUnderline;
    FONT_CACHE[FONT_CACHE_COUNT].fontCacheKey.isStrikeOut = fontKey->isStrikeOut;
    FONT_CACHE[FONT_CACHE_COUNT].fontCacheKey.dpi = fontKey->dpi;

    FONT_CACHE_COUNT++;

    return newFont;
}

HCURSOR GetCachedCursor(CachedCursor *cacheKey)
{
    BOOL isIdGiven = IS_INTRESOURCE(cacheKey->cursorKey);

    for (int i = 0; i < CURSOR_CACHE_COUNT; i++)
    {
        CursorCacheEntry *entry = &CURSOR_CACHE[i];

        if (isIdGiven && entry->cursorCacheKey.isKeyId)
        {
            if (cacheKey->cursorKey == entry->cursorCacheKey.cursorKey)
            {
                return CURSOR_CACHE[i].cursorCacheHandle;
            }
        }

        if (!isIdGiven && !entry->cursorCacheKey.isKeyId)
        {
            if (wcscmp(cacheKey->cursorKey, entry->cursorCacheKey.cursorKey) == 0)
            {
                return CURSOR_CACHE[i].cursorCacheHandle;
            }
        }
    }

    if (CURSOR_CACHE_COUNT >= CURSOR_CACHE_MAX)
    {
        NotifyFatalError(L"CURSOR_CACHE Overflow", L"GetCachedCursor (Cache.c)");
        return LoadCursorW(NULL, (LPCWSTR)IDC_ARROW);
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
        return LoadCursorW(NULL, (LPCWSTR)IDC_ARROW);
    }

    CURSOR_CACHE[CURSOR_CACHE_COUNT].cursorCacheHandle = newCursor;
    CURSOR_CACHE[CURSOR_CACHE_COUNT].cursorCacheKey.cursorKey = permanentNameOrId;
    CURSOR_CACHE[CURSOR_CACHE_COUNT].cursorCacheKey.isKeyId = isIdGiven;

    CURSOR_CACHE_COUNT++;

    return newCursor;
}

HICON GetCachedIcon(CachedIcon *cacheKey)
{
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
                return entry->iconCacheHandle;
            }
        }
        else
        {
            LPCWSTR entryKey = entry->iconCacheKey.iconId.resourceId;
            LPCWSTR givenKey = cacheKey->iconId.resourceId;

            if (entryKey == givenKey || (!isIdGiven && !entry->iconCacheKey.isKeyId && wcscmp(entryKey, givenKey) == 0))
            {
                return entry->iconCacheHandle;
            }
        }
    }

    if (ICON_CACHE_COUNT >= ICON_CACHE_MAX)
    {
        NotifyFatalError(L"ICON_CACHE Overflow", L"GetCachedIcon (Cache.c)");
        return LoadIconW(NULL, (LPCWSTR)IDI_APPLICATION);
    }

    HICON newIcon;
    LPCWSTR permanentResourceId;

    if (cacheKey->iconType == STOCK_ICON)
    {
        newIcon = GetHighDPIIcon(cacheKey->iconId.stockIconId);

        if (newIcon == NULL)
        {
            NotifyFatalError(L"GetHighDPIIcon Failed", L"GetCachedIcon (Cache.c)");
            return LoadIconW(NULL, (LPCWSTR)IDI_APPLICATION);
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
            return LoadIconW(NULL, (LPCWSTR)IDI_APPLICATION);
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

    return newIcon;
}

void FinaliseClassCache(void)
{
    for (int i = 0; i < CLASS_CACHE_COUNT; i++)
    {
        ClassCacheEntry entry = CLASS_CACHE[i];

        UnregisterClassW(entry.fullClassNamePtr, TEAWIN32_MAIN_INSTANCE);

        free((void *)entry.userClassName);
        free(entry.fullClassNamePtr);
    }

    CLASS_CACHE_COUNT = 0;
}

void FinaliseFontCache(void)
{
    for (int i = 0; i < FONT_CACHE_COUNT; i++)
    {
        FontCacheEntry entry = FONT_CACHE[i];

        DeleteObject((HGDIOBJ)entry.fontCacheHandle);

        free((void *)entry.fontCacheKey.fontName);
    }

    FONT_CACHE_COUNT = 0;
}

void FinaliseCursorCache(void)
{
    for (int i = 0; i < CURSOR_CACHE_COUNT; i++)
    {
        CursorCacheEntry entry = CURSOR_CACHE[i];

        if (!entry.cursorCacheKey.isKeyId)
        {
            free((void *)entry.cursorCacheKey.cursorKey);
        }
    }

    CURSOR_CACHE_COUNT = 0;
}

void FinaliseIconCache(void)
{
    for (int i = 0; i < ICON_CACHE_COUNT; i++)
    {
        IconCacheEntry entry = ICON_CACHE[i];

        if (entry.iconCacheKey.iconType == STOCK_ICON)
        {
            DestroyIcon(entry.iconCacheHandle);
        }
        else
        {
            if (!entry.iconCacheKey.isKeyId)
            {
                free((void *)entry.iconCacheKey.iconId.resourceId);
            }
        }
    }

    ICON_CACHE_COUNT = 0;
}
