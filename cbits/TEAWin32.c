#include "TEAWin32.h"

#include <stdio.h>
#include <wchar.h>
#include <windows.h>

typedef struct
{
    LPCWSTR userClassName;
    wchar_t *fullClassNamePtr;
} ClassCacheEntry;

static wchar_t TEAWIN32_INSTANCE_PID_STR[9];
static ClassCacheEntry CLASS_CACHE[1024];
static int CLASS_CACHE_COUNT = 0;

DWORD TEAWIN32_INSTANCE_PID;
HINSTANCE TEAWIN32_MAIN_INSTANCE;
WNDPROC TEAWIN32_WNDPROC;

void InitialiseTEAWin32C(WNDPROC wndProc)
{
    TEAWIN32_INSTANCE_PID = GetCurrentProcessId();

    swprintf(TEAWIN32_INSTANCE_PID_STR, 9, L"%08X", TEAWIN32_INSTANCE_PID);

    TEAWIN32_MAIN_INSTANCE = GetModuleHandleW(NULL);

    TEAWIN32_WNDPROC = wndProc;
}

wchar_t *LookupClassCache(LPCWSTR userClassName)
{
    for (int i = 0; i < CLASS_CACHE_COUNT; i++)
    {
        if (wcscmp(CLASS_CACHE[i].userClassName, userClassName) == 0)
        {
            return CLASS_CACHE[i].fullClassNamePtr;
        }
    }

    return NULL;
}

LRESULT CALLBACK CallHaskellWndProc(HWND hwnd, UINT wMsg, WPARAM wParam, LPARAM lParam)
{
    if (TEAWIN32_WNDPROC != NULL)
    {
        switch (wMsg)
        {
            case WM_DESTROY:
            case WM_COMMAND:
            case WM_ERASEBKGND:
            case WM_DPICHANGED:
                return TEAWIN32_WNDPROC(hwnd, wMsg, wParam, lParam);
        }
    }

    return DefWindowProcW(hwnd, wMsg, wParam, lParam);
}

wchar_t *CreateTEAWin32WindowClassName(LPCWSTR userClass)
{
    wchar_t *cachedClassName = LookupClassCache(userClass);

    if (cachedClassName != NULL)
    {
        return cachedClassName;
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

    if (CLASS_CACHE_COUNT < 1024)
    {
        CLASS_CACHE[CLASS_CACHE_COUNT].userClassName = permanentUserClassName;
        CLASS_CACHE[CLASS_CACHE_COUNT].fullClassNamePtr = permanentFullClassName;
        CLASS_CACHE_COUNT++;
    }

    WNDCLASSEXW wndClass;
    ZeroMemory(&wndClass, sizeof(wndClass));
    wndClass.cbSize = sizeof(wndClass);
    wndClass.lpszClassName = permanentFullClassName;
    wndClass.style = CS_VREDRAW | CS_HREDRAW;
    wndClass.hInstance = TEAWIN32_MAIN_INSTANCE;
    wndClass.lpfnWndProc = CallHaskellWndProc;

    RegisterClassExW(&wndClass);

    return permanentFullClassName;
}

void FinaliseTEAWin32C(void)
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
