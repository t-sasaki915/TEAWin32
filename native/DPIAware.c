#include "DPIAware.h"
#include "Event.h"
#include "Registry.h"
#include "TEAWin32.h"

#include <stdio.h> // IWYU pragma: keep
#include <windows.h>

typedef UINT(WINAPI *PGET_DPI_FOR_WINDOW)(HWND);
typedef BOOL(WINAPI *PSET_PROCESS_DPI_AWARENESS_CONTEXT)(void *);

static PGET_DPI_FOR_WINDOW GET_DPI_FOR_WINDOW_FUNC = NULL;

BOOL InitialiseDPIAwareFunctions(void)
{
    HMODULE user32 = GetModuleHandleW(L"user32.dll");
    if (user32 == NULL)
    {
        NotifyFatalError(L"Failed to load user32.dll", L"InitialiseDPIAwareFunctions (DPIAware.c)");
        return FALSE;
    }

    FARPROC setProcessDpiAwarenessContextPtr = GetProcAddress(user32, "SetProcessDpiAwarenessContext");
    if (setProcessDpiAwarenessContextPtr != NULL)
    {
        PSET_PROCESS_DPI_AWARENESS_CONTEXT setProcessDpiAwarenessContextFunc =
            (PSET_PROCESS_DPI_AWARENESS_CONTEXT)setProcessDpiAwarenessContextPtr;

        setProcessDpiAwarenessContextFunc((void *)-4); // DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2

        DEBUG_LOG(L"SetProcessDpiAwarenessContext found. Using DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2.");
    }
    else
    {
        SetProcessDPIAware();

        DEBUG_LOG(L"SetProcessDpiAwarenessContext not found. Using SetProcessDPIAware().");
    }

    FARPROC getDpiForWindowAddr = GetProcAddress(user32, "GetDpiForWindow");
    if (getDpiForWindowAddr != NULL)
    {
        GET_DPI_FOR_WINDOW_FUNC = (PGET_DPI_FOR_WINDOW)getDpiForWindowAddr;

        DEBUG_LOG(L"GetDpiForWindow found.");
    }

    return TRUE;
}

HICON GetHighDPIIcon(SHSTOCKICONID siid)
{
    SHSTOCKICONINFO iconInfo;
    iconInfo.cbSize = sizeof(iconInfo);

    SHGetStockIconInfo(siid, SHGSI_ICON, &iconInfo);

    return iconInfo.hIcon;
}

int GetDPI(HWND hwnd)
{
    if (GET_DPI_FOR_WINDOW_FUNC == NULL)
    {
        DEBUG_LOG(L"GET_DPI_FOR_WINDOW_FUNC == NULL. Checking DPI for HWND %p using GetDeviceCaps.", (void *)hwnd);

        HDC hdc = GetDC(hwnd);
        int dpi = GetDeviceCaps(hdc, LOGPIXELSX);
        ReleaseDC(hwnd, hdc);
        return dpi;
    }

    DEBUG_LOG(L"GET_DPI_FOR_WINDOW_FUNC /= NULL. Checking DPI for HWND %p using GetDpiForWindow.", (void *)hwnd);

    return GET_DPI_FOR_WINDOW_FUNC(hwnd);
}

BOOL GetCachedDpi(HWND hwnd, int *resultPtr)
{
    HWNDRegistryEntry *entry;
    if (!GetHWNDRegistryEntry(hwnd, &entry))
    {
        return FALSE;
    }

    DEBUG_LOG(L"Checking DPI for HWND %p using HWNDRegistry.", (void *)hwnd);

    *resultPtr = entry->dpi;

    return TRUE;
}

int ResolvePixel(ScalableValue scalable, int dpi)
{
    if (!scalable.isScalable)
    {
        return ((int)scalable.value);
    }

    return MulDiv((int)scalable.value, dpi, 96);
}

int ResolvePoint(ScalableValue scalable, int dpi)
{
    if (!scalable.isScalable)
    {
        return ((int)scalable.value);
    }

    return -MulDiv((int)scalable.value, dpi, 72);
}
